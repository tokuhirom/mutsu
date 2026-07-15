//! Token/rule methods as first-class callables, plus custom-HOW subrule
//! dispatch (the Metamodel::GrammarHOW `find_method` protocol).
//!
//! `Grammar.^find_method("tok")` returns a `Routine { is_regex: true }` stub.
//! Calling that stub with a Match cursor as the first argument must actually
//! RUN the token at the cursor position (rakudo: regex methods take a cursor
//! invocant and return a new cursor). This is what lets a custom grammar HOW's
//! `find_method` wrap token dispatch in a profiling closure that itself calls
//! `$meth($cursor)` (roast integration/advent2011-day07.t).

use std::cell::RefCell;
use std::collections::HashSet;

use super::super::*;
use super::regex_helpers::NamedRegexLookupSpec;

/// The inner regex captures of the most recent token-method call
/// (`run_token_method_at`), identified by (pkg, name, from, to) in the
/// original string. The custom-HOW subrule hook uses this to rebuild the full
/// capture structure from the Match a `find_method` wrapper returned — the
/// Match value itself does not carry `RegexCaptures`.
struct TokenMethodMatch {
    pkg: String,
    name: String,
    from: usize,
    to: usize,
    caps: RegexCaptures,
}

thread_local! {
    static LAST_TOKEN_METHOD_MATCH: RefCell<Option<TokenMethodMatch>> =
        const { RefCell::new(None) };
}

impl Interpreter {
    /// Call a grammar token/rule as a method value: `$meth($cursor, |args)`.
    /// Returns `None` when this Routine does not name a token in `pkg` (the
    /// caller falls through to regular Routine dispatch), `Some(Ok(Match))`
    /// when the token matched at the cursor position, and `Some(Ok(Nil))` on
    /// no match.
    pub(crate) fn try_call_token_method_value(
        &mut self,
        pkg: &str,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if pkg.is_empty() || pkg == "GLOBAL" {
            return None;
        }
        if self.resolve_token_defs_in_pkg(name, pkg).is_empty() {
            return None;
        }
        // Cursor: a Match instance (orig + to = current position) or a plain Str.
        let (text, pos) = match args.first().map(Value::view) {
            Some(ValueView::Instance {
                class_name,
                attributes,
                ..
            }) if class_name.resolve() == "Match" => {
                let attrs = attributes.as_map();
                let orig = attrs.get("orig").map(|v| v.to_string_value())?;
                let to = attrs
                    .get("to")
                    .and_then(|v| v.as_int())
                    .filter(|t| *t >= 0)
                    .unwrap_or(0) as usize;
                (orig, to)
            }
            Some(ValueView::Str(s)) => (s.to_string(), 0usize),
            _ => return None,
        };
        Some(self.run_token_method_at(pkg, name, &args[1..], &text, pos))
    }

    /// Run token `pkg::name` anchored at char position `pos` of `text` and
    /// build a Match value (from = pos). Publishes the inner `RegexCaptures`
    /// to the thread-local side channel for the custom-HOW subrule hook.
    fn run_token_method_at(
        &mut self,
        pkg: &str,
        name: &str,
        extra_args: &[Value],
        text: &str,
        pos: usize,
    ) -> Result<Value, RuntimeError> {
        let tail: String = text.chars().skip(pos).collect();
        let saved_pkg = self.current_package();
        let saved_topic = self.env.get("_").cloned();
        self.set_current_package(pkg.to_string());
        self.env.insert("_".to_string(), Value::str(tail.clone()));
        let pattern_res = self.eval_token_call_values(name, extra_args);
        self.set_current_package(saved_pkg);
        match saved_topic {
            Some(t) => {
                self.env.insert("_".to_string(), t);
            }
            None => {
                self.env.remove("_");
            }
        }
        let Some(pattern) = pattern_res? else {
            return Ok(Value::NIL);
        };
        let Some(parsed) = self.parse_regex(&pattern) else {
            return Ok(Value::NIL);
        };
        let tail_chars: Vec<char> = tail.chars().collect();
        let matches = self.regex_match_ends_from_caps_in_pkg(&parsed, &tail_chars, 0, pkg);
        // Matches come HIGHEST FIRST: the first entry is the token's best
        // (ratcheted) match, which is what a cursor method call returns.
        let Some((end_rel, caps)) = matches.into_iter().next() else {
            return Ok(Value::NIL);
        };
        let matched: String = tail_chars[..end_rel].iter().collect();
        let m = Value::make_match_object_full_q(
            matched,
            pos as i64,
            (pos + end_rel) as i64,
            &caps.positional,
            &caps.named,
            &caps.named_subcaps,
            &caps.positional_subcaps,
            &caps.positional_quantified,
            &caps.positional_nil,
            Some(text),
            &caps.named_quantified,
        );
        LAST_TOKEN_METHOD_MATCH.with(|slot| {
            *slot.borrow_mut() = Some(TokenMethodMatch {
                pkg: pkg.to_string(),
                name: name.to_string(),
                from: pos,
                to: pos + end_rel,
                caps,
            });
        });
        Ok(m)
    }

    /// Custom-HOW subrule dispatch: when the dispatch package was declared as
    /// a grammar under an EXPORTHOW metaclass with a user `find_method`, route
    /// the subrule `<name>` through it (Metamodel::GrammarHOW protocol): call
    /// `HOW.find_method(TypeObj, name)`; when it returns a wrapper CODE object
    /// (not the plain method), invoke the wrapper with a cursor at `pos` and
    /// convert the Match it returns back into an engine candidate.
    ///
    /// Returns `None` to fall through to the normal token path (no custom HOW,
    /// or `find_method` returned the method unwrapped), `Some(vec![])` for a
    /// dispatched non-match (or an error, published via PENDING_REGEX_ERROR),
    /// and `Some(vec![(end, caps)])` for a dispatched match.
    ///
    /// TODO: left-recursive rules bypass the LR seed-growing loop on this
    /// path; a left-recursive grammar under a profiling HOW would recurse.
    pub(super) fn try_custom_how_subrule_dispatch(
        &self,
        spec: &NamedRegexLookupSpec,
        chars: &[char],
        pos: usize,
        pkg: &str,
        arg_values: &[Value],
    ) -> Option<Vec<(usize, RegexCaptures)>> {
        let how = self.registry().grammar_custom_how.get(pkg).cloned()?;
        if spec.lookup_name.is_empty()
            || spec.lookup_name.contains("::")
            || !spec
                .lookup_name
                .chars()
                .all(|c| c.is_alphanumeric() || c == '_' || c == '-')
        {
            return None;
        }
        // Scratch interpreter (mirrors `try_regex_subrule_as_method`): the
        // regex engine runs on `&self`, but user `find_method` / wrapper code
        // needs a mutable interpreter. Shared-cell values (module `our` vars)
        // keep mutations visible to the parent.
        let mut interp = Interpreter {
            env: self.env.clone(),
            current_package: Arc::new(RwLock::new(pkg.to_string())),
            ..Self::new_regex_scratch()
        };
        self.copy_full_registry_into(&mut interp);
        if self.test_module_loaded() {
            interp.loaded_modules = self.loaded_modules.clone();
            interp.tap.ensure_state();
        }
        let typeobj = Value::package(crate::symbol::Symbol::intern(pkg));
        let meth = match interp.call_method_with_values(
            how,
            "find_method",
            vec![typeobj, Value::str(spec.lookup_name.clone())],
        ) {
            Ok(v) => v,
            Err(e) => {
                crate::runtime::regex_parse::PENDING_REGEX_ERROR
                    .with(|slot| *slot.borrow_mut() = Some(e));
                return Some(Vec::new());
            }
        };
        // Only a wrapper CODE object diverts dispatch; the plain method value
        // (a Routine stub) means "unwrapped" — use the normal engine path.
        if !matches!(meth.view(), ValueView::Sub(_)) {
            return None;
        }
        let text: String = chars.iter().collect();
        let cursor = Value::make_match_object_full_q(
            String::new(),
            pos as i64,
            pos as i64,
            &[],
            &HashMap::new(),
            &HashMap::new(),
            &[],
            &[],
            &[],
            Some(&text),
            &HashSet::new(),
        );
        let mut call_args = vec![cursor];
        call_args.extend(arg_values.iter().cloned());
        LAST_TOKEN_METHOD_MATCH.with(|slot| slot.borrow_mut().take());
        let result = match interp.call_sub_value(meth, call_args, false) {
            Ok(v) => v,
            Err(e) => {
                crate::runtime::regex_parse::PENDING_REGEX_ERROR
                    .with(|slot| *slot.borrow_mut() = Some(e));
                return Some(Vec::new());
            }
        };
        let side = LAST_TOKEN_METHOD_MATCH.with(|slot| slot.borrow_mut().take());
        // The wrapper must return a Match/cursor; read its extent.
        let to_abs = if let ValueView::Instance { attributes, .. } = result.view() {
            attributes
                .as_map()
                .get("to")
                .and_then(|t| t.as_int())
                .filter(|&t| t >= pos as i64 && t <= chars.len() as i64)
                .map(|t| t as usize)
        } else {
            None
        };
        let Some(to_abs) = to_abs else {
            return Some(Vec::new());
        };
        // Prefer the side-channel captures from the actual token run inside
        // the wrapper; a wrapper that fabricated its own Match still advances
        // the parse by its extent (with a plain-text capture).
        let inner_caps = match side {
            Some(t)
                if t.pkg == pkg
                    && t.name == spec.lookup_name
                    && t.from == pos
                    && t.to == to_abs =>
            {
                t.caps
            }
            _ => RegexCaptures {
                matched: chars[pos..to_abs].iter().collect(),
                to: to_abs - pos,
                ..RegexCaptures::default()
            },
        };
        let sym = inner_caps.sym.clone();
        Some(Self::build_named_candidates_from_inner(
            vec![(to_abs - pos, inner_caps)],
            pos,
            chars,
            spec,
            sym.as_ref(),
        ))
    }
}
