//! Token/rule methods as first-class callables, plus custom-HOW subrule
//! dispatch (the Metamodel::GrammarHOW `find_method` protocol).
//!
//! `Grammar.^find_method("tok")` returns a `Routine { is_regex: true }` stub.
//! Calling that stub with a Match cursor as the first argument must actually
//! RUN the token at the cursor position (rakudo: regex methods take a cursor
//! invocant and return a new cursor). This is what lets a custom grammar HOW's
//! `find_method` wrap token dispatch in a profiling closure that itself calls
//! `$meth($cursor)` (roast integration/advent2011-day07.t).

use std::cell::{Cell, RefCell};

use super::super::*;
use super::regex_helpers::{CODE_ATOMS_INERT, LTM_DECLARATIVE_MODE, NamedRegexLookupSpec};

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
        pkg: Symbol,
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
        let (text, pos) = match args.first() {
            Some(m) if m.is_match_instance() => {
                // A cursor that has not started matching (`!cursor_init`'s
                // `:c`, marked by `$!from == -1`) means SCAN forward from its
                // position rather than anchor at it -- rakudo's own
                // discriminator, and the only case where a token method call
                // is not anchored. See `regex_cursor`.
                if let Some((orig, start, false)) = Self::cursor_call_position(m) {
                    return Some(self.run_token_method_scanning(
                        pkg,
                        name,
                        &args[1..],
                        &orig,
                        start,
                    ));
                }
                let orig = m.match_orig().map(|v| v.to_string_value())?;
                let to = m.match_to().filter(|t| *t >= 0).unwrap_or(0) as usize;
                (orig, to)
            }
            Some(v) if matches!(v.view(), ValueView::Str(_)) => (v.to_string_value(), 0usize),
            _ => return None,
        };
        Some(self.run_token_method_at(pkg, name, &args[1..], &text, pos))
    }

    /// A token method called on a not-yet-started cursor: try `run_token_method_at`
    /// at each position from `start` on, returning the first cursor that
    /// matched, and a failed cursor if none did. This is the scanning half of
    /// the cursor protocol (#7883); the anchored half is `run_token_method_at`
    /// itself.
    fn run_token_method_scanning(
        &mut self,
        pkg: Symbol,
        name: &str,
        extra_args: &[Value],
        text: &str,
        start: usize,
    ) -> Result<Value, RuntimeError> {
        let len = text.chars().count();
        for pos in start..=len {
            let m = self.run_token_method_at(pkg, name, extra_args, text, pos)?;
            if m.is_match_instance() {
                return Ok(m);
            }
        }
        Ok(Self::cursor_failure(pkg.as_str(), text, start))
    }

    /// Run token `pkg::name` anchored at char position `pos` of `text` and
    /// build a Match value (from = pos). Publishes the inner `RegexCaptures`
    /// to the thread-local side channel for the custom-HOW subrule hook.
    fn run_token_method_at(
        &mut self,
        pkg: Symbol,
        name: &str,
        extra_args: &[Value],
        text: &str,
        pos: usize,
    ) -> Result<Value, RuntimeError> {
        // `Grammar.^find_method($name).wrap(...)` returns a Regex object for a
        // token/rule/regex. The regex engine normally evaluates token bodies
        // directly, so ordinary method-wrap dispatch would never see that
        // call. Re-enter the wrapper chain through the same `callsame` frame
        // used for wrapped subs, with a synthetic callable whose terminal
        // invokes this token at the current cursor.
        if let Some(chain) = self.token_method_wrap_chain(pkg.as_str(), name) {
            return self.call_wrapped_token_method(pkg, name, extra_args, text, pos, &chain);
        }

        self.run_token_method_at_unwrapped(pkg, name, extra_args, text, pos)
    }

    pub(super) fn token_method_wrap_chain(
        &self,
        receiver_pkg: &str,
        name: &str,
    ) -> Option<Vec<(u64, Value)>> {
        // The regex engine asks this for every named atom and `<.ws>` it
        // visits, and the walk below clones the receiver's whole MRO into
        // `String`s plus two more per owner for the registry key. With no
        // method wrap installed anywhere (the overwhelmingly common case)
        // the answer is known without any of that: bail on the empty table.
        if !self.has_any_wrap_chains() {
            return None;
        }
        // A wrapper is user code. The LTM prefix measurement and the
        // failure-position probe are side-effect-free passes (ADR-0009) that
        // re-walk the pattern purely to measure it; Rakudo's NFA measures a
        // wrapped token by its own body and never enters the wrapper. Running
        // the wrapper there made each match of `<word>` call it twice (#9151).
        if LTM_DECLARATIVE_MODE.with(Cell::get) || CODE_ATOMS_INERT.with(Cell::get) {
            return None;
        }
        let owners = self.mro_readonly(receiver_pkg);
        owners
            .into_iter()
            .find_map(|owner| self.get_method_wrap_chain(&owner, name, 0))
    }

    pub(super) fn token_method_has_wrap_chain(&self, receiver_pkg: &str, name: &str) -> bool {
        self.token_method_wrap_chain(receiver_pkg, name).is_some()
    }

    fn call_wrapped_token_method(
        &mut self,
        pkg: Symbol,
        name: &str,
        extra_args: &[Value],
        text: &str,
        pos: usize,
        chain: &[(u64, Value)],
    ) -> Result<Value, RuntimeError> {
        if chain.is_empty() {
            return Err(RuntimeError::new(
                "Cannot dispatch an empty token method wrap chain",
            ));
        }
        let cursor = Value::make_match_object_full(
            pos as i64,
            pos as i64,
            &[],
            &Default::default(),
            MatchTarget::new(text),
        );
        let mut call_args = vec![cursor];
        call_args.extend(extra_args.iter().cloned());

        let mut original_env = crate::env::Env::new();
        original_env.insert(
            "__mutsu_token_method_wrapper_package".to_string(),
            Value::str(pkg.to_string()),
        );
        original_env.insert(
            "__mutsu_token_method_wrapper_name".to_string(),
            Value::str(name.to_string()),
        );
        let original = Value::make_sub(
            pkg,
            Symbol::intern(name),
            crate::value::empty_params(),
            crate::value::empty_param_defs(),
            Vec::new(),
            false,
            original_env,
        );
        let original_id = original
            .as_sub()
            .map(|data| data.id)
            .ok_or_else(|| RuntimeError::new("Synthetic token wrapper terminal is not a Sub"))?;
        let mut remaining = Vec::with_capacity(chain.len());
        for i in (0..chain.len() - 1).rev() {
            remaining.push(chain[i].1.clone());
        }
        remaining.push(original);
        let outermost = chain
            .last()
            .map(|(_, value)| value.clone())
            .ok_or_else(|| RuntimeError::new("Cannot dispatch an empty token method wrap chain"))?;
        self.push_wrap_dispatch_frame(super::super::WrapDispatchFrame {
            sub_id: original_id,
            remaining,
            args: call_args.clone(),
            arg_sources: None,
            dispatch_token: 0,
        });
        let result = self.call_sub_value(outermost, call_args, false);
        self.pop_wrap_dispatch_frame();
        result
    }

    /// Run the token body without consulting a `.wrap()` chain. The synthetic
    /// terminal used by `run_token_method_at` calls this after `callsame` has
    /// advanced through the wrapper frame.
    pub(crate) fn run_token_method_at_unwrapped(
        &mut self,
        pkg: Symbol,
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
        // Matched against the whole subject from `pos` (ADR-0016 P1), so the
        // captures this publishes are already in absolute coordinates.
        let target = MatchTarget::new(text);
        let _target_scope = super::regex_helpers::MatchTargetScope::enter(target.clone());
        // A token/rule is a named grammar routine for backtrace purposes even
        // though its body is evaluated through the regex engine rather than
        // ordinary method dispatch. Keep that frame live while matching so a
        // hidden wrapper can identify the rule that called it (`TOP`, in
        // Grammar::PrettyErrors).
        self.push_routine_with_location(
            pkg,
            Symbol::intern(name),
            self.current_source_line(),
            self.executing_source_file_sym(),
            None,
        );
        let matches = self.regex_match_ends_from_caps_in_pkg(&parsed, target.chars(), pos, pkg);
        // Matches come HIGHEST FIRST: the first entry is the token's best
        // (ratcheted) match, which is what a cursor method call returns.
        let Some((end, mut caps)) = matches.into_iter().next() else {
            self.routine_stack.pop();
            return Ok(Value::NIL);
        };
        self.routine_stack.pop();
        caps.set_target(Some(target.clone()));
        let m = Value::make_match_object_full(
            pos as i64,
            end as i64,
            &caps.positional,
            &caps.named,
            target,
        );
        LAST_TOKEN_METHOD_MATCH.with(|slot| {
            *slot.borrow_mut() = Some(TokenMethodMatch {
                pkg: pkg.to_string(),
                name: name.to_string(),
                from: pos,
                to: end,
                caps,
            });
        });
        Ok(m)
    }

    /// Apply a `.wrap()` chain while a regex subrule is being matched. This is
    /// the regex-engine counterpart of `run_token_method_at`: the ordinary
    /// parser path evaluates token patterns directly instead of calling the
    /// first-class Regex value, so it needs to turn the wrapped token result
    /// back into the engine's `(end, captures)` representation here.
    pub(super) fn try_wrapped_token_subrule_dispatch(
        &mut self,
        spec: &NamedRegexLookupSpec,
        chars: &[char],
        pos: usize,
        pkg: Symbol,
        arg_values: &[Value],
    ) -> Option<Vec<(usize, RegexCaptures)>> {
        let chain = self.token_method_wrap_chain(pkg.as_str(), &spec.lookup_name)?;
        let text: String = chars.iter().collect();
        LAST_TOKEN_METHOD_MATCH.with(|slot| slot.borrow_mut().take());
        let result = match self.call_wrapped_token_method(
            pkg,
            &spec.lookup_name,
            arg_values,
            &text,
            pos,
            &chain,
        ) {
            Ok(result) => result,
            Err(error) => {
                crate::runtime::regex_parse::PENDING_REGEX_ERROR.with(|slot| {
                    *slot.borrow_mut() = Some(error);
                });
                return Some(Vec::new());
            }
        };
        let side = LAST_TOKEN_METHOD_MATCH.with(|slot| slot.borrow_mut().take());
        let to_abs = result
            .match_to()
            .or_else(|| {
                if let ValueView::Instance { attributes, .. } = result.view() {
                    attributes
                        .as_map()
                        .get("to")
                        .and_then(|value| value.as_int())
                } else {
                    None
                }
            })
            .filter(|&to| to >= pos as i64 && to <= chars.len() as i64)
            .map(|to| to as usize);
        let Some(to_abs) = to_abs else {
            return Some(Vec::new());
        };
        let inner_caps = match side {
            Some(token)
                if token.pkg == pkg.as_str()
                    && token.name == spec.lookup_name
                    && token.from == pos
                    && token.to == to_abs =>
            {
                token.caps
            }
            _ => RegexCaptures {
                from: pos,
                to: to_abs,
                ..RegexCaptures::default()
            },
        };
        let sym = inner_caps.sym().cloned();
        Some(Self::build_named_candidates_from_inner(
            vec![(to_abs, inner_caps)],
            pos,
            spec,
            sym.as_ref(),
        ))
    }

    /// Run a wrapped whitespace rule from the compact `WsRule` regex atom.
    /// `<.ws>` is lowered to that atom, so it bypasses the ordinary named
    /// subrule path even when the grammar supplied its own `ws` method.
    pub(super) fn try_wrapped_token_end(
        &mut self,
        chars: &[char],
        pos: usize,
        pkg: Symbol,
        name: &str,
    ) -> Option<Option<usize>> {
        let chain = self.token_method_wrap_chain(pkg.as_str(), name)?;
        let text: String = chars.iter().collect();
        LAST_TOKEN_METHOD_MATCH.with(|slot| slot.borrow_mut().take());
        let result = match self.call_wrapped_token_method(pkg, name, &[], &text, pos, &chain) {
            Ok(result) => result,
            Err(error) => {
                crate::runtime::regex_parse::PENDING_REGEX_ERROR.with(|slot| {
                    *slot.borrow_mut() = Some(error);
                });
                return Some(None);
            }
        };
        Some(
            result
                .match_to()
                .or_else(|| {
                    if let ValueView::Instance { attributes, .. } = result.view() {
                        attributes
                            .as_map()
                            .get("to")
                            .and_then(|value| value.as_int())
                    } else {
                        None
                    }
                })
                .filter(|&to| to >= pos as i64 && to <= chars.len() as i64)
                .map(|to| to as usize),
        )
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
        &mut self,
        spec: &NamedRegexLookupSpec,
        chars: &[char],
        pos: usize,
        pkg: Symbol,
        arg_values: &[Value],
    ) -> Option<Vec<(usize, RegexCaptures)>> {
        let how = self
            .registry()
            .grammar_custom_how
            .get(pkg.as_str())
            .cloned()?;
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
            // The scratch runs in this package. Both the string and its interned
            // mirror are set: `current_package_sym()` reads the mirror, and a
            // scratch that overrode only the string answered for the wrong
            // package ([#7576](https://github.com/tokuhirom/mutsu/issues/7576)).
            current_package: Arc::new(RwLock::new(pkg.as_str().to_owned())),
            current_package_sym: std::sync::Arc::new(std::sync::atomic::AtomicU32::new(pkg.id())),
            ..self.new_regex_scratch_sharing_io()
        };
        self.copy_full_registry_into(&mut interp);
        if self.test_module_loaded() {
            interp.loaded_modules = self.loaded_modules.clone();
            interp.tap.ensure_state();
        }
        let typeobj = Value::package(pkg);
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
        let cursor = Value::make_match_object_full(
            pos as i64,
            pos as i64,
            &[],
            &Default::default(),
            MatchTarget::from_chars(chars),
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
        // The wrapper must return a Match/cursor; read its extent. A non-Match
        // cursor-like instance (user class with a `to` attribute) also counts.
        let to_abs = result
            .match_to()
            .or_else(|| {
                if let ValueView::Instance { attributes, .. } = result.view() {
                    attributes.as_map().get("to").and_then(|t| t.as_int())
                } else {
                    None
                }
            })
            .filter(|&t| t >= pos as i64 && t <= chars.len() as i64)
            .map(|t| t as usize);
        let Some(to_abs) = to_abs else {
            return Some(Vec::new());
        };
        // Prefer the side-channel captures from the actual token run inside
        // the wrapper; a wrapper that fabricated its own Match still advances
        // the parse by its extent (with a plain-text capture).
        let inner_caps = match side {
            Some(t)
                if t.pkg.as_str() == pkg.as_str()
                    && t.name == spec.lookup_name
                    && t.from == pos
                    && t.to == to_abs =>
            {
                t.caps
            }
            _ => RegexCaptures {
                from: pos,
                to: to_abs,
                ..RegexCaptures::default()
            },
        };
        let sym = inner_caps.sym().cloned();
        Some(Self::build_named_candidates_from_inner(
            vec![(to_abs, inner_caps)],
            pos,
            spec,
            sym.as_ref(),
        ))
    }
}
