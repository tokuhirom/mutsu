use super::super::*;

/// What one inline regex `{ … }` / `<?{ … }>` evaluation produced.
pub(super) struct InlineCodeOutcome {
    /// The body's value (`None` when the code failed to parse or threw).
    pub(super) value: Option<Value>,
    /// Writes the body made to the regex's own `:my`/`:let` lexicals.
    pub(super) writes: HashMap<String, Value>,
    /// The value the body's `make` produced, if it ran one. Raku executes
    /// `make` inline, where the cursor is, and it belongs to the rule node
    /// currently being matched — the caller threads it onto that node through
    /// `RegexCaptures::ast`, where the match trail can undo it on backtracking.
    pub(super) made: Option<Value>,
}

impl Interpreter {
    /// Parse a main-slang code string embedded in a regex (`{ … }` block,
    /// `<?{ … }>` assertion, `<{ … }>` interpolation, `** {code}` quantifier,
    /// `:my` declaration) through the thread-local `REGEX_CODE_PARSE_CACHE`.
    /// These strings are re-evaluated per cursor position in the hot match
    /// loop, and the parse of a given string is deterministic under a fixed
    /// declaration registry — so a cache hit (keyed by the string, guarded by
    /// `registry_write_gen`) is a refcount bump instead of a full re-parse.
    /// Returns `None` when the code does not parse (not cached — the caller
    /// treats it as a no-op/no-match either way).
    pub(in crate::runtime) fn parse_regex_code_cached(
        &self,
        code: &str,
    ) -> Option<Arc<Vec<crate::ast::Stmt>>> {
        self.parse_regex_code_cached_with_id(code).map(|(s, _)| s)
    }

    /// `parse_regex_code_cached`, additionally returning the entry's stable
    /// compile-cache id (see `CachedCodeParse`). A caller that evaluates the
    /// body in *this* interpreter passes the id to `eval_block_value_cached`,
    /// so the body is compiled once per code string rather than once per
    /// cursor position; a caller that spins up a scratch interpreter has
    /// nowhere to keep the compile and uses the plain entry point.
    pub(in crate::runtime) fn parse_regex_code_cached_with_id(
        &self,
        code: &str,
    ) -> Option<(Arc<Vec<crate::ast::Stmt>>, u64)> {
        use crate::runtime::regex_parse::REGEX_CODE_PARSE_CACHE;
        let cur_gen = self
            .registry_write_gen
            .load(std::sync::atomic::Ordering::Relaxed);
        if let Some(hit) = REGEX_CODE_PARSE_CACHE.with(|c| {
            c.borrow()
                .get(code)
                .and_then(|(g, stmts, id)| (*g == cur_gen).then(|| (Arc::clone(stmts), *id)))
        }) {
            crate::vm::vm_stats::record_regex_code_parse(true);
            return Some(hit);
        }
        crate::vm::vm_stats::record_regex_code_parse(false);
        let (stmts, _) = crate::parse_dispatch::parse_source(code).ok()?;
        let stmts = Arc::new(stmts);
        let id = crate::value::next_instance_id();
        REGEX_CODE_PARSE_CACHE.with(|c| {
            c.borrow_mut()
                .insert(code.to_string(), (cur_gen, Arc::clone(&stmts), id));
        });
        Some((stmts, id))
    }

    /// Install `self`'s declaration registry into a freshly-built
    /// sub-interpreter used for regex/grammar evaluation.
    ///
    /// This used to copy four fields (`functions` / `proto_functions` /
    /// `token_defs` / `enum_types`) into the registry the sub-interpreter built
    /// for itself, leaving it on its own built-in `classes`/`method_entries`.
    /// Sharing the parent's whole registry instead (the copy-on-write
    /// `Arc<Registry>`, see [`Self::copy_full_registry_into`]) is both a strict
    /// superset of that data — the parent's registry carries every built-in the
    /// sub-interpreter used to build for itself, plus the user declarations it
    /// could not see before — and O(1) instead of four map clones plus a
    /// registry write. It is also what lets `Interpreter::new` skip building the
    /// built-in registry for a scratch interpreter altogether.
    pub(crate) fn copy_decl_registry_into(&self, target: &mut Interpreter) {
        self.copy_full_registry_into(target);
        // Propagate the in-progress `Grammar.parse(:actions(...))` object so the
        // assertion's sub-interpreter can still run the action method mid-parse.
        // None outside a parse, so this is a no-op there.
        target.current_grammar_actions = self.current_grammar_actions.clone();
    }

    /// Snapshot the *entire* declaration registry (classes, roles, methods,
    /// proto-methods, ... in addition to functions/tokens) into `target`. Needed
    /// when the sub-interpreter must dispatch user class methods — e.g. running
    /// grammar action methods on the `:actions` object during an in-parse
    /// `<?{ $<x>.made ... }>` assertion (see `run_named_capture_actions`), where
    /// the action class's methods live in `Registry::classes`, which the leaner
    /// `copy_decl_registry_into` omits.
    pub(crate) fn copy_full_registry_into(&self, target: &mut Interpreter) {
        // The sub-interpreter only READS the registry during regex/grammar
        // evaluation (dispatching methods, resolving tokens/actions); it never
        // declares new classes into it. Since the registry is copy-on-write
        // (`Arc<RwLock<Arc<Registry>>>`, slice 1 of
        // docs/per-task-clone-slimming.md), sharing the inner `Arc<Registry>`
        // here is already O(1) — no per-call deep clone, and no snapshot cache
        // needed. `target` gets its OWN outer `Arc<RwLock<...>>`, so a (rare)
        // write on either side pays its own `Arc::make_mut` clone and never
        // leaks into the other — this also fixes a latent bug in the prior
        // shared-snapshot cache, where one sub-interpreter's write could leak
        // into the next sub-interpreter built from the same cached snapshot.
        target.registry = Arc::new(RwLock::new(Arc::clone(&self.registry.read().unwrap())));
    }

    /// Evaluate a closure interpolation `<{ code }>` inside a regex.
    /// Returns the regex pattern string to match against.
    pub(super) fn eval_regex_closure_interpolation(
        &mut self,
        code: &str,
        caps: &RegexCaptures,
        target: &str,
    ) -> Option<String> {
        let mut env = self.make_regex_eval_env(caps);
        // Set $_ to the match target string. After `make_regex_eval_env`, which
        // installs the `:my`/`:let` lexicals — the topic must win over them.
        env.insert("_".to_string(), Value::str(target.to_string()));
        let stmts = self.parse_regex_code_cached(code)?;
        let mut interp = Interpreter {
            env,
            // The scratch runs in this package. Both the string and its interned
            // mirror are set: `current_package_sym()` reads the mirror, and a
            // scratch that overrode only the string answered for the wrong
            // package ([#7576](https://github.com/tokuhirom/mutsu/issues/7576)).
            current_package: Arc::new(RwLock::new(self.current_package())),
            current_package_sym: std::sync::Arc::new(std::sync::atomic::AtomicU32::new(
                self.current_package_sym().id(),
            )),
            ..self.new_regex_scratch_sharing_io()
        };
        self.copy_decl_registry_into(&mut interp);
        let val = match interp.eval_block_value(&stmts) {
            Ok(v) => v,
            Err(e) => e.return_value?,
        };
        match val.view() {
            ValueView::Regex(pat) => Some(pat.to_string()),
            ValueView::RegexWithAdverbs(a) => Some(a.pattern.to_string()),
            ValueView::Routine {
                is_regex: true,
                name,
                package,
            } => {
                let full_name = if package.resolve().is_empty() {
                    name.resolve()
                } else {
                    format!("{}::{}", package, name)
                };
                Some(format!("<{}>", full_name))
            }
            ValueView::Array(elems, ..) => {
                // Array/List -> alternation of escaped literals
                let alts: Vec<String> = elems
                    .iter()
                    .map(|v| match v.view() {
                        ValueView::Regex(pat) => pat.to_string(),
                        ValueView::RegexWithAdverbs(a) => a.pattern.to_string(),
                        _ => {
                            let s = v.to_string_value();
                            // Quote as regex literal using single quotes
                            format!("'{}'", s.replace('\\', "\\\\").replace('\'', "\\'"))
                        }
                    })
                    .collect();
                if alts.is_empty() {
                    return None;
                }
                Some(format!("[ {} ]", alts.join(" | ")))
            }
            ValueView::Seq(elems) => {
                // Array/List -> alternation of escaped literals
                let alts: Vec<String> = elems
                    .iter()
                    .map(|v| match v.view() {
                        ValueView::Regex(pat) => pat.to_string(),
                        ValueView::RegexWithAdverbs(a) => a.pattern.to_string(),
                        _ => {
                            let s = v.to_string_value();
                            // Quote as regex literal using single quotes
                            format!("'{}'", s.replace('\\', "\\\\").replace('\'', "\\'"))
                        }
                    })
                    .collect();
                if alts.is_empty() {
                    return None;
                }
                Some(format!("[ {} ]", alts.join(" | ")))
            }
            _ => {
                let s = val.to_string_value();
                Some(s)
            }
        }
    }

    /// Run one inline regex code atom — a `<?{ … }>` / `<!{ … }>` assertion or a
    /// plain `{ … }` side-effect block — **on this interpreter**.
    ///
    /// Raku semantics: the code runs inline, once, where the cursor reaches it,
    /// and its side effects are real — visible to later atoms in the same match,
    /// and surviving a match that ultimately fails. `advent2013-day18` needs all
    /// three (`%*PLAYED{$card}++` must be visible to the next card's assertion;
    /// `@dups.push` must survive the failing parses). A scratch interpreter cannot
    /// provide any of them, so this runs in `self` (ADR-0009 part B).
    ///
    /// Only the *regex-internal* bindings this sets up (`$/`, `$0`…, `$<name>`,
    /// the in-regex `:my`/`:let` lexicals) and the body's own `my` declarations are
    /// scoped: they are saved and restored around the body. Every other write is a
    /// genuine side effect and is left in place — that is the whole point.
    ///
    /// Returns the body's value (`None` if the code failed to parse or threw),
    /// the writes it made to the in-regex lexicals, and the value its `make`
    /// produced. All three are regex-scoped, so none of them may stay in
    /// `self.env`; the caller threads the lexical writes back through
    /// `RegexCaptures::regex_vars` and the `make` value through
    /// `RegexCaptures::ast`, where the match trail can undo them on
    /// backtracking.
    ///
    /// `writes_back_to_caller` selects *how* a write to an *outer* lexical is
    /// propagated — both routes reach the caller's compiled local slots, they
    /// just pay for the name set differently. A plain `{ … }` block
    /// (`'123' ~~ / (\d) { $seen = $/.Str } \d+ /` has to leave `$seen` set)
    /// takes the env-diff → `pending_local_updates` bookkeeping of
    /// [`Interpreter::eval_regex_code_block_body`], which sees a write however
    /// it was spelled. An assertion is evaluated at every cursor position, so
    /// ADR-0009 kept its path free of that per-position snapshot: it uses the
    /// compiled body's `free_var_writes` instead
    /// (`writeback_assertion_free_var_writes` below), which the compiler already
    /// computed, so an assertion that assigns nothing pays nothing.
    pub(super) fn eval_regex_inline_code(
        &mut self,
        code: &str,
        caps: &RegexCaptures,
        matched_so_far: &str,
        writes_back_to_caller: bool,
    ) -> InlineCodeOutcome {
        let Some((stmts, code_cache_id)) = self.parse_regex_code_cached_with_id(code) else {
            return InlineCodeOutcome {
                value: None,
                writes: HashMap::new(),
                made: None,
            };
        };
        // The bindings to install for the body, and to restore afterwards.
        let mut env: Vec<(String, Value)> = Vec::new();
        // In-regex `:my`/`:let` lexicals (and anything a preceding inline `{ }`
        // block wrote to them). They are lexical to the regex, so they are
        // installed here and restored with the rest of the regex bindings —
        // `:my $x = 'y'; <?{ $x eq 'y' }>` must see 'y', not an outer `$x`.
        for (k, v) in caps.regex_vars() {
            env.push((k.clone(), v.clone()));
        }
        // The engine-scope subject: derives `$0…` texts from the live
        // accumulator's spans (the accumulator's own `target` is only set at
        // engine exit).
        let live_target = super::regex_helpers::current_match_target()
            .unwrap_or_else(|| MatchTarget::new(matched_so_far));
        // Set positional capture variables ($0, $1, etc.). They are `Match`
        // objects, exactly as `$/[0]` is once the match finishes — raku's
        // `/ (\d) { say $0 } \d+ /` prints `｢1｣`, not the bare `1` a `Str`
        // binding produced here.
        for (i, slot) in caps.positional.iter().enumerate() {
            env.push((i.to_string(), Value::pos_slot_value(slot, &live_target)));
        }
        // Build `$/` as a proper Match object so `$/.Str`/`$/.lc`/`~$/` yield the
        // matched-so-far text (not just an array of positional captures). A
        // `<?{ … $/.lc … }>` assertion inside a `token` relies on this (the card
        // grammar's dup check does `%*PLAYED{$/.lc}++`). `$/[n]` still indexes the
        // positional captures on the Match object.
        let cursor = Value::make_match_object_full(
            caps.match_from as i64,
            (caps.match_from + matched_so_far.chars().count()) as i64,
            &caps.positional,
            &caps.named,
            live_target.clone(),
        );
        // What an EARLIER `{ make … }` of this same rule already produced. raku
        // runs `make` inline, so a later block reads it back through `$/.made`
        // (`/ a { make 7 } b { say $/.made } /` prints 7).
        let cursor = match caps.ast.as_ref() {
            Some(ast) => cursor
                .match_with_attrs(vec![("ast", ast.clone())])
                .unwrap_or(cursor),
            None => cursor,
        };
        // `$¢` is the current match state at this point in the pattern — the same
        // object as `$/` here (`/ .{ $c = $¢ } /` must leave `$c` with a usable
        // `.pos`, roast/S05-capture/match-object.t). The reduce-time replay always
        // installed both; running inline has to as well.
        env.push(("\u{00A2}".to_string(), cursor.clone()));
        env.push(("/".to_string(), cursor));
        // When the assertion references `.made` AND we are inside a
        // `Grammar.parse(:actions(...))`, run the relevant action method on each
        // just-matched named capture so `$<x>.made` is available *during* the
        // parse. raku runs actions incrementally at reduce time; mutsu otherwise
        // only runs them post-parse, which leaves `.made` undefined here (e.g.
        // Template::Mustache's standalone-line rule:
        // `token linetag { ^^ (\h*) <tag> <?{ $<tag>.made<type> ~~ none(...) }> ... }`).
        // The actions run in a scratch interpreter so the assertion's own
        // `$/`/`$0` env below is not clobbered by the action dispatch.
        let made_named: HashMap<String, Value> = if code.contains(".made") {
            if let Some(actions0) = self.current_grammar_actions.clone() {
                self.run_named_capture_actions(caps, actions0)
            } else {
                // No actions: `.made` must still resolve (to Nil) on a Match,
                // not die with method-not-found on a plain Str capture.
                self.named_capture_match_objects(caps)
            }
        } else {
            HashMap::new()
        };

        // Set named captures (texts derive from spans through the engine-scope
        // subject; silent marker keys stay hidden).
        for (k, slot) in &caps.named {
            if k.starts_with(crate::runtime::SILENT_ACTION_MARKER_PREFIX) {
                continue;
            }
            if let Some(m) = made_named.get(k.as_str()) {
                env.push((format!("<{}>", k), m.clone()));
                continue;
            }
            // Like `$0` above: a named capture read mid-match is the same
            // `Match` the finished `$/<name>` holds, so `.from`/`.made`/`.hash`
            // all answer rather than dying on a `Str`.
            env.push((
                format!("<{}>", k),
                Value::named_slot_value(slot, &live_target),
            ));
        }
        // The assertion's own `my` declarations are lexical to it, so scope them
        // alongside the regex bindings. `eval_block_value` does not scope plain
        // lexicals (mutsu's env is flat), and without this an assertion's
        // `my $card = …` would clobber a same-named variable in the enclosing
        // scope — day18's assertion declares exactly that.
        let mut scoped: Vec<String> = env.iter().map(|(k, _)| k.clone()).collect();
        // The names the body declares with `my`. They are lexical to the body, so
        // besides being restored below they must be kept out of the caller-slot
        // writeback: a block-local name in `pending_local_updates` makes the VM
        // treat it as a caller lexical and refresh it *from env* at the body's
        // next call (`writeback_match_locals`), which on a re-run of the same
        // block overwrites the freshly-initialized slot with the outer binding
        // (or `Any` when there is none).
        let mut block_locals: Vec<String> = Vec::new();
        for stmt in stmts.iter() {
            if let Stmt::VarDecl { name, .. } = stmt {
                if !block_locals.contains(name) {
                    block_locals.push(name.clone());
                }
                if !scoped.contains(name) {
                    scoped.push(name.clone());
                }
            }
        }
        let saved: Vec<(String, Option<Value>)> = scoped
            .iter()
            .map(|k| (k.clone(), self.env.get(k).cloned()))
            .collect();
        for (k, v) in env {
            self.env.insert(k, v);
        }
        // `make` writes `env["made"]`. Running the block inline means that slot
        // is shared with every other rule being matched right now, so it is
        // cleared for the body and restored afterwards; what the body left in it
        // is returned as `made` and travels on the capture delta instead.
        let saved_made = self.env.get("made").cloned();
        self.env.remove("made");
        // Marks the body as embedded regex code so a bare free variable's
        // auto-package-qualified write (`$x` inside `grammar G { … }` compiles to
        // `SetGlobal("G::x")`) is redirected back onto the lexical in `env` —
        // otherwise the write strands itself in a `G::x` package slot and the
        // harvest below sees nothing. Same flag the reduce-time replay uses.
        let result = if writes_back_to_caller {
            let before = self.pending_local_updates.len();
            let body_result = self.eval_regex_code_block_body(&stmts, code_cache_id);
            // The body itself can drain the log (any call it makes runs
            // `drain_pending_local_updates_after_call`), so the list may be
            // SHORTER than it was on entry — clamp before splitting.
            let before = before.min(self.pending_local_updates.len());
            // The regex's own `:my`/`:let` lexicals are not caller lexicals — they
            // are harvested into `regex_vars` below and must not be written into
            // the caller's slots as well. Neither is the `make` slot, which is
            // engine state, not a variable the caller can declare, nor the body's
            // own `my` declarations, which are lexical to the body.
            let kept: Vec<(String, Value)> = self
                .pending_local_updates
                .split_off(before)
                .into_iter()
                .filter(|(name, _)| {
                    name != "made"
                        && !caps.regex_vars().contains_key(name)
                        && !block_locals.contains(name)
                })
                .collect();
            self.pending_local_updates.extend(kept);
            body_result.map(|_| Value::NIL)
        } else {
            let saved_in_block = self.in_regex_code_block;
            self.in_regex_code_block = true;
            // Cached compile: an assertion is evaluated once per cursor
            // position, and recompiling its handful of statements every time
            // was the dominant cost of a `<?{ … }>`-driven match (see
            // `news/2026-09/regex-inline-code-recompiled-per-cursor-position.md`).
            let mut free_var_writes: Vec<String> = Vec::new();
            let r = self.eval_block_value_cached_reporting_writes(
                &stmts,
                code_cache_id,
                &mut free_var_writes,
            );
            self.in_regex_code_block = saved_in_block;
            self.writeback_assertion_free_var_writes(&free_var_writes, &scoped);
            r
        };
        // Harvest writes to the in-regex lexicals *before* restoring them: the
        // value has to survive as a `regex_vars` delta so a later `$name`
        // interpolation or `<?{ … }>` assertion in the same match reads it, while
        // `self.env` goes back to what the enclosing scope had.
        let mut writes: HashMap<String, Value> = HashMap::new();
        for k in caps.regex_vars().keys() {
            if let Some(now) = self.env.get(k)
                && caps.regex_vars().get(k) != Some(now)
            {
                writes.insert(k.clone(), now.clone());
            }
        }
        let made = self.env.get("made").cloned();
        match saved_made {
            Some(v) => {
                self.env.insert("made".to_string(), v);
            }
            None => {
                self.env.remove("made");
            }
        }
        for (k, orig) in saved {
            match orig {
                Some(v) => self.env.insert(k, v),
                None => self.env.remove(&k),
            };
        }
        // A genuine exception (`die`) inside an embedded `{ … }` / `<?{ … }>`
        // block propagates out of the whole match in Rakudo — it is not a
        // mismatch. Park it in the pending slot; the match-entry points
        // (smartmatch, Grammar.parse, m//) re-raise it after the engine unwinds.
        let value = match result {
            Ok(v) => Some(v),
            Err(e) => {
                if e.return_value.is_none() {
                    super::super::regex_parse::PENDING_REGEX_ERROR.with(|slot| {
                        let mut slot = slot.borrow_mut();
                        if slot.is_none() {
                            *slot = Some(e);
                        }
                    });
                }
                None
            }
        };
        InlineCodeOutcome {
            value,
            writes,
            made,
        }
    }

    /// Carry an assertion body's assignments to *outer* lexicals through to the
    /// caller's compiled local slots.
    ///
    /// A plain `{ … }` block gets this from `eval_regex_code_block_body`'s env
    /// snapshot + binding-identity diff. An assertion is evaluated at every
    /// cursor position, so ADR-0009 kept its path snapshot-free — and the write
    /// consequently only ever landed in `env`. That is enough for a later atom
    /// of the same match to read it back, and enough for a *mutated* container
    /// (the caller's slot already shares that allocation), but a scalar
    /// rebinding died with the match: `my $n = 0; "aaaa" ~~ / [ <?{ $n++; True }> . ]+ /`
    /// left `$n` at 0 where rakudo leaves 5. The compiler's `free_var_writes` is
    /// exactly the set the diff would have found, at no per-position cost — an
    /// assertion that assigns nothing reports nothing and returns immediately.
    ///
    /// `scoped` holds the names this evaluation installed and is about to
    /// restore (the regex's own `:my`/`:let` lexicals, `$/` / `$¢` / `$0`…, and
    /// the body's own `my` declarations); none of them is a caller lexical.
    /// `made` is engine state, not a variable the caller can declare.
    fn writeback_assertion_free_var_writes(&mut self, names: &[String], scoped: &[String]) {
        if names.is_empty() {
            return;
        }
        // A body compiled inside `grammar G { … }` records a write to an outer
        // `$x` under the auto-package-qualified `G::x`, while
        // `in_regex_code_block` redirects the write itself back onto the bare
        // lexical in `env` — strip the package so the two names agree (the same
        // adjustment the deferred class/role body drain in `run.rs` makes).
        let pkg = self.current_package_sym();
        let pkg_prefix = if pkg == "GLOBAL" {
            String::new()
        } else {
            format!("{pkg}::")
        };
        for name in names {
            let name = if pkg_prefix.is_empty() {
                name.as_str()
            } else {
                name.strip_prefix(&pkg_prefix).unwrap_or(name.as_str())
            };
            if matches!(name, "made" | "_" | "$_" | "@_" | "%_") || scoped.iter().any(|s| s == name)
            {
                continue;
            }
            let Some(v) = self.env.get(name).cloned() else {
                continue;
            };
            if let Some(set) = self.carrier_writes.as_mut() {
                set.insert(name.to_string());
            }
            // Both drains (`drain_pending_local_updates_after_call`,
            // `vm_smartmatch_ops`) key on the NAME and re-read the value from
            // `env`, so a repeat of a name already logged is redundant. Skipping
            // it keeps the log bounded by the number of distinct names rather
            // than by the number of cursor positions the assertion ran at.
            if self.pending_local_updates.iter().any(|(n, _)| n == name) {
                continue;
            }
            self.pending_local_updates.push((name.to_string(), v));
        }
    }

    /// Build the positional (`$0`, `$1`, …), named-capture, and `$/` / `$¢`
    /// bindings that a plain `{ … }` / `<?{ … }>` code block sees at this
    /// point in the match — for an embedded `:my $var = EXPR;` declarator's
    /// initializer, which must see the same in-progress match state (e.g.
    /// `:my $c = ~$0;`) rather than reading an unbound `$0`. Mirrors the
    /// bindings `eval_regex_inline_code` installs above, minus the `.made`
    /// grammar-action dispatch — a declarator's RHS running an action would
    /// be a surprising side effect for what looks like a plain variable read.
    pub(super) fn regex_capture_bindings(
        caps: &RegexCaptures,
        chars: &[char],
        pos: usize,
    ) -> Vec<(String, Value)> {
        let from = caps.match_from.min(chars.len());
        let to = pos.min(chars.len()).max(from);
        let matched_so_far: String = chars[from..to].iter().collect();
        let live_target = super::regex_helpers::current_match_target()
            .unwrap_or_else(|| MatchTarget::new(&matched_so_far));
        let mut env: Vec<(String, Value)> = Vec::new();
        for (i, slot) in caps.positional.iter().enumerate() {
            env.push((i.to_string(), Value::pos_slot_value(slot, &live_target)));
        }
        for (k, slot) in &caps.named {
            if k.starts_with(crate::runtime::SILENT_ACTION_MARKER_PREFIX) {
                continue;
            }
            // Like `$0` above: a named capture read mid-match is the same
            // `Match` the finished `$/<name>` holds, so `.from`/`.made`/`.hash`
            // all answer rather than dying on a `Str`.
            env.push((
                format!("<{}>", k),
                Value::named_slot_value(slot, &live_target),
            ));
        }
        let cursor = Value::make_match_object_full(
            caps.match_from as i64,
            (caps.match_from + matched_so_far.chars().count()) as i64,
            &caps.positional,
            &caps.named,
            live_target.clone(),
        );
        env.push(("\u{00A2}".to_string(), cursor.clone()));
        env.push(("/".to_string(), cursor));
        env
    }

    /// Build a Match object for each named capture in `caps` WITHOUT running
    /// any actions. Used when an embedded code block references `$<x>.made` but
    /// no `:actions` object is in play: the capture must still be a Match (so
    /// `.made` answers Nil) rather than the plain Str the fast path installs —
    /// on a Str, `.made` is a method-not-found, which now propagates as a die.
    fn named_capture_match_objects(&mut self, caps: &RegexCaptures) -> HashMap<String, Value> {
        let target =
            super::regex_helpers::current_match_target().unwrap_or_else(|| MatchTarget::new(""));
        let full = Value::make_match_object_full(
            caps.from as i64,
            caps.to as i64,
            &caps.positional,
            &caps.named,
            target,
        );
        let named_v = full.match_named();
        match named_v.as_ref().map(Value::view) {
            Some(ValueView::Hash(named)) => {
                named.iter().map(|(k, v)| (k.clone(), v.clone())).collect()
            }
            _ => HashMap::new(),
        }
    }

    /// Build a Match object for each named capture in `caps` and run its grammar
    /// action method (proto-regex `:sym<>` variant aware) so the resulting Match
    /// carries `.made`. Used by `eval_regex_code_assertion` to support
    /// `$<x>.made` inside `<?{ ... }>` assertions during parsing. Runs in a
    /// scratch interpreter to avoid mutating the caller's env. Best-effort: a
    /// capture whose action errors or is absent maps to its un-actioned Match.
    fn run_named_capture_actions(
        &mut self,
        caps: &RegexCaptures,
        mut actions: Value,
    ) -> HashMap<String, Value> {
        let mut out = HashMap::new();
        // Mid-match synthesis: the accumulator carries no subject yet, so the
        // subject comes from the live engine scope (empty-subject fallback is
        // unreachable in practice — this only runs from inside a match).
        let target =
            super::regex_helpers::current_match_target().unwrap_or_else(|| MatchTarget::new(""));
        let full = Value::make_match_object_full(
            caps.from as i64,
            caps.to as i64,
            &caps.positional,
            &caps.named,
            target,
        );
        let named_v = full.match_named();
        let Some(ValueView::Hash(named)) = named_v.as_ref().map(Value::view) else {
            return out;
        };
        let mut scratch = Interpreter {
            env: self.env.clone(),
            // The scratch runs in this package. Both the string and its interned
            // mirror are set: `current_package_sym()` reads the mirror, and a
            // scratch that overrode only the string answered for the wrong
            // package ([#7576](https://github.com/tokuhirom/mutsu/issues/7576)).
            current_package: Arc::new(RwLock::new(self.current_package())),
            current_package_sym: std::sync::Arc::new(std::sync::atomic::AtomicU32::new(
                self.current_package_sym().id(),
            )),
            ..self.new_regex_scratch_sharing_io()
        };
        self.copy_full_registry_into(&mut scratch);
        for (k, child) in named.iter() {
            let ran = match child.view() {
                ValueView::Array(items, meta) => {
                    let mut acc = Vec::with_capacity(items.len());
                    for it in items.as_ref() {
                        let dn = Interpreter::get_action_name(it).unwrap_or_else(|| k.clone());
                        let m = scratch
                            .invoke_grammar_actions(it.clone(), &mut actions, &dn)
                            .unwrap_or_else(|_| it.clone());
                        acc.push(m);
                    }
                    Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(acc)),
                        meta,
                    )
                }
                _ => {
                    let dn = Interpreter::get_action_name(child).unwrap_or_else(|| k.clone());
                    scratch
                        .invoke_grammar_actions(child.clone(), &mut actions, &dn)
                        .unwrap_or_else(|_| child.clone())
                }
            };
            out.insert(k.clone(), ran);
        }
        out
    }

    /// Reduce-time grammar action hook for a `<subrule>` quantifier iteration.
    /// Called from the quantifier loop after each iteration commits. When the
    /// live parse is action-driven AND its matching already depends on a `$*`
    /// dynamic var (the SEEN gate), run THIS iteration's subrule action so any
    /// dyn-var write it performs (e.g. Template::Mustache's delimiter finalizer
    /// `($*LEFT,$*RIGHT)=@delim`) is published to the overlay and thus visible to
    /// the next iteration's pattern interpolation. No-op for ordinary grammars.
    pub(super) fn maybe_run_reduce_time_dynvar_action(
        &mut self,
        token: &RegexToken,
        new_caps: &RegexCaptures,
    ) {
        if !super::regex_helpers::dynvar_overlay_active() || !super::regex_helpers::dynvar_seen() {
            return;
        }
        let Some(actions) = self.current_grammar_actions.clone() else {
            return;
        };
        let rule_name = match &token.atom {
            RegexAtom::Named(n) => n.trim().to_string(),
            _ => return,
        };
        // The sub-capture this iteration stored under (explicit `$<alias>=` wins).
        let cap_name = token
            .named_capture
            .clone()
            .unwrap_or_else(|| rule_name.clone());
        let Some(sub) = new_caps
            .named
            .get(&Symbol::intern(&cap_name))
            .and_then(|slot| slot.nodes.last())
            .cloned()
        else {
            return;
        };
        self.run_reduce_time_action(&sub, &rule_name, actions);
    }

    /// Run a single subrule's grammar action in a scratch interpreter and publish
    /// any `$*` dynamic-var writes it makes into the reduce-time overlay. The
    /// scratch is seeded with the overlay's current values so the action sees the
    /// latest dynamic state; only vars whose value actually changes are published.
    fn run_reduce_time_action(&self, sub: &CapNode, rule_name: &str, actions: Value) {
        // Run on an INDEPENDENT deep copy of the actions object so any `self`
        // attribute the action mutates does not leak into the real actions
        // object — the authoritative post-parse action pass will run again and is
        // the one whose `make`/`self` effects count. The reduce-time pass exists
        // ONLY to extract `$*` dynamic-var writes (which flow through the scratch
        // env into the overlay, not through actions state). `InstanceAttrs::clone`
        // is a deep, fresh-cell copy.
        //
        // The copy gets a FRESH instance id: `invoke_grammar_actions` re-reads
        // the actions object from the env after each dispatch by (class, id)
        // match, and the scratch env holds the caller's ORIGINAL instance — a
        // copy sharing the id would be silently swapped back to the original
        // mid-walk, leaking every subsequent attribute mutation (pinned by
        // t/grammar-reduce-time-dynvar.t test 5).
        let mut actions = match actions.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } => {
                // Deep-clone first (detaching shared containers), then rebuild
                // under the fresh id — `InstanceAttrs` carries its id internally
                // and `instance_parts` asserts the two agree.
                let detached = (**attributes).clone().to_map();
                let fresh_id = crate::value::next_instance_id();
                Value::instance_parts(
                    class_name,
                    crate::gc::Gc::new(crate::value::InstanceAttrs::new(
                        class_name, detached, fresh_id, false,
                    )),
                    fresh_id,
                )
            }
            _ => actions.clone(),
        };
        let kids = sub.kids();
        // Mid-match synthesis — subject from the live engine scope (see
        // `run_named_capture_actions`).
        let target =
            super::regex_helpers::current_match_target().unwrap_or_else(|| MatchTarget::new(""));
        let match_obj = Value::make_match_object_full(
            sub.from as i64,
            sub.to as i64,
            &kids.positional,
            &kids.named,
            target,
        );
        let mut scratch = Interpreter {
            env: self.env.clone(),
            // The scratch runs in this package. Both the string and its interned
            // mirror are set: `current_package_sym()` reads the mirror, and a
            // scratch that overrode only the string answered for the wrong
            // package ([#7576](https://github.com/tokuhirom/mutsu/issues/7576)).
            current_package: Arc::new(RwLock::new(self.current_package())),
            current_package_sym: std::sync::Arc::new(std::sync::atomic::AtomicU32::new(
                self.current_package_sym().id(),
            )),
            ..self.new_regex_scratch_sharing_io()
        };
        self.copy_full_registry_into(&mut scratch);
        // Seed the scratch's `$*` vars from the overlay (latest delimiters etc.).
        for (k, v) in super::regex_helpers::dynvar_overlay_snapshot() {
            scratch.env.insert(k, v);
        }
        // Baseline of `$*` vars visible to the action, to diff after it runs.
        let baseline: HashMap<String, Value> = scratch
            .env
            .iter()
            .filter(|(k, _)| k.starts_with("*"))
            .map(|(k, v)| (k.with_str(|s| s.to_string()), v.clone()))
            .collect();
        let _ = scratch.invoke_grammar_actions(match_obj, &mut actions, rule_name);
        // Publish changed `$*` vars into the overlay for subsequent matching.
        let changed: Vec<(String, Value)> = scratch
            .env
            .iter()
            .filter(|(k, _)| k.starts_with("*"))
            .filter_map(|(k, v)| {
                let name = k.with_str(|s| s.to_string());
                match baseline.get(&name) {
                    Some(old) if old == v => None,
                    _ => Some((name, v.clone())),
                }
            })
            .collect();
        for (name, val) in changed {
            super::regex_helpers::dynvar_overlay_put(&name, val);
        }
    }

    /// Create an X::Syntax::Regex::QuantifierValue exception with the given flag attribute set to True.
    pub(super) fn make_quantifier_value_error(flag: &str, message: &str) -> RuntimeError {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(message.to_string()));
        attrs.insert(flag.to_string(), Value::TRUE);
        let ex = Value::make_instance(
            crate::symbol::Symbol::intern("X::Syntax::Regex::QuantifierValue"),
            attrs,
        );
        let mut err = RuntimeError::new(message.to_string());
        err.exception = Some(Box::new(ex));
        err
    }

    /// Set a pending quantifier-value error in the thread-local error store.
    pub(super) fn set_quantifier_value_error(flag: &str, message: &str) {
        let err = Self::make_quantifier_value_error(flag, message);
        crate::runtime::regex_parse::PENDING_REGEX_ERROR.with(|e| {
            *e.borrow_mut() = Some(err);
        });
    }
}
