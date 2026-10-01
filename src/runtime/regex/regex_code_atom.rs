//! The call-out atoms of a regex: a plain `{ … }` block, a `<?{ … }>` /
//! `<!{ … }>` assertion and a `:my $x = …;` declaration. Each runs Raku code on
//! the caller's interpreter, at the place the cursor reaches it (ADR-0009,
//! ADR-0133).
//!
//! This is the one implementation of what those atoms do (ADR-0135 D4): the
//! tree walk's single-candidate matcher and the compiled engine's `Code` /
//! `VarDecl` ops both call it, so the two engines cannot drift on when the code
//! runs, what it sees or what it leaves behind. Under `MUTSU_RX_DIFF=1` every
//! call goes through `rx_code_call` (ADR-0135 D6), which records the
//! invocation for the compiled run and replays it for the walk.

use super::super::*;

impl Interpreter {
    /// Run one `CodeAssertion` atom `atom` at `pos`: the position it leaves the
    /// cursor at and the capture delta it adds (the `:my` lexicals it wrote and
    /// the `make` value it produced), or `None` when an assertion fails or a
    /// block dies.
    // Cost: O(c + m) plus one run of the body, c = the captures visible to the
    // body (`$/`, `$0`, … are bound for it), m = the matched-so-far text.
    pub(super) fn regex_code_atom(
        &mut self,
        atom: &RegexAtom,
        chars: &[char],
        pos: usize,
        current_caps: &RegexCaptures,
    ) -> Option<(usize, RegexCaptures)> {
        let RegexAtom::CodeAssertion {
            code,
            negated,
            is_assertion,
            body,
            code_cache_id,
        } = atom
        else {
            debug_assert!(false, "regex_code_atom takes a CodeAssertion");
            return None;
        };
        // Declarative-prefix (LTM) measurement: never execute the code
        // (ADR-0009). The two kinds are treated differently, per
        // roast/S05-grammar/protoregex.t:
        if super::regex_helpers::LTM_DECLARATIVE_MODE.with(std::cell::Cell::get) {
            if *is_assertion {
                // "<?{...}> does not terminate LTM" / "<!{...}> does not
                // terminate LTM": Rakudo's NFA treats an assertion as a
                // zero-width pass and keeps measuring the atoms after it, so
                // `token ass1:sym<a> { a <?{ 1 }> .+ }` has declarative
                // prefix `a .+` and beats a bare `aa` candidate on 'aaa'.
                return Some((pos, RegexCaptures::default()));
            }
            // "However, code blocks do terminate LTM": `token
            // block:sym<a> { a {} .+ }` has declarative prefix `a` only, so
            // on 'aaa' the bare `aa` candidate wins. The block is a fate:
            // it ends this path of the measurement (`regex_ltm_fate`).
            super::regex_ltm_fate::ltm_record_fate(pos);
            return None;
        }
        // Failure-position probe: don't execute, don't stop — a code atom is
        // a zero-width no-op so the probe measures the declarative skeleton.
        if super::regex_helpers::CODE_ATOMS_INERT.with(std::cell::Cell::get) {
            return Some((pos, RegexCaptures::default()));
        }
        self.rx_code_call(code, pos, current_caps, |interp| {
            // The text matched up to this atom — becomes `$/.Str` inside the
            // code, so `$/.lc` / `~$/` see the matched-so-far text (e.g. the
            // card grammar's `%*PLAYED{$/.lc}++` dup check).
            let matched_so_far: String = chars
                [current_caps.inline_match_from().min(chars.len())..pos]
                .iter()
                .collect();
            if *is_assertion {
                // Runs on THIS interpreter, with real side effects, right here
                // (ADR-0009 part B). It is therefore NOT recorded as a code block
                // for `execute_regex_code_blocks` to replay on the winning path —
                // that replay would run it a second time.
                let outcome = interp.eval_regex_inline_code(
                    code,
                    body.as_ref(),
                    *code_cache_id,
                    current_caps,
                    &matched_so_far,
                    false,
                );
                let result = outcome.value.map(|v| v.truthy()).unwrap_or(false);
                let pass = if *negated { !result } else { result };
                return if pass {
                    let mut new_caps = RegexCaptures::default();
                    new_caps.extend_regex_vars(outcome.writes);
                    new_caps.ast = outcome.made;
                    Some((pos, new_caps))
                } else {
                    None
                };
            }
            // raku runs EVERY plain `{ … }` block inline, left-to-right,
            // during matching: a write to an in-regex `:my` lexical is
            // visible to the atoms that follow it (YAMLish's `root-block`
            // computes its indent this way), a `make` is visible to a later
            // block in the same rule as `$/.made`, and a subrule's `make` has
            // already landed on the child node by the time the parent's next
            // block reads `$<child>.made`. So does a block that mentions a
            // `$*` dynamic variable: the rule's `:my $*x` declaration and its
            // `$*` parameters are both live in `self.env` right here, and the
            // per-match value the block writes travels onward on the capture
            // delta's `regex_vars`, which `install_fresh_rule_dynvars` reads
            // back at reduce time. A block inside a later `||` branch is not a
            // special case either: `walk_seq_alternation` only evaluates that
            // branch once raku's cursor would enter it, so reaching this point
            // means the block really is on the cursor's path.
            let outcome = interp.eval_regex_inline_code(
                code,
                body.as_ref(),
                *code_cache_id,
                current_caps,
                &matched_so_far,
                true,
            );
            // The block `die`d: fail the match so the engine unwinds; the
            // parked pending error is re-raised at the match entry point.
            if super::super::regex_parse::PENDING_REGEX_ERROR.with(|e| e.borrow().is_some()) {
                return None;
            }
            let mut new_caps = RegexCaptures::default();
            new_caps.extend_regex_vars(outcome.writes);
            // The `make` belongs to the rule node being matched: it rides
            // the capture delta so the trail undoes it if this branch is
            // abandoned, and `build_named_candidates_from_inner` commits it
            // to the subrule's own node rather than the parent's.
            new_caps.ast = outcome.made;
            Some((pos, new_caps))
        })
    }

    /// Run one `:my $x = …;` / `:our` / `:temp` / `:let` declaration `code` at
    /// `pos`. It is zero-width and always succeeds; the capture delta carries the
    /// declared lexicals (`regex_vars`), which later code atoms and `$x`
    /// interpolations read back.
    // Cost: O(c + v) plus one run of each initializer, c = the captures bound
    // for it, v = the `:my` lexicals in scope.
    pub(super) fn regex_var_decl_atom(
        &mut self,
        code: &str,
        chars: &[char],
        pos: usize,
        current_caps: &RegexCaptures,
    ) -> Option<(usize, RegexCaptures)> {
        self.rx_code_call(code, pos, current_caps, |interp| {
            interp.regex_var_decl_run(code, chars, pos, current_caps)
        })
    }

    fn regex_var_decl_run(
        &mut self,
        code: &str,
        chars: &[char],
        pos: usize,
        current_caps: &RegexCaptures,
    ) -> Option<(usize, RegexCaptures)> {
        let source = format!("{};", code);
        if let Some(stmts) = self.parse_regex_code_cached(&source) {
            let mut new_caps = RegexCaptures::default();
            // Grammar-rule dynamic declarations are initialized by the
            // rule-entry frame. Keep the declaration zero-width here,
            // and carry the installed value into the capture delta,
            // rather than evaluating the initializer once per LTM
            // candidate/end (or a second time on the winning path).
            let declared_dynamic_keys: Vec<String> = stmts
                .iter()
                .filter_map(|stmt| match stmt {
                    Stmt::VarDecl { name, .. } if crate::env::is_dynamic_var_name(name) => Some(
                        Self::grammar_dynvar_env_keys(name)
                            .into_iter()
                            .next()
                            .unwrap_or_else(|| name.clone()),
                    ),
                    _ => None,
                })
                .collect();
            if declared_dynamic_keys
                .iter()
                .any(|name| super::regex_helpers::grammar_dynvar_scope_active(name))
            {
                for name in declared_dynamic_keys {
                    if let Some(value) = self.env.get(&name).cloned() {
                        new_caps.regex_vars_mut().insert(name, value);
                    }
                }
                return Some((pos, new_caps));
            }
            // The initializer may reference the regex's own
            // in-progress match state — `:my $c = ~$0;` needs `$0`
            // bound to the capture matched so far, exactly like a
            // plain `{ … }` code block sees it. Install those
            // bindings around both evaluation paths below (they are
            // restored just before this function returns).
            let capture_env = Self::regex_capture_bindings(current_caps, chars, pos);
            let mut capture_saved: Vec<(String, Option<Value>)> = Vec::new();
            for (k, v) in &capture_env {
                capture_saved.push((k.clone(), self.env.get(k).cloned()));
                self.env.insert(k.clone(), v.clone());
            }
            // Evaluate each non-dynamic declaration here, with the
            // lexicals declared before it installed around the call so
            // a later one can read — and dispatch on — an earlier one
            // (`:my @segs = $req.path-segments;`, Cro's route matcher).
            // Dynamics (`:my %*PLAYED = ()`) and any other statement
            // shape run together below over an isolated copy of the
            // env, and are harvested by an env diff.
            let mut scratch_stmts: Vec<Stmt> = Vec::new();
            for stmt in stmts.iter() {
                let Stmt::VarDecl {
                    name, expr, is_our, ..
                } = stmt
                else {
                    scratch_stmts.push(stmt.clone());
                    continue;
                };
                if name.trim_start_matches(['@', '%']).starts_with('*') || name == "_" {
                    scratch_stmts.push(stmt.clone());
                    continue;
                }
                let mut saved: Vec<(String, Option<Value>)> = Vec::new();
                for (k, v) in current_caps
                    .regex_vars()
                    .iter()
                    .chain(new_caps.regex_vars())
                {
                    saved.push((k.clone(), self.env.get(k).cloned()));
                    self.env.insert(k.clone(), v.clone());
                }
                let v = if *is_our {
                    // `:our $var = ...;` is a real package-scoped
                    // declaration, not merely a regex-local lexical
                    // like `:my`/`:constant`/`:temp`/`:let`. Run the
                    // WHOLE statement (not just its RHS expr) through
                    // the normal `our` compile path
                    // (`Compiler::qualify_variable_name` /
                    // `OpCode::DeclareOurScalar`) so it writes through
                    // to the package's `our`-scoped storage exactly
                    // like a plain (non-regex) `our $var = ...;`
                    // would — see
                    // todo/tickets/regex-our-declarator-writeback-missing.md.
                    // `eval_block_value` runs on `self` (the real
                    // interpreter), so a method-calling initializer
                    // still dispatches correctly, same as the
                    // plain-expr branch below.
                    let _ = self.eval_block_value(std::slice::from_ref(stmt));
                    self.env.get(name).cloned().unwrap_or(Value::NIL)
                } else {
                    self.eval_block_value(&[Stmt::Expr(expr.clone())])
                        .unwrap_or(Value::NIL)
                };
                for (k, orig) in saved {
                    match orig {
                        Some(prev) => self.env.insert(k, prev),
                        None => self.env.remove(&k),
                    };
                }
                new_caps.regex_vars_mut().insert(name.clone(), v);
            }
            if scratch_stmts.is_empty() {
                for (k, orig) in capture_saved {
                    match orig {
                        Some(prev) => self.env.insert(k, prev),
                        None => self.env.remove(&k),
                    };
                }
                return Some((pos, new_caps));
            }
            let mut env = self.env.clone();
            for (k, v) in current_caps
                .regex_vars()
                .iter()
                .chain(new_caps.regex_vars())
            {
                env.insert(k.clone(), v.clone());
            }
            // Run over an isolated copy of the env; what the
            // declarations wrote is read back out of it before it is
            // restored.
            let after = self.run_regex_sub_eval(env, None, |interp| {
                let _ = interp.eval_block_value(&scratch_stmts);
                interp.env.clone()
            });
            for (k, v) in &after {
                // The topic is not a `:my` declaration — the isolated
                // run leaves `$_` holding the declaration's value, and
                // recording it would let a later code block / assertion
                // (which installs `regex_vars` into its env) see that stale
                // value as `$_` instead of the real topic.
                if k.resolve() == "_" {
                    continue;
                }
                if !self.env.contains_key_sym(*k) || self.env.get_sym(*k) != Some(v) {
                    new_caps.regex_vars_mut().insert(k.resolve(), v.clone());
                }
            }
            // The env diff above only sees a *change*. The isolated env is
            // cloned from this one, so a declaration whose write reached
            // the shared storage compares equal and is missed — which left
            // the lexical out of `regex_vars` entirely. It then only worked
            // for blocks that run inline (they read the write from `env`);
            // a `make`-bearing block, which runs on the reduce walk long
            // after that write is gone, saw nothing. Record what the
            // declaration itself introduced, by name.
            for stmt in stmts.iter() {
                let Stmt::VarDecl { name, .. } = stmt else {
                    continue;
                };
                if name == "_" || new_caps.regex_vars().contains_key(name) {
                    continue;
                }
                if let Some(v) = after.get(name) {
                    new_caps.regex_vars_mut().insert(name.clone(), v.clone());
                }
            }
            for (k, orig) in capture_saved {
                match orig {
                    Some(prev) => self.env.insert(k, prev),
                    None => self.env.remove(&k),
                };
            }
            return Some((pos, new_caps));
        }
        Some((pos, RegexCaptures::default()))
    }

    /// Run one `<{ code }>` closure interpolation at `pos`: evaluate `code` to a
    /// pattern, then match that pattern here. Only the pattern's first match
    /// counts (the caller cannot backtrack into it), and its positional and
    /// named captures join the caller's.
    // Cost: O(n) for the subject text handed to the code, plus one run of the
    // code and the match of the pattern it returns, n = the subject's chars.
    pub(super) fn regex_closure_interp_atom(
        &mut self,
        code: &str,
        body: Option<&std::sync::Arc<Vec<crate::ast::Stmt>>>,
        chars: &[char],
        pos: usize,
        current_caps: &RegexCaptures,
    ) -> Option<(usize, RegexCaptures)> {
        self.rx_code_call(code, pos, current_caps, |interp| {
            let target: String = chars.iter().collect();
            let matched_so_far: String = chars
                [current_caps.inline_match_from().min(chars.len())..pos]
                .iter()
                .collect();
            let pattern_str = interp.eval_regex_closure_interpolation(
                code,
                body,
                current_caps,
                &target,
                &matched_so_far,
            );
            if let Some(ref pat_str) = pattern_str
                && Interpreter::contains_dangerous_regex_code(pat_str)
            {
                super::super::regex_parse::PENDING_REGEX_ERROR.with(|e| {
                    *e.borrow_mut() = Some(Interpreter::make_security_policy_error());
                });
                return None;
            }
            if let Some(pat_str) = pattern_str
                && let Some(parsed) = interp.parse_regex(&pat_str)
            {
                let pkg = interp.current_package_sym();
                // Rakudo gives the interpolated pattern a match of its own:
                // its captures are discarded, not merged into the caller's.
                if let Some((end, _inner_caps)) =
                    interp.regex_match_end_from_caps_in_pkg(&parsed, chars, pos, pkg)
                {
                    return Some((end, RegexCaptures::default()));
                }
            }
            None
        })
    }
}
