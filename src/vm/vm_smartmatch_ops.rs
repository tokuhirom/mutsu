//! Smartmatch (`~~`) and divisibility (`%%`) ops.
use super::*;

impl Interpreter {
    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_smart_match_expr_op(
        &mut self,
        code: &CompiledCode,
        ip: &mut usize,
        rhs_end: u32,
        negate: bool,
        lhs_var: &Option<String>,
        lhs_slot: Option<u32>,
        rhs_is_match_regex: bool,
        lhs_is_literal: bool,
        rhs_pure_regex: bool,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        let left = self.stack.pop().unwrap();
        let rhs_start = *ip + 1;
        let rhs_end = rhs_end as usize;
        // Slice 6.3 step 2: clear the regex engine's caller-variable write log so
        // that, after the match, a non-empty `pending_local_updates` reliably means
        // *this* match's embedded `{ }` code blocks wrote a caller variable by name
        // (the engine records them there but nothing consumes the log otherwise).
        self.pending_local_updates.clear();
        let saved_topic = self.env().get("_").cloned();
        self.env_mut().insert("_".to_string(), left.clone());
        // While the RHS runs, `$_` is *aliased* to the LHS variable (`$x ~~ s///`
        // topicalizes `$x`). A destructive `s///`/`tr///` checks `$_`'s readonly
        // status, but the enclosing scope may have marked `_` readonly for an
        // unrelated reason — e.g. a `for ^N { $x ~~ s/// }` loop marks its *topic*
        // `_` readonly. The match's `_` must instead reflect the *aliased
        // variable*'s mutability: mutable `my $x` allows the write, a readonly
        // param still blocks it. Save and override `_`'s readonly flag for the
        // duration when the LHS is an explicit variable other than the topic.
        let topic_ro_override = match lhs_var {
            Some(v) if v != "_" => {
                let saved = self.is_readonly("_");
                let bare = v.trim_start_matches(['$', '@', '%', '&']);
                let target_ro = self.is_readonly(v) || self.is_readonly(bare);
                if target_ro {
                    self.mark_readonly("_");
                } else {
                    self.unmark_readonly("_");
                }
                Some(saved)
            }
            _ => None,
        };
        // Clear the env-dirty flag before overwriting env with local values for
        // regex interpolation. The reverse env->locals pull this used to perform
        // was removed in Stage 3 (the interpreter-bridge writes it covered, e.g.
        // EVAL modifying a `$GLOBAL::` variable, are now reconciled by precise
        // write-throughs at their own sites).
        self.sync_regex_interpolation_env_from_locals(code);
        let saved_in_smartmatch_rhs = self.in_smartmatch_rhs;
        self.in_smartmatch_rhs = true;
        self.transliterate_in_smartmatch = false;
        self.substitution_in_smartmatch = false;
        let rhs_run = self.run_range(code, rhs_start, rhs_end, compiled_fns);
        self.in_smartmatch_rhs = saved_in_smartmatch_rhs;
        let was_transliterate = self.transliterate_in_smartmatch;
        let was_substitution = self.substitution_in_smartmatch;
        self.transliterate_in_smartmatch = false;
        self.substitution_in_smartmatch = false;
        // Restore the topic's readonly flag (overridden above for an aliased LHS
        // variable) on every path, including the error path below.
        if let Some(saved) = topic_ro_override {
            if saved {
                self.mark_readonly("_");
            } else {
                self.unmark_readonly("_");
            }
        }
        rhs_run?;
        let right = self.stack.pop().unwrap_or(Value::NIL);
        // A destructive `s///`/`tr///` that actually matched against a string
        // literal has no writable container to update, so Raku throws
        // X::Assignment::RO (e.g. `'abc' ~~ s/b/g/`). A non-matching attempt is
        // a no-op and does not throw.
        if lhs_is_literal && (was_substitution || was_transliterate) && right.truthy() {
            // Restore the topic before propagating the error.
            if let Some(v) = saved_topic {
                self.env_mut().insert("_".to_string(), v);
            } else {
                self.env_mut().remove("_");
            }
            return Err(RuntimeError::assignment_ro(Some("Str")));
        }
        // When the LHS alias *is* the topic itself (`$_ ~~ s///`), the
        // substitution-modified topic must persist — restoring `saved_topic`
        // below would clobber it. Skip the restore in that case.
        let lhs_is_topic = lhs_var.as_deref() == Some("_");
        // The LHS-variable writeback exists for a DESTRUCTIVE RHS (`$x ~~ s///`,
        // `tr///`, or a block that assigns the topic): only then has the topic
        // been modified and must flow back into the variable. A pure predicate
        // match (`$json ~~ Int`) must NOT re-write the variable — the
        // `note_caller_env_write` below would flag it for reverse sync into the
        // caller, clobbering a same-named caller lexical of a different sigil
        // (JSON::Unmarshal: the callee's `$json` Int overwrote the caller's
        // `%json` hash after `if $json ~~ Int`).
        let topic_after = self.env().get("_").cloned().unwrap_or(Value::NIL);
        let topic_modified = was_substitution
            || was_transliterate
            || !crate::runtime::utils::values_identical(&topic_after, &left);
        if let Some(var_name) = lhs_var.as_ref().filter(|_| topic_modified) {
            let modified_topic = topic_after.clone();
            self.env_mut()
                .insert(var_name.clone(), modified_topic.clone());
            // Reverse write-through: if the lhs alias names a compiled local slot,
            // mirror the (possibly substitution-modified) topic into it so the
            // caller sees it without an O(locals) sync_locals_from_env pull.
            // §1.5: prefer the compile-time-baked `lhs_slot` (scope-correct even
            // once a name occupies several slots) over the by-name resolution; the
            // by-name fallback stays for a `None` slot (global LHS, or an EVAL /
            // carrier where the var is an outer lexical with no current-frame slot).
            match lhs_slot {
                Some(slot) if (slot as usize) < self.locals.len() => {
                    self.locals[slot as usize] = modified_topic.clone();
                }
                _ => self.update_local_if_exists(code, var_name, &modified_topic),
            }
            // The env insert above bypasses `set_env_with_main_alias`, and
            // `update_local_if_exists` only touches the *current* frame's slot —
            // when `$x ~~ s///` runs inside an EVAL/carrier, `var_name` is an outer
            // lexical with no current-frame slot, so flag it for the reverse sync
            // (carrier log + env_dirty). Otherwise a carrier dropping its blanket
            // net loses the `~~ s///` writeback (Slice C', open-question #2).
            self.note_caller_env_write(var_name);
            // A scalar-attribute LHS (`$!query ~~ s/...//`) lives in self's
            // shared attribute cell, not just the env copy — mirror the
            // substitution result there so the mutation persists past the
            // method frame.
            if var_name.starts_with('!') || var_name.starts_with('.') {
                self.write_self_attr_cell(var_name, modified_topic.clone());
            }
            // When the topic itself was modified (`$_ ~~ s///` inside a
            // `given $x` / `with $x` block), `$_` aliases the source scalar
            // variable, so the in-place substitution must propagate back to it —
            // mirroring the whole-topic writeback the `$_ = ...` assign path does.
            if lhs_is_topic
                && let Some(ref source_var) = self.topic_source_var
                && !source_var.starts_with('@')
                && !source_var.starts_with('%')
            {
                let sv = source_var.clone();
                self.set_env_with_main_alias(&sv, modified_topic.clone());
                self.update_local_if_exists(code, &sv, &modified_topic);
            }
        }
        if !lhs_is_topic {
            if let Some(v) = saved_topic {
                self.env_mut().insert("_".to_string(), v);
            } else {
                self.env_mut().remove("_");
            }
        }
        // When RHS was a transliterate (tr///), return the result directly.
        // In Raku, $x ~~ tr/a/b/ returns a StrDistance that stringifies to the after-string.
        let out = if was_transliterate {
            if negate {
                // !~~ tr/// — negate the truthiness of the result
                Value::truth(!right.truthy())
            } else {
                right
            }
        } else if was_substitution {
            // s/// returns Match on success, False on failure.
            // For !~~, negate the boolean result.
            if negate {
                Value::truth(!right.truthy())
            } else {
                right
            }
        } else {
            // When the RHS is a callable (`$x ~~ (* =:= $y)`), Raku binds the
            // topic to the code's parameter `is raw` — so a container-identity
            // body (`Code.ACCEPTS preserves container`) sees the caller's actual
            // container, not a decontainerized copy. Record the LHS variable as
            // the argument source so the raw-`_` binding aliases `_` to it (the
            // same path an explicit `(* =:= $y)($x)` call takes). Harmless for a
            // by-value param, which ignores `arg_sources`.
            // `lhs_var` is only `Some` for a scalar LHS (`Expr::Var`); an array /
            // hash LHS compiles to `ArrayVar` / `HashVar` and yields `None`. The
            // name is already sigil-less (`$foo` -> `"foo"`), matching the alias
            // root used by the raw-`_` binding.
            if matches!(right.view(), ValueView::Sub(_))
                && let Some(v) = lhs_var
                && !v.is_empty()
            {
                self.set_pending_call_arg_sources(Some(vec![Some(v.clone())]));
            }
            self.eval_smartmatch_with_junctions_ex(left, right, negate, rhs_is_match_regex)?
        };
        self.stack.push(out);
        // Slice 6.3 step 2 — precise env_dirty for smartmatch. Skip the
        // O(caller-locals) re-sync only when this was a side-effect-free match:
        //   * a plain `Regex` value literal (rhs_pure_regex) — excludes
        //     RegexWithAdverbs (`:pos`/`:g` carry state), named/Sub regexes, and
        //     value smartmatch, all of which can write caller state by name;
        //   * not a substitution / transliteration (those mutate the topic alias);
        //   * the match ran no embedded `{ }` code block that wrote a caller
        //     variable (the regex engine logs those in `pending_local_updates`);
        //   * `$/` is not itself a compiled local slot here (`my $/` — e.g.
        //     continued matching reads it back across calls).
        // A plain regex match otherwise only writes `$/`/captures, which are
        // special vars read by name, never a caller local slot, so no pull.
        let wrote_caller_via_code = !self.pending_local_updates.is_empty();
        let match_var_is_local = code.locals.iter().any(|n| n == "/");
        let pure = rhs_pure_regex
            && !was_substitution
            && !was_transliterate
            && !wrote_caller_via_code
            && !match_var_is_local;
        // Slice F: a match writes caller lexicals straight into `env` — the match
        // variable `$/`, numbered captures (`$0`/`$1`/…), and an embedded `{ }` /
        // `:my` / `:let` block's writes (logged in `pending_local_updates`). Write
        // those through to the caller's local slots here so the slot stays
        // coherent without the reverse `sync_locals_from_env` pull. `env_dirty`
        // below is kept as a net (reverse-sync ON behaves identically); with it
        // OFF this write-through is what keeps the value correct.
        if !pure {
            let written: std::collections::HashSet<String> = self
                .pending_local_updates
                .iter()
                .map(|(n, _)| n.clone())
                .collect();
            self.writeback_match_locals(code, &written);
            // env_dirty substrate (docs/captured-outer-cell-sharing.md §10): an
            // embedded `{ }` / `:my` / `:let` block writes a caller lexical that is
            // NOT a slot of *this* frame — e.g. a regex run inside `sub do-match { … }`
            // mutates a lexical captured from the sub's caller. `writeback_match_locals`
            // only reaches this frame's slots, and the owning slot lives one or more
            // frames up, so record those names for the retain-on-miss caller-var
            // writeback; the call site that returns to the owning frame drains them
            // (`apply_pending_caller_var_writeback`). This is what keeps the slot
            // coherent once the blanket/precise reconcile is gone (double-OFF). Only
            // armed under boxing; the default build's blanket reconcile covers it.
            for name in &written {
                let is_match_name =
                    name == "/" || (!name.is_empty() && name.bytes().all(|b| b.is_ascii_digit()));
                if is_match_name || self.find_local_slot(code, name).is_some() {
                    continue;
                }
                self.record_caller_var_writeback(name);
            }
        }
        self.pending_local_updates.clear();
        // env_dirty substrate (docs/captured-outer-cell-sharing.md §10): a custom
        // HOW `type_check`/`accepts_type`/`find_method` invoked by this smartmatch
        // (`$obj ~~ $custom-type`) may mutate a captured-outer caller scalar (e.g.
        // `++$counter`); the slow HOW dispatch records those names into the
        // retain-on-miss caller-var writeback but never sets `env_dirty`. Drain it
        // here so the owning slot is refreshed before the next statement reads it
        // (critical for the read-modify-write `++$counter`). No-op in default
        // builds (the recording is gated on cell_boxing_active); cross-frame
        // names recorded by the regex path above are retained for the caller.
        self.apply_pending_caller_var_writeback(code);
        *ip = rhs_end;
        Ok(())
    }

    pub(super) fn exec_scalarize_regex_match_result_op(&mut self) -> Result<(), RuntimeError> {
        let value = self.stack.pop().unwrap_or(Value::NIL);
        let scalarized = match value.view() {
            ValueView::Nil => Value::int(0),
            ValueView::Array(items, _) => Value::int(items.len() as i64),
            ValueView::Seq(items)
            | ValueView::HyperSeq(items)
            | ValueView::RaceSeq(items)
            | ValueView::Slip(items) => Value::int(items.len() as i64),
            ValueView::Capture { positional, .. } => Value::int(positional.len() as i64),
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Match" => {
                if let Some(ValueView::Array(items, _)) =
                    attributes.as_map().get("list").map(Value::view)
                {
                    Value::int(items.len() as i64)
                } else {
                    Value::int(1)
                }
            }
            _ if value.truthy() => Value::int(1),
            _ => Value::int(0),
        };
        self.stack.push(scalarized);
        Ok(())
    }

    pub(super) fn exec_divisible_by_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // Thread over junctions
        if matches!(left.view(), ValueView::Junction { .. })
            || matches!(right.view(), ValueView::Junction { .. })
        {
            let result = self
                .eval_binary_with_junctions(left, right, |vm, l, r| vm.divisible_by_values(l, r))?;
            self.stack.push(result);
            return Ok(());
        }
        let result = self.divisible_by_values(left.clone(), right)?;
        self.stack.push(result);
        Ok(())
    }

    fn divisible_by_values(&self, left: Value, right: Value) -> Result<Value, RuntimeError> {
        Ok(Value::truth(self.is_divisible(left, right)?))
    }

    /// `$a %% $b` is `$a % $b == 0`. Compute the modulo with the exact-rational
    /// semantics of `arith_mod` (so Rat/Num/BigInt operands work, not just Int),
    /// then test the remainder for zero. A zero divisor reports the dividend and
    /// `infix:<%%>`, matching Rakudo.
    fn is_divisible(&self, left: Value, right: Value) -> Result<bool, RuntimeError> {
        let (l, r) = runtime::coerce_numeric(left.clone(), right);
        if !r.truthy() {
            return Err(RuntimeError::numeric_divide_by_zero_full(
                Some(left),
                Some("infix:<%%>"),
            ));
        }
        let remainder = crate::builtins::arith_mod(l, r)?;
        Ok(!remainder.truthy())
    }

    pub(super) fn exec_not_divisible_by_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // Thread over junctions
        if matches!(left.view(), ValueView::Junction { .. })
            || matches!(right.view(), ValueView::Junction { .. })
        {
            let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
                vm.not_divisible_by_values(l, r)
            })?;
            self.stack.push(result);
            return Ok(());
        }
        let result = self.not_divisible_by_values(left.clone(), right)?;
        self.stack.push(result);
        Ok(())
    }

    fn not_divisible_by_values(&self, left: Value, right: Value) -> Result<Value, RuntimeError> {
        Ok(Value::truth(!self.is_divisible(left, right)?))
    }
}
