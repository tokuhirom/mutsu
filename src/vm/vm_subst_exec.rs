use super::vm_subst_repl::{SubstMatchCaps, SubstReplPlan};
use super::*;

/// Everything a substitution op needs from the bytecode operands, resolved once.
pub(super) struct SubstOp {
    pub(super) pattern: String,
    pub(super) plan: SubstReplPlan,
    pub(super) nth_spec: Option<String>,
    pub(super) x_spec: Option<String>,
    pub(super) samecase: bool,
    pub(super) sigspace: bool,
    pub(super) samemark: bool,
    pub(super) samespace: bool,
    pub(super) global: bool,
    pub(super) perl5: bool,
}

/// The result of running a substitution over the topic.
pub(super) struct SubstOutcome {
    /// The substituted text — the untouched subject when nothing matched.
    pub(super) text: String,
    /// What `$/` becomes: a Match, a List of Matches, `Nil`, or an empty List.
    pub(super) slash: Value,
    /// True when at least one match was replaced.
    pub(super) matched: bool,
    /// True when this substitution's `$/` is a List (`:g`, `:x`, multi-`:nth`).
    pub(super) is_list: bool,
}

impl Interpreter {
    #[allow(clippy::too_many_arguments)]
    fn subst_op(
        &mut self,
        code: &CompiledCode,
        pattern_idx: u32,
        replacement_idx: u32,
        samecase: bool,
        sigspace: bool,
        samemark: bool,
        samespace: bool,
        global: bool,
        nth_idx: Option<u32>,
        x_idx: Option<u32>,
        perl5: bool,
    ) -> SubstOp {
        let pattern = Self::const_str(code, pattern_idx).to_string();
        // The replacement is a `qq` quote (see `vm_subst_repl`): parse it with
        // the real interpolation grammar, once, and cache the plan.
        let plan = self.subst_replacement_plan(Self::const_str(code, replacement_idx));
        SubstOp {
            pattern,
            plan,
            nth_spec: nth_idx.map(|idx| Self::const_str(code, idx).to_string()),
            x_spec: x_idx.map(|idx| Self::const_str(code, idx).to_string()),
            samecase,
            sigspace,
            samemark,
            samespace,
            global,
            perl5,
        }
    }

    /// Every match of `op`'s pattern in `text`, with its captures. Only the
    /// first match is searched for when the op can never use more than one
    /// (no `:g`, no `:nth`, no `:x`), so a plain `s///` still stops early.
    fn subst_collect_matches(
        &mut self,
        op: &SubstOp,
        text: &str,
    ) -> Vec<(usize, usize, SubstMatchCaps)> {
        let first_only = op.nth_spec.is_none() && op.x_spec.is_none() && !op.global;
        if op.perl5 {
            let mut all: Vec<(usize, usize, Vec<String>)> =
                loan_env!(self, regex_find_all_p5_with_captures(&op.pattern, text));
            if first_only {
                all.truncate(1);
            }
            return all
                .into_iter()
                .map(|(s, e, positional)| {
                    (
                        s,
                        e,
                        SubstMatchCaps {
                            positional,
                            named: std::collections::HashMap::new(),
                        },
                    )
                })
                .collect();
        }
        let mut out = Vec::new();
        let mut pos = 0usize;
        while let Some((start, end, positional, named)) = loan_env!(
            self,
            regex_find_first_from_with_all_captures(&op.pattern, text, pos)
        ) {
            out.push((start, end, SubstMatchCaps { positional, named }));
            if first_only {
                break;
            }
            pos = if end > start { end } else { start + 1 };
        }
        out
    }

    /// Run a substitution against the topic and report what it produced. Shared
    /// by `s///` (which then writes the topic) and `S///` (which does not).
    fn run_subst(&mut self, op: &SubstOp) -> Result<SubstOutcome, RuntimeError> {
        let target = self.env().get("_").cloned().unwrap_or(Value::NIL);
        let text = target.to_string_value();
        self.reset_capture_env_vars();

        let all = self.subst_collect_matches(op, &text);
        let all_ranges: Vec<(usize, usize)> = all.iter().map(|(s, e, _)| (*s, *e)).collect();
        let selected = if op.global && op.nth_spec.is_none() && op.x_spec.is_none() {
            all_ranges
        } else {
            Self::select_substitution_ranges(
                &all_ranges,
                op.nth_spec.as_deref(),
                op.x_spec.as_deref(),
            )?
        };
        let caps: Vec<SubstMatchCaps> = selected
            .iter()
            .filter_map(|r| {
                all.iter()
                    .find(|(s, e, _)| *s == r.0 && *e == r.1)
                    .map(|(_, _, c)| c.clone())
            })
            .collect();

        // With :g, :x, or a multi-value :nth the result (and `$/`) is a List of
        // Match objects; a bare substitution yields a single Match. A *single*
        // :nth(N) forces a single Match even when combined with :g.
        let single_nth = op.nth_spec.as_deref().is_some_and(|s| !s.contains(','));
        let nth_is_multi = op.nth_spec.as_deref().is_some_and(|s| s.contains(','));
        let is_list = !single_nth && (op.global || op.x_spec.is_some() || nth_is_multi);

        if selected.is_empty() {
            return Ok(SubstOutcome {
                text,
                slash: if is_list {
                    Value::array(Vec::new())
                } else {
                    Value::NIL
                },
                matched: false,
                is_list,
            });
        }

        let out = match &op.plan {
            SubstReplPlan::Static(repl) => Self::apply_substitutions(
                &text,
                &selected,
                repl,
                op.samecase,
                op.sigspace,
                op.samemark,
                op.samespace,
            ),
            SubstReplPlan::Dynamic {
                body,
                cache_id,
                capture_parts,
            } => {
                let body = body.clone();
                let parts = capture_parts.clone();
                self.apply_substitutions_dynamic(
                    &text,
                    &selected,
                    &body,
                    *cache_id,
                    parts.as_deref().map(|p| p.as_slice()),
                    &caps,
                    op,
                )?
            }
        };
        let slash = if is_list {
            let matches: Vec<Value> = selected
                .iter()
                .enumerate()
                .map(|(i, (s, e))| {
                    Self::make_subst_match(
                        &text,
                        *s,
                        *e,
                        caps.get(i).unwrap_or(&Default::default()),
                    )
                })
                .collect();
            Value::array(matches)
        } else {
            let (s, e) = selected[0];
            Self::make_subst_match(&text, s, e, caps.first().unwrap_or(&Default::default()))
        };
        Ok(SubstOutcome {
            text: out,
            slash,
            matched: true,
            is_list,
        })
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_subst_op(
        &mut self,
        code: &CompiledCode,
        pattern_idx: u32,
        replacement_idx: u32,
        samecase: bool,
        sigspace: bool,
        samemark: bool,
        samespace: bool,
        global: bool,
        nth_idx: Option<u32>,
        x_idx: Option<u32>,
        perl5: bool,
    ) -> Result<(), RuntimeError> {
        let op = self.subst_op(
            code,
            pattern_idx,
            replacement_idx,
            samecase,
            sigspace,
            samemark,
            samespace,
            global,
            nth_idx,
            x_idx,
            perl5,
        );
        let outcome = self.run_subst(&op)?;
        if outcome.matched {
            self.write_subst_topic_checked(code, Value::str(outcome.text))?;
        }
        self.env_mut()
            .insert("/".to_string(), outcome.slash.clone());
        self.substitution_in_smartmatch = self.in_smartmatch_rhs;
        self.stack.push(if outcome.matched || outcome.is_list {
            outcome.slash
        } else {
            Value::FALSE
        });
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_non_destructive_subst_op(
        &mut self,
        code: &CompiledCode,
        pattern_idx: u32,
        replacement_idx: u32,
        samecase: bool,
        sigspace: bool,
        samemark: bool,
        samespace: bool,
        global: bool,
        nth_idx: Option<u32>,
        x_idx: Option<u32>,
        perl5: bool,
    ) -> Result<(), RuntimeError> {
        let op = self.subst_op(
            code,
            pattern_idx,
            replacement_idx,
            samecase,
            sigspace,
            samemark,
            samespace,
            global,
            nth_idx,
            x_idx,
            perl5,
        );
        let outcome = self.run_subst(&op)?;
        // S/// sets $/ to the match (without mutating $_) and yields the string.
        self.env_mut().insert("/".to_string(), outcome.slash);
        self.stack.push(Value::str(outcome.text));
        Ok(())
    }

    /// Write a destructive `s///` result back to the topic, throwing
    /// `X::Assignment::RO` when the topic is bound read-only (e.g. a
    /// `method ro($_) {...}` / `sub ($x is readonly) {...}` parameter). Only
    /// reached after a substitution actually occurred, so a non-matching
    /// `s///` against a read-only topic stays a no-op.
    fn write_subst_topic_checked(
        &mut self,
        code: &CompiledCode,
        result: Value,
    ) -> Result<(), RuntimeError> {
        if self.is_readonly("_") {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert(
                "message".to_string(),
                Value::str("Cannot modify an immutable Str".to_string()),
            );
            attrs.insert("value".to_string(), result);
            return Err(RuntimeError::typed("X::Assignment::RO", attrs));
        }
        self.env_mut().insert("_".to_string(), result.clone());
        self.env_mut().insert("$_".to_string(), result.clone());
        self.env_mut()
            .insert("__mutsu_rw_map_topic__".to_string(), result.clone());
        // Slice F (regex carrier): a bare `s///` modifies the topic `$_` by name
        // in `env`. When `my $_` makes `_` a compiled local slot, write the new
        // value through to that slot so it stays coherent without the reverse
        // `sync_locals_from_env` pull. If `$_` aliases a `given`/`for` source
        // scalar (`topic_source_var`), mirror the modified topic back to it too,
        // matching the `$x ~~ s///` smartmatch writeback path. Both `_` and the
        // source name are flagged for the carrier (env_dirty) so an enclosing
        // EVAL/carrier dropping its blanket net still reconciles the slot.
        self.update_local_if_exists(code, "_", &result);
        self.note_caller_env_write("_");
        // Inside a smartmatch RHS (`$frag ~~ s///`) the topic is temporarily the
        // smartmatch LHS, not the enclosing `given`/`for` source — mirroring
        // there would clobber the source scalar with the substitution result
        // (`given $in { $frag ~~ s/^"row="//; }` must not touch `$in`). The
        // smartmatch handler owns every writeback in that case, including
        // `$_ ~~ s///` via its own topic-source mirror.
        if !self.in_smartmatch_rhs
            && let Some(source_var) = self.topic_source_var.clone()
            && !source_var.starts_with('@')
            && !source_var.starts_with('%')
        {
            self.set_env_with_main_alias(&source_var, result.clone());
            self.update_local_if_exists(code, &source_var, &result);
            self.note_caller_env_write(&source_var);
        }
        Ok(())
    }
}
