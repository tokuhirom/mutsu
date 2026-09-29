//! `OpCode::DoBlockExpr`: a bare block in value position (`do { ... }`, a
//! block used as a term, a `"{...}"` interpolation), and the
//! [`DoBlockIsolation`](crate::opcode::DoBlockIsolation) policies that decide
//! which of its bindings are reverted when it exits.

use super::*;

impl Interpreter {
    // Cost: O(1) plus the body; a scope-isolating block (string-interpolation `{...}`)
    // adds O(w + k + s), w = names it wrote by name (its block tier), k = names it
    // declares, s = the chunk's special/internal local slots (saved and reverted);
    // a source block (`DoBlockIsolation::Lexical`) adds O(w + k);
    // `scope_routines` adds O(R), R = routine-registry entries. Rakudo: O(1) -- see #9170.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_do_block_expr_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        label: &Option<String>,
        isolation: crate::opcode::DoBlockIsolation,
        isolate_decls_idx: u32,
        scope_routines: bool,
        ip: &mut usize,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        let body_start = *ip + 1;
        let end = body_end as usize;
        let label = label.clone();
        let stack_base = self.stack.len();
        let saved_when_matched = self.when_matched();
        let once_scope = self.next_once_scope_id();
        self.push_once_scope(once_scope);
        self.push_enum_scope();
        // A `do { ... }` block is a block, so a routine declared directly in it
        // is lexical to it: it must not leak out, and an outer routine of the
        // same name must come back when the block exits. `OpCode::BlockScope`
        // (the statement-form bare block), every routine call and every
        // for-loop body that declares routines already take this
        // snapshot/restore pair; the value-position `do` form was the one that
        // did not, so `sub foo() {...}` inside it permanently replaced an outer
        // `foo`. The compiler sets `scope_routines` only when the body actually
        // declares a routine, so the common case pays nothing.
        let routine_snapshot = scope_routines.then(|| self.snapshot_routine_registry());
        // `OpCode::BlockScope` raises `block_scope_depth` for every statement
        // bare block unconditionally, which is what lets a closure escaping the
        // block still call the routine it declared: `RegisterSub` only stashes
        // that escape-hatch copy (`BLOCK_LEXICAL_SUB_PREFIX`) while
        // `block_scope_depth() > 0`. This op never raised it, so the identical
        // `do { sub foo {...}; -> { foo() } }` -- a closure that escapes a
        // VALUE-position block instead of a statement one -- died with "Unknown
        // function" once the registry snapshot above was restored (#9636).
        // Scoped to `scope_routines` (this block actually declares one) rather
        // than raised unconditionally, matching the snapshot's own gate: no
        // other reader of `block_scope_depth` needs it bumped for a block that
        // declares no routine.
        if scope_routines {
            self.push_block_scope_depth();
        }
        // A scope-isolating block runs over a block tier (see `vm_block_env`),
        // so its exit reads back only what it wrote by name, and saves just the
        // local slots it may have to revert rather than the whole frame (#9170).
        let lexical = isolation == crate::opcode::DoBlockIsolation::Lexical;
        let saved_env = if isolation != crate::opcode::DoBlockIsolation::None {
            let isolate_set = Self::isolate_decl_names(code, isolate_decls_idx, lexical);
            let revert_slots: Vec<(usize, Value)> = if lexical {
                Self::lexical_revert_slots(code, &isolate_set)
            } else {
                Self::isolate_revert_slots(code, &isolate_set)
            }
            .into_iter()
            .filter_map(|idx| self.locals.get(idx).map(|v| (idx, v.clone())))
            .collect();
            Some((self.open_block_env_tier(), isolate_set, revert_slots))
        } else {
            None
        };
        // A bare block — labelled or not — is not a loop construct in rakudo:
        // `last`/`next`/`redo` (even `last LAB` naming this block's own label)
        // must NOT be caught here, and entering this block must not raise
        // `loop_handler_depth` either, or `LAB: { next }` would look handled
        // and silently leave the block instead of raising
        // `X::ControlFlow` ("labeled next without loop construct")
        // (`todo/tickets/labelled-bare-block-is-not-a-loop-construct.md`).
        // Only `leave` (a block-exit statement, not a loop-control one) is
        // caught by an enclosing block regardless of label.
        let result = match self.run_range(code, body_start, end, compiled_fns) {
            Ok(()) => Ok(()),
            Err(e)
                if e.is_leave
                    && e.leave_callable_id().is_none()
                    && e.leave_routine().is_none()
                    && Self::label_matches(&e.label, &label) =>
            {
                self.stack.truncate(stack_base);
                self.stack.push(
                    e.return_value
                        .unwrap_or(Value::slip_arc(std::sync::Arc::new(vec![]))),
                );
                Ok(())
            }
            // A `when`/`default` succeed exits its innermost enclosing
            // block — this one. `do { when Int { "i" } }` yields the matched
            // body's value and execution continues after the `do`; raku does
            // NOT let the succeed travel on to an outer `given`/`with`
            // (DBIish's execute loses its whole bind-setup `given` when the
            // preceding `do { when ... }` escapes it). The `when_matched`
            // flag is reset too — an enclosing `given` breaks its body on it
            // after every op, which would skip the rest of the given.
            Err(e) if e.is_succeed() => {
                self.stack.truncate(stack_base);
                self.stack.push(e.return_value.unwrap_or(Value::NIL));
                loan_env!(self, set_when_matched(saved_when_matched));
                Ok(())
            }
            Err(e) => Err(e),
        };
        self.pop_enum_scope();
        self.pop_once_scope();
        if scope_routines {
            self.pop_block_scope_depth();
        }
        if let Some(routine_snapshot) = routine_snapshot {
            self.restore_routine_registry(routine_snapshot);
        }
        // Restore scope if the block isolates
        if let Some((saved_env, isolate_set, revert_slots)) = saved_env {
            let block_result = self.stack.pop().unwrap_or(Value::NIL);
            // Scope-isolating exit. The block isolates its OWN scalar/array `my`/
            // `state` declarations (reverted to the pre-block value so they don't
            // leak), but a mutation of an OUTER variable must persist —
            // `"{ $x = 1 }"` / `"{ foo() }"` where `foo` writes an outer/`our`
            // var. New hashes still leak (the `:into(my %h := :{})` idiom).
            //   - declared scalar/array name (isolate set) -> revert (skip).
            //   - other plain user var that changed            -> keep (outer mutation).
            //   - new hash                                      -> keep (`:into`).
            //   - internal `__mutsu_*` / specials / dynamics    -> revert.
            // Isolate-set keyed by BARE name (sigil stripped) so it matches an env
            // key regardless of whether that scalar/array is stored with or
            // without its sigil (e.g. a `state $a` whose env mirror would
            // otherwise be re-carried and pollute the next evaluation's init).
            //
            // Only the block's own writes are candidates: every other binding
            // is, by construction, still the entry env's.
            let closed = self.close_block_env_tier(saved_env);
            let mut restored_env = closed.base;
            let mut new_vars: Vec<(Symbol, Value)> = Vec::new();
            for (name, value) in closed.writes.iter() {
                let nm = name.resolve();
                // A source block reverts exactly its own declarations: every
                // other write it made is to a binding that outlives it.
                if lexical {
                    if !Self::lexical_block_owns(&nm, &isolate_set) {
                        new_vars.push((*name, value.clone()));
                    }
                    continue;
                }
                if isolate_set.contains(Self::strip_isolate_sigil(&nm)) {
                    continue;
                }
                let keep = match restored_env.get_sym(*name) {
                    None => nm.starts_with('%') && !nm.starts_with("%*"),
                    Some(saved_val) => Self::is_plain_isolate_user_var(&nm) && value != saved_val,
                };
                if keep {
                    new_vars.push((*name, value.clone()));
                }
            }
            // Re-insert the kept writes
            for (name, value) in new_vars {
                restored_env.insert_sym(name, value);
            }
            // A source block's removal of an outer binding outlives it too.
            if lexical {
                for name in closed.removed.iter().flatten() {
                    if !name.with_str(|nm| Self::lexical_block_owns(nm, &isolate_set)) {
                        restored_env.remove_sym(*name);
                    }
                }
            }
            *self.env_mut() = restored_env;
            // (B) per-store env-write: the env mirror is suppressed, so re-seeding
            // every slot from the restored env would clobber a propagating outer
            // var's LIVE in-block value with its stale decl-time seed — e.g.
            // `my $ct = CT.new(...)` whose slot never mirrored into env, so a second
            // `$ct.x` inside one interpolation read `Any`. The live in-block
            // `self.locals` already holds the correct value for a propagating outer
            // name (the block body read/mutated the slot directly). Revert ONLY the
            // block's OWN isolated declarations (reverted to their pre-block value so
            // they don't leak) and internal/special/dynamic slots; leave every other
            // slot at its live value. This mirrors the env-side keep/revert decision
            // above without depending on the env mirror.
            for (idx, val) in revert_slots {
                self.locals[idx] = val;
            }
            self.stack.push(block_result);
        }
        *ip = end;
        result
    }

    /// A scope-isolating block's own declarations, from the
    /// `isolate_decls_idx` constant of `OpCode::DoBlockExpr`: by exact env name
    /// for a `DoBlockIsolation::Lexical` block, by bare (sigil-stripped) name
    /// for the interpolation policy.
    // Cost: O(k), k = names the block declares.
    fn isolate_decl_names(
        code: &CompiledCode,
        isolate_decls_idx: u32,
        exact: bool,
    ) -> std::collections::HashSet<String> {
        if isolate_decls_idx == u32::MAX {
            return std::collections::HashSet::new();
        }
        match code.constants[isolate_decls_idx as usize].view() {
            ValueView::Array(items, ..) => items
                .iter()
                .filter_map(|v| match v.view() {
                    ValueView::Str(s) if exact => Some(s.to_string()),
                    ValueView::Str(s) => Some(Self::strip_isolate_sigil(s.as_ref()).to_string()),
                    _ => None,
                })
                .collect(),
            _ => std::collections::HashSet::new(),
        }
    }

    fn strip_isolate_sigil(name: &str) -> &str {
        name.strip_prefix(['$', '@', '%', '&']).unwrap_or(name)
    }

    fn is_plain_isolate_user_var(name: &str) -> bool {
        crate::opcode::is_plain_user_var_name(name)
    }

    /// The local slots a scope-isolating block reverts on exit: its own
    /// declarations (`isolate_set`) and every slot that is not a plain user
    /// variable. Found through the chunk's indexes rather than by scanning the
    /// frame's locals.
    // Cost: O(k + s), k = names the block declares, s = the chunk's non-plain
    // (special/internal/dynamic) local slots.
    fn isolate_revert_slots(
        code: &CompiledCode,
        isolate_set: &std::collections::HashSet<String>,
    ) -> Vec<usize> {
        let mut slots: Vec<usize> = code
            .non_plain_local_slots()
            .iter()
            .map(|&i| i as usize)
            .collect();
        for bare in isolate_set {
            slots.extend(code.local_slots_of_bare(bare).iter().map(|&i| i as usize));
        }
        slots.sort_unstable();
        slots.dedup();
        slots
    }

    /// Whether the env key `name` is a binding a `DoBlockIsolation::Lexical`
    /// block's own declarations (`decls`) made, and so reverts on exit. Besides
    /// the declared name itself that is its type-constraint metadata
    /// (`__mutsu_type::o` for a `my Int $o`) and the sigilled mirror of a
    /// `my $*x` (`$*x` next to `*x`) -- the same ownership rule
    /// `exec_block_scope_op` applies to a statement block.
    // Cost: O(1) (a constant number of set probes).
    fn lexical_block_owns(name: &str, decls: &std::collections::HashSet<String>) -> bool {
        decls.contains(name)
            || name
                .strip_prefix("__mutsu_type::")
                .or_else(|| name.strip_prefix("__mutsu_hash_key_type::"))
                .is_some_and(|base| decls.contains(base))
            || crate::runtime::utils::twigil_dynamic_alias(name)
                .is_some_and(|alias| decls.contains(alias.as_str()))
    }

    /// The local slots a `DoBlockIsolation::Lexical` block reverts on exit:
    /// exactly those of its own declarations.
    // Cost: O(k), k = names the block declares.
    fn lexical_revert_slots(
        code: &CompiledCode,
        decls: &std::collections::HashSet<String>,
    ) -> Vec<usize> {
        let mut slots: Vec<usize> = decls
            .iter()
            .flat_map(|name| code.local_slots_named(name).iter().map(|&i| i as usize))
            .collect();
        slots.sort_unstable();
        slots.dedup();
        slots
    }
}
