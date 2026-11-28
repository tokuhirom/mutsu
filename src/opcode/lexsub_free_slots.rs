//! Slot reads for a routine-nested sub's aliased free variables (ADR-18244
//! stage B, #18274; the runtime half is `vm/vm_lexsub_free_slot.rs`).

use super::{CompiledCode, LexFreeKind, OpCode};
use crate::symbol::Symbol;
use crate::value::{Value, ValueView};

/// Prefix of the hidden local that holds one free variable's binding for a
/// call. A `__mutsu_` name is a system name no user code can spell.
const LEX_FREE_SLOT_PREFIX: &str = "__mutsu_lexfree_";

/// The reads [`CompiledCode::apply_lexsub_free_slot_plan`] rewrites: each
/// chosen variable with its slot plus one (0 = not allocated yet), and each
/// read's op position, variable index, name constant and kind.
pub(crate) struct LexFreePlan {
    chosen: Vec<(Symbol, u32)>,
    rewrites: Vec<(usize, usize, u32, LexFreeKind)>,
}

impl CompiledCode {
    /// Plan the rewrite of this finalized routine body's by-name reads of `vars` -- free
    /// variables its declaring scope binds per activation (ADR-0114 aliases)
    /// -- into [`OpCode::GetLexFree`], each name reading through a hidden
    /// local of its own appended to the frame.
    ///
    /// Only pure reads are rewritten (`GetGlobal`, `GetArrayVar`,
    /// `GetHashVar`); writes keep their by-name ops, which reach the same
    /// alias cell. A name stays by-name when this body could see another
    /// binding of it in the middle of a call: one it rebinds, one a by-name
    /// loop parameter of the body also binds, one whose raw container a
    /// `WrapVarRef` captures (its by-name cell recovery), and an aggregate
    /// whose name the body loads as a string (an atomic helper addresses it
    /// by name).
    ///
    /// Runs after `compute_needs_env_sync`, so the appended locals are in no
    /// env-sync set; every per-local table the finalize pass fills is
    /// extended to match.
    // Cost: O(o + v * c), o = ops, v = `vars`, c = constants.
    pub(crate) fn lexsub_free_slot_plan(&self, vars: &[Symbol]) -> Option<LexFreePlan> {
        let mut chosen: Vec<(Symbol, u32)> = Vec::new();
        for &var in vars {
            if self.free_var_rebinds.contains(&var)
                || self.for_loop_param_syms.contains(&var)
                || self.container_ref_capture_syms.contains(&var)
                || !self.free_var_syms.contains(&var)
            {
                continue;
            }
            let name = var.resolve();
            if name.starts_with(['@', '%']) && self.loads_name_as_string(&name) {
                continue;
            }
            // The body's own local of the name (a parameter, a `my`) shadows
            // the free variable for every read the compiler resolved by name.
            if self.locals.iter().any(|l| *l == *name) {
                continue;
            }
            chosen.push((var, 0));
        }
        if chosen.is_empty() {
            return None;
        }
        let mut rewrites: Vec<(usize, usize, u32, LexFreeKind)> = Vec::new();
        for (pos, op) in self.ops.iter().enumerate() {
            let (idx, kind) = match op {
                OpCode::GetGlobal(idx) => (*idx, LexFreeKind::Scalar),
                OpCode::GetArrayVar(idx) => (*idx, LexFreeKind::Array),
                OpCode::GetHashVar(idx) => (*idx, LexFreeKind::Hash),
                _ => continue,
            };
            let sym = self.const_sym(idx);
            if let Some(i) = chosen.iter().position(|(v, _)| *v == sym) {
                rewrites.push((pos, i, idx, kind));
            }
        }
        if rewrites.is_empty() {
            return None;
        }
        Some(LexFreePlan { chosen, rewrites })
    }

    /// Apply a [`Self::lexsub_free_slot_plan`] of this same chunk.
    // Cost: O(r), r = rewritten reads.
    pub(crate) fn apply_lexsub_free_slot_plan(&mut self, plan: LexFreePlan) {
        let LexFreePlan {
            mut chosen,
            rewrites,
        } = plan;
        for (pos, i, name_idx, kind) in rewrites {
            if chosen[i].1 == 0 {
                chosen[i].1 = self.push_hidden_local(chosen[i].0) + 1;
            }
            self.ops[pos] = OpCode::GetLexFree {
                slot: chosen[i].1 - 1,
                name_idx,
                kind,
            };
        }
    }

    /// Append a hidden local for `var`'s free-variable binding to this
    /// finalized chunk and return its slot.
    // Cost: O(1) amortized.
    fn push_hidden_local(&mut self, var: Symbol) -> u32 {
        let n = self.locals.len();
        let name = var.with_str(|v| format!("{LEX_FREE_SLOT_PREFIX}{v}"));
        // Each per-local table is extended only when it already covers every
        // local, so one the finalize pass left unfilled stays unfilled.
        if self.locals_sym.len() == n {
            self.locals_sym.push(Symbol::intern(&name));
        }
        if self.binding_descs.len() == n {
            let flags = crate::binding_desc::BindingFlags::new(false, false);
            self.binding_descs
                .push(crate::binding_desc::BindingDesc::new(flags));
        }
        if self.dup_named_locals.len() == n {
            self.dup_named_locals.push(false);
        }
        self.locals.push(name);
        n as u32
    }

    /// Whether some `LoadConst` of this chunk loads `name` as a string.
    // Cost: O(o), o = ops.
    fn loads_name_as_string(&self, name: &str) -> bool {
        self.ops.iter().any(|op| {
            matches!(op, OpCode::LoadConst(idx)
                if matches!(self.constants.get(*idx as usize).map(Value::view),
                    Some(ValueView::Str(s)) if s.as_str() == name))
        })
    }
}
