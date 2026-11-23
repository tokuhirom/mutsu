//! `OpCode::CheckReadOnlyModify`: the read-only check of a read-modify-write
//! target (`$x OP= v`, `$x .= meth`), #18986.
//!
//! It refuses exactly what `CheckReadOnly` refuses. Only the wording of one
//! refusal differs, as in rakudo: a name bound to an immutable value
//! (`my $x := "a"`) has no container, so modifying it reports the value,
//! "Cannot modify an immutable Str (a)", where a plain `$x = 1` says "Cannot
//! assign to an immutable value".

use super::vm_check_read_only::FreeVarBinding;
use super::*;
use crate::ast::ReadonlyKind;

impl Interpreter {
    // Cost: O(1), as `CheckReadOnly`; the error path is cold.
    pub(crate) fn exec_check_read_only_modify_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        self.exec_check_read_only_op(code, name_idx).map_err(|err| {
            self.immutable_binding_modify_error(code, name_idx)
                .unwrap_or(err)
        })
    }

    /// The error a modification of `name_idx` raises when it is bound to an
    /// immutable value: X::Assignment::RO naming that value. `None` for every
    /// other refusal, which keeps its own wording.
    // Cost: O(d), d = env tiers walked to resolve a non-local name.
    fn immutable_binding_modify_error(
        &self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Option<RuntimeError> {
        let sym = code.const_sym(name_idx);
        let name = Self::const_str(code, name_idx);
        let slots = code.local_slots_of(sym);
        let decision = match slots {
            [] => self.free_var_nonlocal_decision(name, sym),
            [slot] => self.slot_rebind_decision(*slot),
            _ => None,
        };
        let (kind, value) = match decision {
            Some(FreeVarBinding::Readonly(kind, bound)) => (Some(kind), Some(bound)),
            _ => {
                let value = match slots {
                    [slot] => self.locals.get(*slot as usize).cloned(),
                    _ => self.env().get(name).cloned(),
                };
                (self.readonly_kind_sym(sym), value)
            }
        };
        if !matches!(
            kind,
            Some(ReadonlyKind::Immutable | ReadonlyKind::ImmutableDeep)
        ) {
            return None;
        }
        let value = value?.deref_container();
        crate::runtime::types::value_is_defined(&value)
            .then(|| RuntimeError::assignment_ro_value(value))
    }
}
