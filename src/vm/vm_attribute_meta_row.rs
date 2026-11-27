//! A metamethod of an `Attribute` metaobject (`$attr.get_value($obj)`,
//! `.set_value`, `.name`, ...) answered from `Attribute`'s rows directly.
//!
//! An `Attribute` instance has no dispatch shape in the method table, so the
//! guard step declines it, and a call fell into the interpreter's whole method
//! cascade (`call_method_with_values`: the by-name dispatch groups, the
//! metamodel probes, the carried-state owners, the MRO walk) before reaching
//! the same `invoke_owner(["Attribute"], ...)` answer, about 22k Ir of a
//! 42k Ir `get_value`. AttrX::Mooish calls `get_value` several times per
//! attribute access, on an attribute that `does` its role. When neither the
//! attribute's class nor a mixed-in role declares the method and no user
//! `^find_method` exists, the row is the answer and is reached directly.

use crate::opcode::CompiledCode;
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

impl Interpreter {
    /// Preserve the source Scalar when Attribute.set_value binds its argument.
    /// A tagged variable passed to any other `set_value` keeps its plain value.
    // Cost: O(l + m + r), l = caller locals, m = receiver MRO, r = mixins.
    pub(crate) fn prepare_attribute_set_value_arg(
        &mut self,
        code: &CompiledCode,
        target: &Value,
        method_sym: Symbol,
        args: &mut [Value],
    ) {
        if method_sym != crate::sym!("set_value") || args.len() != 2 {
            return;
        }
        let slot_hint = args[1].varref_slot();
        let ValueView::VarRef { name, value, .. } = args[1].view() else {
            return;
        };
        let name = name.resolve();
        let inner = value.clone();
        args[1] = if self.attribute_meta_row_will_handle(target, method_sym)
            && !self.scalar_name_has_no_container(&name)
        {
            self.capture_rw_arg_cell(code, &name, inner, slot_hint)
        } else {
            inner
        };
    }

    /// Whether this call will reach the built-in Attribute method row.
    // Cost: O(m + r), m = receiver MRO, r = mixed-in roles.
    pub(crate) fn attribute_meta_row_will_handle(
        &mut self,
        target: &Value,
        method_sym: Symbol,
    ) -> bool {
        let attribute = crate::symbol::wk::attribute_class();
        let method = method_sym.as_str();
        let receiver = match target.view() {
            ValueView::Instance { class_name, .. } => {
                class_name == attribute
                    || (self.class_mro(class_name.as_str()).contains(&attribute)
                        && !self.has_user_method_of(class_name, method_sym))
            }
            ValueView::Mixin(inner, mixins) => {
                matches!(inner.view(), ValueView::Instance { class_name, .. } if class_name == attribute)
                    && !Self::mixin_role_applications(mixins)
                        .iter()
                        .any(|(role, _)| self.role_or_parent_has_method(role, method))
            }
            _ => false,
        };
        receiver
            && crate::builtins::method_table::owner_declares("Attribute", method_sym)
            && !crate::runtime::find_method_intercept::any_user_find_method()
            && !self.has_user_method("Attribute", method)
    }

    /// `method(args)` on an `Attribute` instance (or one mixed with roles)
    /// answered by its `Attribute` row; `None` when the receiver is anything
    /// else, a user method could claim the call, or no row answers it.
    // Cost: O(m + r), m = the receiver class's MRO (cached), r = mixed-in roles
    // (each a memoized method probe), plus the row's own cost.
    pub(crate) fn try_attribute_meta_row(
        &mut self,
        target: &Value,
        method_sym: Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !self.attribute_meta_row_will_handle(target, method_sym) {
            return None;
        }
        let instance = match target.view() {
            ValueView::Mixin(inner, _) => Value::clone(inner),
            _ => target.clone(),
        };
        crate::builtins::method_table::invoke_owner(
            self,
            &["Attribute"],
            method_sym.as_str(),
            args,
            || instance,
        )
    }
}
