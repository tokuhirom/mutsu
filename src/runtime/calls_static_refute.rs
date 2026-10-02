//! Whether a statically-typed call site's binding failure is one rakudo's
//! optimizer would have refuted at compile time.
//!
//! A call whose argument types are all known at compile time
//! (`OpCode::CallFunc::static_arg_types`) is reported as the compile-time
//! `X::TypeCheck::Argument` ("Calling f(Str) will never work ..."). Rakudo only
//! does that when no argument could bind: a type-object argument that is a
//! *supertype* of its parameter's type (`sub f(Int $x) {}; f(Cool)`, or `Mu`
//! for any parameter) might still name a value that binds, so the optimizer
//! leaves the call to the run-time binder (#10944).

use super::*;

impl Interpreter {
    /// [`Self::enhance_binding_error`] for a binding failure of the call the
    /// current `CallFunc` site published (`static_call_args`).
    pub(crate) fn enhance_binding_error_at_site(
        &mut self,
        err: RuntimeError,
        func_name: &str,
        param_defs: &[crate::ast::ParamDef],
        args: &[Value],
    ) -> RuntimeError {
        let static_site = self.static_call_args && !self.static_args_may_bind(param_defs, args);
        Self::enhance_binding_error(err, func_name, param_defs, args, static_site)
    }

    /// Whether some positional type-object argument is a strict supertype of
    /// its parameter's nominal type, so the call cannot be refuted statically.
    /// An untyped parameter's nominal type is `Any` (`Mu` for a block's).
    // Cost: O(p * t), p = positional parameters, t = one type-object match.
    fn static_args_may_bind(
        &mut self,
        param_defs: &[crate::ast::ParamDef],
        args: &[Value],
    ) -> bool {
        let positional_args = args
            .iter()
            .filter(|arg| !matches!(arg.view(), ValueView::Pair(..)));
        let positional_params = param_defs
            .iter()
            .filter(|pd| !pd.named && !pd.is_invocant)
            .take_while(|pd| !pd.slurpy && !pd.double_slurpy);
        for (pd, arg) in positional_params.zip(positional_args) {
            let ValueView::Package(arg_type) = arg.unwrap_varref().deref_container().view() else {
                continue;
            };
            let param_type = match pd.type_constraint.as_deref() {
                Some(t) => t,
                None if pd.block_param => "Mu",
                None => "Any",
            };
            if param_type.contains([':', '[', '(']) || arg_type.as_str() == param_type {
                continue;
            }
            let param_type_object = Value::package(crate::symbol::Symbol::intern(param_type));
            if self.type_matches_value(arg_type.as_str(), &param_type_object) {
                return true;
            }
        }
        false
    }
}
