//! `.wrap` on a method of a built-in class.
//!
//! `$*OUT.^find_method('print').wrap: method (|c) { ... }` is the classic
//! output-capture idiom (Terminal::MultiProgress's test helper). The wrap lands
//! in `Registry::method_wrap_chains` under `("IO::Handle", "print", 0)`, exactly
//! like a wrap of a user method. A built-in method has no `MethodDef`, though,
//! so the user-method entry sites never see the chain. The native dispatch
//! doors call [`Interpreter::try_builtin_method_wrap`] instead. It runs the
//! chain as a dispatcher wrap does (`runtime::dispatcher_wrap`): the innermost
//! `callsame` re-dispatches the method by name with this chain bypassed, which
//! reaches the native implementation.

use super::*;

impl Interpreter {
    /// Run `method` on `invocant`, an instance of the built-in class
    /// `class_name`, through its `.wrap` chain, if it has one. `None` when the
    /// method is not wrapped, or when this is the chain's own terminal
    /// re-dispatch.
    ///
    // Cost: O(1) when nothing in the program is wrapped; otherwise O(1) to find
    // the chain, plus the wrappers' own calls.
    pub(crate) fn try_builtin_method_wrap(
        &mut self,
        class_name: &str,
        invocant: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let chain = self.builtin_method_wrap_chain(class_name, method)?;
        Some(self.run_builtin_method_wrap(class_name, invocant, method, args, &chain))
    }

    /// [`Self::try_builtin_method_wrap`] for any call target: the class is the
    /// instance's class or the built-in type of a plain value. A class with a
    /// user method of that name is left to the user-method wrap sites.
    ///
    // Cost: O(1) once `has_any_wrap_chains()` holds; the caller checks it.
    pub(crate) fn try_builtin_value_method_wrap(
        &mut self,
        target: &Value,
        method_sym: Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let (class_name, chain) = self.builtin_value_wrap_chain(target, method_sym)?;
        let method = method_sym.resolve();
        Some(self.run_builtin_method_wrap(&class_name, target, &method, args, &chain))
    }

    /// The class and `.wrap` chain [`Self::try_builtin_value_method_wrap`]
    /// would run for `target.method`, without running it.
    ///
    // Cost: O(1) once `has_any_wrap_chains()` holds; the caller checks it.
    pub(crate) fn builtin_value_wrap_chain(
        &mut self,
        target: &Value,
        method_sym: Symbol,
    ) -> Option<(String, Vec<(u64, Value)>)> {
        let class_name: String = match target.view() {
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            _ => crate::runtime::utils::value_type_name(target).to_string(),
        };
        let chain = self.builtin_method_wrap_chain(&class_name, &method_sym.resolve())?;
        // A class declared in the program (its generated accessors too) is
        // wrapped at the user-method sites; running the chain here as well
        // would wrap twice.
        if self.has_user_method_sym(&class_name, method_sym) || self.has_class(&class_name) {
            return None;
        }
        Some((class_name, chain))
    }

    /// Whether the outermost wrapper of `chain` binds its first parameter
    /// `is rw` / `is raw`, so the caller's container is worth handing over.
    ///
    // Cost: O(1).
    pub(crate) fn wrap_chain_binds_rw_invocant(chain: &[(u64, Value)]) -> bool {
        let Some((_, wrapper)) = chain.last() else {
            return false;
        };
        let ValueView::Sub(data) = wrapper.view() else {
            return false;
        };
        data.param_defs.first().is_some_and(|pd| {
            pd.traits
                .iter()
                .any(|t| matches!(t.as_str(), "rw" | "raw" | "is rw" | "is raw"))
        })
    }

    /// The `.wrap` chain of `method` on the built-in class `class_name`, if any.
    /// `None` too inside the chain's own terminal re-dispatch.
    ///
    // Cost: O(1).
    pub(crate) fn builtin_method_wrap_chain(
        &self,
        class_name: &str,
        method: &str,
    ) -> Option<Vec<(u64, Value)>> {
        if !self.has_any_wrap_chains() {
            return None;
        }
        if let Some((name, frames, routines)) = &self.dispatch.dispatcher_wrap_bypass
            && name == method
            && *frames == self.call_frames.len()
            && *routines == self.routine_stack_len()
        {
            return None;
        }
        self.registry()
            .method_wrap_chain(class_name, method, 0)
            .cloned()
    }

    /// Call `chain`'s outermost wrapper with `[invocant, ...args]`, the rest of
    /// the chain and the native method queued behind it.
    ///
    // Cost: O(w), w = wrappers in the chain, plus their own calls.
    pub(crate) fn run_builtin_method_wrap(
        &mut self,
        class_name: &str,
        invocant: &Value,
        method: &str,
        args: &[Value],
        chain: &[(u64, Value)],
    ) -> Result<Value, RuntimeError> {
        self.run_builtin_method_wrap_in(class_name, invocant, None, method, args, chain)
    }

    /// [`Self::run_builtin_method_wrap`] with the caller's container for the
    /// invocant (`invocant_cell`, a `ContainerRef`): the wrapper's first
    /// argument and the `callsame` context hold the container, so a wrapper
    /// whose invocant is `is rw` writes through to the caller's variable and
    /// `callsame` re-dispatches on the updated value.
    ///
    // Cost: O(w), w = wrappers in the chain, plus their own calls.
    pub(crate) fn run_builtin_method_wrap_in(
        &mut self,
        class_name: &str,
        invocant: &Value,
        invocant_cell: Option<&Value>,
        method: &str,
        args: &[Value],
        chain: &[(u64, Value)],
    ) -> Result<Value, RuntimeError> {
        let invocant = invocant_cell.unwrap_or(invocant);
        let outermost = chain
            .last()
            .map(|(_, wrapper)| wrapper.clone())
            .unwrap_or(Value::NIL);
        self.push_method_samewith_context(class_name, method, args, Some(invocant.clone()));
        self.push_dispatcher_wrap_frame(class_name, method, args, invocant.clone(), chain);
        let mut call_args = Vec::with_capacity(args.len() + 1);
        call_args.push(invocant.clone());
        call_args.extend(args.iter().cloned());
        self.shift_arg_sources_for_wrap_invocant();
        let result = self.call_sub_value(outermost, call_args, false);
        self.pop_method_samewith_context();
        self.dispatch.method_dispatch_stack.pop();
        result
    }
}
