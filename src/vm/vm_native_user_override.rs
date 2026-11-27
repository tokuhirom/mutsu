use super::*;

impl Interpreter {
    // Cost: O(m * c), m = MRO depth, c = candidates per level.
    pub(crate) fn native_lever_a_user_override_sym(
        &mut self,
        target: &Value,
        method_sym: crate::symbol::Symbol,
    ) -> bool {
        let type_name = crate::runtime::utils::value_type_name(target);
        self.refresh_method_caches_for_generation();
        let key = (type_name.as_ptr() as usize, method_sym);
        if self.native_base_bypass_hit(target, method_sym) {
            return false;
        }
        // The receiver's class when it is an instance. A lazy Match is one of
        // its cursor class (`type_name`), asked on its tag: `view()` would
        // materialize every capture on each native call (#12917).
        let inst_class: Option<&'static str> = if target.is_lazy_match_value() {
            Some(type_name)
        } else if let ValueView::Instance { class_name, .. } = target.view() {
            Some(class_name.as_str())
        } else {
            None
        };
        // A `handles <clone>` forwarder is a method of the user class, and
        // the method resolver must see it before the inherited `Mu.clone` row.
        if method_sym == crate::sym!("clone")
            && let Some(class_name) = inst_class
            && self.has_user_method_sym(class_name, method_sym)
        {
            return true;
        }
        // A `.wrap` of a built-in method (`$*OUT.^find_method('print').wrap`)
        // lands in the registry's wrap chains, like an `augment`, and takes the
        // call from the native row the same way: the wrapper runs first, and
        // its `callsame` re-dispatches to the native method. An instance's
        // type name here is `Any`, so the chain is looked up by its class; the
        // answer is not cached, but it costs one bool while nothing is wrapped.
        if self.registry().has_any_method_wrap_chains()
            && let Some(class_name) = inst_class
            // A built-in instance whose class inherits the method (`NFD.list`
            // is `Uni.list`) is wrapped through the class the wrap landed on.
            && self
                .registry()
                .builtin_method_wrap_chain_along_mro(class_name, method_sym.as_str())
                .is_some()
        {
            return true;
        }
        // A built-in instance reblessed by a role mixed in place (`$h does R`
        // names it `IO::CatHandle+{R}`): a method the role declares takes the
        // call from the built-in row, and its `callsame` reaches the row
        // (#17754).
        if let Some(class_name) = inst_class
            && class_name.contains("+{")
            && self.has_user_method_sym(class_name, method_sym)
        {
            return true;
        }
        // Both halves are functions of (type, method) and the registry alone,
        // so the cache holds the combined answer: the outranking half is an MRO
        // walk (~12k Ir) that a call on a type with a same-named user method
        // would otherwise repeat every time (#14855).
        match self.caches.native_lever_a_override_cache.get(&key) {
            Some(&hit) => hit,
            None => {
                let answer = self.has_user_method(type_name, method_sym.as_str())
                    && !self.user_override_outranked_by_builtin(type_name, method_sym.as_str());
                self.caches
                    .native_lever_a_override_cache
                    .insert(key, answer);
                answer
            }
        }
    }
}
