//! An argument-less `.?name` on a plain user-class instance whose class is
//! known not to answer `name` (#17987).
//!
//! `.?` maps an `X::Method::NotFound` to `Nil`, so every miss used to build
//! the whole exception — the "Did you mean" walk over the class's methods,
//! the message and its word wrap — only to throw it away; rakudo's
//! `dispatch:<.?>` asks `nqp::tryfindmethod` and builds nothing. XML::Class
//! asks `self.parent.?xml-namespace-prefix` once per node.
//!
//! The first miss runs the ordinary dispatch. When its error proves that the
//! lookup itself missed (`note_maybe_call_miss`), the `(class, method)` pair
//! is remembered for the registry's method generation, and later calls
//! answer `Nil` straight away (`try_maybe_call_miss_lane`). Anything that can
//! change the answer either changes the generation (a method added, a class
//! or role composed, `augment`), is per value and so never reaches the lane
//! (a mixin is not a plain `Instance`), or keeps the lane shut while it
//! exists (`.^add_fallback`). A call whose answer is not a plain lookup miss
//! is never recorded: `FALLBACK`, `handles`, a user `^find_method`, a method
//! whose body raises `NotFound` for some other name or type.
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Answer `Nil` for a recorded `.?name` miss, consuming the invocant.
    /// `false` leaves the stack untouched.
    // Cost: O(1): a constant read, one hash probe and a generation compare.
    #[inline]
    pub(super) fn try_maybe_call_miss_lane(
        &mut self,
        code: &CompiledCode,
        method_sym: Symbol,
        arity: u32,
        modifier_idx: Option<u32>,
    ) -> bool {
        if arity != 0 || self.caches.maybe_call_misses.is_empty() {
            return false;
        }
        let Some(idx) = modifier_idx else {
            return false;
        };
        if Self::const_str(code, idx) != "?" || !self.types.method_fallbacks.is_empty() {
            return false;
        }
        let Some(class) = self.stack.last().and_then(Self::maybe_miss_class) else {
            return false;
        };
        let generation = self.registry().method_generation;
        if self.caches.maybe_call_misses_generation != generation {
            self.caches.maybe_call_misses.clear();
            self.caches.maybe_call_misses_generation = generation;
            return false;
        }
        if !self.caches.maybe_call_misses.contains(&(class, method_sym)) {
            return false;
        }
        self.stack.pop();
        self.stack.push(Value::NIL);
        true
    }

    /// The class of a plain user-class instance, the only receiver the lane
    /// serves.
    // Cost: O(1).
    #[inline]
    pub(super) fn maybe_miss_class(target: &Value) -> Option<Symbol> {
        match target.view() {
            ValueView::Instance { class_name, .. } => Some(class_name),
            _ => None,
        }
    }

    /// Record that the argument-less `.?method` on an instance of `class`
    /// missed, when `e` proves the lookup itself missed: an
    /// `X::Method::NotFound` for this very method on this very type. An error
    /// for another name or type came from code the call ran (`FALLBACK`, a
    /// `handles` target, a method body), which a later call must run again.
    // Cost: O(1) plus the error's attribute reads.
    pub(super) fn note_maybe_call_miss(
        &mut self,
        class: Symbol,
        method: &str,
        method_sym: Symbol,
        e: &RuntimeError,
    ) {
        // `.message` on an instance carrying a thrown message is answered
        // per value before the dispatch (`exec_call_method_op_impl`).
        if method == "message"
            || !self.types.method_fallbacks.is_empty()
            || !e.is_method_not_found_for(method)
        {
            return;
        }
        let class_name = class.resolve();
        let display = crate::value::user_facing_type_name(&class_name);
        let names_this_type = e.exception.as_deref().is_some_and(|ex| match ex.view() {
            ValueView::Instance { attributes, .. } => attributes
                .as_map()
                .get("typename")
                .is_some_and(|t| t.to_string_value() == display),
            _ => false,
        });
        if !names_this_type {
            return;
        }
        let generation = self.registry().method_generation;
        if self.caches.maybe_call_misses_generation != generation {
            self.caches.maybe_call_misses.clear();
            self.caches.maybe_call_misses_generation = generation;
        }
        self.caches.maybe_call_misses.insert((class, method_sym));
    }
}
