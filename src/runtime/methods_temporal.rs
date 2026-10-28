//! A `Date` or `DateTime` instance whose call the shape lookup did not take: an
//! instance of a user subclass (which has no shape), or a call the guard step
//! declined. The rows of `Date` / `DateTime` answer it through their owner
//! (ADR-11276 §9.127); their handlers read the instance's attributes and
//! re-bless a derived value into the receiver's class.

use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

impl super::Interpreter {
    /// The owner whose rows answer `target`: `DateTime` for an instance of it
    /// or of a subclass, `Date` likewise; `None` for anything else.
    // Cost: O(1) for `Date`/`DateTime` themselves; O(m), m = the subclass's
    // MRO length, for a subclass instance.
    pub(crate) fn temporal_instance_owners(
        &mut self,
        target: &Value,
    ) -> Option<&'static [&'static str]> {
        let ValueView::Instance { class_name, .. } = target.view() else {
            return None;
        };
        match class_name.as_str() {
            "Date" => return Some(&["Date"]),
            "DateTime" => return Some(&["DateTime"]),
            _ => {}
        }
        if !self.registry().classes.contains_key(class_name.as_str()) {
            return None;
        }
        let mro = self.class_mro(class_name.as_str());
        if mro.contains(&Symbol::intern("DateTime")) {
            Some(&["DateTime"])
        } else if mro.contains(&Symbol::intern("Date")) {
            Some(&["Date"])
        } else {
            None
        }
    }

    /// Answer `target.method(|args)` by a row of `Date` / `DateTime`, found by
    /// owner; `None` when `target` is neither or no row binds the call.
    // Cost: the owner lookup's, plus the row's.
    pub(super) fn dispatch_temporal_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let owners = self.temporal_instance_owners(target)?;
        // `Date.gist` and `Mu.Stringy` are `self.Str`: a subclass that
        // overrides `Str` answers both through it.
        if args.is_empty()
            && matches!(method, "gist" | "Stringy")
            && let ValueView::Instance { class_name, .. } = target.view()
            && !matches!(class_name.as_str(), "Date" | "DateTime")
            && self.has_user_method(&class_name.resolve(), "Str")
        {
            return Some(self.call_method_with_values(target.clone(), "Str", Vec::new()));
        }
        crate::builtins::method_table::invoke_owner(self, owners, method, args, || target.clone())
    }
}
