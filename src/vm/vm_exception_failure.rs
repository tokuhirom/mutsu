//! `Exception.Failure`: an exception wrapped in an unhandled `Failure`.

use super::*;

impl Interpreter {
    /// `$exception.Failure` (the idiom `CATCH { return .Failure }`): the same
    /// value `Failure.new($exception)` builds. `None` when the call is not
    /// that -- arguments, a non-exception invocant, or a user class that
    /// defines its own `Failure` method -- so ordinary dispatch continues.
    ///
    /// Cost: O(m), m = the invocant class's MRO length (a built-in exception
    /// type answers by name in O(1)).
    pub(super) fn exception_failure_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Value> {
        if method != "Failure" || !args.is_empty() {
            return None;
        }
        let ValueView::Instance { class_name, .. } = target.view() else {
            return None;
        };
        let class = class_name.as_str();
        let is_exception = target.instance_is_exception_by_name()
            || self.class_mro(class).iter().any(|p| p == "Exception");
        if !is_exception || self.has_user_method(class, "Failure") {
            return None;
        }
        Some(self.build_native_failure_value(std::slice::from_ref(target)))
    }
}
