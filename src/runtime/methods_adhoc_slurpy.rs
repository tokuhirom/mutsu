//! `X::AdHoc.from-slurpy` — the documented class method that builds an
//! `X::AdHoc` from a slurpy positional argument list.
//!
//! rakudo's definition is:
//!
//! ```raku
//! method from-slurpy (|cap) {
//!     my $revised-cap = cap.list.map({ nqp::istype($_, Failure) ?? .exception !! $_ });
//!     self.new(payload => $revised-cap does X::AdHoc::SlurpySentry)
//! }
//! ```
//!
//! and the resulting exception's `.message` is the concatenation of the
//! payload's elements' stringifications (the `SlurpySentry` role is exactly
//! the marker that selects that rendering over the plain "stringify the
//! payload" one). Verified against
//! `raku -e 'my $e = X::AdHoc.from-slurpy(3, False, "Not here"); say $e.payload.^name; say $e.message'`,
//! which prints `Capture+{X::AdHoc::SlurpySentry}` and `3FalseNot here`.

use super::*;

/// The marker role rakudo mixes into `from-slurpy`'s payload capture.
pub(crate) const SLURPY_SENTRY_ROLE: &str = "X::AdHoc::SlurpySentry";

impl Interpreter {
    pub(super) fn dispatch_adhoc_from_slurpy(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // rakudo replaces a `Failure` argument with the exception it carries,
        // so the message names the underlying error rather than "Failure".
        let items: Vec<Value> = args.iter().map(Self::adhoc_slurpy_unwrap_failure).collect();
        let message: String = items.iter().map(|v| v.to_string_value()).collect();
        let payload = self.eval_does_values(
            Value::capture(items, std::collections::HashMap::new()),
            Value::package(Symbol::intern(SLURPY_SENTRY_ROLE)),
        )?;
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("payload".to_string(), payload);
        attrs.insert("message".to_string(), Value::str(message));
        Ok(Value::make_instance(Symbol::intern("X::AdHoc"), attrs))
    }

    /// A `Failure` argument contributes its wrapped exception, not itself.
    fn adhoc_slurpy_unwrap_failure(v: &Value) -> Value {
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = v.view()
            && class_name == "Failure"
            && let Some(ex) = attributes.as_map().get("exception")
        {
            return ex.clone();
        }
        v.clone()
    }
}
