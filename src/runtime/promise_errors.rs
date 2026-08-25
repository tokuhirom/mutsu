//! Exception construction and shape helpers for the `Promise` resolution
//! protocol: the `X::Promise::Vowed` / `X::Promise::Resolved` builders, plus
//! the "is this reason already an exception object?" check that `.cause` and
//! `.result` share.
//!
//! Split out of `methods_promise.rs` to keep that file under the 500-line
//! limit. The `X::Promise::Broken` role's own gist rendering lives next door
//! in `promise_broken_gist.rs`.

use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Is this broken-promise reason already an exception *object* (so
    /// `.cause`/`.result` must hand it back with its real type), rather than
    /// a plain `Str`/number to wrap in `X::AdHoc`?
    ///
    /// Shape-based on purpose: a name-based check ("contains Exception" /
    /// "starts with X::") cannot see a user class's `is Exception` ancestry.
    /// A role mixed into an exception wraps the instance in a `Mixin`, which
    /// is how a cause that has already crossed a `.then` boundary arrives
    /// (the callback rethrew `...+{X::Promise::Broken}`), so look through it
    /// — otherwise the mixin was stripped and re-wrapped as a bare X::AdHoc.
    pub(crate) fn is_exception_object(value: &Value) -> bool {
        Self::exception_instance_of(value).is_some()
    }

    /// The underlying object `Instance` of a (possibly role-mixed) exception
    /// value, looking through any number of `but`/`does` wrappers.
    pub(crate) fn exception_instance_of(value: &Value) -> Option<Value> {
        match value.view() {
            ValueView::Instance { .. } => Some(value.clone()),
            ValueView::Mixin(inner, _) => Self::exception_instance_of(inner),
            _ => None,
        }
    }

    /// Compose the `X::Promise::Broken` role into a broken promise's cause,
    /// the way rakudo does on the way out of `Promise.result`.
    ///
    /// The composition builds a *new* value, which is why `.cause` keeps
    /// handing back the plain, un-mixed original (rakudo's `.cause` and the
    /// exception `.result` throws are `!===` for exactly this reason).
    ///
    /// A promise broken with a bare reason has no backtrace of its own —
    /// rakudo's `$p.cause.backtrace` is undefined after `$p.break('oh no')` —
    /// so the throw site stamps one on, and the "Original exception:" half of
    /// the mixin's gist renders it. An exception that already carries a
    /// backtrace (a `die` inside `Promise.start`, captured on the worker
    /// thread) keeps it: this is a rethrow, not a fresh throw.
    ///
    /// Composition can only fail for a value that is not an object instance,
    /// and the caller has already normalised the reason into one — but rather
    /// than unwrap, fall back to the uncomposed exception so a broken promise
    /// still throws its real cause under any future reason shape.
    pub(super) fn compose_promise_broken_role(&mut self, ex: Value) -> Value {
        let ex = self.stamp_throw_site_backtrace(ex);
        let role = Value::package(Symbol::intern(
            crate::runtime::promise_broken_gist::PROMISE_BROKEN_ROLE,
        ));
        self.eval_does_values(ex.clone(), role).unwrap_or(ex)
    }

    /// Give `ex` the current throw-site backtrace if it does not already have
    /// one. Returns a copy; the original instance is left untouched so an
    /// alias (here: the promise's stored cause) does not gain a backtrace.
    fn stamp_throw_site_backtrace(&self, ex: Value) -> Value {
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = ex.view()
        else {
            return ex;
        };
        if attributes.contains_key("backtrace") {
            return ex;
        }
        let mut attrs = attributes.as_map().clone();
        attrs.insert("backtrace".to_string(), self.build_backtrace_value());
        Value::make_instance(class_name, attrs)
    }

    /// `X::Promise::Vowed` — a `.keep`/`.break`/`.vow` through the Promise
    /// after its vow was already handed out. Carries the `promise` attribute
    /// Rakudo's exception exposes; the human-readable text comes from
    /// `format_exception_message()` so `.message`/`.Str`/`.gist` all agree
    /// (storing it in a `message` attribute would shadow that table).
    pub(super) fn promise_vowed_error(shared: &SharedPromise) -> RuntimeError {
        let mut attrs = HashMap::new();
        attrs.insert("promise".to_string(), Value::promise(shared.clone()));
        let ex = Value::make_instance(Symbol::intern("X::Promise::Vowed"), attrs);
        let mut err = RuntimeError::new(
            "Access denied to keep/break this Promise; already vowed".to_string(),
        );
        err.exception = Some(Box::new(ex));
        err
    }

    /// `X::Promise::Resolved` — resolving a promise that has already settled.
    pub(crate) fn promise_resolved_error(shared: &SharedPromise, status: &str) -> RuntimeError {
        let mut attrs = HashMap::new();
        attrs.insert("status".to_string(), Value::str(status.to_string()));
        attrs.insert("promise".to_string(), Value::promise(shared.clone()));
        let ex = Value::make_instance(Symbol::intern("X::Promise::Resolved"), attrs);
        let mut err = RuntimeError::new(format!(
            "Cannot keep/break a Promise more than once (status: {})",
            status
        ));
        err.exception = Some(Box::new(ex));
        err
    }
}
