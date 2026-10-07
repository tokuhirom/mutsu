//! The rows of `Semaphore` and `Thread` (ADR-11276 §9.22): receivers that carry
//! live OS state (a semaphore's id into the runtime's table, a thread's id and
//! name). Neither has a shape, so every row is reached through its owner
//! (`RowFlags::OWNER_ONLY`, `invoke_owner`).

use crate::builtins::method_table::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{AttrMap, RuntimeError, Value, ValueView};

macro_rules! row {
    ($owner:literal, $name:literal, $handler:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: $handler,
            flags: RowFlags::OWNER_ONLY,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("Semaphore", "acquire", Handler::Interp(acquire_row)),
    row!("Semaphore", "try_acquire", Handler::Interp(try_acquire_row)),
    row!("Semaphore", "release", Handler::Interp(release_row)),
    row!("Thread", "id", Handler::Narrow(id_row)),
    row!("Thread", "Numeric", Handler::Narrow(id_row)),
    row!("Thread", "name", Handler::Narrow(name_row)),
    row!("Thread", "is-initial-thread", Handler::Narrow(initial_row)),
    row!("Thread", "app_lifetime", Handler::Narrow(app_lifetime_row)),
    row!("Thread", "Str", Handler::Narrow(str_row)),
    row!("Thread", "gist", Handler::Narrow(gist_row)),
    row!("Thread", "finish", Handler::Interp(finish_row)),
];

/// The attributes of an instance of `class`.
// Cost: O(a), a = attributes (one copy of the map).
fn attrs_of(target: &Value, class: &str) -> Option<AttrMap> {
    match target.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == class => Some(AttrMap::clone(&attributes.as_map())),
        _ => None,
    }
}

/// `Semaphore.acquire`.
// Cost: O(1) plus the wait for a permit.
fn acquire_row(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let attrs = attrs_of(target, "Semaphore")?;
    Some(interp.semaphore_acquire_method(&attrs))
}

/// `Semaphore.try_acquire`.
// Cost: O(1).
fn try_acquire_row(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let attrs = attrs_of(target, "Semaphore")?;
    Some(interp.semaphore_try_acquire_method(&attrs))
}

/// `Semaphore.release`.
// Cost: O(1).
fn release_row(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let attrs = attrs_of(target, "Semaphore")?;
    Some(interp.semaphore_release_method(&attrs))
}

/// The thread's id, `0` when it has none.
// Cost: O(1).
fn thread_id(attrs: &AttrMap) -> Value {
    attrs
        .get("id")
        .or_else(|| attrs.get("thread_id"))
        .cloned()
        .unwrap_or(Value::int(0))
}

/// `Thread.id` and `.Numeric`.
// Cost: O(a), a = attributes.
fn id_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(thread_id(&attrs_of(target, "Thread")?)))
}

/// `Thread.name`.
// Cost: O(a), a = attributes.
fn name_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let attrs = attrs_of(target, "Thread")?;
    Some(Ok(attrs
        .get("name")
        .cloned()
        .unwrap_or_else(|| Value::str_from("<anon>"))))
}

/// `Thread.is-initial-thread`.
// Cost: O(a), a = attributes.
fn initial_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let attrs = attrs_of(target, "Thread")?;
    Some(Ok(Value::truth(
        attrs.get("is_initial").is_some_and(Value::truthy),
    )))
}

/// `Thread.app_lifetime`.
// Cost: O(a), a = attributes.
fn app_lifetime_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let attrs = attrs_of(target, "Thread")?;
    Some(Ok(attrs
        .get("app_lifetime")
        .cloned()
        .unwrap_or(Value::FALSE)))
}

/// `Thread.Str` and `.gist`: `Thread<id>(name)`.
// Cost: O(a), a = attributes.
fn str_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let attrs = attrs_of(target, "Thread")?;
    let id = thread_id(&attrs).as_int().unwrap_or(0);
    let name = attrs
        .get("name")
        .map(Value::to_string_value)
        .unwrap_or_else(|| "<anon>".to_string());
    Some(Ok(Value::str(format!("Thread<{id}>({name})"))))
}

/// `Thread.gist`: `Thread #id (name)`, prefixed `Immortal ` unless the thread
/// is an `app_lifetime` one, and without the name of an anonymous thread: the
/// text Rakudo's `Thread.gist` builds.
// Cost: O(a), a = attributes.
fn gist_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let attrs = attrs_of(target, "Thread")?;
    let id = thread_id(&attrs).as_int().unwrap_or(0);
    let prefix = if attrs.get("app_lifetime").is_some_and(Value::truthy) {
        ""
    } else {
        "Immortal "
    };
    let name = match attrs.get("name").map(Value::to_string_value) {
        Some(name) if !name.is_empty() && name != "<anon>" => format!(" ({name})"),
        _ => String::new(),
    };
    Some(Ok(Value::str(format!("{prefix}Thread #{id}{name}"))))
}

/// `Thread.finish`: wait for the thread to end.
// Cost: O(1) plus the wait for the thread.
fn finish_row(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let attrs = attrs_of(target, "Thread")?;
    Some(interp.dispatch_thread_finish(&attrs))
}
