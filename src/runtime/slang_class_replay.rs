//! A class a slang production builds reaches the importing unit by replay
//! (ADR-13370).
//!
//! The production runs in the slang host, a separate interpreter with its own
//! class registry. While a hook runs, the metamodel calls that build a type the
//! hook minted (`new_type`, `add_method`, `add_role`, `compose`, ...) are
//! recorded. The statement the host returns for the production carries that log
//! as a literal and calls `__mutsu_slang_replay_class` with it, which applies the
//! same calls in the unit's own interpreter, where the class is then an ordinary
//! registered class. The method values the log holds are closures; they run in
//! the unit like any other closure.

use super::*;
use std::cell::RefCell;
use std::collections::HashSet;

/// What one running hook has built so far.
#[derive(Default)]
struct ClassLog {
    /// The names `new_type` minted during the hook.
    minted: HashSet<String>,
    /// `(metamethod, arguments)` in call order; `new_type` is
    /// `("new_type", [how class name, the call's arguments..., minted name])`.
    ops: Vec<(String, Vec<Value>)>,
}

thread_local! {
    /// One entry per running hook, innermost last (productions nest).
    static LOGS: RefCell<Vec<ClassLog>> = const { RefCell::new(Vec::new()) };
}

/// Start recording for a hook.
// Cost: O(1).
pub(super) fn begin_class_log() {
    LOGS.with(|l| l.borrow_mut().push(ClassLog::default()));
}

/// Stop recording for the innermost hook and answer its log as a list of
/// `[metamethod, [arguments]]` pairs (empty when it built no type).
// Cost: O(k), k = the recorded calls.
pub(super) fn end_class_log() -> Value {
    let log = LOGS.with(|l| l.borrow_mut().pop()).unwrap_or_default();
    Value::array(
        log.ops
            .into_iter()
            .map(|(op, args)| Value::array(vec![Value::str(op), Value::array(args)]))
            .collect(),
    )
}

/// The log of the innermost running hook, as `end_class_log` shapes it, without
/// stopping it.
// Cost: O(k), k = the recorded calls.
pub(super) fn peek_class_log() -> Value {
    LOGS.with(|l| {
        let logs = l.borrow();
        let ops = logs.last().map(|log| log.ops.as_slice()).unwrap_or(&[]);
        Value::array(
            ops.iter()
                .map(|(op, args)| Value::array(vec![Value::str(op.clone()), Value::array(args.clone())]))
                .collect(),
        )
    })
}

impl Interpreter {
    /// Record the metamodel call `op(args)` when it mutates a type the running
    /// hook minted.
    // Cost: O(1) when no hook runs; otherwise O(1) per call.
    pub(super) fn record_slang_mop(&self, op: &str, args: &[Value]) {
        LOGS.with(|l| {
            let mut logs = l.borrow_mut();
            let Some(log) = logs.last_mut() else { return };
            let Some(target) = args.first() else { return };
            let ValueView::Package(name) = target.view() else {
                return;
            };
            if log.minted.contains(name.resolve().as_str()) {
                log.ops.push((op.to_string(), args.to_vec()));
            }
        });
    }

    /// Record a `new_type` call that minted `minted`.
    // Cost: O(a), a = the call's arguments.
    pub(super) fn record_slang_new_type(&self, how_class: &str, args: &[Value], minted: &Value) {
        LOGS.with(|l| {
            let mut logs = l.borrow_mut();
            let Some(log) = logs.last_mut() else { return };
            let ValueView::Package(name) = minted.view() else {
                return;
            };
            log.minted.insert(name.resolve().to_string());
            let mut recorded = vec![Value::str(how_class.to_string())];
            recorded.extend(args.iter().cloned());
            recorded.push(minted.clone());
            log.ops.push(("new_type".to_string(), recorded));
        });
    }

    /// `__mutsu_slang_class_log()`: the recorded calls of the running hook.
    // Cost: O(k), k = the recorded calls.
    pub(super) fn builtin_slang_class_log(&mut self) -> Result<Value, RuntimeError> {
        Ok(peek_class_log())
    }

    /// `__mutsu_slang_replay_class($log, $name)`: apply the recorded calls here
    /// and answer the type that was minted as `$name` in the host.
    // Cost: O(k), k = the recorded calls (each a metamodel call).
    pub(super) fn builtin_slang_replay_class(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let log = args.first().cloned().unwrap_or(Value::NIL);
        let wanted = match args.get(1).map(Value::view) {
            Some(ValueView::Package(name)) => name.resolve().to_string(),
            _ => String::new(),
        };
        let Some(entries) = log.as_list_items() else {
            return Err(RuntimeError::new("slang class replay without a log"));
        };
        // The host's name of each minted type -> this interpreter's.
        let mut renamed: Vec<(String, Value)> = Vec::new();
        let remap = |value: &Value, renamed: &[(String, Value)]| -> Value {
            if let ValueView::Package(name) = value.view()
                && let Some((_, now)) = renamed.iter().find(|(old, _)| old.as_str() == &*name.resolve())
            {
                return now.clone();
            }
            value.clone()
        };
        for entry in entries.iter() {
            let Some(parts) = entry.as_list_items() else {
                continue;
            };
            let op = parts.first().map(Value::to_string_value).unwrap_or_default();
            let call_args: Vec<Value> = parts
                .get(1)
                .and_then(Value::as_list_items)
                .map(|items| items.iter().map(|a| remap(a, &renamed)).collect())
                .unwrap_or_default();
            if op == "new_type" {
                let Some((minted_host, rest)) = call_args.split_last() else {
                    continue;
                };
                let Some((how_class, new_type_args)) = rest.split_first() else {
                    continue;
                };
                let minted = self.metamodel_new_type(&how_class.to_string_value(), new_type_args)?;
                if let ValueView::Package(host_name) = minted_host.view() {
                    renamed.push((host_name.resolve().to_string(), minted));
                }
                continue;
            }
            match op.as_str() {
                "add_method" => self.mop_add_method(call_args)?,
                "add_private_method" => self.mop_add_private_method(call_args)?,
                "add_multi_method" => self.mop_add_multi_method(call_args)?,
                "add_fallback" => self.mop_add_fallback(call_args)?,
                "add_role" => self.mop_add_role(call_args)?,
                "add_parent" => self.mop_add_parent(call_args)?,
                "add_attribute" => self.mop_add_attribute(call_args)?,
                "compose" => self.mop_compose(call_args)?,
                other => {
                    return Err(RuntimeError::new(format!(
                        "slang class replay: unknown metamethod '{other}'"
                    )));
                }
            };
        }
        Ok(renamed
            .iter()
            .find(|(old, _)| *old == wanted)
            .map(|(_, now)| now.clone())
            // A type the production did not mint (`Int`) stands for itself.
            .unwrap_or_else(|| args.get(1).cloned().unwrap_or(Value::NIL)))
    }
}
