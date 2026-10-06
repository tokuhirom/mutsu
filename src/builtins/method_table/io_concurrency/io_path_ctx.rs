//! What an `IO::Path` row reads of its receiver, shared by the family modules
//! (ADR-11276 §9.19).

use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{AttrMap, Value, ValueView};

/// What a path method reads of its receiver: the concrete class (the result
/// round-trips it), the attributes and the path string.
pub(super) struct PathCtx {
    pub(super) class: Symbol,
    pub(super) attributes: AttrMap,
    pub(super) path: String,
}

impl PathCtx {
    /// The context of an `IO::Path` instance, `None` for any other value.
    // Cost: O(a), a = attributes of the instance (one copy of the map).
    pub(super) fn of(target: &Value) -> Option<PathCtx> {
        match target.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } => {
                let attributes = AttrMap::clone(&attributes.as_map());
                let path = attributes
                    .get("path")
                    .map(Value::to_string_value)
                    .unwrap_or_default();
                Some(PathCtx {
                    class: class_name,
                    attributes,
                    path,
                })
            }
            _ => None,
        }
    }

    /// The same path value with another `path` attribute.
    // Cost: O(a + p), a = attributes of the instance, p = chars of the path.
    pub(super) fn with_path(&self, path: String) -> Value {
        Interpreter::clone_io_path_with_path(&self.attributes, self.class, path)
    }

    /// The receiver itself, rebuilt with the same class and attributes.
    // Cost: O(a), a = attributes of the instance.
    pub(super) fn same(&self) -> Value {
        Value::make_instance(self.class, self.attributes.clone())
    }
}

/// The arguments of a call as one list, positional first and the named
/// `Pair`s after, which is the shape the interpreter's `IO` primitives read.
// Cost: O(a), a = arguments of the call.
pub(super) fn joined_args(positional: &[Value], named: &[Value]) -> Vec<Value> {
    positional.iter().chain(named).cloned().collect()
}
