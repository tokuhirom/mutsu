//! `Cool` string methods on a user subclass of `IO::Path` (#12149).
//!
//! `IO::Path` is a `Cool`, so `.uc` / `.chars` / `.starts-with` coerce the
//! receiver through `.Str`, which for a path is the path text. The built-in
//! `IO::Path` instances stringify that way in `value::display`, a pure function
//! with no MRO to walk, so an instance of `class MyPath is IO::Path` fell back
//! to the default `MyPath<31>` rendering. The native dispatch entry therefore
//! swaps such a receiver for its path string before the by-name `Cool` cascades
//! read it.

use super::Interpreter;
use crate::value::{Value, ValueView};

impl Interpreter {
    /// The path string standing in for `target` when it is an instance of a
    /// user subclass of `IO::Path` and `method` is a `Cool`-only method the
    /// subclass does not override; `None` otherwise.
    // Cost: O(1) for a non-instance receiver; for an instance O(|method|) plus
    // the memoized `has_user_method` probe and one MRO read, d = MRO depth.
    pub(crate) fn io_path_subclass_cool_receiver(
        &mut self,
        target: &Value,
        method: &str,
    ) -> Option<Value> {
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
        else {
            return None;
        };
        if class_name == "IO::Path"
            || class_name.as_str().starts_with("IO::Path::")
            || !super::any_cool_method_gate::is_cool_only_method(method)
            // `lines`, `words`, `comb`, ... are `IO::Path`'s own, not `Cool`'s.
            || crate::builtins::builtin_type_methods::builtin_type_method_names("IO::Path")
                .contains(&method)
        {
            return None;
        }
        let path = attributes.as_map().get("path")?.to_string_value();
        let class = class_name.as_str();
        let inherits = self
            .class_mro(class)
            .iter()
            .any(|c| c.as_str() == "IO::Path" || c.as_str().starts_with("IO::Path::"));
        if !inherits || self.has_user_method(class, method) {
            return None;
        }
        Some(Value::str_from(&path))
    }
}
