//! The one class-method lookup for a type object (ADR-11276 §9.120-§9.124,
//! §9.164): `Promise.start`, `Seq.from-loop`, `Thread.start`,
//! `X::AdHoc.from-slurpy`, `Supply.from-list`, `List.from-iterator`, ...

use super::*;

impl Interpreter {
    /// The rows of the built-in type `target` is a type object of (or whose
    /// nearest built-in ancestor is), when it declares the class method
    /// `method`: the type's own name if it is a class-method owner, else the
    /// nearest such class on its MRO (a user `class P is Promise` reaches
    /// `Promise`'s rows; an unrelated `Foo.start` reaches none). A `Supply`
    /// instance reaches `Supply`'s rows too, which refuse it with Rakudo's
    /// message. A user method of the type object wins.
    // Cost: O(1) for a name no class-method row has; otherwise O(d) for a MRO of
    // depth d, plus the row's own.
    pub(super) fn type_object_class_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !crate::builtins::method_table::class_method_named(method) {
            return None;
        }
        let owner = match target.view() {
            ValueView::Package(name) => {
                let name = name.resolve();
                if crate::builtins::method_table::class_method_owner(&name) {
                    name
                } else {
                    self.class_mro(&name)
                        .iter()
                        .map(|parent| parent.resolve())
                        .find(|parent| crate::builtins::method_table::class_method_owner(parent))?
                }
            }
            ValueView::Instance { class_name, .. } if class_name == "Supply" => {
                "Supply".to_string()
            }
            _ => return None,
        };
        if !crate::builtins::method_table::owner_has_row(&owner, method)
            || self.user_shadows_builtin(target, Symbol::intern(method), args, false)
        {
            return None;
        }
        crate::builtins::method_table::invoke_owner(self, &[owner.as_str()], method, args, || {
            target.clone()
        })
    }
}
