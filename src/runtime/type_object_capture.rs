//! `Mu.Capture` on a user type object (#18778): rakudo's `Mu.Capture` reads
//! the public attributes, so a class that has one dies on the type object,
//! and one without answers `\()`, unless a core ancestor decides otherwise
//! (`methods_0arg::type_capture`).

use super::*;
use crate::builtins::methods_0arg::type_capture::{TypeCapture, answer, builtin_kind};

impl Interpreter {
    /// `.Capture` of the type object `name`, walking its MRO: the first user
    /// class with a public attribute, or the first core class the table
    /// lists, decides. A class with neither answers `\()`, and so does a role
    /// without a public attribute.
    // Cost: O(d + a), d = the MRO length, a = the attributes of its user classes.
    pub(crate) fn type_object_capture(&mut self, name: Symbol) -> Option<Result<Value, RuntimeError>> {
        let got = name.resolve();
        if let Some(kind) = builtin_kind(&got) {
            return Some(answer(kind, &got, &got));
        }
        let is_user_class = self.registry().classes.contains_key(got.as_str());
        if !is_user_class {
            let role_attr = self.registry().roles.get(got.as_str()).map(|role| {
                role.attributes.iter().find(|a| a.is_public).map(|a| a.name.clone())
            })?;
            // A role puns: `Mu.Capture` calls the punned class's accessor on
            // the role type object, whose invocant is refused.
            return Some(match role_attr {
                Some(attr) => Err(RuntimeError::parameter_invalid_concreteness(
                    "Mu", &got, &attr, "self", true, true,
                )),
                None => answer(TypeCapture::Empty, &got, &got),
            });
        }
        for class in self.class_mro(&got).iter() {
            let class = class.resolve();
            if let Some(kind) = builtin_kind(&class) {
                return Some(answer(kind, &got, &class));
            }
            let has_public = self
                .registry()
                .classes
                .get(class.as_str())
                .is_some_and(|def| def.attributes.iter().any(|a| a.is_public));
            if has_public {
                return Some(answer(TypeCapture::Attributes, &got, &class));
            }
        }
        Some(answer(TypeCapture::Empty, &got, &got))
    }
}
