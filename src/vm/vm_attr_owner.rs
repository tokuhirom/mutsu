//! Whose private attribute a `$!x` access reads (#17936).
//!
//! A class and its parent may each declare `$!x`; each keeps its own copy,
//! and `$!x` names the copy of the package the code is *written* in. A method
//! body learns that package from the dispatch stack (the running method's
//! owner). A closure the method made, or a block inside it, runs in a frame
//! of its own: once the method has returned, or when the closure is called
//! from another method, the dispatch stack names someone else (or nobody), so
//! the access fell back to the bare key -- the most-derived class's copy.
//! Such a frame carries the fixture record of the method it was created in
//! (`?CLASS` / `?ROLE`, ADR-14843), which names the lexical owner.

use super::*;

impl Interpreter {
    /// The owner of a `$!x` access made at `site`, and whether it is a role:
    /// the running method's owner for a method body, the creating method's
    /// `?ROLE` (else `?CLASS`) for any other frame that has a fixture record.
    // Cost: O(1).
    pub(crate) fn attr_access_owner(
        &self,
        site: Option<(&CompiledCode, usize)>,
    ) -> (Option<crate::symbol::Symbol>, bool) {
        let method_owner = || {
            (
                self.method_class_stack_top_sym(),
                self.method_class_top_is_role(),
            )
        };
        let Some((code, _)) = site else {
            return method_owner();
        };
        if code.locals_sym.first() == Some(&crate::symbol::wk::self_()) {
            return method_owner();
        }
        let Some(fixtures) = self.frame_fixtures() else {
            return method_owner();
        };
        if let Some(role) = fixtures.role {
            return (Some(role), true);
        }
        match fixtures.class.as_ref().map(Value::view) {
            Some(ValueView::Package(class)) => (Some(class), false),
            _ => method_owner(),
        }
    }
}
