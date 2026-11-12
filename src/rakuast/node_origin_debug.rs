//! `Debug` for [`NodeOrigin`].
//!
//! Only which members are set, never their values: the resolution and the
//! meta-object point back into the tree (a `Var::Lexical` resolved to its own
//! declaration, a routine whose body holds the node), so rendering them walks a
//! cycle forever. A literal holding a node is hashed by its `Debug` form
//! (`value::identity_hash`), which overflowed the stack on EEL's `--eval`
//! (`"use EEL; event Bla { ... }".AST`). Like equality, the rendering is blind
//! to the origin's contents.

use super::NodeOrigin;

// Cost: O(1).
impl std::fmt::Debug for NodeOrigin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NodeOrigin")
            .field("origin", &self.0.is_some())
            .field("resolution", &self.2.is_some())
            .field("meta_object", &self.4.is_some())
            .finish_non_exhaustive()
    }
}

#[cfg(test)]
mod tests {
    use super::NodeOrigin;
    use crate::value::Value;

    #[test]
    fn debug_does_not_render_the_linked_values() {
        let origin = NodeOrigin(
            Some(Value::str("ORIGIN-VALUE".to_string())),
            Default::default(),
            Some(Value::str("RESOLUTION-VALUE".to_string())),
            None,
            Some(Value::str("META-VALUE".to_string())),
        );
        let rendered = format!("{origin:?}");
        assert!(!rendered.contains("VALUE"), "{rendered}");
        assert!(rendered.contains("resolution: true"), "{rendered}");
    }
}
