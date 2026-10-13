//! Loading a `unit class`/`unit role` body's modules before the declaration.

use crate::ast::Stmt;

/// Copies of the `use`/`need`/`import` statements of a unit package's body,
/// to run before the declaration: `use` is compile-time in Raku, so
/// `unit class Foo is Bar; use Bar;` (or `unit role R; use Base; also does
/// Base;`) must load the module before the parents or roles resolve, while the
/// unit body itself only runs at registration time, after that.
///
/// The originals stay in the body. An import is lexical to the unit body, so
/// the methods declared there must see it through the package's own import,
/// not through the hoisted copy, which runs at the file's top level and so
/// imports into `GLOBAL`. A `GLOBAL` alias is the shape of an import made for
/// the importing scope, so a scope rollback around the module's load (a
/// `require` in a parameter default) drops it, and the class's methods died
/// with "Unknown function" (PDF's `unit class PDF::COS::Dict; also is Hash;
/// use PDF::COS::Util :&from-ast;`). Importing again from an already loaded
/// module is cheap.
// Cost: O(n), n = statements in the body.
pub(super) fn hoist_unit_uses(body: &[Stmt]) -> Vec<Stmt> {
    body.iter()
        .filter(|stmt| matches!(stmt, Stmt::Use { .. } | Stmt::Need { .. } | Stmt::Import { .. }))
        .cloned()
        .collect()
}
