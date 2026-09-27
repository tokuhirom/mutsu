//! Compile-time lowering of a regex literal's interpolating double-quoted
//! atoms (`/"x @a[0]"/`) to qq thunks — see [`crate::regex_qq_atoms`].

use super::*;
use crate::runtime::meta_ns::MetaNs;
use crate::value::ValueView;

impl Compiler {
    /// Compile one closure per interpolating `"..."` atom of the regex
    /// literal `v`, each into a fresh local, and return the capture entries
    /// (`MetaNs::RegexQq` key, local slot) that put them on the regex value's
    /// scope. Empty when the literal has no such atom.
    ///
    /// The body goes through the one qq-string interpolation parser
    /// (`parse_dispatch::parse_qq_interpolation`) and is compiled as an
    /// ordinary block, so its variables, subscripts, method calls and
    /// embedded blocks resolve in the literal's defining scope exactly as the
    /// same `"..."` would outside a regex.
    pub(super) fn compile_regex_qq_thunks(&mut self, v: &Value) -> Vec<(Symbol, u32)> {
        let mut captures = Vec::new();
        for body in Self::regex_qq_thunk_bodies(v) {
            let expr = crate::parse_dispatch::parse_qq_interpolation(&body);
            if matches!(expr, Expr::Literal(_)) {
                continue;
            }
            let thunk = Expr::AnonSub {
                body: vec![Stmt::Expr(expr)],
                is_rw: false,
                is_raw: false,
                is_block: true,
            };
            let name = format!("__mutsu_regex_qq_{}", self.code.locals.len());
            let slot = self.alloc_fresh_local(&name);
            self.compile_expr(&thunk);
            self.code.emit(OpCode::SetLocal(slot));
            captures.push((MetaNs::RegexQq.key(Symbol::intern(&body)), slot));
        }
        captures
    }

    /// The `"..."` atom bodies of regex literal `v` that
    /// [`Compiler::compile_regex_qq_thunks`] lowers.
    pub(super) fn regex_qq_thunk_bodies(v: &Value) -> Vec<String> {
        let pattern = match v.view() {
            ValueView::Regex(p) => p.as_str().to_string(),
            ValueView::RegexWithAdverbs(a) => a.pattern.as_str().to_string(),
            _ => return Vec::new(),
        };
        // An anonymous `token ($x) { ... }` binds its parameters at match
        // time, where a thunk built here cannot see them.
        if v.regex_signature().is_some() {
            return Vec::new();
        }
        crate::regex_qq_atoms::thunk_bodies(&pattern)
    }
}
