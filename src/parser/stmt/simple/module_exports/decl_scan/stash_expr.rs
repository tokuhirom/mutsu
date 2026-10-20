//! What an expression of a scanned module contributes to its export tables:
//! a slang activation, and the routines and values bound into an export stash.

use super::super::is_export_stash_package;
use super::DeclScan;
use crate::ast::Expr;

impl DeclScan {
    // Cost: O(n), n = bytes of the method, package and key names inspected.
    pub(super) fn scan_expr(&mut self, expr: &Expr) {
        match expr {
            // An L10N distribution's generated EXPORT hook calls
            // `$*LANG.define_slang(...)` instead of `use Slangify`. Read off
            // the AST, not the source text: a comment mentioning
            // `define_slang` must not make a module run at parse time.
            Expr::MethodCall { name, .. } | Expr::HyperMethodCall { name, .. }
                if name.resolve() == "define_slang" =>
            {
                self.out.defines_slang = true;
            }
            // Literal package subscripts keep their IndexAssign form. Read
            // only the key: neither BEGIN nor the initializer runs in a scan.
            Expr::IndexAssign { target, index, .. }
                if matches!(target.as_ref(), Expr::PseudoStash(stash)
                    if stash.strip_suffix("::").is_some_and(is_export_stash_package)) =>
            {
                if let Expr::Literal(key) = index.as_ref()
                    && let Some(key) = key.as_str()
                {
                    if let Some(routine) = key.strip_prefix('&').filter(|name| !name.is_empty()) {
                        self.export(routine.to_string());
                    } else if !key.is_empty()
                        && !key.starts_with(['$', '@', '%'])
                        && !crate::qualified::is_qualified_str(key)
                    {
                        self.out.constant_names.push(key.to_string());
                    }
                }
            }
            // `OUR::{'&infix:<@~~>'} := ...` in an export stash binds that
            // routine into the tag's export list like an `our sub` declared
            // there (Data::Record). A literal key names it; a computed one
            // (`OUR::{'&postfix:<' ~ $c ~ '>'}`) flags the module for a
            // parse-time probe instead (#9500).
            Expr::IndexAssign { target, index, .. }
                if matches!(target.as_ref(), Expr::PseudoStash(s) if s == "OUR::")
                    && is_export_stash_package(&self.package) =>
            {
                if let Expr::Literal(key) = index.as_ref() {
                    if let crate::value::ValueView::Str(key) = key.view()
                        && let Some(routine) = key.strip_prefix('&')
                        && !routine.is_empty()
                    {
                        self.export(routine.to_string());
                    }
                } else {
                    self.out.dynamic_export_stash = true;
                }
            }
            // `%EXPORT<&slack> := &trampoline;` (Object::Delayed): a module
            // that hands out its routines through a `sub EXPORT { %EXPORT }`
            // hook names them as literal keys of a lexical `%EXPORT`, which
            // the importer's parse must know are routines (a listop call
            // `slack { ... }` is otherwise "two terms in a row").
            Expr::IndexAssign { target, index, .. } if matches!(target.as_ref(), Expr::HashVar(h) if h == "EXPORT") => {
                if let Expr::Literal(key) = index.as_ref()
                    && let crate::value::ValueView::Str(key) = key.view()
                    && let Some(routine) = key.strip_prefix('&')
                    && !routine.is_empty()
                {
                    self.export(routine.to_string());
                }
            }
            // `EXPORT::special::<Answer> := 42` binds a sigilless value into a
            // tag's export stash: the importer's bare `Answer` is a value term,
            // like an exported constant (Interval's `:refine` hook).
            Expr::AssignExpr { name, .. } => {
                if let Some(path) = name.strip_prefix("\\EXPORT::")
                    && let Some((tag, value)) = path.split_once("::")
                    && !tag.is_empty()
                    && !value.is_empty()
                    && !crate::qualified::is_qualified_str(value)
                {
                    self.out.constant_names.push(value.to_string());
                }
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn literal_export_stash_bindings_are_static() {
        let _no_execute = crate::parser::no_execute::NoExecuteGuard::enter();
        let (stmts, _) = crate::parser::parse_program(
            "BEGIN { die 'a declaration scan must not execute'; \
             EXPORT::special::<Answer> := 42; \
             EXPORT::special::{'Other'} := 17; \
             EXPORT::special::<&helper> := sub { 42 } }",
        )
        .unwrap();
        let decls = super::super::scan_module_decls(&stmts);
        assert!(decls.constant_names.iter().any(|name| name == "Answer"));
        assert!(decls.constant_names.iter().any(|name| name == "Other"));
        assert!(decls.exports.contains_key("helper"));
        assert!(!decls.dynamic_export_stash);
    }
}
