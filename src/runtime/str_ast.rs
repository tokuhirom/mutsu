//! `Str.AST` on the interpreter: the parse needs this interpreter's module
//! search paths and the names its running scope declares.

use super::*;

impl Interpreter {
    /// `Str.AST`: parse source using this interpreter's module search paths.
    ///
    /// Module exports must be scanned while parsing the source, just as they
    /// are for a program being run by this interpreter. The pure builtin path
    /// has no access to those paths, so `.AST` is dispatched through this
    /// interpreter carrier. The names the calling scope declares are installed
    /// for the conversion (see [`Interpreter::str_ast_caller_terms`]).
    pub(crate) fn str_ast_with_paths(
        &mut self,
        source: &str,
        compunit: bool,
    ) -> Result<Value, RuntimeError> {
        crate::parser::set_parser_lib_paths(self.parser_scan_lib_paths());
        crate::parser::set_parser_program_path(self.io.program_path.clone());
        let scope = crate::rakuast::frontend::str_ast_scope(
            self.str_ast_caller_terms(),
            self.str_ast_caller_trait_types(),
        );
        let result = if compunit {
            crate::rakuast::str_dot_ast_compunit(&scope, source)
        } else {
            crate::rakuast::str_dot_ast(&scope, source)
        };
        crate::parser::clear_parser_lib_paths();
        result
    }

    /// `Str.AST($slang)`: parse `source` under the localized surface syntax of
    /// the `L10N::<$slang>` distribution.
    ///
    /// Lives on the interpreter, like `EVAL`, because the sub-parse resolves a
    /// module: the parser's search-path list is only populated around a parse,
    /// so this installs this interpreter's paths for the duration exactly as
    /// `run_program` / `require` do. An undefined `$slang` (rakudo's `Mu
    /// $slang?` default) is the plain, unlocalized parse.
    pub(crate) fn str_ast_with_slang(
        &mut self,
        source: &str,
        slang: &Value,
    ) -> Result<Value, RuntimeError> {
        let slang = crate::runtime::types::value_is_defined(slang).then(|| slang.to_string_value());
        crate::parser::set_parser_lib_paths(self.parser_scan_lib_paths());
        crate::parser::set_parser_program_path(self.io.program_path.clone());
        let scope = crate::rakuast::frontend::str_ast_scope(
            self.str_ast_caller_terms(),
            self.str_ast_caller_trait_types(),
        );
        let result = crate::rakuast::str_dot_ast_with_slang(&scope, source, slang.as_deref());
        crate::parser::clear_parser_lib_paths();
        result
    }

    /// The type names the scope running `.AST` declares, for the `is NAME`
    /// traits of the declarations in its source. Unlike the caller *terms*, an
    /// environment entry only counts when the package it holds is the one its
    /// key names (`my class P`), not a variable that happens to hold a type
    /// object (`my $rw`, whose slot is the type object `Any`).
    // Cost: O(r + e + m + x*d), as [`Interpreter::eval_caller_type_names`].
    fn str_ast_caller_trait_types(&self) -> Vec<String> {
        let held_by_variable: std::collections::HashSet<String> = self
            .env
            .iter()
            .filter_map(|(key, value)| {
                let ValueView::Package(held) = value.view() else {
                    return None;
                };
                let key = key.resolve();
                let base = key.split('\u{0}').next().unwrap_or(&key);
                let held = held.resolve();
                (held.split('\u{0}').next().unwrap_or(&held) != base).then(|| base.to_string())
            })
            .collect();
        let mut types = self.eval_caller_type_names();
        types.retain(|name| !held_by_variable.contains(name));
        types
    }

    /// The names the scope running `.AST` declares, for the conversion of its
    /// source. Rakudo resolves the source's bare names against that scope and
    /// renders each as a `Term::Name` -- a class the caller declares too
    /// (measured on rakudo 2026.09); only the source's own and the setting's
    /// types are `Type::Simple`.
    // Cost: O(e + t), e = visible env entries scanned for constant markers,
    // t = registered user types and enum values.
    fn str_ast_caller_terms(&self) -> Vec<String> {
        let mut terms = self.eval_caller_type_names();
        terms.extend(self.eval_caller_enum_value_names());
        terms
    }
}
