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
        let result = crate::rakuast::frontend::with_caller_trait_types(
            self.eval_caller_type_names(),
            || {
                crate::rakuast::frontend::with_caller_names(Vec::new(), self.str_ast_caller_terms(), || {
                    if compunit {
                        crate::rakuast::str_dot_ast_compunit(source)
                    } else {
                        crate::rakuast::str_dot_ast(source)
                    }
                })
            },
        );
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
        let result = crate::rakuast::frontend::with_caller_trait_types(
            self.eval_caller_type_names(),
            || {
                crate::rakuast::frontend::with_caller_names(Vec::new(), self.str_ast_caller_terms(), || {
                    crate::rakuast::str_dot_ast_with_slang(source, slang.as_deref())
                })
            },
        );
        crate::parser::clear_parser_lib_paths();
        result
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
