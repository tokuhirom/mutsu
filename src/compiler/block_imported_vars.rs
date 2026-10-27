//! A block-scoped `use` declares the variables it imports in the block
//! (ADR-8751 stage 1).
//!
//! Rakudo installs an imported `$x` in the lexical pad of the block that
//! holds the `use`: it hides an outer `$x` there, a closure taken in the block
//! keeps it, and a routine written outside the block never sees it. An
//! exported variable is always `our`-scoped, so the import is a binding of
//! the module's package variable: the compiler compiles `use M` as if the
//! block went on with `my $x := $M::x;`, for each variable the parse-time
//! scan found the `use` importing (`parser::scanned_exported_vars`). The
//! binding's source is the module's own container (`OpCode::GetImportedVar`),
//! not a by-name read of `$M::x`. The run-time import writes no env alias for
//! such a variable (`runtime::lexical_imports::BLOCK_VARS_MARK`).
//!
//! A `sub EXPORT` hook's export set is known only once the hook has run. The
//! names it spells as literal pair keys (`'$x' => ...`, `'vrai' => ...`) are
//! declared the same way, bound to the value the hook returned for the key:
//! the import stores it in the `use`'s lexical import set instead of an env
//! alias, and the binding reads it from there (`OpCode::GetHookExport`).
//! Whatever else a hook or an export stash under a computed key exports
//! keeps the run-time import.

use super::*;
use crate::value::ValueView;

impl Compiler {
    /// Declare in the enclosing block each variable the `use` of `module`
    /// with `tags` imports, bound to the module's package variable. Called
    /// right after the `UseModule` of a `use` below the unit's top level, so
    /// the package variable exists when the binding reads it.
    // Cost: O(v), v = the module's exported variables.
    pub(super) fn bind_block_imported_vars(
        &mut self,
        module: &str,
        tags: &[String],
        set_id: Symbol,
        set_idx: u32,
    ) {
        let module_idx = self.code.add_constant(Value::str(module.to_string()));
        // The set id lists what the block declares (a module unit leaves its
        // own names out, `module_unit_sets`).
        let text = set_id.resolve();
        let listed: Vec<&str> = text
            .split_once(crate::runtime::lexical_imports::BLOCK_VARS_MARK)
            .map(|(_, rest)| rest.split('\u{1}').next().unwrap_or_default())
            .unwrap_or_default()
            .split(',')
            .collect();
        for (key, source, hook, stmt) in Self::block_imported_var_bindings(module, tags) {
            if !listed.contains(&key.as_str()) {
                continue;
            }
            let name_idx = self
                .code
                .add_constant(Value::str(if hook { key } else { source }));
            self.imported_var_source = Some(if hook {
                ImportedVarSource::Hook { set_idx, name_idx }
            } else {
                ImportedVarSource::Module {
                    module_idx,
                    name_idx,
                }
            });
            self.compile_stmt(&stmt);
            self.imported_var_source = None;
        }
    }

    /// The binding declarations a `use` of `module` with `tags` adds to the
    /// scope holding it, by the parse-time scan: what an `EVAL`'s lexical
    /// cleanup counts among the snippet's own lexicals.
    // Cost: O(v), v = the module's exported variables.
    pub(crate) fn imported_var_decls(module: &str, tags: &[String]) -> Vec<Stmt> {
        Self::scanned_block_imported_var_bindings(module, tags)
            .into_iter()
            .map(|(.., stmt)| stmt)
            .collect()
    }

    /// The suffix of a lexical import set id that names the variables the
    /// block declares for the `use` of `module` with `tags`
    /// (`runtime::lexical_imports::BLOCK_VARS_MARK`); empty when there are
    /// none.
    // Cost: O(v), v = the module's exported variables.
    pub(super) fn block_imported_vars_id_suffix(module: &str, tags: &[String]) -> String {
        let names: Vec<String> = Self::block_imported_var_bindings(module, tags)
            .into_iter()
            .map(|(key, ..)| key)
            .collect();
        if names.is_empty() {
            return String::new();
        }
        format!(
            "{}{}",
            crate::runtime::lexical_imports::BLOCK_VARS_MARK,
            names.join(",")
        )
    }

    /// The key the binding is stored under, the name the module exports it
    /// by, whether a `sub EXPORT` hook returns it, and the binding
    /// declaration of each variable the `use` of `module` with `tags`
    /// imports, by the parse-time scan of the module.
    // Cost: O(v), v = the module's exported variables.
    #[allow(clippy::type_complexity)]
    fn block_imported_var_bindings(
        module: &str,
        tags: &[String],
    ) -> Vec<(String, String, bool, Stmt)> {
        super::compile_inputs::note_block_imported_vars(module, tags);
        Self::scanned_block_imported_var_bindings(module, tags)
    }

    /// What [`Self::block_imported_var_bindings`] answers, as one string a
    /// cached compile is validated against (`compile_inputs`).
    // Cost: O(v), v = the module's exported variables.
    pub(crate) fn block_imported_vars_fingerprint(module: &str, tags: &[String]) -> String {
        Self::scanned_block_imported_var_bindings(module, tags)
            .iter()
            .map(|(key, source, hook, _)| format!("{key}={source}{}", if *hook { "!" } else { "" }))
            .collect::<Vec<_>>()
            .join(",")
    }

    // Cost: O(v), v = the module's exported variables.
    #[allow(clippy::type_complexity)]
    fn scanned_block_imported_var_bindings(
        module: &str,
        tags: &[String],
    ) -> Vec<(String, String, bool, Stmt)> {
        let mut bindings: Vec<(String, String, bool, Stmt)> =
            crate::parser::scanned_exported_vars(module)
                .iter()
                .flat_map(|vars| vars.iter())
                .filter(|var| var.imported_by(tags))
                .filter_map(|var| {
                    let key = var.export_key();
                    Some((key.clone(), key, var.hook, Self::imported_var_binding(var)?))
                })
                .collect();
        // An exported type is a term of the block too (`my \Foo := M::Foo`),
        // whose value is the module's type object; its type positions in the
        // block were qualified at parse time (`parser::type_qualify`).
        for ty in crate::parser::scanned_exported_types(module)
            .iter()
            .flat_map(|types| types.iter())
            .filter(|ty| ty.imported_by(tags))
        {
            // A type a module declares outside any package is a `GLOBAL`
            // one; its binding's source is the type itself
            // (`exec_get_imported_var_op`).
            let package = ty
                .qualified
                .rsplit_once("::")
                .map_or(module, |(package, _)| package);
            let term = crate::parser::ExportedVar {
                name: ty.name.clone(),
                package: package.to_string(),
                tags: ty.tags.clone(),
                term: true,
                hook: false,
            };
            // Keyed by the term key the binding is stored under.
            let key = crate::term_names::term_key(&ty.name);
            if bindings.iter().any(|(k, ..)| *k == ty.name) {
                continue;
            }
            if let Some(stmt) = Self::imported_var_binding(&term) {
                bindings.push((key, ty.name.clone(), false, stmt));
            }
        }
        bindings
    }

    /// `my <name> := <sigil><package>::<name>` for the exported variable
    /// `var` (`my \NAME := ...` for a sigil-less constant), expanded as the
    /// parser expands a binding declaration, or `None` for a name with a
    /// twigil, which names no plain package variable. The right-hand side
    /// only selects the bind shape: its value comes from `GetImportedVar`.
    // Cost: O(n), n = length of the qualified name.
    fn imported_var_binding(var: &crate::parser::ExportedVar) -> Option<Stmt> {
        let (sigil, bare) = match var.name.chars().next()? {
            sigil @ ('@' | '%') => (Some(sigil), &var.name[1..]),
            _ => (None, var.name.as_str()),
        };
        if bare.is_empty() || !bare.starts_with(|c: char| c.is_alphabetic() || c == '_') {
            return None;
        }
        let qualified = format!("{}::{bare}", var.package);
        if var.term {
            let decl = Stmt::VarDecl {
                anonymous: false,
                name: var.name.clone(),
                expr: Expr::BareWord(qualified),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: vec![(
                    crate::ast::sigilless_decl::SIGILLESS_DECL.to_string(),
                    Some(Expr::Literal(Value::FALSE)),
                )],
                has_initializer: true,
                constant: None,
                bind: None,
                where_constraint: None,
            };
            return Some(Stmt::SyntheticBlock(vec![
                Stmt::MarkBind,
                decl,
                Stmt::MarkSigilless(var.name.clone()),
            ]));
        }
        let (expr, bind) = match sigil {
            Some('@') => (Expr::ArrayVar(qualified), None),
            Some(_) => (Expr::HashVar(qualified), None),
            None => (
                Expr::Var(qualified),
                Some(crate::ast::bind_decl::BindKind::Scalar),
            ),
        };
        Some(crate::ast::bind_decl::expand(Stmt::VarDecl {
            anonymous: false,
            name: var.name.clone(),
            expr,
            type_constraint: None,
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: Vec::new(),
            has_initializer: false,
            constant: None,
            bind,
            where_constraint: None,
        }))
    }
}

/// Where the bind source of a block's import declaration comes from
/// (`Compiler::bind_block_imported_vars`).
#[derive(Clone, Copy)]
pub(super) enum ImportedVarSource {
    /// The module's package variable (`OpCode::GetImportedVar`).
    Module { module_idx: u32, name_idx: u32 },
    /// The value a `sub EXPORT` hook returned for the key, which the import
    /// stored in the lexical import set (`OpCode::GetHookExport`).
    Hook { set_idx: u32, name_idx: u32 },
}

/// Marks the id of the hook-value set a block's `require M <...>` fills
/// (`Compiler::compile_block_require`), which no `use`'s set id spells.
pub(crate) const REQUIRE_SET_MARK: &str = "\u{1}require:";

impl Compiler {
    /// The literal import list of a statement-level `require M <&f $x T>`.
    /// The module may be computed (`require ::($name) <&f>`, a path): only
    /// the list decides which names the statement binds.
    // Cost: O(s), s = listed symbols.
    pub(crate) fn static_require_import(expr: &Expr) -> Option<Vec<String>> {
        let Expr::Call { name, args, .. } = expr else {
            return None;
        };
        if name.resolve() != "require" {
            return None;
        }
        let [_module, rest @ ..] = args.as_slice() else {
            return None;
        };
        // `<&f>` is one string literal, `<&f $x>` a list of them.
        let literal_str = |item: &Expr| match item {
            Expr::Literal(v) => match v.view() {
                ValueView::Str(s) => Some(s.to_string()),
                _ => None,
            },
            _ => None,
        };
        let mut symbols = Vec::new();
        for arg in rest {
            match arg {
                Expr::ArrayLiteral(items) => {
                    for item in items {
                        symbols.push(literal_str(item)?);
                    }
                }
                // The parser's own `__mutsu_require_*` adverbs.
                Expr::Binary {
                    left,
                    op: crate::token_kind::TokenKind::FatArrow,
                    ..
                } if literal_str(left).is_some_and(|k| k.starts_with("__mutsu_require")) => {}
                other => symbols.push(literal_str(other)?),
            }
        }
        (!symbols.is_empty()).then_some(symbols)
    }

    /// A `require M <&f $x T>` below the unit's top level (ADR-8751, #14140):
    /// the symbols it imports are bindings of the enclosing scope, as a
    /// block-scoped `use`'s are. The call stores the value of each listed
    /// symbol in a hook-value set (the `__mutsu_require_set` argument), and
    /// the block goes on with `my &f := ...; my $x := ...; my \T := ...`
    /// bound to those values (`OpCode::GetHookExport`), so a closure taken in
    /// the block keeps them after the block's import scope rolled back the
    /// run-time aliases.
    // Cost: O(s), s = listed symbols.
    pub(super) fn compile_block_require(&mut self, expr: &Expr, symbols: &[String]) {
        // The bindings read the set right after the call, so statements
        // listing the same names may share it.
        let set_id = format!("{REQUIRE_SET_MARK}{}", symbols.join(","));
        let set_idx = self.code.add_constant(Value::str(set_id.clone()));
        let Expr::Call { name, args, listop } = expr else {
            return;
        };
        let mut args = args.clone();
        args.push(Expr::Literal(Value::pair(
            "__mutsu_require_set".to_string(),
            Value::str(set_id),
        )));
        let call = Expr::Call {
            name: *name,
            args,
            listop: *listop,
        };
        self.with_stmt_root(|c| c.compile_condition_expr(&call));
        self.emit_sink_pop_call(*name);
        for symbol in symbols {
            let Some(stmt) = Self::required_symbol_binding(symbol) else {
                continue;
            };
            let name_idx = self.code.add_constant(Value::str(symbol.clone()));
            self.imported_var_source = Some(ImportedVarSource::Hook { set_idx, name_idx });
            self.compile_stmt(&stmt);
            self.imported_var_source = None;
        }
    }

    /// The binding declaration of the `require`d symbol `symbol` (`&f`,
    /// `$x`, `@a`, `%h`, or a bare type or term name), expanded as the parser
    /// expands a binding declaration. Its right-hand side only selects the
    /// bind shape: the value comes from `GetHookExport`.
    // Cost: O(|symbol|).
    fn required_symbol_binding(symbol: &str) -> Option<Stmt> {
        let (sigil, bare) = match symbol.chars().next()? {
            sigil @ ('$' | '@' | '%' | '&') => (Some(sigil), &symbol[1..]),
            _ => (None, symbol),
        };
        if bare.is_empty()
            || !bare.starts_with(|c: char| c.is_alphabetic() || c == '_')
            || crate::qualified::is_qualified_str(bare)
        {
            return None;
        }
        // Only the declaration's shape is read: its name in declaration
        // spelling (a scalar without the sigil) and whether it is a term.
        let var = crate::parser::ExportedVar {
            name: if sigil == Some('$') {
                bare.to_string()
            } else {
                symbol.to_string()
            },
            package: "GLOBAL".to_string(),
            tags: Vec::new(),
            term: sigil.is_none(),
            hook: true,
        };
        if sigil == Some('&') {
            let decl = Stmt::VarDecl {
                anonymous: false,
                name: symbol.to_string(),
                expr: Expr::CodeVar(bare.to_string()),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                has_initializer: false,
                constant: None,
                bind: None,
                where_constraint: None,
            };
            return Some(Stmt::SyntheticBlock(vec![Stmt::MarkBind, decl]));
        }
        Self::imported_var_binding(&var)
    }
}
