//! The compile-time name checks an `EVAL`'d snippet goes through before it
//! runs: calls to routines that are declared only after a `BEGIN` that uses
//! them, illegally post-declared types, calls to undeclared routines and
//! undeclared bareword names.
//!
//! Every walk is the typed AST visitor (ADR-0137), so a name is judged in
//! every position rakudo judges it — a routine body, a method of a class, a
//! closure, a parameter default — not only at the snippet's top level.

use super::undeclared_routines::{ScanMode, scope_blind_declared_names};
use super::*;
use crate::ast_visit::{NameKind, Visit, walk_expr, walk_stmt, walk_stmts};

/// Statement keywords and terms the parser can leave as a `BareWord` that do
/// not name a symbol, so an undeclared-name check must not report them.
const NON_SYMBOL_BAREWORDS: &[&str] = &[
    "NaN",
    "Inf",
    "Empty",
    "True",
    "False",
    "Nil",
    "Any",
    "Mu",
    "self",
    "given",
    "when",
    "default",
    "if",
    "elsif",
    "else",
    "unless",
    "with",
    "without",
    "orwith",
    "for",
    "while",
    "until",
    "loop",
    "repeat",
    "do",
    "try",
    "anon",
    "my",
    "our",
    "has",
    "state",
    "sub",
    "method",
    "submethod",
    "multi",
    "proto",
    "only",
    "class",
    "role",
    "grammar",
    "token",
    "rule",
    "regex",
    "module",
    "package",
    "enum",
    "subset",
    "constant",
    "return",
    "leave",
    "last",
    "next",
    "redo",
    "succeed",
    "proceed",
    "die",
    "fail",
    "is",
    "does",
    "of",
    "where",
    "but",
    "use",
    "no",
    "need",
    "require",
    "import",
    "lazy",
    "eager",
    "hyper",
    "race",
    "sink",
    "react",
    "supply",
    "whenever",
    "start",
    "gather",
    "take",
    "quietly",
    "now",
    "time",
    "rand",
    "pi",
    "e",
    "tau",
    "i",
    "IterationEnd",
];

/// Rakudo core names mutsu builds lazily or only as the result of a method
/// (`REPL` is registered on first use, `Mu.WALK` returns a `WalkList`), so no
/// type registry knows them before the program runs. `GLOBALish` is the
/// compiler's name for the unit's `GLOBAL`.
const CORE_CLASSES_REGISTERED_ON_USE: &[&str] = &[
    "REPL",
    "WalkList",
    "GLOBALish",
    "PROCESS",
    "Perl6::Compiler",
];

/// A keyword or core term the parser can leave where a symbol would be.
// Cost: O(k), k = length of the keyword list.
pub(super) fn is_core_term(name: &str) -> bool {
    NON_SYMBOL_BAREWORDS.contains(&name)
}

/// The modules a subtree names (`use`/`need`/`import`/`require`).
#[derive(Default)]
struct ModuleNames {
    names: HashSet<String>,
}

impl<'ast> Visit<'ast> for ModuleNames {
    fn visit_name(&mut self, name: &str, kind: NameKind) {
        if kind == NameKind::Module {
            self.names.insert(name.to_string());
        }
    }
}

/// The callees of every by-name call in a subtree.
#[derive(Default)]
struct CallNames {
    names: HashSet<String>,
}

impl<'ast> Visit<'ast> for CallNames {
    fn visit_name(&mut self, name: &str, kind: NameKind) {
        if matches!(kind, NameKind::Call | NameKind::UserRoutineCall) {
            self.names.insert(name.to_string());
        }
    }
}

/// The capitalised bareword terms of a subtree (`Foo`, `Foo.bar`): the type
/// names it refers to.
#[derive(Default)]
struct TypeRefs {
    names: Vec<String>,
}

impl<'ast> Visit<'ast> for TypeRefs {
    fn visit_expr(&mut self, expr: &'ast Expr) {
        if let Expr::BareWord(name) = expr
            && name.starts_with(|c: char| c.is_ascii_uppercase())
        {
            self.names.push(name.clone());
        }
        walk_expr(self, expr);
    }
}

/// The first bareword term that names nothing in scope.
struct UndeclaredName<'a> {
    interp: &'a Interpreter,
    /// Everything the unit declares, scope-blind (the safe direction: a name
    /// declared anywhere is never reported).
    declared: &'a HashSet<String>,
    found: Option<String>,
    /// Judge only capitalised (type-like) names. The mainline leaves a
    /// lowercase term to the undeclared-routine check: rakudo reports one as
    /// an undeclared routine, and the parser leaves several lowercase
    /// keywords and regex names as barewords.
    type_like_only: bool,
    /// The line of the statement being walked, for the mainline's message.
    line: i64,
    found_line: i64,
}

impl UndeclaredName<'_> {
    fn is_known(&self, name: &str) -> bool {
        let interp = self.interp;
        // A bare `_` is only declared when the caller has a sigilless `_`
        // term. Its value lives under a private key; the ordinary topic entry
        // must not make an undeclared `_` look valid.
        if name == "_" {
            return self.declared.contains(name)
                || interp
                    .env()
                    .contains_key(crate::symbol::SIGILLESS_UNDERSCORE_STORAGE);
        }
        // A definite/undefined type object (`Str:D`, `K:U`, `Int:_`) is known
        // exactly when its base type is (#10814).
        if let (base, Some(_)) = crate::runtime::types::strip_type_smiley(name)
            && !base.is_empty()
        {
            return self.is_known(base);
        }
        // A parameterised type (`Tree[Type]`) is known when its base type is.
        if let Some(open) = name.find('[')
            && open > 0
            && name.ends_with(']')
        {
            return self.is_known(&name[..open]);
        }
        // A module's own declaration resolves only where that module is
        // merged (ADR-11136); the snippet may merge it itself (`use M; C`).
        if interp.module_name_hidden_here(name) {
            return self.declared.contains(name);
        }
        is_core_term(name)
            // Package-qualified names are looked up elsewhere.
            || crate::qualified::is_qualified_str(name)
            || self.declared.contains(name)
            || interp.has_type(name)
            || interp.has_class(name)
            || interp.has_function(name)
            || interp.has_multi_function_unindexed(name)
            || interp.env().contains_key(name)
            || interp.env().contains_key(&format!("&{name}"))
            // An in-scope sigil-less constant (#9962).
            || interp.term_binding(name).is_some()
            // `our`-scoped constants/variables installed in the package
            // survive in `our_vars` even after their lexical block exits.
            || interp.get_our_var(name).is_some()
            // A file-scope constant of the running routine's own module.
            || interp.module_scope_lexical(name).is_some()
            || Interpreter::is_builtin_type(name)
            || crate::builtin_types::catalog::builtin_type_info(name).is_some()
            || Interpreter::is_pseudo_package_name(name)
            || CORE_CLASSES_REGISTERED_ON_USE.contains(&name)
            || Interpreter::is_implicit_zero_arg_builtin(name)
            || Interpreter::is_builtin_function(name)
            || super::system_eval_names::EVAL_KNOWN_ROUTINE_NAMES.contains(&name)
            || crate::parser::is_imported_function(name)
    }
}

impl<'ast> Visit<'ast> for UndeclaredName<'_> {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.found.is_some() {
            return;
        }
        if let Stmt::SetLine(n) = stmt {
            self.line = *n;
        }
        walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if self.found.is_some() {
            return;
        }
        if let Expr::BareWord(name) = expr
            && (!self.type_like_only || name.starts_with(|c: char| c.is_ascii_uppercase()))
            && !self.is_known(name)
        {
            self.found = Some(name.clone());
            self.found_line = self.line;
            return;
        }
        walk_expr(self, expr);
    }
}

impl Interpreter {
    /// The first bareword term of `stmts` that names nothing in `declared` or
    /// in the interpreter's scope, with the line it is on.
    // Cost: O(n * l), n = size of the unit's AST, l = cost of one
    // registry/env lookup.
    pub(super) fn first_undeclared_name(
        &self,
        stmts: &[Stmt],
        declared: &HashSet<String>,
        type_like_only: bool,
    ) -> Option<(String, i64)> {
        let mut scan = UndeclaredName {
            interp: self,
            declared,
            found: None,
            type_like_only,
            line: 1,
            found_line: 1,
        };
        walk_stmts(&mut scan, stmts);
        scan.found.map(|name| (name, scan.found_line))
    }

    /// A `BEGIN { ... }` block runs at compile time, so it can only see
    /// routines declared *before* it. Calling a sub that is declared *later*
    /// in the same unit (`BEGIN { ohnoes() }; sub ohnoes() {}`) is
    /// X::Undeclared::Symbols at BEGIN time, even though the sub exists by the
    /// end of the unit — wherever in the BEGIN block the call sits.
    // Cost: O(n), n = size of the unit's AST.
    pub(crate) fn check_eval_begin_forward_calls(
        &self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        // All sub names declared at this level (forward + backward).
        let all_subs: HashSet<String> = stmts
            .iter()
            .filter_map(|s| match s {
                Stmt::SubDecl { name, .. } => Some(name.resolve()),
                _ => None,
            })
            .collect();
        if all_subs.is_empty() {
            return Ok(());
        }
        let mut declared_before: HashSet<String> = HashSet::new();
        for s in stmts {
            match s {
                Stmt::SubDecl { name, .. } => {
                    declared_before.insert(name.resolve());
                }
                Stmt::Phaser {
                    kind: PhaserKind::Begin,
                    body,
                    ..
                } => {
                    let mut calls = CallNames::default();
                    walk_stmts(&mut calls, body);
                    // A call to a sub declared only *after* this BEGIN.
                    if let Some(fwd) = calls
                        .names
                        .iter()
                        .find(|c| all_subs.contains(*c) && !declared_before.contains(*c))
                    {
                        let mut attrs = ValueMap::default();
                        attrs.insert("symbol".to_string(), Value::str(fwd.clone()));
                        attrs.insert(
                            "message".to_string(),
                            Value::str(format!("Undeclared routine:\n    {} used at line 1", fwd)),
                        );
                        return Err(RuntimeError::typed("X::Undeclared::Symbols", attrs));
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    /// Detect an *illegally post-declared* type: a type name used (as a term or
    /// method invocant) before its textual declaration in the same EVAL'd unit.
    /// Raku resolves type names lexically; using `Foo.bar` and only declaring
    /// `class Foo {}` (or grammar/role/enum/subset) *afterwards* is
    /// X::Undeclared::Symbols with a `post_types` entry, distinct from a
    /// never-declared name (`unk_types`). Mirrors rakudo's CHECK-time check,
    /// which also sees a use inside a routine or method body.
    // Cost: O(n), n = size of the unit's AST.
    pub(crate) fn check_eval_post_declared_types(
        &self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        // Map each top-level type declaration name -> the index of the statement
        // that declares it (first declaration wins).
        let mut decl_index: HashMap<String, usize> = HashMap::new();
        for (i, stmt) in stmts.iter().enumerate() {
            match stmt {
                Stmt::ClassDecl { name, .. }
                | Stmt::RoleDecl { name, .. }
                | Stmt::SubsetDecl { name, .. }
                | Stmt::EnumDecl { name, .. } => {
                    decl_index.entry(name.resolve()).or_insert(i);
                }
                _ => {}
            }
        }
        if decl_index.is_empty() {
            return Ok(());
        }
        // For each statement, collect the type names it references and flag any
        // whose declaration only appears at a *later* top-level statement.
        for (i, stmt) in stmts.iter().enumerate() {
            let mut refs = TypeRefs::default();
            refs.visit_stmt(stmt);
            for name in refs.names {
                if let Some(&j) = decl_index.get(&name)
                    && j > i
                {
                    let msg = format!("Illegally post-declared type:\n    {} used at line 1", name);
                    return Err(RuntimeError::post_declared_type_symbols(&name, msg));
                }
            }
        }
        Ok(())
    }

    /// Detect a call to an undeclared *routine* in EVAL'd code. Raku resolves
    /// routine names at compile time, so `EVAL '$x = 1; no_such_routine()'`
    /// throws X::Undeclared::Symbols *before* `$x = 1` runs. This is the
    /// mainline CHECK-time analysis (`undeclared_routines`) in its `EVAL` mode,
    /// so a call is judged in every position — a method body included — and a
    /// name resolving to a routine, type or `&name` the caller has in scope is
    /// fine.
    // Cost: see `check_undeclared_routines`.
    pub(crate) fn check_eval_undeclared_routines(
        &self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        self.check_undeclared_routines(stmts, ScanMode::Eval)
    }

    /// Reject a bareword term that names nothing — not a type, routine,
    /// constant or other declaration of this unit, nor anything the caller
    /// has in scope — anywhere in an `EVAL`'d snippet (rakudo: "Undeclared
    /// name").
    // Cost: O(n * l), n = size of the unit's AST, l = cost of one
    // registry/env lookup.
    pub(crate) fn check_eval_undeclared_names(&self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        let mut declared = scope_blind_declared_names(stmts);
        // What the snippet's `use`/`need` statements bring in: the modules
        // themselves and the types their sources declare (`use A; A.new`).
        // They are not loaded yet when this check runs.
        let mut modules = ModuleNames::default();
        walk_stmts(&mut modules, stmts);
        declared.extend(modules.names);
        let types = self.eval_declared_types(stmts);
        // A `use` of a module that imports through a `sub EXPORT` hook brings
        // in whatever names the hook returns for the `use`'s arguments
        // (`use M <U>; U.k`), known only once it runs. Rakudo runs `use` at
        // compile time, so such a term is declared; the check cannot see it
        // and must not judge any bareword of the unit (the mainline
        // undeclared-routine check bails out on unseen imports the same way).
        if types.imports_through_export_hook {
            return Ok(());
        }
        declared.extend(types.types);
        declared.extend(types.packages);
        if let Some((name, _)) = self.first_undeclared_name(stmts, &declared, false) {
            let suggestions = self.suggest_type_names(&name);
            return Err(RuntimeError::undeclared_type_symbols(
                &name,
                format!("Undeclared name:\n    {} used at line 1", name),
                suggestions,
            ));
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{CallNames, TypeRefs};
    use crate::ast_visit::walk_stmts;

    fn parse(src: &str) -> Vec<crate::ast::Stmt> {
        crate::parser::parse_program(src).expect("parse").0
    }

    #[test]
    fn call_names_reach_nested_bodies() {
        let mut calls = CallNames::default();
        walk_stmts(&mut calls, &parse("if True { later() }; sub x { inner() }"));
        assert!(calls.names.contains("later"));
        assert!(calls.names.contains("inner"));
    }

    #[test]
    fn type_refs_reach_routine_bodies_but_not_literals() {
        let stmts = parse("sub f { Foo.new; say 'Bar' }");
        let mut refs = TypeRefs::default();
        walk_stmts(&mut refs, &stmts);
        assert!(refs.names.iter().any(|n| n == "Foo"));
        assert!(!refs.names.iter().any(|n| n == "Bar"));
    }
}
