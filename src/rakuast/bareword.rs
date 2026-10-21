//! What a bareword renders as (`Expr::BareWord` → RakuAST).
//!
//! Rakudo resolves a bareword at parse time, so its RakuAST node says what the
//! name *is*: a type object renders as `Type::Simple`, a constant or other
//! defined term as `Term::Name`, a setting enum value as `Term::Enum`, and a
//! routine called without arguments as `Call::Name::WithoutParentheses`.
//! mutsu's parser leaves all of these as `Expr::BareWord`, so the converter
//! re-derives which is which from the setting (the generated name lists) and
//! from the compilation unit's own declarations. A bareword none of those
//! resolves stays the conversion boundary: guessing would render a wrong node.

use std::cell::RefCell;
use std::collections::HashMap;

use super::convert::{leaf_field, name_from_identifier, node_field};
use super::{RakuAstClass, RakuAstNode, core_term_names, core_type_names, name_parts};
use crate::ast::{Expr, ParamDef, Stmt};
use crate::ast_visit::{Visit, walk_expr, walk_param, walk_stmt, walk_stmts};
use crate::qualified::qualified;
use crate::runtime::utils::is_known_type_constraint;

/// Setting types and the finite RakuAST model registry share one classifier.
// Cost: O(k + c), k = name length, c = number of registered model classes.
fn setting_type(name: &str) -> bool {
    is_known_type_constraint(name)
        || core_type_names::contains_in_version(
            name,
            crate::parser::current_language_version_starts_with("6.e"),
        )
        || super::is_registered_type_object(name)
}
use crate::symbol::Symbol;
use crate::value::Value;

mod caller_names;
mod package_alias;

pub(super) use caller_names::CallerNames;

/// What a bareword naming something the same compilation unit declared means.
///
/// raku resolves such a name at parse time, so `class C { }; C.new` renders `C`
/// as a `Type::Simple` — exactly like a builtin type — and
/// `constant X = 5; X` renders `X` as a `Term::Name`. Both measured against
/// rakudo 2026.07.
#[derive(Clone, Copy, PartialEq, Eq)]
enum DeclaredKind {
    /// A `class` / `role` / `grammar` / `enum` name.
    Type,
    /// A `constant` name, an enum value the unit declares (`Red`,
    /// `Color::Red`), or a sigilless parameter (`\x`): all render as
    /// `Term::Name` (measured on rakudo 2026.09).
    Term,
    /// An `our sub` of a package, reached by its qualified name (`M::foo`):
    /// rakudo renders the bare reference as an argument-less `Call::Name`.
    Routine,
}

thread_local! {
    /// The names the compilation unit currently being converted declares.
    /// Empty outside a conversion, so a nested/re-entrant conversion that never
    /// ran `statement_list` simply sees no declarations and keeps the old
    /// bareword boundary.
    static DECLARED_NAMES: RefCell<HashMap<String, DeclaredKind>> =
        RefCell::new(HashMap::new());
}

/// RAII guard installing the unit's declared names for the duration of a
/// conversion, restoring whatever was there before (so a nested conversion
/// cannot leak its names into the outer one).
pub(super) struct DeclaredNames(HashMap<String, DeclaredKind>);

impl DeclaredNames {
    // Cost: O(n), n = size of the unit's AST (one `collect_declared_names` scan).
    pub(super) fn collect(stmts: &[Stmt]) -> Self {
        let mut names = HashMap::new();
        names.extend(
            crate::parser::imported_value_term_names()
                .into_iter()
                .map(|name| (name, DeclaredKind::Term)),
        );
        collect_declared_names(stmts, &mut names);
        Self(DECLARED_NAMES.with(|d| std::mem::replace(&mut *d.borrow_mut(), names)))
    }
}

impl Drop for DeclaredNames {
    fn drop(&mut self) {
        DECLARED_NAMES.with(|d| {
            *d.borrow_mut() = std::mem::take(&mut self.0);
        });
    }
}

/// What a `constant` renders as when named later. Rakudo evaluates the
/// initializer at compile time, so a constant holding a type object is a type
/// (`my constant E = Metamodel::EnumHOW.new_type(...)`, `constant T = Int`) and
/// any other constant a term. Only the initializers whose value is visibly a
/// type object are recognised: a type name, or the MOP `new_type` constructor.
fn constant_kind(init: &Expr) -> DeclaredKind {
    let type_valued = match init {
        // A type the unit declares too (`constant E = Outer::Inner`); while
        // the unit's own scan runs, `package_alias` settles those instead.
        Expr::BareWord(n) => setting_type(n) || declared_kind(n) == Some(DeclaredKind::Type),
        Expr::MethodCall { name, target, .. } => {
            name.resolve() == "new_type"
                && matches!(target.as_ref(), Expr::BareWord(n)
                    if n.starts_with("Metamodel::") || n.ends_with("HOW"))
        }
        _ => false,
    };
    if type_valued {
        DeclaredKind::Type
    } else {
        DeclaredKind::Term
    }
}

/// The declarations of the blocks being converted, innermost last. Each holds
/// only the names its own statement list declares: a lexical enum variable
/// shadows a class of the same name declared further out, which the unit-wide
/// [`DECLARED_NAMES`] (one entry per name) cannot express.
#[derive(Default)]
struct Scopes(Vec<HashMap<String, DeclaredKind>>);

thread_local! {
    static SCOPES: RefCell<Scopes> = RefCell::new(Scopes::default());
}

/// RAII guard for one block's own declarations (see [`SCOPES`]).
pub(super) struct BlockScope;

impl BlockScope {
    // Cost: O(m), m = number of statements in the list (direct children only).
    pub(super) fn enter(stmts: &[Stmt]) -> Self {
        let mut own = HashMap::new();
        for stmt in stmts {
            match stmt {
                Stmt::EnumDecl { name, variants, .. } => {
                    own.insert(name.resolve(), DeclaredKind::Type);
                    for variant in crate::parser::enum_value_names(variants) {
                        own.insert(variant, DeclaredKind::Term);
                    }
                }
                Stmt::ClassDecl { name, .. }
                | Stmt::RoleDecl { name, .. }
                | Stmt::SubsetDecl { name, .. }
                | Stmt::Package { name, .. } => {
                    own.insert(name.resolve(), DeclaredKind::Type);
                }
                Stmt::VarDecl {
                    name,
                    custom_traits,
                    expr,
                    ..
                } if custom_traits.iter().any(|(n, _)| n == "__constant") => {
                    own.insert(name.clone(), constant_kind(expr));
                }
                _ => {}
            }
        }
        SCOPES.with(|s| s.borrow_mut().0.push(own));
        Self
    }
}

impl Drop for BlockScope {
    fn drop(&mut self) {
        SCOPES.with(|s| {
            s.borrow_mut().0.pop();
        });
    }
}

fn scoped_kind(name: &str) -> Option<DeclaredKind> {
    SCOPES.with(|s| s.borrow().0.iter().rev().find_map(|m| m.get(name).copied()))
}

fn declared_kind(name: &str) -> Option<DeclaredKind> {
    scoped_kind(name)
        .or_else(|| DECLARED_NAMES.with(|d| d.borrow().get(name).copied()))
        .or_else(|| caller_names::kind(name))
}

/// Whether `name` names a type at parse time: a builtin type or one the unit
/// declares. Rakudo's `is NAME` trait takes this test to choose between a
/// container type (`Trait::Is(type => …)`) and a named trait.
// Cost: O(k), k = length of `name`.
pub(super) fn names_type(name: &str) -> bool {
    setting_type(name) || declared_kind(name) == Some(DeclaredKind::Type)
}

/// Whether a `::`-qualified package name resolves at parse time: a run of
/// pseudo-packages (`MY`, `OUTER::OUTER`), a builtin type, or a type the unit
/// declares (including the stub `A` a `class A::B { }` creates).
pub(super) fn package_resolves(stem: &str) -> bool {
    name_parts::identifier_segments(stem).all(name_parts::is_pseudo_package)
        || setting_type(stem)
        || declared_kind(stem) == Some(DeclaredKind::Type)
}

/// Record a declared type name, together with the stub packages a qualified
/// name implies: `class A::B { }` makes `A` resolve too, and raku renders a
/// later bareword `A` as a `Type::Simple` (measured on 2026.09).
fn insert_declared_type(name: Symbol, out: &mut HashMap<String, DeclaredKind>) {
    out.insert(name.resolve(), DeclaredKind::Type);
    for stub in crate::qualified::package_ancestors(name).skip(1) {
        out.entry(stub.resolve()).or_insert(DeclaredKind::Type);
    }
    // `package GLOBAL::X::Y { class C }` declares `X::Y::C` absolutely: the
    // root namespace is implicit in every spelling that reaches it.
    if let Some(absolute) = name.resolve().strip_prefix("GLOBAL::")
        && !absolute.is_empty()
    {
        insert_declared_type(Symbol::intern(absolute), out);
    }
}

/// Record a sigilless term. A routine of the same name loses to it: the unit
/// scan has no scopes, and a `\\d` parameter reads as the term inside the sub
/// that also happens to be called `d` elsewhere.
fn insert_term(out: &mut HashMap<String, DeclaredKind>, name: String) {
    let kind = out.entry(name).or_insert(DeclaredKind::Term);
    if *kind == DeclaredKind::Routine {
        *kind = DeclaredKind::Term;
    }
}

/// The names a statement list declares, at any depth: raku resolves a name
/// declared anywhere the reference can see it, and a bareword that reaches
/// conversion at all was already accepted by the parser. So the scan enters
/// every child -- a declaration in an `if` or loop body, a closure or a `do`
/// block counts as well as one in a class, routine or bare block.
// Cost: O(n), n = size of the AST.
fn collect_declared_names(stmts: &[Stmt], out: &mut HashMap<String, DeclaredKind>) {
    /// The names collected so far, and the packages whose body is being scanned
    /// (outermost first): a declaration nested in `class Outer { class Inner }`
    /// is also `Outer::Inner`.
    struct Scan<'o>(
        &'o mut HashMap<String, DeclaredKind>,
        Vec<Symbol>,
        Vec<package_alias::PackageAlias>,
    );

    impl Scan<'_> {
        /// Register `name` under every spelling a reference inside or outside
        /// the enclosing packages can use: `Inner`, `Outer::Inner`, ...
        fn insert_nested_type(&mut self, name: Symbol) {
            for composed in self.compositions(name) {
                insert_declared_type(composed, self.0);
            }
        }

        /// `name`, then `name` under each suffix of the enclosing packages:
        /// for `A`, `B` open, `n`, `B::n` and `A::B::n`.
        fn compositions(&self, name: Symbol) -> Vec<Symbol> {
            let mut all = vec![name];
            for start in 0..self.1.len() {
                let mut composed = name;
                for outer in self.1[start..].iter().rev() {
                    composed = qualified(*outer, composed);
                }
                all.push(composed);
            }
            all
        }
    }

    impl<'ast> Visit<'ast> for Scan<'_> {
        fn visit_stmt(&mut self, stmt: &'ast Stmt) {
            match stmt {
                Stmt::Label { name, .. }
                | Stmt::While {
                    label: Some(name), ..
                }
                | Stmt::For {
                    label: Some(name), ..
                }
                | Stmt::Loop {
                    label: Some(name), ..
                } => insert_term(self.0, name.clone()),
                // An enum's values are terms of their own, bare and qualified
                // by the enum's name (`Red`, `Color::Red`). An `our` enum also
                // installs them in the enclosing packages (`module M { enum E
                // <a> }; M::a`).
                Stmt::EnumDecl {
                    name,
                    variants,
                    is_my,
                    ..
                } => {
                    if !is_my {
                        for variant in crate::parser::enum_value_names(variants) {
                            for composed in self
                                .compositions(Symbol::intern(&variant))
                                .into_iter()
                                .skip(1)
                            {
                                self.0
                                    .entry(composed.resolve())
                                    .or_insert(DeclaredKind::Term);
                            }
                        }
                    }
                    for composed in self.compositions(*name) {
                        insert_declared_type(composed, self.0);
                        for variant in crate::parser::enum_value_names(variants) {
                            let qualified = qualified(composed, Symbol::intern(&variant));
                            self.0.entry(variant).or_insert(DeclaredKind::Term);
                            self.0
                                .entry(qualified.resolve())
                                .or_insert(DeclaredKind::Term);
                        }
                    }
                }
                // A `module`/`package`/`grammar` name resolves at parse time
                // just like a class one: raku renders a later bareword `M` as
                // a `Type::Simple` (measured on `module M { }; M.HOW`).
                Stmt::ClassDecl { name, .. }
                | Stmt::RoleDecl { name, .. }
                | Stmt::SubsetDecl { name, .. }
                | Stmt::Package { name, .. } => self.insert_nested_type(*name),
                // `given X -> \y { y }`: the parameter is declared by the
                // expansion's bind, which the record keeps as written.
                Stmt::SourceForm(form) => {
                    match form.as_ref() {
                        crate::ast::SourceForm::GivenPointy { param_def, .. } => {
                            self.visit_param(param_def)
                        }
                        // `with X -> \y { y }` likewise.
                        crate::ast::SourceForm::WithPointy {
                            param_def: Some(param_def),
                            ..
                        } => self.visit_param(param_def),
                        // `if X -> \y { y }` likewise.
                        crate::ast::SourceForm::IfPointy { param_defs, .. }
                        | crate::ast::SourceForm::ControlPointy { param_defs, .. } => {
                            param_defs.iter().for_each(|p| self.visit_param(p))
                        }
                        // `my (\a, \b) := …` declares each sigilless element as a term.
                        crate::ast::SourceForm::SignatureDecl(decl) => {
                            for var in decl.vars.iter().filter(|v| v.sigilless) {
                                insert_term(self.0, var.name.clone());
                            }
                        }
                        _ => {}
                    }
                }
                // `my \x = 5` / `my \x := $s` declares the term `x`.
                Stmt::SyntheticBlock(_) => {
                    if let Some(decl) = crate::ast::sigilless_decl::declaration(stmt) {
                        insert_term(self.0, decl.name.to_string());
                    }
                }
                // A method literal's folded sigilless invocant is a bind
                // followed by this marker, just like any other term binding.
                Stmt::MarkSigilless(name) | Stmt::MarkSigillessReadonly(name) => {
                    insert_term(self.0, name.clone());
                }
                Stmt::VarDecl {
                    name,
                    custom_traits,
                    expr,
                    ..
                } if custom_traits.iter().any(|(n, _)| n == "__constant") => {
                    self.0.insert(name.clone(), constant_kind(expr));
                    if let Expr::BareWord(target) = expr {
                        self.2.push(package_alias::PackageAlias {
                            spellings: self.compositions(Symbol::intern(name)),
                            target: target.clone(),
                        });
                    }
                    // A `constant` is `our`-scoped: `M::c` reaches it too.
                    for composed in self.compositions(Symbol::intern(name)).into_iter().skip(1) {
                        self.0.insert(composed.resolve(), DeclaredKind::Term);
                    }
                }
                // `my $pkg::x` creates the stub package `pkg`, which a later
                // bareword (`OUR::pkg`) names as a type (measured on 2026.09).
                Stmt::VarDecl { name, .. }
                    if crate::qualified::is_qualified_str(
                        name.trim_start_matches(['@', '%', '&', '*']),
                    ) =>
                {
                    let bare = Symbol::intern(name.trim_start_matches(['@', '%', '&', '*']));
                    for stub in crate::qualified::package_ancestors(bare).skip(1) {
                        self.0.entry(stub.resolve()).or_insert(DeclaredKind::Type);
                    }
                }
                // An `our sub` of a package is reached by its qualified name.
                Stmt::SubDecl {
                    name,
                    custom_traits,
                    ..
                } if custom_traits
                    .iter()
                    .any(|(n, _)| n == super::convert::OUR_SCOPED) =>
                {
                    for composed in self.compositions(*name).into_iter().skip(1) {
                        self.0.insert(composed.resolve(), DeclaredKind::Routine);
                    }
                    self.0
                        .entry(name.resolve())
                        .or_insert(DeclaredKind::Routine);
                }
                // Any other sub the unit declares: a bare mention of its name
                // is an argument-less call (`sub f { }; f`, measured on 2026.09).
                Stmt::SubDecl { name, .. } => {
                    self.0
                        .entry(name.resolve())
                        .or_insert(DeclaredKind::Routine);
                }
                _ => {}
            }
            // The body of a package-like declaration is nested in its name.
            let package = match stmt {
                Stmt::ClassDecl { name, .. }
                | Stmt::RoleDecl { name, .. }
                | Stmt::Package { name, .. } => Some(*name),
                _ => None,
            };
            if let Some(name) = package {
                self.1.push(name);
            }
            walk_stmt(self, stmt);
            if package.is_some() {
                self.1.pop();
            }
        }

        fn visit_expr(&mut self, expr: &'ast Expr) {
            if let Expr::DoBlock {
                label: Some(label), ..
            } = expr
            {
                insert_term(self.0, label.clone());
            }
            // `-> \v { v }` keeps its one parameter outside a `ParamDef`.
            if let Expr::Lambda {
                param,
                param_sigilless: true,
                ..
            } = expr
            {
                insert_term(self.0, param.clone());
            }
            walk_expr(self, expr);
        }

        fn visit_param(&mut self, param: &'ast ParamDef) {
            // A sigilless parameter (`\x`, `-> \v`) is a term; a `+a` /
            // `|c` slurpy carries the same flag and renders the same way.
            if param.sigilless && !param.name.is_empty() {
                insert_term(self.0, param.name.clone());
            }
            // A `&cb` parameter is a routine: a bare `cb` is an argument-less
            // call of it (`sub f(&cb) { cb }`, measured on 2026.09).
            if let Some(routine) = param.name.strip_prefix('&')
                && !routine.is_empty()
            {
                self.0
                    .entry(routine.to_string())
                    .or_insert(DeclaredKind::Routine);
            }
            // A `::T` capture declares the type name `T` (`sub f(::T $x) { T }`
            // renders `T` as a `Type::Simple`, measured on 2026.09).
            if let Some(name) = super::convert::type_capture_name(param) {
                insert_declared_type(Symbol::intern(name), self.0);
            }
            walk_param(self, param);
        }
    }

    let mut scan = Scan(out, Vec::new(), Vec::new());
    walk_stmts(&mut scan, stmts);
    package_alias::apply(&scan.2, scan.0);
}

/// Core routines which are meaningful as bare calls without parentheses.
/// `done` completes the current supply; the redispatch routines have their
/// special meaning in a method body. In RakuAST all render as
/// `Call::Name::WithoutParentheses`.
const CORE_BARE_CALLS: &[&str] = &["callsame", "nextsame", "lastcall", "nextcallee", "done"];

/// The RakuAST node a bareword renders as, or `None` when nothing resolves the
/// name (the conversion boundary).
// Cost: O(k + d), k = length of `name`, d = parser scope depth.
pub(super) fn convert(name: &str) -> Option<RakuAstNode> {
    // `self` -> `Term::Self`, a node with no fields.
    if name == "self" {
        return Some(RakuAstNode {
            class: RakuAstClass::TermSelf,
            fields: Vec::new(),
        });
    }
    // The innermost block's own declaration wins over everything outside it.
    match scoped_kind(name) {
        Some(DeclaredKind::Type) => return Some(simple_type_node(name)),
        Some(DeclaredKind::Term) => return Some(term_name(name)),
        Some(DeclaredKind::Routine) | None => {}
    }
    // A bare type name used as a term (`Int`, `X::AdHoc`) -> `Type::Simple`.
    if setting_type(name) {
        return Some(simple_type_node(name));
    }
    // A name the same compilation unit declared shadows a setting one.
    match declared_kind(name) {
        Some(DeclaredKind::Type) => return Some(simple_type_node(name)),
        Some(DeclaredKind::Term) => return Some(term_name(name)),
        Some(DeclaredKind::Routine) => return Some(call_name(name)),
        None => {}
    }
    // A user term (`constant \term:<ℵ₀>`) is declared under its `term:<...>`
    // name and referenced by the bare symbol; rakudo renders the reference as
    // `Term::Name` of the declared name.
    let term = format!("term:<{name}>");
    if scoped_kind(&term) == Some(DeclaredKind::Term)
        || declared_kind(&term) == Some(DeclaredKind::Term)
    {
        return Some(term_name(&term));
    }
    // `Str:D` / `Int:U` / `Int:_` used as a term: a definedness type over the type. A base
    // that resolves as a term (`constant X = Int; X:D`, or NativeCall's
    // `my constant CArray is export`) takes the smiley the same way: rakudo
    // renders it `Type::Definedness(base-type => Type::Simple(X))` too (`:_` is
    // `Type::AnyDefinedness`)
    // (measured on 2026.09), since a smiley can only follow a type name.
    if let Some(base) = name
        .strip_suffix(":D")
        .or_else(|| name.strip_suffix(":U"))
        .or_else(|| name.strip_suffix(":_"))
        && convert(base).is_some_and(|node| {
            matches!(
                node.class,
                RakuAstClass::TypeSimple | RakuAstClass::TermName
            )
        })
    {
        return super::convert::build_type_node(name).ok();
    }
    // `GLOBAL` and a pseudo-package prefix on a name that resolves
    // (`CORE::DateTime`, `GLOBAL::A`): the same node as the bare name, over the
    // qualified one.
    if crate::qualified::is_global_package(Symbol::intern(name)) {
        return Some(simple_type_node(name));
    }
    if let Some(rest) = name
        .strip_prefix("GLOBAL::")
        .or_else(|| name.strip_prefix("CORE::"))
        && let Some(node) = convert(rest)
    {
        match node.class {
            RakuAstClass::TypeSimple => return Some(simple_type_node(name)),
            RakuAstClass::TermName => return Some(term_name(name)),
            _ => {}
        }
    }
    // `OUR::A` / `MY::A` over a name that resolves: a `Term::Name` of the
    // qualified name, whether `A` is a type or a term (measured on 2026.09).
    if let Some(rest) = name
        .strip_prefix("OUR::")
        .or_else(|| name.strip_prefix("MY::"))
        && convert(rest).is_some_and(|node| {
            matches!(
                node.class,
                RakuAstClass::TypeSimple | RakuAstClass::TermName
            )
        })
    {
        return Some(term_name(name));
    }
    // A name a `use` brought in, which the unit's own declarations do not
    // list: what the parse's scope tables resolved it to.
    match crate::parser::declared_name_kind(name) {
        Some(crate::parser::DeclaredNameKind::Type) => return Some(simple_type_node(name)),
        Some(crate::parser::DeclaredNameKind::Term) => return Some(term_name(name)),
        None => {}
    }
    // A bare imported function is an argument-less call in Rakudo's tree, just
    // like a routine declared in this compilation unit. The parser's module
    // scan has already resolved this name; do not guess for unresolved names.
    if !crate::parser::term_keywords_shadowable() && crate::parser::is_imported_function(name) {
        return Some(call_name(name));
    }
    match core_term_names::kind(name) {
        Some(core_term_names::TermKind::Enum) => {
            return Some(RakuAstNode {
                class: RakuAstClass::TermEnum,
                fields: vec![leaf_field(None, Value::str(name.to_string()))],
            });
        }
        Some(core_term_names::TermKind::Name) => return Some(term_name(name)),
        None => {}
    }
    // `nqp::const::NAME` -> `Nqp::Const`; an `nqp::op` written without
    // parentheses is the same `Nqp` node as `nqp::op()`.
    if let Some(constant) = name.strip_prefix("nqp::const::")
        && !constant.is_empty()
        && !crate::qualified::is_qualified_str(constant)
    {
        return Some(RakuAstNode {
            class: RakuAstClass::NqpConst,
            fields: vec![leaf_field(None, Value::str(constant.to_string()))],
        });
    }
    if let Some(op) = super::convert::nqp_op(name) {
        return Some(RakuAstNode {
            class: RakuAstClass::Nqp,
            fields: vec![leaf_field(None, Value::str(op.to_string()))],
        });
    }
    if CORE_BARE_CALLS.contains(&name) {
        return Some(RakuAstNode {
            class: RakuAstClass::CallNameWithoutParentheses,
            fields: vec![node_field(Some("name"), name_from_identifier(name))],
        });
    }
    None
}

/// An argument-less call by name: `Call::Name::WithoutParentheses` for a plain
/// identifier (`foo`, never parenthesised here), `Call::Name` for a qualified
/// one (`M::foo`).
fn call_name(name: &str) -> RakuAstNode {
    RakuAstNode {
        class: if crate::qualified::is_qualified_str(name) {
            RakuAstClass::CallName
        } else {
            RakuAstClass::CallNameWithoutParentheses
        },
        fields: vec![node_field(Some("name"), name_from_identifier(name))],
    }
}

fn term_name(name: &str) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::TermName,
        fields: vec![node_field(None, name_from_identifier(name))],
    }
}

/// A bare simple type `Int` -> `Type::Simple(Name.from-identifier("Int"))`.
pub(super) fn simple_type_node(t: &str) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::TypeSimple,
        fields: vec![node_field(None, name_from_identifier(t))],
    }
}
