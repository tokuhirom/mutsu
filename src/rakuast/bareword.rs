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
use crate::symbol::Symbol;
use crate::value::Value;

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
        Expr::BareWord(n) => is_known_type_constraint(n) || core_type_names::contains(n),
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
                    for (variant, _) in variants {
                        own.insert(variant.clone(), DeclaredKind::Term);
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
    scoped_kind(name).or_else(|| DECLARED_NAMES.with(|d| d.borrow().get(name).copied()))
}

/// Whether `name` names a type at parse time: a builtin type or one the unit
/// declares. Rakudo's `is NAME` trait takes this test to choose between a
/// container type (`Trait::Is(type => …)`) and a named trait.
// Cost: O(k), k = length of `name`.
pub(super) fn names_type(name: &str) -> bool {
    is_known_type_constraint(name)
        || core_type_names::contains(name)
        || declared_kind(name) == Some(DeclaredKind::Type)
}

/// Whether a `::`-qualified package name resolves at parse time: a run of
/// pseudo-packages (`MY`, `OUTER::OUTER`), a builtin type, or a type the unit
/// declares (including the stub `A` a `class A::B { }` creates).
pub(super) fn package_resolves(stem: &str) -> bool {
    name_parts::identifier_segments(stem).all(name_parts::is_pseudo_package)
        || is_known_type_constraint(stem)
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
    struct Scan<'o>(&'o mut HashMap<String, DeclaredKind>, Vec<Symbol>);

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
                // An enum's values are terms of their own, bare and qualified
                // by the enum's name (`Red`, `Color::Red`).
                Stmt::EnumDecl { name, variants, .. } => {
                    for composed in self.compositions(*name) {
                        insert_declared_type(composed, self.0);
                        for (variant, _) in variants {
                            self.0.entry(variant.clone()).or_insert(DeclaredKind::Term);
                            let qualified = qualified(composed, Symbol::intern(variant));
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
                // `my \x = 5` / `my \x := $s` declares the term `x`.
                Stmt::SyntheticBlock(_) => {
                    if let Some(decl) = crate::ast::sigilless_decl::declaration(stmt) {
                        insert_term(self.0, decl.name.to_string());
                    }
                }
                Stmt::VarDecl {
                    name,
                    custom_traits,
                    expr,
                    ..
                } if custom_traits.iter().any(|(n, _)| n == "__constant") => {
                    self.0.insert(name.clone(), constant_kind(expr));
                    // A `constant` is `our`-scoped: `M::c` reaches it too.
                    for composed in self.compositions(Symbol::intern(name)).into_iter().skip(1) {
                        self.0.insert(composed.resolve(), DeclaredKind::Term);
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
            // A `::T` capture declares the type name `T` (`sub f(::T $x) { T }`
            // renders `T` as a `Type::Simple`, measured on 2026.09).
            if let Some(name) = super::convert::type_capture_name(param) {
                insert_declared_type(Symbol::intern(name), self.0);
            }
            walk_param(self, param);
        }
    }

    walk_stmts(&mut Scan(out, Vec::new()), stmts);
}

/// The redispatch routines a body calls without arguments (`callsame`,
/// `nextsame`): raku renders each as `Call::Name::WithoutParentheses` with no
/// `args`, the node an argument-less listop call of a declared sub gets too.
/// mutsu's parser keeps them as barewords; the call they lower back to
/// dispatches the same way.
const REDISPATCH_CALLS: &[&str] = &["callsame", "nextsame", "lastcall", "nextcallee"];

/// The RakuAST node a bareword renders as, or `None` when nothing resolves the
/// name (the conversion boundary).
// Cost: O(k), k = length of `name` (a fixed number of hash lookups).
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
    if is_known_type_constraint(name) || core_type_names::contains(name) {
        return Some(simple_type_node(name));
    }
    // A name the same compilation unit declared shadows a setting one.
    match declared_kind(name) {
        Some(DeclaredKind::Type) => return Some(simple_type_node(name)),
        Some(DeclaredKind::Term) => return Some(term_name(name)),
        Some(DeclaredKind::Routine) => return Some(call_name(name)),
        None => {}
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
    // A name a `use` brought in, which the unit's own declarations do not
    // list: what the parse's scope tables resolved it to.
    match crate::parser::declared_name_kind(name) {
        Some(crate::parser::DeclaredNameKind::Type) => return Some(simple_type_node(name)),
        Some(crate::parser::DeclaredNameKind::Term) => return Some(term_name(name)),
        None => {}
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
    if REDISPATCH_CALLS.contains(&name) {
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
