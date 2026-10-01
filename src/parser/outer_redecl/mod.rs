//! Post-parse check for redeclaring a lexical that is already bound to an outer
//! symbol *after it has been referenced* in the current scope.
//!
//! Rakudo raises a compile-time `X::Redeclaration::Outer`
//! ("Lexical symbol '$x' is already bound to an outer symbol...") when, within a
//! single lexical scope, a name is first *referenced* — resolving to a binding in
//! an enclosing scope — and then *redeclared* with `my`/`state`. For example:
//!
//! ```raku
//! sub s($i is copy) {
//!     for 1..3 {
//!         @array.push($i);   # references the outer (parameter) $i
//!         my $i = 1;         # ERROR: $i is already bound to an outer symbol
//!     }
//! }
//! ```
//!
//! The error fires only when *all three* hold in the same scope, in order:
//!   1. the name is declared in an *enclosing* scope (a `my`/`state`/param), and
//!   2. it is referenced in this scope *before* being redeclared here, and
//!   3. it is then redeclared with `my`/`state` in this scope.
//!
//! A same-scope redeclaration (no enclosing binding) is only a warning, and a
//! redeclaration *before* any reference is fine — so this walker tracks, per
//! lexical scope, the set of names referenced-as-outer so far, and checks each
//! `my`/`state` declaration against it.
//!
//! The walk is a [`VisitMut`] (ADR-10499), so it reaches every child; what it
//! decides is where scopes open (`visit.rs`). A false *positive* is only
//! possible if an in-scope declaration is missed, so every declaration form
//! (`my`/`state`, params, `for`/pointy/`whenever` loop variables, and inline
//! `do my $x`) is registered before the following statements are examined.
//! References held as source text (a `s///` quote-form replacement) are not
//! seen.
//!
//! The same walk also finds a variable used in its own declaration's
//! initializer (`my $x = $x + 1`), which rakudo rejects at compile time with
//! `X::Syntax::Variable::Initializer`: the new `$x` is already in scope there,
//! so the initializer could only ever read the not-yet-initialized binding. A
//! reference inside a nested code object (`my $x = sub { $x }`,
//! `my $x = do { $x }`) is legal -- it sees the new binding -- so only a
//! reference at the declaration's own scope depth counts. Dynamic variables
//! (`my $*X = $*X`) are exempt: rakudo lets that read the fresh `Any`.

mod errors;
mod visit;

pub(crate) use errors::scope_diagnostic_error;

use crate::ast::{ParamDef, Stmt};
use crate::ast_visit::VisitMut;
use std::collections::HashSet;

/// The internal trait marking a declaration initialized by `.=` on its own
/// (untyped) variable: `my @c .= new(...)` is `my @c = @c.new(...)`, whose
/// self-read is the invocant, not a use in its own initializer.
pub(crate) const METHOD_ASSIGN_DECL_TRAIT: &str = "__method_assign_decl";

/// A single lexical scope: names declared here so far, and names referenced here
/// that resolved to an enclosing scope (before any local redeclaration).
struct Scope {
    declared: HashSet<String>,
    ref_outer: HashSet<String>,
}

impl Scope {
    fn new() -> Self {
        Scope {
            declared: HashSet::new(),
            ref_outer: HashSet::new(),
        }
    }
}

/// A compile-time error the walk found.
#[derive(Debug, PartialEq, Eq)]
pub(crate) enum ScopeDiagnostic {
    /// `X::Redeclaration::Outer` for `(sigil+name, line)`.
    OuterRedeclaration(String, i64),
    /// `X::Syntax::Variable::Initializer` for `(sigil+name, line)`.
    SelfInitializer(String, i64),
}

/// A declaration whose initializer is being walked.
struct Initializing {
    /// The declared key (`$x`, or `$*X` for a dynamic variable).
    key: String,
    /// The scope depth the declaration is in.
    depth: usize,
    /// Whether the initializer reads the new binding: from a nested code
    /// object for a lexical, from anywhere for a dynamic variable.
    sees_self: bool,
}

struct Ctx {
    scopes: Vec<Scope>,
    line: i64,
    /// Declarations whose initializer is being walked, innermost last.
    initializing: Vec<Initializing>,
    /// The first offense found, if any.
    found: Option<ScopeDiagnostic>,
}

impl Ctx {
    /// Register a reference to `key` in the current (innermost) scope. If the
    /// name is not declared here but *is* declared in some enclosing scope, it is
    /// an outer reference and recorded as such.
    fn reference(&mut self, key: String) {
        let depth = self.scopes.len();
        if depth == 0 {
            return;
        }
        if let Some(init) = self.initializing.iter_mut().rev().find(|i| i.key == key) {
            if init.depth == depth {
                if self.found.is_none() {
                    self.found = Some(ScopeDiagnostic::SelfInitializer(key, self.line));
                }
                return;
            }
            // A nested code object in the initializer reads the new binding,
            // unless a scope in between declares its own `key`.
            let (from, to) = (init.depth, depth);
            if !self.scopes[from..to]
                .iter()
                .any(|s| s.declared.contains(&key))
            {
                init.sees_self = true;
            }
        }
        if self.scopes[depth - 1].declared.contains(&key) {
            return; // resolves locally
        }
        let outer = self.scopes[..depth - 1]
            .iter()
            .any(|s| s.declared.contains(&key));
        if outer {
            self.scopes[depth - 1].ref_outer.insert(key);
        }
    }

    /// Register a reference to the dynamic variable `key` (`$*X`): any read
    /// of it in its own declaration's initializer sees the new binding.
    fn reference_dynamic(&mut self, key: &str) {
        if let Some(init) = self.initializing.iter_mut().rev().find(|i| i.key == key) {
            init.sees_self = true;
        }
    }

    /// Register a `my`/`state` declaration of `key` in the current scope. If it
    /// was already referenced-as-outer here, that is the error.
    fn declare(&mut self, key: String) {
        let depth = self.scopes.len();
        if depth == 0 {
            return;
        }
        if self.found.is_none() && self.scopes[depth - 1].ref_outer.contains(&key) {
            self.found = Some(ScopeDiagnostic::OuterRedeclaration(key.clone(), self.line));
        }
        self.scopes[depth - 1].declared.insert(key);
    }

    /// Register a declared name (param / loop var) without the outer-reference
    /// check — used when seeding a fresh scope with its parameters.
    fn seed(&mut self, key: String) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.declared.insert(key);
        }
    }
}

/// Normalize a declaration/param name (`"i"`, `"@b"`, `"%c"`) to a sigil-keyed
/// form (`"$i"`, `"@b"`, `"%c"`), or `None` if it is a special/synthetic name we
/// do not track.
fn decl_key(name: &str) -> Option<String> {
    let (sigil, base) = match name.chars().next()? {
        '@' => ('@', &name[1..]),
        '%' => ('%', &name[1..]),
        '$' => ('$', &name[1..]),
        '&' => return None, // subs are hoisted; not subject to this rule
        _ => ('$', name),
    };
    normalized(sigil, base)
}

/// Build the tracked key for a reference of the given sigil and base name.
fn ref_key(sigil: char, base: &str) -> Option<String> {
    normalized(sigil, base)
}

fn normalized(sigil: char, base: &str) -> Option<String> {
    let first = base.chars().next()?;
    // Reject twigils, special variables, package-qualified names and synthetic
    // compiler-generated temporaries.
    if !(first.is_ascii_alphabetic() || first == '_') {
        return None;
    }
    if base == "_" || base.starts_with("__") || base.contains(':') {
        return None;
    }
    Some(format!("{}{}", sigil, base))
}

/// Returns the first `my`/`state` redeclaration of a referenced outer symbol or
/// self-referencing initializer, or `None` if there is no such offense.
///
/// Also marks each declaration whose initializer reads the new binding (see
/// [`Initializing::sees_self`]) with the internal `__init_sees_self` trait, so
/// the compiler resolves those reads to it.
// Cost: O(n * d), n = size of the program's tree, d = lexical scope depth.
pub(crate) fn find_scope_diagnostic(stmts: &mut [Stmt]) -> Option<ScopeDiagnostic> {
    let mut ctx = Ctx {
        scopes: vec![Scope::new()],
        line: 0,
        initializing: Vec::new(),
        found: None,
    };
    walk_list(stmts, &mut ctx);
    ctx.found
}

/// The keys a routine's or block's parameters declare in its scope.
fn param_keys(params: &[String], param_defs: &[ParamDef]) -> Vec<String> {
    let names = params.iter().chain(param_defs.iter().map(|d| &d.name));
    names.filter_map(|n| decl_key(n)).collect()
}

/// Walks a statement list in order, so each declaration is registered before
/// the statements after it are examined.
fn walk_list(stmts: &mut [Stmt], ctx: &mut Ctx) {
    for i in 0..stmts.len() {
        // `my \x = ...` lowers to a `VarDecl` of `x` followed by a sigilless
        // marker. The sigilless `x` is a different symbol from `$x`, so the
        // initializer may freely read `$x`.
        let sigilless = match (&stmts[i], stmts.get(i + 1)) {
            (
                Stmt::VarDecl { name, .. },
                Some(Stmt::MarkSigilless(n) | Stmt::MarkSigillessReadonly(n)),
            ) => n == name,
            _ => false,
        };
        if sigilless {
            walk_var_decl(&mut stmts[i], false, ctx);
        } else {
            ctx.visit_stmt_mut(&mut stmts[i]);
        }
    }
}

/// The key a dynamic declaration (`*X`, `@*X`) is read back under.
fn dynamic_key(sigil: char, name: &str) -> Option<String> {
    name.starts_with('*').then(|| format!("{sigil}{name}"))
}

/// Split a declared name into its sigil and the rest (`"x"` is a `$`).
fn split_sigil(name: &str) -> (char, &str) {
    match name.chars().next() {
        Some(c @ ('@' | '%' | '$')) => (c, &name[1..]),
        _ => ('$', name),
    }
}

/// Walk a declaration; `check_self` arms the self-initializer check.
fn walk_var_decl(stmt: &mut Stmt, check_self: bool, ctx: &mut Ctx) {
    let Stmt::VarDecl {
        name,
        expr,
        type_constraint: _,
        is_state: _,
        is_our,
        is_dynamic: _,
        is_export: _,
        export_tags: _,
        custom_traits,
        where_constraint,
    } = stmt
    else {
        return;
    };
    // A `where` clause is parsed before the variable is introduced (rakudo
    // reports `my $x where $x` as an undeclared `$x`), so it reads the
    // enclosing bindings.
    if let Some(e) = where_constraint {
        ctx.visit_expr_mut(e);
    }
    let key = decl_key(name);
    if let Some(key) = &key {
        if *is_our {
            // `our` is package-scoped and not subject to the outer-binding
            // rule; still record it so later references resolve locally.
            ctx.seed(key.clone());
        } else {
            ctx.declare(key.clone());
        }
    }
    // The declared name is in scope for its own initializer, so walk the RHS
    // *after* declaring (a self-reference is then local, not outer) -- and a
    // direct self-reference there is `X::Syntax::Variable::Initializer`.
    let (sigil, bare) = split_sigil(name);
    let check_self = check_self
        && !custom_traits
            .iter()
            .any(|(t, _)| t == METHOD_ASSIGN_DECL_TRAIT);
    let armed_key = if check_self {
        key.or_else(|| dynamic_key(sigil, bare))
    } else {
        None
    };
    let armed = armed_key.is_some();
    if let Some(key) = armed_key {
        ctx.initializing.push(Initializing {
            key,
            depth: ctx.scopes.len(),
            sees_self: false,
        });
    }
    ctx.visit_expr_mut(expr);
    // `is default(...)` and other trait arguments are part of the declaration.
    for arg in custom_traits.iter_mut().filter_map(|(_, arg)| arg.as_mut()) {
        ctx.visit_expr_mut(arg);
    }
    if armed
        && let Some(init) = ctx.initializing.pop()
        && init.sees_self
    {
        custom_traits.push(("__init_sees_self".to_string(), None));
    }
}
