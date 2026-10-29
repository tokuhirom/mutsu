//! The lowering of a sigilless declaration (`my \x = ...`, `:=`, `::=`) to the
//! bind the compiler consumes (ADR-15334).

use crate::ast::var_marks::TermMark;
use crate::ast::{Expr, Stmt};

/// Build the statement for a sigilless bind/declaration (`my \name := expr`,
/// `my \name ::= expr`, or `my \name = expr` — all three bind the sigilless
/// name to `expr` rather than assigning into a container, since a sigilless
/// term has none).
///
/// Mirrors the sigilled `:=` logic in `my_decl_assign::handle_binding`: when
/// the RHS can denote a CONTAINER, the sigilless name becomes an alias of it
/// through the same bound-declaration/`WrapVarRef`/`bind_source` machinery the sigilled
/// case uses (raku: `my $a = 5; my \x := $a; x = 10; say $a` prints `10`).
///
/// The set of such shapes is a variable, an element (`@a[0]`, `%h<k>`, a
/// computed index) and a method call — the last because an `is rw` accessor
/// returns a container and the parser cannot tell it from an ordinary method.
/// Whether the binding is actually writable is therefore settled at RUN time by
/// `OpCode::MarkSigillessBind`: `my \x := $c.v` aliases the attribute, while
/// `my \x := $s.uc` binds a plain `Str` and stays immutable.
///
/// This used to be `matches!(expr, Expr::Var(_))` alone, so every element and
/// accessor bind was marked readonly outright and `x = 9` died with "Cannot
/// modify an immutable Int". A literal or any other computed rvalue keeps the
/// readonly path: it can never denote a container, and routing it through the
/// bind machinery would give it a container cell that a later same-named
/// declaration in a callee can collide with.
///
/// `is_value_term` says whether a bareword names a declared sigilless value
/// term: the parser asks its own scopes, the RakuAST lowering the terms the
/// unit declares (there is no parse scope to ask by then).
///
/// This does NOT depend on whether a type constraint was written — `my Mu
/// \a := $a` and `my \a := $a` behave the same way in raku.
/// The prefix of the name an anonymous `$` bound to a sigilless term gets.
const ANON_SIGILLESS_SRC_PREFIX: &str = "anon-sigilless-src-";

static ANON_SIGILLESS_SRC_COUNTER: std::sync::atomic::AtomicUsize =
    std::sync::atomic::AtomicUsize::new(0);

// Cost: O(1), plus the cost of the container test on the right-hand side.
pub(super) fn build_sigilless_bind_stmt(
    name: String,
    expr: Expr,
    type_constraint: Option<String>,
    is_state: bool,
    is_our: bool,
    is_value_term: fn(&str) -> bool,
) -> Stmt {
    // `my \foo = my $x = 3` / `my \foo = my $ = -3`: the RHS declaration
    // evaluates to its new Scalar container, and `foo` binds THAT (raku: `foo
    // = 5` writes `$x`). Split it into the inner declaration followed by an
    // ordinary bind of `foo` to the declared variable, which takes the
    // container path below. An anonymous `$` is renamed first: every `my $`
    // parses to the same name, so a second one in the scope would otherwise
    // re-declare the container the first alias points at. The fresh name is
    // not a valid Raku identifier (a hyphen followed by a digit), so it cannot
    // collide with a user variable; it must not start with `__` either, since
    // such compiler-internal names do not take the container-alias path.
    if let Expr::DoStmt(inner) = &expr
        && let Stmt::VarDecl {
            name: inner_name,
            anonymous,
            is_our: false,
            is_dynamic: false,
            ..
        } = inner.as_ref()
        && !inner_name.starts_with(['@', '%', '&', '*'])
    {
        let mut inner_decl = inner.as_ref().clone();
        let var_name = if *anonymous {
            let fresh = format!(
                "{ANON_SIGILLESS_SRC_PREFIX}{}",
                ANON_SIGILLESS_SRC_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
            );
            if let Stmt::VarDecl { name, .. } = &mut inner_decl {
                *name = fresh.clone();
            }
            fresh
        } else {
            inner_name.clone()
        };
        let bind = build_sigilless_bind_stmt(
            name,
            Expr::Var(var_name),
            type_constraint,
            is_state,
            is_our,
            is_value_term,
        );
        return Stmt::SyntheticBlock(vec![inner_decl, bind]);
    }
    let binds_a_container = match &expr {
        Expr::Var(_) | Expr::Index { .. } | Expr::MethodCall { .. } => true,
        Expr::Grouped(inner) => matches!(
            inner.as_ref(),
            Expr::Var(_) | Expr::Index { .. } | Expr::MethodCall { .. }
        ),
        Expr::Ternary {
            then_expr,
            else_expr,
            ..
        } => matches!(
            (then_expr.as_ref(), else_expr.as_ref()),
            (
                Expr::Var(_) | Expr::Index { .. } | Expr::MethodCall { .. },
                _
            ) | (
                _,
                Expr::Var(_) | Expr::Index { .. } | Expr::MethodCall { .. }
            )
        ),
        // A SIGILLESS source (`my \y := $a; my \x := y`) denotes whatever `y`
        // was bound to, so the chain must be able to reach the first alias's
        // container. A bareword is only admitted when it is a declared
        // sigilless value term -- an ordinary bareword (a type name, a listop
        // call) keeps the readonly path. The sigiled-target twin
        // (`my $x := y`) already routes this way, through the `__scalar_bind`
        // branch in `compile_stmt`'s `VarDecl` arm.
        Expr::BareWord(n) => is_value_term(n),
        _ => false,
    };
    // A scopeless expansion, as in expression position (`foo(my \x = 1)`).
    Stmt::SyntheticBlock(vec![Stmt::VarDecl {
        marks: crate::ast::var_marks::VarMarks {
            sigilless_decl: true,
            bound: binds_a_container,
            term: if binds_a_container {
                TermMark::Bound
            } else {
                TermMark::Readonly
            },
            ..Default::default()
        },
        anonymous: false,
        name: name.clone(),
        expr,
        type_constraint,
        is_state,
        is_our,
        is_dynamic: false,
        is_export: false,
        export_tags: Vec::new(),
        // A sigilless term always carries a user-written initializer, so an
        // explicit `Nil` RHS (`my \x = Nil`) must not be mistaken for the
        // synthesized default and re-seeded as `Any`.
        custom_traits: Vec::new(),
        has_initializer: true,
        constant: None,
        bind: None,
        where_constraint: None,
    }])
}

