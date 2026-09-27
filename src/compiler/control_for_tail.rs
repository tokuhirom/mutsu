//! The `for`-source and collected-tail helpers of the `for` lowering in
//! `control_for.rs`, split out to keep that file's size in check.

use super::*;

impl Compiler {
    /// Whether a `for` source could be an attribute-accessor read — a no-arg
    /// method call, by literal (`$obj.attr`) or dynamic (`$obj."$name"()`)
    /// name — and so can answer an accessor-ref marker with the attribute's
    /// container.
    pub(super) fn is_accessor_shaped_source(iterable: &Expr) -> bool {
        Self::is_accessor_shaped_arg(iterable) && matches!(iterable, Expr::MethodCall { .. })
            || matches!(
                iterable,
                Expr::DynamicMethodCall { args, modifier: None, .. } if args.is_empty()
            )
    }

    /// The variable whose *container* a value-collecting `for` body hands back,
    /// when the body's tail statement is a bare read of one.
    ///
    /// A Raku block's value is not decontainerized, so a collecting `for` whose
    /// body ends in `$g` gathers the `Scalar` container `$g` denotes and every
    /// collected slot reads it at the point the list is consumed — after the
    /// loop, so after each iteration's `temp` restore. `do for 1..2 { temp $g =
    /// 9; $g }` is therefore `(1 1)` and not `(9 9)`, and `do for 1..2 { $g =
    /// $g + 1; $g }` is `(3 3)` and not `(2 3)`. Decontainerizing the tail
    /// (`$g + 0`) opts back out, because that expression is a value.
    ///
    /// Returning the name here makes the loop tag it with `TagContainerRef`,
    /// which is the same signal `compile_expr_assign` already emits for a tail
    /// *assignment* (`do for 1..3 { $s += $_ }` → `(6 6 6)`); the VM re-reads
    /// every tagged slot once the loop is over.
    ///
    /// Only a container that outlives the iteration qualifies:
    ///
    /// - the loop's own parameters and the topic are rebound per iteration, so
    ///   `do for 1..3 -> $i { $i }` must stay `(1 2 3)`;
    /// - so is a `my` declared anywhere in the body, hence
    ///   `do for 1..3 { my $x = $_ * 2; $x }` is `(2 4 6)`. A `state`
    ///   declaration is the exception — its storage is one cell for the whole
    ///   loop, and raku collects it as one (`(6 6 6)`, not `(1 3 6)`);
    /// - a twigil'd or punctuation name (`$*d`, `$!a`, `$^a`, `$/`) is left
    ///   alone: those do not resolve through the plain env lookup the VM's
    ///   re-read uses, and nothing here has measured them.
    pub(super) fn collected_tail_container_name(
        body: &[Stmt],
        param: &Option<String>,
        params: &[String],
    ) -> Option<String> {
        let Some(Stmt::Expr(Expr::Var(name))) = body.last() else {
            return None;
        };
        if name == "_"
            || !name.starts_with(|c: char| c.is_ascii_alphabetic() || c == '_')
            || param.as_deref() == Some(name.as_str())
            || params
                .iter()
                .any(|p| p.strip_prefix('\\').unwrap_or(p) == name)
        {
            return None;
        }
        let mut body_declared = std::collections::HashSet::new();
        crate::ast::collect_all_my_decl_names(body, &mut body_declared);
        let declared_as_state = body.iter().any(|s| {
            matches!(
                s,
                Stmt::VarDecl {
                    name: declared,
                    is_state: true,
                    ..
                } if declared == name
            )
        });
        if body_declared.contains(name.as_str()) && !declared_as_state {
            return None;
        }
        Some(name.clone())
    }
}
