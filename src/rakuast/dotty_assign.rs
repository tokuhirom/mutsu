//! The `.=` (dotty assignment) expansion: `X .= meth` becomes the assignment of
//! `X.meth` back to `X`, evaluated once, for each shape of lvalue (a simple
//! variable, an element, a chained `.=`, an inline declaration, the topic).
//! One implementation for the parser and for `rakuast::lower`; the written
//! `.=` marker is `compound::expansion::dotty_assign_marker`.

use crate::ast::{Expr, LvaluePeel, LvalueRoot, Stmt};
use crate::symbol::Symbol;

/// The writeback variable name (`AssignExpr`-convention: `x` / `@a` / `%h`) for an
/// expression whose value is a simple-variable lvalue, or `None` otherwise. Used to
/// route an outer `.=` through a `do { … }`-block target back to its lvalue.
// Cost: O(w + |name|), w = wrappers peeled.
fn lvalue_assign_name(e: &Expr) -> Option<String> {
    // A compound-assignment marker (`$x .= m`, `$x += 1`) is transparent: its
    // expansion carries the lvalue the chain must write back through.
    e.lvalue_root(LvaluePeel::GROUPED | LvaluePeel::ASSIGN)
        .map(LvalueRoot::into_spelled_key)
}

/// `EXPR .= meth` where `EXPR` is itself an assignment writing to `assign_name`
/// (`($x = 5).=uc`, or the left half of a chained `$s .= uc .= flip`): run the
/// inner assignment, then read the variable back and assign the method result
/// to it, so the chain keeps mutating the same lvalue.
fn chained_assign_writeback(
    target: Expr,
    assign_name: String,
    method_call_fn: &impl Fn(Expr) -> Expr,
) -> Expr {
    let var_expr = if let Some(rest) = assign_name.strip_prefix('@') {
        Expr::ArrayVar(rest.to_string())
    } else if let Some(rest) = assign_name.strip_prefix('%') {
        Expr::HashVar(rest.to_string())
    } else {
        Expr::Var(assign_name.clone())
    };
    let method_result = method_call_fn(var_expr);
    Expr::desugar_block(vec![
        Stmt::Expr(target),
        Stmt::Expr(dot_assign_to_name(assign_name, method_result)),
    ])
}

/// The `.=` expansion for a simple-variable lvalue, tagged with its
/// read-modify-write origin.
///
/// `$x .= meth` runs as the ordinary assignment `$x = $x.meth`, but it is *not*
/// one: raku evaluates the lvalue once as a container and stores into it. The
/// distinction is invisible in the expansion and decides whether
/// `$.attr .= meth` lives (writing the itemized throwaway a non-`rw` scalar
/// accessor returns) or dies like the hand-written `$.attr = $.attr.meth`, so
/// every `.=` lowering keeps the marker. See
/// [`super::compound::expansion::dotty_assign_marker`].
pub(crate) fn dot_assign_to_name(name: String, method_call: Expr) -> Expr {
    let target = dot_assign_target(&name);
    let expanded = Expr::AssignExpr {
        name,
        expr: Box::new(method_call.clone()),
        is_bind: false,
    };
    super::compound::expansion::dotty_assign_marker(target, method_call, expanded)
}

/// `call`, a `.=` method call written over one invocant, applied to
/// `invocant` instead.
// Cost: O(n), n = size of the call.
pub(crate) fn retarget_dotty_call(call: &Expr, invocant: Expr) -> Expr {
    let mut call = call.clone();
    if let Expr::MethodCall { target, .. }
    | Expr::DynamicMethodCall { target, .. }
    | Expr::HyperMethodCall { target, .. } = &mut call
    {
        **target = invocant;
    }
    call
}

/// The variable an `AssignExpr`-convention name (`x`, `@a`, `%h`, a term key)
/// spells as an expression.
// Cost: O(|name|).
pub(crate) fn dot_assign_target(name: &str) -> Expr {
    if let Some(rest) = name.strip_prefix('@') {
        Expr::ArrayVar(rest.to_string())
    } else if let Some(rest) = name.strip_prefix('%') {
        Expr::HashVar(rest.to_string())
    } else if let Some(term) = crate::term_names::term_spelling(name) {
        Expr::BareWord(term.to_string())
    } else {
        Expr::Var(name.to_string())
    }
}

/// Wrap a `.=` method call result in the appropriate assignment expression:
/// [`dot_assign_to_name`]'s marked `AssignExpr` for a simple variable, an
/// `IndexAssign` in a block for an element, the method call as-is for a
/// non-lvalue target. Every shape carries the source-level `.=` marker
/// ([`super::compound::expansion::dotty_assign_marker`]). `is_term` is the
/// caller's notion of which barewords name a sigilless term: the parser asks
/// its scopes, the RakuAST lowering asks the declarations its unit carries.
// Cost: O(n), n = size of the target and the method call (both are cloned).
pub(crate) fn wrap_dot_assign_with_terms(
    target: Expr,
    method_call_fn: impl Fn(Expr) -> Expr,
    is_term: fn(&str) -> bool,
) -> Expr {
    let source = target.clone();
    let method_call = method_call_fn(source.clone());
    let expanded = expand_dot_assign(target, &method_call_fn, is_term);
    // The simple-variable expansion already carries the marker.
    if super::compound::expansion::is_dotty_assign(&expanded) {
        return expanded;
    }
    super::compound::expansion::dotty_assign_marker(source, method_call, expanded)
}

fn expand_dot_assign(
    target: Expr,
    method_call_fn: &impl Fn(Expr) -> Expr,
    is_term: fn(&str) -> bool,
) -> Expr {
    // Unwrap Grouped and identity `.self` calls to get at the lvalue. `.self`
    // returns its invocant unchanged, so `(X.self).=meth` is exactly `X.=meth`
    // (the assignment writes back through `X`).
    let mut target = target;
    let target = loop {
        let is_self_call = matches!(
            &target,
            Expr::MethodCall { name, args, modifier: None, .. }
                if name.resolve() == "self" && args.is_empty()
        );
        target = match target {
            Expr::Grouped(inner) => *inner,
            Expr::MethodCall { target: inner, .. } if is_self_call => *inner,
            other => break other,
        };
    };
    // A previous `.=` (or other compound) step whose expansion is not a simple
    // variable write: continue from the expansion, which is what the chain
    // keeps mutating (`@a[i] .= m1 .= m2`).
    let target = match target {
        Expr::CompoundAssign { expanded, .. } if lvalue_assign_name(&expanded).is_none() => {
            *expanded
        }
        other => other,
    };
    match &target {
        // NOTE: an *expression*-position `$_ .= meth` keeps the plain `AssignExpr`
        // form (so a chain-retarget can redirect the implied `$_`). The whole-
        // container topic write-through (`given @a { $_ .= uc }`) is a *statement*
        // and is handled via `Expr::TopicDotAssign` in the statement
        // assign parser instead.
        Expr::Var(name) => {
            let name = name.clone();
            dot_assign_to_name(name, method_call_fn(target))
        }
        Expr::ArrayVar(name) => {
            let name = format!("@{}", name);
            dot_assign_to_name(name, method_call_fn(target))
        }
        Expr::HashVar(name) => {
            let name = format!("%{}", name);
            dot_assign_to_name(name, method_call_fn(target))
        }
        // A sigilless term (`my \foo`): its assignments are named by its
        // term key, the same one `foo += 1` writes back to (#9551,
        // ADR-12441).
        Expr::BareWord(name) if is_term(name) => {
            let name = crate::term_names::bareword_target_key(name);
            dot_assign_to_name(name, method_call_fn(target))
        }
        Expr::Index {
            target: idx_target,
            index,
            is_positional,
            spelling,
            ..
        } => {
            use std::sync::atomic::Ordering;
            let tmp_idx = format!(
                "__mutsu_idx_{}",
                super::compound::TMP_INDEX_COUNTER.fetch_add(1, Ordering::Relaxed)
            );
            let tmp_idx_expr = Expr::Var(tmp_idx.clone());
            let lhs_expr = Expr::Index {
                target: idx_target.clone(),
                index: Box::new(tmp_idx_expr.clone()),
                is_positional: *is_positional,
                spelling: Default::default(),
            };
            let assigned_value = method_call_fn(lhs_expr);
            Expr::desugar_block(vec![
                Stmt::VarDecl {
                    marks: Default::default(),
                    anonymous: false,
                    name: tmp_idx,
                    expr: *index.clone(),
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
                },
                Stmt::Expr(Expr::IndexAssign {
                    target: idx_target.clone(),
                    index: Box::new(tmp_idx_expr),
                    value: Box::new(assigned_value),
                    is_positional: *is_positional,
                    spelling: *spelling,
                }),
            ])
        }
        // ($var = expr).=method => evaluate the assignment, then $var = $var.method
        Expr::AssignExpr { name, .. } => {
            let assign_name = name.clone();
            chained_assign_writeback(target, assign_name, method_call_fn)
        }
        // The same, for a compound-assignment marker on the left: a chained
        // `.=` (`$s .= uc .= flip`) now arrives as the previous step's `.=`
        // marker, and `($x += 2).=uc` as a `+=` one.
        Expr::CompoundAssign { expanded, .. } if lvalue_assign_name(expanded).is_some() => {
            let assign_name = lvalue_assign_name(expanded).unwrap();
            chained_assign_writeback(target, assign_name, method_call_fn)
        }
        // An inline declaration target (`(my Int $x .= new).= new: 42`) parses to
        // `DoStmt(VarDecl)`. Run the declaration (which declares and initializes the
        // variable), then assign the method result back to the just-declared
        // variable so the outer `.=` writes through, mirroring the AssignExpr case.
        Expr::DoStmt(stmt) if matches!(stmt.as_ref(), Stmt::VarDecl { .. }) => {
            let decl_name = match stmt.as_ref() {
                Stmt::VarDecl { name, .. } => name.clone(),
                _ => unreachable!(),
            };
            chained_assign_writeback(target, decl_name, method_call_fn)
        }
        // A chained `.=` on an index lvalue (`@a[i] .= m1 .= m2`). The first `.=`
        // already lowered `@a[i] .= m1` to `do { my idx = i; @a[idx] = @a[idx].m1 }`
        // (an `IndexAssign` tail), which `lvalue_assign_name` does not recognize.
        // Re-read `@a[idx]` (now holding m1's result, via the already-declared `idx`
        // temp), apply this method, and write it back to the same element so the
        // chain keeps mutating `@a[i]` rather than a detached copy.
        Expr::DoBlock { body, origin, .. }
            if matches!(body.last(), Some(Stmt::Expr(Expr::IndexAssign { .. }))) =>
        {
            let (idx_target, index, is_positional) = match body.last() {
                Some(Stmt::Expr(Expr::IndexAssign {
                    target,
                    index,
                    is_positional,
                    ..
                })) => (target.clone(), index.clone(), *is_positional),
                _ => unreachable!(),
            };
            let read_expr = Expr::Index {
                target: idx_target.clone(),
                index: index.clone(),
                is_positional,
                spelling: Default::default(),
            };
            let new_value = method_call_fn(read_expr);
            let mut new_body = body.clone();
            new_body.push(Stmt::Expr(Expr::IndexAssign {
                target: idx_target,
                index,
                value: Box::new(new_value),
                is_positional,
                spelling: Default::default(),
            }));
            // A rewrap of the node that came in: appending the writeback does
            // not change whether it was a block, so carry its origin over.
            Expr::DoBlock {
                body: new_body,
                label: None,
                origin: *origin,
            }
        }
        // A `do { … }` block whose value is an lvalue (`do { …; ($x .= new) }.= new`)
        // writes the outer `.=` result back to that lvalue. Run the block once (its
        // side effects included), then assign `$x = $x.method`.
        Expr::DoBlock { body, .. }
            if let Some(Stmt::Expr(e)) =
                crate::ast::last_value_stmt(body, crate::ast::TailSkip::Markers)
                && let Some(assign_name) = lvalue_assign_name(e) =>
        {
            chained_assign_writeback(target, assign_name, method_call_fn)
        }
        // A parenthesized list of lvalues (`($x, $y) .= reverse`) applies the
        // method to the WHOLE list and assigns the result back element-wise
        // (`($x, $y) = ($x, $y).reverse`), not per-element.
        Expr::ArrayLiteral(_) => {
            let lhs = target.clone();
            let value = method_call_fn(target);
            crate::rakuast::callable_lvalue_assign_expr(lhs, Vec::new(), value)
        }
        // A method-call lvalue (`$obj.attr .= meth`). In Raku `$obj.attr .= meth`
        // is `$obj.attr = $obj.attr.meth`, so when `.attr` is an `is rw` accessor
        // the result is written back through the accessor (the attribute slot), not
        // snapshotted. We restrict this to a no-modifier, no-argument method call —
        // the common accessor shape — and keep the runtime `STORE` check so a Proxy
        // accessor (`$obj.proxy-attr`) still routes through `.STORE`. Evaluate the
        // invocant once, read the current value once, then branch at runtime:
        //   do { my $inv = INVOCANT;
        //        my $cur = $inv.attr;
        //        __mutsu_custom_store($cur)
        //            ?? do { $cur.STORE($cur.meth); $cur }
        //            !! ($inv.attr = $cur.meth) }
        // For a non-lvalue invocant (`Foo.new.=meth`) the writeback path raises
        // X::Assignment::RO at runtime, matching Raku's "Cannot modify an immutable
        // value".
        Expr::MethodCall {
            target: inv,
            name: acc_name,
            args: acc_args,
            modifier: None,
            quoted,
            ..
        } if acc_args.is_empty() => {
            use std::sync::atomic::Ordering;
            let n = super::compound::TMP_INDEX_COUNTER.fetch_add(1, Ordering::Relaxed);
            let inv_tmp = format!("__mutsu_dotassign_inv_{}", n);
            let cur_tmp = format!("__mutsu_dotassign_cur_{}", n);
            let inv_var = Expr::Var(inv_tmp.clone());
            let cur_var = Expr::Var(cur_tmp.clone());
            let acc_name = *acc_name;
            let quoted = *quoted;
            // Read the current accessor value (evaluated once into `$cur`).
            let read_expr = Expr::MethodCall {
                target: Box::new(inv_var.clone()),
                name: acc_name,
                args: Vec::new(),
                modifier: None,
                quoted,
                sugar: false,
            };
            // `$cur.meth(margs)` — the mutating method applied to the current value.
            let meth_result = method_call_fn(cur_var.clone());
            // STORE branch: container accessor (Proxy) updates in place.
            let store_call = Expr::MethodCall {
                target: Box::new(cur_var.clone()),
                name: Symbol::intern("STORE"),
                args: vec![meth_result.clone()],
                modifier: None,
                quoted: false,
                sugar: false,
            };
            let then_expr =
                Expr::desugar_block(vec![Stmt::Expr(store_call), Stmt::Expr(cur_var.clone())]);
            // Writeback branch: `$inv.attr = $cur.meth` through the accessor slot.
            let writeback = super::method_lvalue_assign_expr(
                inv_var,
                Some(inv_tmp.clone()),
                acc_name.resolve(),
                Vec::new(),
                meth_result,
            );
            let can_store = custom_store_check(cur_var);
            let ternary = Expr::Ternary {
                cond: Box::new(can_store),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(writeback),
            };
            Expr::desugar_block(vec![
                Stmt::VarDecl {
                    marks: Default::default(),
                    anonymous: false,
                    name: inv_tmp,
                    expr: *inv.clone(),
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
                },
                Stmt::VarDecl {
                    marks: Default::default(),
                    anonymous: false,
                    name: cur_tmp,
                    expr: read_expr,
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
                },
                Stmt::Expr(ternary),
            ])
        }
        // A non-lvalue target (`(Foo.new).=meth`, `Foo.new.=meth`, ...). In Raku
        // `X.=meth` is `X = X.meth`; when X (evaluated once) provides a `STORE`
        // method it is a custom container, so this becomes `X.STORE(X.meth)` and
        // the expression value is the container X itself (not STORE's return). An
        // object without STORE keeps mutsu's historical plain-method-call value
        // (Raku errors with "Cannot modify an immutable ..." there, but that path
        // is untested and the lax form is relied on elsewhere). Bind the target to
        // a temp so it is evaluated exactly once, then branch at runtime on
        // `__mutsu_custom_store` (a user `STORE`, not a built-in one):
        //   do { my $t = TARGET;
        //        __mutsu_custom_store($t) ?? do { $t.STORE($t.meth); $t } !! $t.meth }
        _ => {
            use std::sync::atomic::Ordering;
            let tmp = format!(
                "__mutsu_dotassign_{}",
                super::compound::TMP_INDEX_COUNTER.fetch_add(1, Ordering::Relaxed)
            );
            let tmp_var = Expr::Var(tmp.clone());
            // `method_call_fn` is `FnOnce`; call it once against the temp and clone
            // the resulting `$t.meth(args)` for both the STORE argument and the
            // no-STORE fallback (both branches share the same target `$t`).
            let meth_result = method_call_fn(tmp_var.clone());
            let can_store = custom_store_check(tmp_var.clone());
            let store_call = Expr::MethodCall {
                target: Box::new(tmp_var.clone()),
                name: Symbol::intern("STORE"),
                args: vec![meth_result.clone()],
                modifier: None,
                quoted: false,
                sugar: false,
            };
            let then_expr = Expr::desugar_block(vec![Stmt::Expr(store_call), Stmt::Expr(tmp_var)]);
            let ternary = Expr::Ternary {
                cond: Box::new(can_store),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(meth_result),
            };
            Expr::desugar_block(vec![
                Stmt::VarDecl {
                    marks: Default::default(),
                    anonymous: false,
                    name: tmp,
                    expr: target,
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
                },
                Stmt::Expr(ternary),
            ])
        }
    }
}

/// The statement `.=` on the topic (`$_ .= meth`, `.= meth`), tagged with its
/// source-level shape. It is an [`Expr::TopicDotAssign`]
/// (compiled to `TopicDotAssign`) so it can reassign a read-only
/// whole-container topic (`given @a { .=uc }`, writing through to `@a`) while a
/// plain `$_ = ...` keeps throwing X::Assignment::RO.
pub(crate) fn topic_dot_assign(method_call: Expr) -> Expr {
    let expanded = Expr::TopicDotAssign(Box::new(method_call.clone()));
    super::compound::expansion::dotty_assign_marker(
        Expr::Var("_".to_string()),
        method_call,
        expanded,
    )
}

/// `__mutsu_custom_store(VALUE)`: whether `.=` stores through the value's own
/// `STORE` (a `Proxy`, or a user class or role declaring one) rather than
/// assigning (`runtime/builtins_dot_assign_store.rs`). A built-in container's
/// `STORE` does not count: `self.x .= Hash` on a `$` attribute rebinds it.
fn custom_store_check(value: Expr) -> Expr {
    Expr::Call {
        name: Symbol::intern("__mutsu_custom_store"),
        args: vec![value],
        listop: false,
    }
}
