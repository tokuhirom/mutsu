use super::call_method::{QuotedMethodName, parse_quoted_method_name};
use crate::ast::Expr;
use crate::parser::expr::listop_arg_expr_list_infix;
use crate::parser::helpers::ws;
use crate::parser::parse_result::{PResult, parse_char};
use crate::parser::primary::{colonpair_expr, parse_call_arg_list, primary};
use crate::symbol::Symbol;

pub(crate) fn atomic_var_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Var(name) => Some(name.clone()),
        _ => None,
    }
}

/// `@arr[i]` / `%h{k}` as the target of an atomic update (`@a[0]⚛++`,
/// `++⚛@a[0]`): such an update parses to the atomic routine call
/// (`atomic-fetch-inc(@a[0])`), which the compiler lowers onto the element's
/// atomic cell (#11812).
pub(crate) fn is_atomic_elem_target(expr: &Expr) -> bool {
    matches!(expr, Expr::Index { target, .. }
        if target.container_var_key().is_some_and(|k| k.starts_with(['@', '%'])))
}

/// The atomic update `routine(expr)` for an element target, else `None`.
pub(crate) fn atomic_elem_update(routine: &str, expr: &Expr) -> Option<Expr> {
    is_atomic_elem_target(expr).then(|| Expr::Call {
        name: Symbol::intern(routine),
        args: vec![expr.clone()],
        listop: false,
    })
}

/// An integer-atomic operator applied to a variable or an array/hash element:
/// the call of Rakudo's own routine for it (`postfix:<⚛++>($x)`,
/// `prefix:<--⚛>(@a[0])`), else `None`.
///
/// The operator keeps its real routine name, so a refused target is reported
/// under the spelling the program used, and the AST stays an ordinary call that
/// round-trips through RakuAST (an internal `__mutsu_*` marker would not,
/// #11834). The compiler lowers it onto the one atomic implementation.
pub(crate) fn atomic_operator_call(operator: &str, expr: &Expr) -> Option<Expr> {
    (atomic_var_name(expr).is_some() || is_atomic_elem_target(expr)).then(|| Expr::Call {
        name: Symbol::intern(operator),
        args: vec![expr.clone()],
        listop: false,
    })
}

/// Parse `.=` mutating method call after the `=` has been identified.
/// `input` starts after the `=` character. `expr` is the target expression.
/// Kept as a separate function to avoid bloating the postfix loop's stack frame.
#[inline(never)]
/// Consume colonpair adverbs trailing a `.= method(...)` argument list and
/// append them to `args`: `%h<k> .= subst('a', 'b'):g` passes `:g` to
/// `.subst`, exactly as the postfix loop binds `$s.subst('a', 'b'):g`.
pub(crate) fn parse_trailing_call_adverbs<'a>(
    mut rest: &'a str,
    args: &mut Vec<Expr>,
) -> PResult<'a, ()> {
    loop {
        let (r_adv, _) = ws(rest)?;
        if r_adv.starts_with(':')
            && !r_adv.starts_with("::")
            && let Ok((r_next, adverb)) = crate::parser::primary::colonpair_expr(r_adv)
        {
            args.push(adverb);
            rest = r_next;
            continue;
        }
        return Ok((rest, ()));
    }
}

pub(crate) fn parse_dot_assign<'a>(input: &'a str, expr: Expr) -> PResult<'a, Expr> {
    let (r, _) = ws(input)?;
    // Parse quoted method name
    if let Some((r_after, qname)) = parse_quoted_method_name(r) {
        let (r_after, _) = ws(r_after)?;
        let (r_final, args) = if r_after.starts_with('(') {
            let (r2, _) = parse_char(r_after, '(')?;
            let (r2, _) = ws(r2)?;
            let (r2, a) = parse_call_arg_list(r2)?;
            let (r2, _) = ws(r2)?;
            let (r2, _) = parse_char(r2, ')')?;
            (r2, a)
        } else {
            (r_after, vec![])
        };
        let result = match qname {
            QuotedMethodName::Static(mname) => {
                let mname_sym = Symbol::intern(&mname);
                written_dotty(expr, |target| Expr::MethodCall {
                    target: Box::new(target),
                    name: mname_sym,
                    args: args.clone(),
                    modifier: None,
                    quoted: true,
                    sugar: false,
                })
            }
            QuotedMethodName::Dynamic(name_expr) => {
                written_dotty(expr, |target| Expr::DynamicMethodCall {
                    target: Box::new(target),
                    name_expr: Box::new(name_expr.clone()),
                    args: args.clone(),
                    modifier: None,
                    quoted: true,
                })
            }
        };
        return Ok((r_final, result));
    }
    // `.= &sub` / `.= $meth` — mutating method-call-via-sub or dynamic method
    // name (`$x .= &f` means `$x = $x.&f`). The name is a sigiled expression.
    if r.starts_with('&') || r.starts_with('$') || r.starts_with('@') || r.starts_with('%') {
        let (r_name, name_expr) = primary(r)?;
        let (r_after, _) = ws(r_name)?;
        let (r_final, args) = if r_after.starts_with('(') {
            let (r2, _) = parse_char(r_after, '(')?;
            let (r2, _) = ws(r2)?;
            let (r2, a) = parse_call_arg_list(r2)?;
            let (r2, _) = ws(r2)?;
            let (r2, _) = parse_char(r2, ')')?;
            (r2, a)
        } else {
            (r_name, vec![])
        };
        let result = written_dotty(expr, |target| Expr::DynamicMethodCall {
            target: Box::new(target),
            name_expr: Box::new(name_expr.clone()),
            args: args.clone(),
            modifier: None,
            quoted: false,
        });
        return Ok((r_final, result));
    }
    // Parse regular method name
    let (r, method_name) = crate::parser::primary::var::parse_ident_with_hyphens(r)?;
    let r_before_ws = r;
    let (r, _) = ws(r)?;
    let (r_final, args) = if r.starts_with('(') {
        let (r2, _) = parse_char(r, '(')?;
        let (r2, _) = ws(r2)?;
        let (r2, mut a) = parse_call_arg_list(r2)?;
        let (r2, _) = ws(r2)?;
        let (r2, _) = parse_char(r2, ')')?;
        let (r2, ()) = parse_trailing_call_adverbs(r2, &mut a)?;
        (r2, a)
    } else if r_before_ws.starts_with(':') && !r_before_ws.starts_with("::") {
        // Colon-arg syntax: .=method: arg (no space before colon). The arg list
        // is a comma-list at listop precedence, so each element stops before the
        // loose word-logical ops (`andthen`/`and`/`or`): `$x .=new: 1 andthen $y`
        // is `($x .=new: 1) andthen $y`, not `$x .=new(1 andthen $y)`.
        let r2 = &r[1..];
        let (r2, _) = ws(r2)?;
        // An empty colon-arg list is a plain zero-argument call: `$s .= uc:  ;`
        // means `$s .= uc`. The list is empty whenever the next token cannot
        // start a term (statement terminator, closing bracket, end of input).
        let mut args = Vec::new();
        let r2 = if r2.is_empty() || r2.starts_with([';', '}', ')', ']']) {
            r2
        } else {
            let (r2, first_arg) = listop_arg_expr_list_infix(r2)?;
            args.push(first_arg);
            r2
        };
        let mut r_inner = r2;
        loop {
            let (r3, _) = ws(r_inner)?;
            if r3.starts_with(':')
                && !r3.starts_with("::")
                && let Ok((r4, arg)) = colonpair_expr(r3)
            {
                args.push(arg);
                r_inner = r4;
                continue;
            }
            if !r3.starts_with(',') {
                r_inner = r3;
                break;
            }
            let r3 = &r3[1..];
            let (r3, _) = ws(r3)?;
            if r3.starts_with(';') || r3.starts_with('}') || r3.is_empty() {
                r_inner = r3;
                break;
            }
            let (r3, next) = listop_arg_expr_list_infix(r3)?;
            args.push(next);
            r_inner = r3;
        }
        (
            r_inner,
            crate::parser::primary::lift_list_infix_in_arg_list(args),
        )
    } else if r.starts_with(':') && !r.starts_with("::") {
        // Fake-infix adverb form (space before ':'): .=method :key<val>
        let mut args = Vec::new();
        let mut r_inner = r;
        while r_inner.starts_with(':') && !r_inner.starts_with("::") {
            if let Ok((r2, arg)) = colonpair_expr(r_inner) {
                args.push(arg);
                let (r3, _) = ws(r2)?;
                r_inner = r3;
            } else {
                break;
            }
        }
        (r_inner, args)
    } else {
        (r, vec![])
    };
    let method_sym = Symbol::intern(method_name);
    let result = written_dotty(expr, |target| Expr::MethodCall {
        target: Box::new(target),
        name: method_sym,
        args: args.clone(),
        modifier: None,
        quoted: false,
        sugar: false,
    });
    Ok((r_final, result))
}

/// `EXPR .= call` as the typed `.=` node over the method call `call_fn`
/// builds for `EXPR` (ADR-10723 Stage 2, #14948).
// Cost: O(n), n = size of the target and the call.
fn written_dotty(expr: Expr, call_fn: impl Fn(Expr) -> Expr) -> Expr {
    let call = call_fn(expr.clone());
    crate::parser::stmt::assign::written_dotty_assignment(expr, call)
}
