//! The write-through calls the front ends lower an assignment to (ADR-17198).
//!
//! A write through a method call, a named routine or a callable is carried as a
//! call whose callee is one of these names, and the compiler, the VM and the
//! runtime builtins recognise it by that name. The names are executable IR, so
//! they are defined once, here, and every producer and consumer spells them
//! through these constants (`make check-frontend-debt` counts a quoted
//! spelling outside this file).
//!
//! The argument layouts, which the consumers rely on:
//!
//! | callee | arguments |
//! | --- | --- |
//! | [`ASSIGN_METHOD`] | invocant, method name, method arguments, value, target variable or Nil, preserve-hash-entries flag, dot-twigil flag (the last three optional) |
//! | [`ASSIGN_NAMED_SUB`] | routine name, call arguments, value |
//! | [`ASSIGN_CALLABLE`] | callable, call arguments, value |
//! | [`INCDEC_NAMED_SUB`] | routine name, call arguments, increment kind |
//! | [`INDEX_ASSIGN_METHOD`] | invocant, method name, method arguments, index, value |
//! | [`INDEX_DELETE_METHOD`] | invocant, method name, method arguments, index |

/// `$obj.attr = $v`, `$obj.method(...) = $v`: the invocant's rw method is
/// called and its container written.
pub(crate) const ASSIGN_METHOD: &str = "__mutsu_assign_method_lvalue";
/// `f(...) = $v`: the rw routine `f` returns the container written.
pub(crate) const ASSIGN_NAMED_SUB: &str = "__mutsu_assign_named_sub_lvalue";
/// `$code(...) = $v`, `(...) = ...` over a literal list of lvalues, a `my`
/// declaration assigned to.
pub(crate) const ASSIGN_CALLABLE: &str = "__mutsu_assign_callable_lvalue";
/// `f(...)++` and `--f(...)`.
pub(crate) const INCDEC_NAMED_SUB: &str = "__mutsu_incdec_named_sub_lvalue";
/// `$obj.method(...)[$i] = $v`.
pub(crate) const INDEX_ASSIGN_METHOD: &str = "__mutsu_index_assign_method_lvalue";
/// `$obj.method(...)[$i]:delete`.
pub(crate) const INDEX_DELETE_METHOD: &str = "__mutsu_index_delete_method_lvalue";

use crate::ast::Expr;
use crate::symbol::Symbol;
use crate::value::Value;

/// The optional trailing arguments of an [`ASSIGN_METHOD`] call (ADR-17198).
#[derive(Clone, Debug, Default)]
pub(crate) struct AssignMethodHints {
    /// The caller variable the result is written back to (`$x.attr = 1` over
    /// `my $x`), or `None` when the invocant is no variable (an element, a
    /// call result).
    pub(crate) target_var: Option<String>,
    /// `%h{...}.method = ...`: keep the hash entries the call returns.
    pub(crate) preserve_hash_entries: bool,
    /// The `$.attr = value` spelling: the accessor is checked for being
    /// writable before it is assigned through.
    pub(crate) dot_twigil: bool,
}

fn call(callee: &str, args: Vec<Expr>) -> Expr {
    Expr::Call {
        name: Symbol::intern(callee),
        args,
        listop: false,
    }
}

/// An [`ASSIGN_METHOD`] call. The layout is `invocant, method, [method
/// arguments], value, target variable (or Nil)`, then the preserve-hash-entries
/// flag (`True`, or `Nil` when only the twigil flag follows), then the
/// dot-twigil flag; a trailing argument that is not set is not emitted.
// Cost: O(n), n = size of the arguments.
pub(crate) fn assign_method(
    invocant: Expr,
    method: Expr,
    method_args: Vec<Expr>,
    value: Expr,
    hints: AssignMethodHints,
) -> Expr {
    let mut args = vec![invocant, method, Expr::ArrayLiteral(method_args), value];
    args.push(match hints.target_var {
        Some(name) => Expr::Literal(Value::str(name)),
        None => Expr::Literal(Value::NIL),
    });
    if hints.preserve_hash_entries || hints.dot_twigil {
        args.push(Expr::Literal(if hints.preserve_hash_entries {
            Value::TRUE
        } else {
            Value::NIL
        }));
    }
    if hints.dot_twigil {
        args.push(Expr::Literal(Value::TRUE));
    }
    call(ASSIGN_METHOD, args)
}

/// An [`ASSIGN_NAMED_SUB`] call: `routine(call arguments) = value`.
// Cost: O(n), n = size of the arguments.
pub(crate) fn assign_named_sub(routine: String, call_args: Vec<Expr>, value: Expr) -> Expr {
    call(
        ASSIGN_NAMED_SUB,
        vec![
            Expr::Literal(Value::str(routine)),
            Expr::ArrayLiteral(call_args),
            value,
        ],
    )
}

/// An [`ASSIGN_CALLABLE`] call: `callable(call arguments) = value`.
// Cost: O(n), n = size of the arguments.
pub(crate) fn assign_callable(callable: Expr, call_args: Vec<Expr>, value: Expr) -> Expr {
    call(
        ASSIGN_CALLABLE,
        vec![callable, Expr::ArrayLiteral(call_args), value],
    )
}

/// An [`INCDEC_NAMED_SUB`] call: `routine(call arguments)` incremented or
/// decremented in place; `op_label` is the operator (`postfix:<++>`, ...).
// Cost: O(n), n = size of the arguments.
pub(crate) fn incdec_named_sub(routine: String, call_args: Vec<Expr>, op_label: &str) -> Expr {
    call(
        INCDEC_NAMED_SUB,
        vec![
            Expr::Literal(Value::str(routine)),
            Expr::ArrayLiteral(call_args),
            Expr::Literal(Value::str_from(op_label)),
        ],
    )
}

/// An [`INDEX_ASSIGN_METHOD`] call: `invocant.method(args)[index] = value`,
/// written back to the caller variable `writeback`. The method arguments are
/// one bracket-array argument, left out when there are none.
// Cost: O(n), n = size of the arguments.
pub(crate) fn index_assign_method(
    invocant: Expr,
    method: &str,
    method_args: &[Expr],
    index: Expr,
    value: Expr,
    writeback: String,
) -> Expr {
    let mut args = vec![invocant, Expr::Literal(Value::str(method.to_string()))];
    if !method_args.is_empty() {
        args.push(Expr::BracketArray(method_args.to_vec(), false));
    }
    args.extend([index, value, Expr::Literal(Value::str(writeback))]);
    call(INDEX_ASSIGN_METHOD, args)
}

/// An [`INDEX_DELETE_METHOD`] call: `invocant.method[index]:delete`, written
/// back to the caller variable `writeback`.
// Cost: O(n), n = size of the arguments.
pub(crate) fn index_delete_method(
    invocant: Expr,
    method: &str,
    index: Expr,
    writeback: String,
) -> Expr {
    call(
        INDEX_DELETE_METHOD,
        vec![
            invocant,
            Expr::Literal(Value::str(method.to_string())),
            index,
            Expr::Literal(Value::str(writeback)),
        ],
    )
}

/// The arguments of an [`ASSIGN_METHOD`] call, named (ADR-17198 step 2b).
#[derive(Clone, Copy, Debug)]
pub(crate) struct AssignMethodArgs<'a> {
    pub(crate) invocant: &'a Expr,
    pub(crate) method: &'a Expr,
    /// The bracketed array of method arguments.
    pub(crate) method_args: &'a Expr,
    pub(crate) value: &'a Expr,
}

/// The arguments of an [`ASSIGN_NAMED_SUB`] or [`INCDEC_NAMED_SUB`] call.
#[derive(Clone, Copy, Debug)]
pub(crate) struct NamedSubArgs<'a> {
    pub(crate) routine: &'a Expr,
    /// The bracketed array of call arguments.
    pub(crate) call_args: &'a Expr,
    /// The assigned value, or the operator label of an increment/decrement.
    pub(crate) last: &'a Expr,
    /// A fourth argument a later compile step appended (the caller variable of
    /// a `substr-rw`-style first argument).
    pub(crate) target_var: Option<&'a Expr>,
}

/// The arguments of an [`ASSIGN_CALLABLE`] call.
#[derive(Clone, Copy, Debug)]
pub(crate) struct CallableArgs<'a> {
    pub(crate) callable: &'a Expr,
    pub(crate) value: &'a Expr,
}

/// A write-through call, classified by its callee with its arguments named.
#[derive(Clone, Copy, Debug)]
pub(crate) enum LvalueCall<'a> {
    AssignMethod(AssignMethodArgs<'a>),
    AssignNamedSub(NamedSubArgs<'a>),
    AssignCallable(CallableArgs<'a>),
    IncDecNamedSub(NamedSubArgs<'a>),
}

/// Read the call `callee(args)` as a write-through call, `None` when the callee
/// is no such name or the argument count does not match the layout in the module
/// docs (the index forms are read by the VM from run-time values, not here).
// Cost: O(1).
pub(crate) fn classify<'a>(callee: &str, args: &'a [Expr]) -> Option<LvalueCall<'a>> {
    match callee {
        ASSIGN_METHOD if args.len() >= 4 => Some(LvalueCall::AssignMethod(AssignMethodArgs {
            invocant: &args[0],
            method: &args[1],
            method_args: &args[2],
            value: &args[3],
        })),
        ASSIGN_NAMED_SUB | INCDEC_NAMED_SUB if args.len() >= 3 => {
            let named = NamedSubArgs {
                routine: &args[0],
                call_args: &args[1],
                last: &args[2],
                target_var: args.get(3),
            };
            Some(if callee == ASSIGN_NAMED_SUB {
                LvalueCall::AssignNamedSub(named)
            } else {
                LvalueCall::IncDecNamedSub(named)
            })
        }
        ASSIGN_CALLABLE if args.len() == 3 => Some(LvalueCall::AssignCallable(CallableArgs {
            callable: &args[0],
            value: &args[2],
        })),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const ALL: [&str; 6] = [
        ASSIGN_METHOD,
        ASSIGN_NAMED_SUB,
        ASSIGN_CALLABLE,
        INCDEC_NAMED_SUB,
        INDEX_ASSIGN_METHOD,
        INDEX_DELETE_METHOD,
    ];

    #[test]
    fn the_spellings_are_distinct_reserved_names() {
        for (i, a) in ALL.iter().enumerate() {
            assert!(a.starts_with("__mutsu_") && a.ends_with("_lvalue"), "{a}");
            for b in &ALL[i + 1..] {
                assert_ne!(a, b);
            }
        }
    }

    #[test]
    fn the_reader_names_the_arguments_each_constructor_wrote() {
        let lit = |text: &str| Expr::Literal(Value::str(text.to_string()));
        let Expr::Call { name, args, .. } = assign_method(
            lit("inv"),
            lit("meth"),
            vec![lit("a")],
            lit("val"),
            AssignMethodHints {
                target_var: Some("x".to_string()),
                ..Default::default()
            },
        ) else {
            panic!("a call");
        };
        let Some(LvalueCall::AssignMethod(view)) = classify(name.resolve().as_str(), &args) else {
            panic!("an assign-method call");
        };
        assert!(matches!(view.invocant, Expr::Literal(v) if v.as_str() == Some("inv")));
        assert!(matches!(view.method, Expr::Literal(v) if v.as_str() == Some("meth")));
        assert!(matches!(view.method_args, Expr::ArrayLiteral(items) if items.len() == 1));
        assert!(matches!(view.value, Expr::Literal(v) if v.as_str() == Some("val")));

        let Expr::Call { name, args, .. } =
            assign_named_sub("f".to_string(), vec![lit("a")], lit("v"))
        else {
            panic!("a call");
        };
        let Some(LvalueCall::AssignNamedSub(view)) = classify(name.resolve().as_str(), &args)
        else {
            panic!("an assign-named-sub call");
        };
        assert!(matches!(view.routine, Expr::Literal(v) if v.as_str() == Some("f")));
        assert!(view.target_var.is_none());

        assert!(
            classify(ASSIGN_CALLABLE, &args[..2]).is_none(),
            "arity is checked"
        );
        assert!(classify("unrelated", &args).is_none());
    }

    #[test]
    fn the_runtime_dispatches_the_three_assign_forms() {
        // The builtin table and the producers meet on these exact strings.
        assert_eq!(ASSIGN_METHOD, "__mutsu_assign_method_lvalue");
        assert_eq!(ASSIGN_NAMED_SUB, "__mutsu_assign_named_sub_lvalue");
        assert_eq!(ASSIGN_CALLABLE, "__mutsu_assign_callable_lvalue");
    }
}
