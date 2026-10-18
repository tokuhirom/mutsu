//! The methods a user class gets from composing `Real` (ADR-11276 §9.179).
//!
//! Rakudo's `Real` role (which does `Numeric`) answers its methods through the
//! class's `Bridge`: `method sqrt() { self.Bridge.sqrt }`, and so on for the
//! conversions, the rounding methods, the transcendental functions and the
//! renderings. `Real.Bridge` itself is `self.Num`, so a class supplies either
//! a `Bridge` or a `Num`. The role's own answers that do not bridge: `Real`,
//! `Numeric` and `conj` are the invocant, `isNaN` is `False`.
//!
//! The rows are composed-role rows ([`super::composed_roles::answer`]): an
//! instance of a user class reaches them after its own methods and accessors,
//! and a handler declines a class that does not compose `Real`. A type object
//! takes the cascades (`R.sqrt` is Rakudo's `X::Multi::NoMatch` for an
//! undefined invocant).

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

const ROLE: RowFlags = RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS);

macro_rules! row {
    ($owner:literal, $name:literal, $arity:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: Handler::Interp($handler),
            flags: ROLE,
            named: &[],
        }
    };
}

/// The rows that answer `self.Bridge.METHOD(...)`, at each arity Rakudo's
/// candidates take.
pub(super) static BRIDGED_ROWS: &[MethodRow] = &[
    row!("Real", "Complex", 0, bridged_complex),
    row!("Real", "Int", 0, bridged_int),
    row!("Real", "Num", 0, bridged_num),
    row!("Real", "Rat", 0, bridged_rat),
    row!("Real", "Rat", 1, bridged_rat),
    row!("Real", "abs", 0, bridged_abs),
    row!("Real", "sign", 0, bridged_sign),
    row!("Real", "sqrt", 0, bridged_sqrt),
    row!("Real", "rand", 0, bridged_rand),
    row!("Real", "cis", 0, bridged_cis),
    row!("Real", "floor", 0, bridged_floor),
    row!("Real", "ceiling", 0, bridged_ceiling),
    row!("Real", "round", 0, bridged_round),
    row!("Real", "round", 1, bridged_round),
    row!("Real", "truncate", 0, bridged_truncate),
    row!("Real", "log", 0, bridged_log),
    row!("Real", "log", 1, bridged_log),
    row!("Real", "exp", 0, bridged_exp),
    row!("Real", "exp", 1, bridged_exp),
    row!("Real", "atan2", 0, bridged_atan2),
    row!("Real", "atan2", 1, bridged_atan2),
    row!("Real", "base", 1, bridged_base),
    row!("Real", "base", 2, bridged_base),
    row!("Real", "polymod", 1, bridged_polymod),
    row!("Real", "unpolar", 1, bridged_unpolar),
    row!("Real", "Str", 0, bridged_str),
    row!("Real", "sin", 0, bridged_sin),
    row!("Real", "cos", 0, bridged_cos),
    row!("Real", "tan", 0, bridged_tan),
    row!("Real", "asin", 0, bridged_asin),
    row!("Real", "acos", 0, bridged_acos),
    row!("Real", "atan", 0, bridged_atan),
    row!("Real", "sec", 0, bridged_sec),
    row!("Real", "cosec", 0, bridged_cosec),
    row!("Real", "cotan", 0, bridged_cotan),
    row!("Real", "asec", 0, bridged_asec),
    row!("Real", "acosec", 0, bridged_acosec),
    row!("Real", "acotan", 0, bridged_acotan),
    row!("Real", "sinh", 0, bridged_sinh),
    row!("Real", "cosh", 0, bridged_cosh),
    row!("Real", "tanh", 0, bridged_tanh),
    row!("Real", "asinh", 0, bridged_asinh),
    row!("Real", "acosh", 0, bridged_acosh),
    row!("Real", "atanh", 0, bridged_atanh),
    row!("Real", "sech", 0, bridged_sech),
    row!("Real", "cosech", 0, bridged_cosech),
    row!("Real", "cotanh", 0, bridged_cotanh),
    row!("Real", "asech", 0, bridged_asech),
    row!("Real", "acosech", 0, bridged_acosech),
    row!("Real", "acotanh", 0, bridged_acotanh),
    row!("Numeric", "FatRat", 0, bridged_fatrat),
    row!("Numeric", "log10", 0, bridged_log10),
    row!("Numeric", "log2", 0, bridged_log2),
    row!("Numeric", "succ", 0, bridged_succ),
    row!("Numeric", "pred", 0, bridged_pred),
    row!("Numeric", "roots", 1, bridged_roots),
    row!("Numeric", "Bool", 0, bridged_bool),
];

/// One handler per bridged name: the guard step hands a handler no name.
macro_rules! bridged_fns {
    ($($fn:ident => $name:literal),* $(,)?) => {$(
        // Cost: as [`bridge_then`].
        fn $fn(interp: &mut Interpreter, target: &Value, args: &[Value], _named: Named<'_>) -> Answer {
            bridge_then(interp, $name, target, args)
        }
    )*};
}

bridged_fns!(
    bridged_complex => "Complex",
    bridged_int => "Int",
    bridged_num => "Num",
    bridged_rat => "Rat",
    bridged_abs => "abs",
    bridged_sign => "sign",
    bridged_sqrt => "sqrt",
    bridged_rand => "rand",
    bridged_cis => "cis",
    bridged_floor => "floor",
    bridged_ceiling => "ceiling",
    bridged_round => "round",
    bridged_truncate => "truncate",
    bridged_log => "log",
    bridged_exp => "exp",
    bridged_atan2 => "atan2",
    bridged_base => "base",
    bridged_polymod => "polymod",
    bridged_unpolar => "unpolar",
    bridged_str => "Str",
    bridged_sin => "sin",
    bridged_cos => "cos",
    bridged_tan => "tan",
    bridged_asin => "asin",
    bridged_acos => "acos",
    bridged_atan => "atan",
    bridged_sec => "sec",
    bridged_cosec => "cosec",
    bridged_cotan => "cotan",
    bridged_asec => "asec",
    bridged_acosec => "acosec",
    bridged_acotan => "acotan",
    bridged_sinh => "sinh",
    bridged_cosh => "cosh",
    bridged_tanh => "tanh",
    bridged_asinh => "asinh",
    bridged_acosh => "acosh",
    bridged_atanh => "atanh",
    bridged_sech => "sech",
    bridged_cosech => "cosech",
    bridged_cotanh => "cotanh",
    bridged_asech => "asech",
    bridged_acosech => "acosech",
    bridged_acotanh => "acotanh",
    bridged_fatrat => "FatRat",
    bridged_log10 => "log10",
    bridged_log2 => "log2",
    bridged_succ => "succ",
    bridged_pred => "pred",
    bridged_roots => "roots",
    bridged_bool => "Bool",
);

pub(super) static OWN_ROWS: &[MethodRow] = &[
    row!("Real", "Bridge", 0, bridge),
    row!("Real", "Real", 0, itself),
    row!("Real", "conj", 0, itself),
    row!("Real", "isNaN", 0, is_nan),
    row!("Numeric", "Numeric", 0, itself),
    row!("Numeric", "gist", 0, gist),
];

/// Whether `target` is an instance of a user class that composes `Real`.
// Cost: one memoized role lookup.
fn composes_real(interp: &mut Interpreter, target: &Value) -> bool {
    match target.view() {
        ValueView::Instance { class_name, .. } => {
            interp.class_does_role(&class_name.resolve(), "Real")
        }
        _ => false,
    }
}

/// The value the role's methods compute with: the class's `Bridge` (or the
/// role's, `self.Num`), and the same of an argument that composes `Real`.
// Cost: one `Bridge` call, plus the user code it runs.
fn bridged(interp: &mut Interpreter, value: &Value) -> Result<Value, RuntimeError> {
    interp.call_method_with_values(value.clone(), "Bridge", Vec::new())
}

/// `self.Bridge.METHOD(...)`, an argument that composes `Real` bridged too.
// Cost: one `Bridge` call per receiver and `Real` argument, plus the bridged
// method's own.
fn bridge_then(interp: &mut Interpreter, method: &str, target: &Value, args: &[Value]) -> Answer {
    if !composes_real(interp, target) {
        return None;
    }
    Some((|| {
        let receiver = bridged(interp, target)?;
        let mut bridged_args = Vec::with_capacity(args.len());
        for arg in args {
            bridged_args.push(if composes_real(interp, arg) {
                bridged(interp, arg)?
            } else {
                arg.clone()
            });
        }
        interp.call_method_with_values(receiver, method, bridged_args)
    })())
}

/// `Real.Bridge`: `self.Num`.
// Cost: one `Num` call, plus the user code it runs.
fn bridge(interp: &mut Interpreter, target: &Value, _args: &[Value], _named: Named<'_>) -> Answer {
    if !composes_real(interp, target) {
        return None;
    }
    Some(interp.call_method_with_values(target.clone(), "Num", Vec::new()))
}

/// `Real.Real`, `Real.conj`, `Numeric.Numeric`: the invocant.
// Cost: O(1).
fn itself(interp: &mut Interpreter, target: &Value, _args: &[Value], _named: Named<'_>) -> Answer {
    composes_real(interp, target).then(|| Ok(target.clone()))
}

/// `Real.isNaN`: a `Real` is never `NaN`.
// Cost: O(1).
fn is_nan(interp: &mut Interpreter, target: &Value, _args: &[Value], _named: Named<'_>) -> Answer {
    composes_real(interp, target).then(|| Ok(Value::FALSE))
}

/// `Numeric.gist`: `self.Str`.
// Cost: one `Str` call, plus the user code it runs.
fn gist(interp: &mut Interpreter, target: &Value, _args: &[Value], _named: Named<'_>) -> Answer {
    if !composes_real(interp, target) {
        return None;
    }
    Some(interp.call_method_with_values(target.clone(), "Str", Vec::new()))
}
