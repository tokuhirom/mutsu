//! The transcendental math methods: `sin`, `cos`, `tan` and their reciprocal,
//! hyperbolic, inverse and inverse-hyperbolic forms, `exp`, `log`, `log2`,
//! `log10`, `sqrt`, `atan2`, `cis`, `unpolar`, `polar`, `roots` and `expmod`.
//!
//! Rakudo declares each of them on `Int`, `Num`, `Rat` and `Complex` (the
//! numeric owners) and on `Cool`, where the body is `self.Numeric.METHOD`.
//! Every owner's row points at one handler per method. The handler reads its
//! receiver through [`numify`], which is the identity on a number and the
//! `Cool` coercion on a `Str`, `List`, `Array` or `Hash`, so `"0.5".sin`,
//! `[1, 2, 3].sin` and `0.5.sin` reach one implementation. A `Complex`
//! receiver takes the complex formulas; every other number goes through
//! `f64` and answers a `Num`.

use super::complex_math::complex_trig;
use super::numify::numify;
use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

type RealFn = fn(f64) -> f64;
type ComplexFn = fn(f64, f64) -> (f64, f64);

/// A zero-argument row.
macro_rules! pure {
    ($owner:literal, $name:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Pure($handler),
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}

/// A row with arguments. It binds any plain argument (a `Complex` base is
/// one), and its handler declines the ones it has no meaning for.
macro_rules! narrow {
    ($owner:literal, $name:literal, $arity:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: Handler::Narrow($handler),
            flags: RowFlags::ANY_ARGS,
            named: &[],
        }
    };
}

/// The rows every owner of the family has: the 29 unary methods, then the
/// owner's `$extra` rows.
macro_rules! math_rows {
    ($owner:literal; $($extra:expr),* $(,)?) => {
        &[
            pure!($owner, "sin", sin),
            pure!($owner, "cos", cos),
            pure!($owner, "tan", tan),
            pure!($owner, "sec", sec),
            pure!($owner, "cosec", cosec),
            pure!($owner, "cotan", cotan),
            pure!($owner, "sinh", sinh),
            pure!($owner, "cosh", cosh),
            pure!($owner, "tanh", tanh),
            pure!($owner, "sech", sech),
            pure!($owner, "cosech", cosech),
            pure!($owner, "cotanh", cotanh),
            pure!($owner, "asin", asin),
            pure!($owner, "acos", acos),
            pure!($owner, "atan", atan),
            pure!($owner, "asec", asec),
            pure!($owner, "acosec", acosec),
            pure!($owner, "acotan", acotan),
            pure!($owner, "asinh", asinh),
            pure!($owner, "acosh", acosh),
            pure!($owner, "atanh", atanh),
            pure!($owner, "asech", asech),
            pure!($owner, "acosech", acosech),
            pure!($owner, "acotanh", acotanh),
            pure!($owner, "exp", exp),
            pure!($owner, "log", log),
            pure!($owner, "log2", log2),
            pure!($owner, "log10", log10),
            pure!($owner, "sqrt", sqrt),
            $($extra),*
        ]
    };
}

/// The rows of a real owner (`Int`, `Num`, `Rat`): the family plus `cis`,
/// `atan2` and `unpolar`, which a `Complex` has no `Real`-style form of.
macro_rules! real_math_rows {
    ($owner:literal; $($extra:expr),* $(,)?) => {
        math_rows!($owner;
            pure!($owner, "cis", cis),
            pure!($owner, "atan2", atan2),
            narrow!($owner, "atan2", 1, atan2_1),
            narrow!($owner, "exp", 1, exp_1),
            narrow!($owner, "log", 1, log_1),
            narrow!($owner, "roots", 1, roots),
            narrow!($owner, "unpolar", 1, unpolar),
            $($extra),*
        )
    };
}

pub(super) static INT_ROWS: &[MethodRow] =
    real_math_rows!("Int"; narrow!("Int", "expmod", 2, expmod));
pub(super) static NUM_ROWS: &[MethodRow] = real_math_rows!("Num";);
pub(super) static RAT_ROWS: &[MethodRow] = real_math_rows!("Rat";);
pub(super) static COOL_ROWS: &[MethodRow] = real_math_rows!("Cool";);
pub(super) static COMPLEX_ROWS: &[MethodRow] = math_rows!("Complex";
    pure!("Complex", "cis", cis),
    pure!("Complex", "polar", polar),
    narrow!("Complex", "exp", 1, exp_1),
    narrow!("Complex", "log", 1, log_1),
    narrow!("Complex", "roots", 1, roots),
);

/// What a handler reads its receiver as.
enum Operand {
    Real(f64),
    Complex(f64, f64),
    /// The `Failure` a non-numeric `Str` numifies to.
    Failure(Value),
}

// Cost: O(n) for a Str receiver, n = chars (the parse); O(1) for a number,
// O(d) for a big one, d = limbs (the conversion to f64).
fn operand(target: &Value) -> Operand {
    match numify(target) {
        Ok(number) => match number.view() {
            ValueView::Complex(re, im) => Operand::Complex(re, im),
            _ => Operand::Real(number.to_f64()),
        },
        Err(failure) => Operand::Failure(failure),
    }
}

/// Apply a unary function: `real` to a real receiver, `complex` to a
/// `Complex` one.
// Cost: O(1) once the receiver is numified.
fn apply(target: &Value, real: RealFn, complex: ComplexFn) -> Result<Value, RuntimeError> {
    Ok(match operand(target) {
        Operand::Real(x) => Value::num(real(x)),
        Operand::Complex(re, im) => {
            let (re, im) = complex(re, im);
            Value::complex(re, im)
        }
        Operand::Failure(failure) => failure,
    })
}

/// Handlers for the methods `complex_trig` knows by name.
macro_rules! trig {
    ($($handler:ident: $name:literal => $real:expr;)*) => {
        $(
            // Cost: O(1) (O(n) for a Str receiver, n = chars of the parse).
            fn $handler(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
                apply(target, $real, |re, im| complex_trig($name, re, im))
            }
        )*
    };
}

trig! {
    sin: "sin" => f64::sin;
    cos: "cos" => f64::cos;
    tan: "tan" => f64::tan;
    sec: "sec" => |x| 1.0 / x.cos();
    cosec: "cosec" => |x| 1.0 / x.sin();
    cotan: "cotan" => |x| 1.0 / x.tan();
    sinh: "sinh" => f64::sinh;
    cosh: "cosh" => f64::cosh;
    tanh: "tanh" => f64::tanh;
    sech: "sech" => |x| 1.0 / x.cosh();
    cosech: "cosech" => |x| 1.0 / x.sinh();
    cotanh: "cotanh" => |x| 1.0 / x.tanh();
    asin: "asin" => f64::asin;
    acos: "acos" => f64::acos;
    atan: "atan" => f64::atan;
    asec: "asec" => |x| (1.0 / x).acos();
    acosec: "acosec" => |x| (1.0 / x).asin();
    acotan: "acotan" => |x| (1.0 / x).atan();
    asinh: "asinh" => |x| x.signum() * (x.abs() + (x * x + 1.0).sqrt()).ln();
    acosh: "acosh" => |x| if x < 1.0 { f64::NAN } else { (x + (x * x - 1.0).sqrt()).ln() };
    atanh: "atanh" => crate::builtins::math_prim::atanh;
}

// Cost: O(1) (O(n) for a Str receiver, n = chars of the parse).
fn inverse_hyperbolic_reciprocal(method: &str, target: &Value) -> Result<Value, RuntimeError> {
    Ok(match operand(target) {
        Operand::Real(x) => crate::builtins::math_prim::inverse_hyperbolic_reciprocal(method, x),
        Operand::Complex(re, im) if re == 0.0 && im == 0.0 => {
            crate::builtins::math_prim::reciprocal_divide_by_zero_failure()
        }
        Operand::Complex(re, im) => {
            let (result_re, result_im) = complex_trig(method, re, im);
            Value::complex(result_re, result_im)
        }
        Operand::Failure(failure) => failure,
    })
}

// Cost: O(1) (O(n) for a Str receiver, n = chars of the parse).
fn asech(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    inverse_hyperbolic_reciprocal("asech", target)
}

// Cost: O(1) (O(n) for a Str receiver, n = chars of the parse).
fn acosech(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    inverse_hyperbolic_reciprocal("acosech", target)
}

// Cost: O(1) (O(n) for a Str receiver, n = chars of the parse).
fn acotanh(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    inverse_hyperbolic_reciprocal("acotanh", target)
}

/// `ln z` of a `Complex`: the log of its magnitude and its argument.
fn complex_ln(re: f64, im: f64) -> (f64, f64) {
    ((re * re + im * im).sqrt().ln(), im.atan2(re))
}

// Cost: O(1) (O(n) for a Str receiver, n = chars of the parse).
fn exp(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    apply(target, f64::exp, |re, im| {
        // exp(a+bi) = exp(a) * (cos(b) + i*sin(b))
        let scale = re.exp();
        (scale * im.cos(), scale * im.sin())
    })
}

// Cost: O(1) (O(n) for a Str receiver, n = chars of the parse).
fn log(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    apply(target, f64::ln, complex_ln)
}

// Cost: O(1) (O(n) for a Str receiver, n = chars of the parse).
fn log2(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    apply(target, f64::log2, |re, im| {
        let (mag, arg) = complex_ln(re, im);
        let ln2 = 2.0f64.ln();
        (mag / ln2, arg / ln2)
    })
}

// Cost: O(1) (O(n) for a Str receiver, n = chars of the parse).
fn log10(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    apply(target, crate::builtins::math_prim::log10, |re, im| {
        let (mag, arg) = complex_ln(re, im);
        let ln10 = 10.0f64.ln();
        (mag / ln10, arg / ln10)
    })
}

/// The shared square-root primitive (`arith::sqrt_numeric`, which the routine
/// form `sqrt(...)` calls too); a big `Int` goes through `f64`.
// Cost: O(1) (O(d) for a big Int or rational, d = limbs; O(n) for a Str
// receiver, n = chars of the parse).
pub(crate) fn sqrt(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(match numify(target) {
        Ok(number) => crate::builtins::arith::sqrt_numeric(&number)
            .unwrap_or_else(|| Value::num(number.to_f64().sqrt())),
        Err(failure) => failure,
    })
}

/// A real number's value for the methods that have no `Complex` form.
/// `Err` is the answer when the receiver is not one: the `Failure` of a
/// non-numeric `Str`, or `None` (decline) for a `Complex`.
fn real_of(value: &Value) -> Result<f64, Option<Value>> {
    match operand(value) {
        Operand::Real(x) => Ok(x),
        Operand::Complex(..) => Err(None),
        Operand::Failure(failure) => Err(Some(failure)),
    }
}

/// `Cool.atan2` and `Cool.unpolar` of a `Complex` are not defined: the call
/// fails the way an undeclared method does.
fn no_such_method(method: &str, target: &Value) -> RuntimeError {
    crate::runtime::did_you_mean::method_not_found(method, crate::runtime::value_type_name(target))
}

// Cost: O(1) (O(n) for a Str receiver, n = chars of the parse).
fn atan2(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    match real_of(target) {
        Ok(y) => Ok(Value::num(y.atan2(1.0))),
        Err(Some(failure)) => Ok(failure),
        Err(None) => Err(no_such_method("atan2", target)),
    }
}

// Cost: O(1) (O(n) for a Str receiver or argument, n = chars of the parse).
fn atan2_1(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let y = match real_of(target) {
        Ok(y) => y,
        Err(Some(failure)) => return Some(Ok(failure)),
        Err(None) => return Some(Err(no_such_method("atan2", target))),
    };
    // A `Complex` argument is not a `Real`: the call fails to bind.
    let x = match real_of(&args[0]) {
        Ok(x) => x,
        Err(Some(failure)) => return Some(Ok(failure)),
        Err(None) => return None,
    };
    Some(Ok(Value::num(y.atan2(x))))
}

/// A real or `Complex` number as `(re, im)`, or the `Failure` of a
/// non-numeric `Str`.
fn parts_of(value: &Value) -> Result<(f64, f64), Value> {
    match operand(value) {
        Operand::Real(x) => Ok((x, 0.0)),
        Operand::Complex(re, im) => Ok((re, im)),
        Operand::Failure(failure) => Err(failure),
    }
}

// Cost: O(1) (O(n) for a Str receiver or argument, n = chars of the parse).
fn log_1(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ((xr, xi), (br, bi)) = match (parts_of(target), parts_of(&args[0])) {
        (Ok(x), Ok(b)) => (x, b),
        (Err(failure), _) | (_, Err(failure)) => return Some(Ok(failure)),
    };
    if bi == 0.0 && xi == 0.0 {
        if br.is_finite() && br > 0.0 && br != 1.0 && xr > 0.0 {
            return Some(Ok(Value::num(xr.ln() / br.ln())));
        }
        return Some(Ok(Value::num(f64::NAN)));
    }
    let (ln_x_mag, ln_x_arg) = complex_ln(xr, xi);
    let (ln_b_mag, ln_b_arg) = complex_ln(br, bi);
    let denom = ln_b_mag * ln_b_mag + ln_b_arg * ln_b_arg;
    if denom == 0.0 {
        return Some(Ok(Value::num(f64::NAN)));
    }
    Some(Ok(Value::complex(
        (ln_x_mag * ln_b_mag + ln_x_arg * ln_b_arg) / denom,
        (ln_x_arg * ln_b_mag - ln_x_mag * ln_b_arg) / denom,
    )))
}

/// `$x.exp($base)`, which is `$base ** $x`.
// Cost: O(M(d) log k + n), d = bits in the exact result, k = integer exponent
// magnitude, n = chars parsed from a Str receiver or argument.
fn exp_1(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(crate::builtins::arith::arith_pow(
        args[0].clone(),
        target.clone(),
    )))
}

// Cost: O(1) (O(n) for a Str receiver, n = chars of the parse).
fn cis(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(match operand(target) {
        Operand::Real(x) => Value::complex(x.cos(), x.sin()),
        // cis(a+bi) = e^(i*(a+bi)) = e^(-b) * (cos(a) + i*sin(a))
        Operand::Complex(re, im) => {
            let scale = (-im).exp();
            Value::complex(scale * re.cos(), scale * re.sin())
        }
        Operand::Failure(failure) => failure,
    })
}

/// `Complex.polar`: the magnitude and the angle.
// Cost: O(1).
fn polar(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    match target.view() {
        ValueView::Complex(re, im) => Ok(Value::array(vec![
            Value::num((re * re + im * im).sqrt()),
            Value::num(im.atan2(re)),
        ])),
        _ => Err(RuntimeError::new(
            "Complex.polar: receiver is not a Complex",
        )),
    }
}

/// `$magnitude.unpolar($angle)`: `$magnitude * cis($angle)`.
// Cost: O(1) (O(n) for a Str receiver or argument, n = chars of the parse).
fn unpolar(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let mag = match real_of(target) {
        Ok(mag) => mag,
        Err(Some(failure)) => return Some(Ok(failure)),
        Err(None) => return Some(Err(no_such_method("unpolar", target))),
    };
    // A `Complex` angle is not a `Real`: the call fails to bind.
    let angle = match real_of(&args[0]) {
        Ok(angle) => angle,
        Err(Some(failure)) => return Some(Ok(failure)),
        Err(None) => return None,
    };
    Some(Ok(Value::complex(mag * angle.cos(), mag * angle.sin())))
}

/// `$x.roots($n)`: the `n` complex `n`th roots.
// Cost: O(n), n = the root count.
fn roots(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let number = match numify(target) {
        Ok(number) => number,
        Err(failure) => return Some(Ok(failure)),
    };
    Some(Ok(crate::builtins::methods_narg::compute_roots(
        &number, &args[0],
    )))
}

/// `Int.expmod($exponent, $modulus)`.
// Cost: O(b^2) for b-bit operands (a modular exponentiation).
fn expmod(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(crate::builtins::expmod(target, &args[0], &args[1]))
}
