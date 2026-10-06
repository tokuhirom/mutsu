//! `Complex` coerced to a `Real` type: `.Int`, `.UInt`, `.Num`, `.Rat`,
//! `.FatRat` and `.Real`.
//!
//! Rakudo's `Complex.Real` accepts the number when its imaginary part is
//! `≅ 0` under `$*TOLERANCE` and answers the real part; every other coercion
//! goes through it. Reading the dynamic variable needs the interpreter, so the
//! builtin cascade declines a `Complex` receiver for these methods and this
//! handler answers them, once, for all six.

use super::Interpreter;
use super::native_io::fs_errors::failure_of;
use crate::builtins::methods_0arg::{complex_not_real_error, complex_not_real_exception};
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};

impl Interpreter {
    /// `(re + im·i).METHOD(args)` for a `Complex` receiver `target`, `METHOD`
    /// one of `Int`/`UInt`/`Num`/`Rat`/`FatRat`/`Real`; `args` is empty, or the
    /// one epsilon of `.Rat(epsilon)` / `.FatRat(epsilon)`.
    ///
    /// A negligible imaginary part leaves the real part `re` as a `Num`, whose
    /// own method then answers (so `(3.7+1e-20i).Int` is `3.7e0.Int`, a `NaN`
    /// real part fails the way `NaN.Int` does, `(3.14159+0i).Rat(0.01)` is
    /// `3.14159e0.Rat(0.01)`, ...). Otherwise the number is not a `Real`:
    /// `.Real` returns a lazy `Failure`, the others throw `X::Numeric::Real`
    /// naming the type the coercion attempted (`Int` for `.UInt`). (Rakudo's
    /// epsilon forms die there too, with an accidental "Too many positionals"
    /// `X::AdHoc`; the exception named here is the one the zero-argument forms
    /// throw.)
    // Cost: O(d) for the `$*TOLERANCE` lookup (d = caller-stack depth), plus
    // the cost of the real part's own coercion (O(1) for a word-sized result).
    pub(super) fn dispatch_complex_to_real(
        &mut self,
        method: &str,
        re: f64,
        im: f64,
        target: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if self.complex_im_is_negligible(im) {
            let real = Value::num(re);
            let method_sym = Symbol::intern(method);
            let answered = match args {
                [] => crate::builtins::methods_0arg::native_method_0arg(&real, method_sym),
                [epsilon] => crate::builtins::native_method_1arg(&real, method_sym, epsilon),
                _ => None,
            };
            return answered.unwrap_or_else(|| {
                Err(RuntimeError::new(format!(
                    "No such method '{method}' for invocant of type 'Num'"
                )))
            });
        }
        if method == "Real" {
            return Ok(failure_of(complex_not_real_exception(
                re, im, "Real", target,
            )));
        }
        // `.UInt` is `.Int` plus a range check, and Rakudo names `Int` as the
        // type the coercion attempted.
        let attempted = if method == "UInt" { "Int" } else { method };
        Err(complex_not_real_error(re, im, attempted, target))
    }
}
