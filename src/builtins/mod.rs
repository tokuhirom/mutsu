#![allow(clippy::result_large_err)]

mod arith;
mod functions;
mod methods_0arg;
mod methods_narg;
pub(crate) mod rng;
pub(crate) mod unicode;

pub(crate) use arith::{
    arith_add, arith_div, arith_mod, arith_mul, arith_negate, arith_pow, arith_sub,
};
pub(crate) use functions::native_function;
pub(crate) use methods_0arg::native_method_0arg;
pub(crate) use methods_narg::{native_method_1arg, native_method_2arg};
pub(crate) use unicode::unicode_titlecase_first;
