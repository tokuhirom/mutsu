#![allow(clippy::result_large_err)]

mod arith;
mod functions;
mod methods_0arg;
mod methods_narg;
pub(crate) mod rng;
pub(crate) mod unicode;

fn split_lines_impl(input: &str, chomp: bool) -> Vec<String> {
    let bytes = input.as_bytes();
    let mut lines = Vec::new();
    let mut start = 0usize;
    let mut i = 0usize;

    while i < bytes.len() {
        let sep_len = if bytes[i] == b'\n' {
            1
        } else if bytes[i] == b'\r' {
            if i + 1 < bytes.len() && bytes[i + 1] == b'\n' {
                2
            } else {
                1
            }
        } else {
            i += 1;
            continue;
        };

        let end = if chomp { i } else { i + sep_len };
        lines.push(input[start..end].to_string());
        i += sep_len;
        start = i;
    }

    if start < input.len() {
        lines.push(input[start..].to_string());
    }

    lines
}

pub(crate) fn split_lines_chomped(input: &str) -> Vec<String> {
    split_lines_impl(input, true)
}

pub(crate) fn split_lines_with_chomp(input: &str, chomp: bool) -> Vec<String> {
    split_lines_impl(input, chomp)
}

pub(crate) use arith::{
    arith_add, arith_div, arith_mod, arith_mul, arith_negate, arith_pow, arith_sub,
};
pub(crate) use functions::native_function;
pub(crate) use methods_0arg::native_method_0arg;
pub(crate) use methods_narg::{native_method_1arg, native_method_2arg};
pub(crate) use unicode::unicode_titlecase_first;
