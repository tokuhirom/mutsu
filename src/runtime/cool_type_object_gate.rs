//! `Cool`'s string methods on an undefined `Cool` receiver (`Str.comb`,
//! `Int.uc`, `substr(Str, 0, 2)`).
//!
//! Every candidate raku declares for these methods takes a *defined* invocant
//! (`Cool:D`/`Str:D`), so calling one on a `Cool` type object is
//! `X::Multi::NoMatch` ("Cannot resolve caller comb(Str:U: )"). `Str` is the
//! one exception: it declares `:U` candidates for the case-mapping and
//! counting methods, which stringify the type object to `""` with the usual
//! uninitialized-value warning (`Str.uc` is `""`, `Str.chars` is `0`).
//!
//! mutsu's by-name native cascades recognized the method NAME and answered
//! out of the stringified receiver -- and a type object stringifies to its
//! gist, so `Str.comb` produced `("(", "S", "t", "r", ")")` and `Str.chars`
//! answered `5` (#9772). This gate is the `Cool`-subtype sibling of
//! [`super::any_cool_method_gate`], which covers the `Any`/`Mu` type objects
//! (those do not answer `Cool`'s methods at all).

use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

/// String methods whose every raku candidate requires a defined invocant.
/// Verified against rakudo v2026.07 on `Str`, `Int`, `Num`, `Rat`, `Bool`,
/// `Cool`, `List`, `Hash`, `Range` and `Seq` type objects (2026-09-27).
///
/// `split`, `match`, `samecase`, `encode` and `fmt` are deliberately absent:
/// raku answers them differently per type (`Int.split` returns `("",)`,
/// `Str.match` returns `Any`), so they keep their current behaviour.
const DEFINITE_ONLY: &[&str] = &[
    "chars",
    "chomp",
    "chop",
    "chr",
    "chrs",
    "codes",
    "comb",
    "contains",
    "ends-with",
    "fc",
    "flip",
    "index",
    "indices",
    "lc",
    "lines",
    "NFC",
    "NFD",
    "ord",
    "ords",
    "rindex",
    "starts-with",
    "subst",
    "substr",
    "substr-eq",
    "tc",
    "tclc",
    "trans",
    "trim",
    "trim-leading",
    "trim-trailing",
    "uc",
    "uniname",
    "uninames",
    "wordcase",
    "words",
];

/// The subset of [`DEFINITE_ONLY`] that `Str` also declares for `Str:U`: they
/// stringify the type object to `""` (with a warning) and answer from that.
const STR_UNDEFINED_STRINGIFIES: &[&str] =
    &["chars", "codes", "fc", "flip", "lc", "tc", "tclc", "uc"];

/// The builtin `Cool` type objects the gate applies to. Allomorphs (`IntStr`,
/// ...) are left out: raku refuses those with `X::Parameter::InvalidConcreteness`
/// rather than `X::Multi::NoMatch`.
fn cool_type_object(target: &Value) -> Option<String> {
    let ValueView::Package(name) = target.view() else {
        return None;
    };
    let name = name.resolve();
    let is_cool = name == "Cool"
        || (!matches!(
            name.as_str(),
            "IntStr" | "NumStr" | "RatStr" | "ComplexStr" | "Allomorph"
        ) && Interpreter::is_builtin_type(&name)
            && Interpreter::type_matches("Cool", &name));
    is_cool.then(|| name.to_string())
}

/// The subject argument of a `Cool` string routine whose sub form is the
/// method on that argument: the first one, except `comb($matcher, $input)`.
fn string_sub_subject<'a>(name: &str, args: &'a [Value]) -> Option<&'a Value> {
    match name {
        "comb" => args.get(1),
        "chars" | "chomp" | "chop" | "codes" | "fc" | "flip" | "index" | "indices" | "lc"
        | "lines" | "ord" | "ords" | "rindex" | "substr" | "tc" | "tclc" | "trim"
        | "trim-leading" | "trim-trailing" | "uc" | "uniname" | "uninames" | "wordcase"
        | "words" => args.first(),
        _ => None,
    }
}

impl Interpreter {
    /// The sub-form twin of [`Self::cool_type_object_string_method`]:
    /// `substr($undefined, 0, 2)` answers what `$undefined.substr(0, 2)` does
    /// -- `X::Method::NotFound` for `Any`/`Mu`, the `Cool` type-object answer
    /// otherwise -- instead of slicing the type object's gist.
    // Cost: O(|name|), a constant-table match.
    pub(crate) fn type_object_string_sub_gate(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let subject = string_sub_subject(name, args)?;
        if !matches!(subject.view(), ValueView::Package(_)) {
            return None;
        }
        if let Some(err) = super::any_cool_method_gate::cool_method_not_found(subject, name) {
            return Some(Err(err));
        }
        let subject = subject.clone();
        self.cool_type_object_string_method(&subject, name)
    }

    /// Answer a `Cool` string method called on a builtin `Cool` type object,
    /// or `None` when the call is not gated (see the module docs).
    // Cost: O(|method|), a scan of two short constant tables.
    pub(crate) fn cool_type_object_string_method(
        &mut self,
        target: &Value,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        if !DEFINITE_ONLY.contains(&method) {
            return None;
        }
        let type_name = cool_type_object(target)?;
        if type_name == "Str" && STR_UNDEFINED_STRINGIFIES.contains(&method) {
            return Some(self.warn_type_object_string_context("Str", false).map(|_| {
                if matches!(method, "chars" | "codes") {
                    Value::int(0)
                } else {
                    Value::str(String::new())
                }
            }));
        }
        Some(Err(
            super::methods_signature_errors::make_multi_no_match_error_detailed(
                method,
                &type_name,
                false,
                "",
                &[],
            ),
        ))
    }
}
