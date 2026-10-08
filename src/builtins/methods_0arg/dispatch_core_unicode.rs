/// Unicode and character methods: bytes, decode, chars, ord, ords, uniprop, uniname,
/// uninames, uniparse, uniprops, unival, univals, chr, chrs
use crate::builtins::method_table::unicode;
use crate::value::{RuntimeError, Value, ValueView};

pub(super) fn dispatch(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    match method {
        "bytes" => Some(match target.view() {
            ValueView::Instance { class_name, .. }
                if {
                    let cn = class_name.resolve();
                    crate::runtime::utils::is_buf_or_blob_class(&cn)
                } =>
            {
                // The `Blob`/`Buf` rows' implementation (`method_table::blob`).
                crate::builtins::method_table::blob::bytes(target, &[])
            }
            // `Blob.bytes` is `Blob:D:` only: a Buf/Blob type object must
            // throw rather than count the bytes of its name (`$buf //
            // Buf[uint8]` handed to `BIO_write` as a 12-byte buffer).
            ValueView::Package(name)
                if crate::runtime::utils::is_buf_or_blob_class(&name.resolve()) =>
            {
                Some(Err(RuntimeError::parameter_invalid_concreteness(
                    "Blob",
                    &name.resolve(),
                    "bytes",
                    "self",
                    true,
                    true,
                )))
            }
            ValueView::Str(s) => Some(Ok(Value::int(s.len() as i64))),
            _ => Some(Ok(Value::int(target.to_string_value().len() as i64))),
        }),
        "decode" => Some(super::super::decode_buf_method(target, None)),
        // Cost: O(1) amortized: the grapheme count comes from the payload's
        // cached index (built in O(n) on first use, `grapheme_index`).
        "chars" => {
            // Buf/Blob instances: throw X::Buf::AsStr
            if let ValueView::Instance { class_name, .. } = target.view()
                && crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
            {
                return Some(Some(Err(crate::runtime::Interpreter::buf_as_str_error(
                    target, "chars",
                ))));
            }
            Some(Some(Ok(Value::int(
                crate::builtins::str_prim::chars(target) as i64,
            ))))
        }
        // Cost: O(1) (borrows the payload).
        "ord" => Some(Some(crate::builtins::method_table::str::ord(target, &[]))),
        // Cost: O(n), n = chars of the invocant.
        // The `Str`/`Cool` row's handler (ADR-11276); a Seq, like `.comb`.
        "ords" => Some(Some(crate::builtins::method_table::str_iter::ords(
            target,
            &[],
        ))),
        // The Unicode methods of `Cool` are rows (`method_table::unicode`); these
        // arms keep the `Cool` receivers with no table shape (a `Match`, a
        // `Range`, a `Seq`, an instance of a `Cool` subclass), which the rows'
        // handlers answer through the receiver's string form.
        // Cost: O(1), a table lookup.
        "uniname" => unicode::uniname(target, &[]).map(Some),
        // Cost: O(n), n = chars of the invocant.
        "uninames" => unicode::uninames(target, &[]).map(Some),
        // Cost: O(1), a table lookup.
        "uniprop" => unicode::uniprop(target, &[]).map(Some),
        // Cost: O(n), n = chars of the invocant.
        "uniprops" => unicode::uniprops(target, &[]).map(Some),
        // Cost: O(1), a table lookup.
        "unival" => unicode::unival(target, &[]).map(Some),
        // Cost: O(n), n = chars of the invocant.
        "univals" => unicode::univals(target, &[]).map(Some),
        // Cost: O(n), n = chars of the invocant; a name no table knows also walks
        // the CLDR emoji list (O(E) per such name, E = emoji count).
        "uniparse" | "parse-names" => unicode::uniparse(target, &[]).map(Some),
        // Cost: O(e), e = elements of the invocant list.
        "chrs" => {
            // .chrs on a list/array of ints or a range
            let val_to_i64 = |v: &Value| -> i64 {
                match v.view() {
                    ValueView::Int(i) => i,
                    ValueView::Num(f) => f as i64,
                    _ => v.to_string_value().parse::<i64>().unwrap_or(0),
                }
            };
            let items: Vec<i64> = match target.view() {
                ValueView::Array(items, ..) => items.iter().map(&val_to_i64).collect(),
                ValueView::Seq(items) => items.iter().map(&val_to_i64).collect(),
                ValueView::Range(a, b) => (a..=b).collect(),
                ValueView::RangeExcl(a, b) => (a..b).collect(),
                _ => vec![val_to_i64(target)],
            };
            let s: String = items
                .iter()
                .filter_map(|&code| char::from_u32(code as u32))
                .collect();
            Some(Some(Ok(Value::str(s))))
        }
        _ => None,
    }
}
