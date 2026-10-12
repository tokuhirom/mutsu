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
        // Cost: O(n), n = chars of the invocant; a name no table knows also walks
        // the CLDR emoji list (O(E) per such name, E = emoji count).
        "uniparse" | "parse-names" => unicode::uniparse(target, &[]).map(Some),
        // The `List.chrs` row's implementation (`method_table::collections::any_misc`).
        "chrs" => Some(Some(
            crate::builtins::method_table::collection_any_misc::chrs(target, &[]),
        )),
        _ => None,
    }
}
