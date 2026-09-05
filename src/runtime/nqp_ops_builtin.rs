//! The `nqp::` ops whose implementations need the full `Interpreter` surface
//! (method re-dispatch, the NativeCall pointer layer, the class registry) and
//! therefore cannot live in the pure value table of [`super::nqp_ops`].
//!
//! These used to be ordinary arms of `Interpreter::call_function`'s builtin
//! `match`, which meant every `nqp::` call first walked the whole dispatch
//! chain — light-call caches, proto/multi candidate scans, the native-function
//! tables — before its name could be recognized. The `nqp::` namespace is
//! reserved and never carries a user routine, so that walk was always dead
//! work; it is now short-circuited at the VM's call opcode
//! (`dispatch_nqp_op`), and this module is where the interpreter-coupled half
//! of that dispatch lives.

use super::*;

impl Interpreter {
    /// The single entry point for every `nqp::` op, with `op` being the name
    /// **without** the `nqp::` prefix.
    ///
    /// Ordering mirrors the historical dispatch: the interpreter-coupled ops
    /// below were matched inside `call_function` (i.e. before the builtin
    /// fallback chain), and everything else falls through to the pure value
    /// table in `nqp_ops.rs`. An op neither knows stays a loud error rather
    /// than silently reaching a same-named Raku builtin with different
    /// semantics — `nqp::index` returns -1 where Raku's `index` returns Nil,
    /// and nqp code branches on exactly that.
    pub(crate) fn dispatch_nqp_op(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if let Some(result) = self.call_nqp_interpreter_op(op, args) {
            return result;
        }
        if let Some(result) = self.call_nqp_op(op, args) {
            return result;
        }
        Err(RuntimeError::new(format!(
            "Unsupported nqp:: op: nqp::{op}"
        )))
    }

    /// The interpreter-coupled `nqp::` ops. `None` means "not one of these" —
    /// the caller then tries the pure value table.
    fn call_nqp_interpreter_op(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        Some(match op {
            "atkey" => {
                let hash = args.first().cloned().unwrap_or(Value::NIL);
                let key = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
                match hash.view() {
                    ValueView::Hash(map) => Ok(map.get(&key).cloned().unwrap_or(Value::NIL)),
                    _ => {
                        let h = hash.clone();
                        self.call_method_with_values(h, "AT-KEY", vec![Value::str(key)])
                    }
                }
            }
            "atpos" => {
                let list = args.first().cloned().unwrap_or(Value::NIL);
                let idx = args
                    .get(1)
                    .and_then(|v| match v.view() {
                        ValueView::Int(i) => Some(i as usize),
                        _ => v.to_string_value().parse::<usize>().ok(),
                    })
                    .unwrap_or(0);
                match list.view() {
                    ValueView::Array(items, _) => Ok(items.get(idx).cloned().unwrap_or(Value::NIL)),
                    _ => Ok(Value::NIL),
                }
            }
            // nqp::ordat($str, $pos): the Unicode codepoint of the character at
            // position `$pos` in `$str` (equivalent to `$str.substr($pos, 1).ord`).
            // Returns -1 when the position is past the end, matching nqp. Used by
            // Text::Diff::Sift4's inner char-comparison loop.
            "ordat" => {
                let s = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let pos = args
                    .get(1)
                    .and_then(|v| match v.view() {
                        ValueView::Int(i) => Some(i),
                        _ => v.to_string_value().parse::<i64>().ok(),
                    })
                    .unwrap_or(0);
                let cp = usize::try_from(pos)
                    .ok()
                    .and_then(|p| s.chars().nth(p))
                    .map(|c| c as i64)
                    .unwrap_or(-1);
                Ok(Value::int(cp))
            }
            // nqp::sha1($str): the SHA-1 digest of the string's UTF-8 encoding,
            // as 40 uppercase hex digits. Needed by two components mutsu ships:
            // the vendored zef (`Zef::Distribution.id` and the source-path
            // computation) and bundled OpenSSL's `dll-resource()`.
            "sha1" => {
                let s = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Ok(Value::str(crate::builtins::sha1::sha1_hex_uppercase(
                    s.as_bytes(),
                )))
            }
            // nqp::gethostname(): the system hostname as a native str. Used by
            // Sys::Hostname's `hostname` sub (`nqp::gethostname.subst(...)`).
            "gethostname" => Ok(Value::str(Self::hostname())),
            // nqp::bindattr($obj, Type, '$!attr', value): write an attribute
            // cell directly, bypassing accessors (roast's Test::Compile uses it
            // to install a precomp repository into a CUR::FileSystem instance).
            "bindattr" => {
                let obj = args.first().cloned().unwrap_or(Value::NIL);
                let attr = args.get(2).map(|v| v.to_string_value()).unwrap_or_default();
                let val = args.get(3).cloned().unwrap_or(Value::NIL);
                let attr_key = attr
                    .trim_start_matches(['$', '@', '%', '&'])
                    .trim_start_matches(['!', '.']);
                if attr_key.is_empty() {
                    return Some(Err(RuntimeError::new(
                        "nqp::bindattr: empty attribute name",
                    )));
                }
                if let ValueView::Instance { attributes, .. } = obj.view() {
                    let mut updated = attributes.to_map();
                    updated.insert_through(attr_key, val.clone());
                    attributes.commit_attrs(updated);
                }
                Ok(val)
            }
            // nqp::decont($x): strip the container off a value. Raku's `.item`
            // twin at the nqp level; the ops below take their argument through
            // it, so a `$`-variable argument does not arrive as a Scalar.
            "decont" => Ok(crate::runtime::types::unwrap_varref_value(
                args.first().cloned().unwrap_or(Value::NIL),
            )),
            // nqp::unbox_i($x): the native integer inside a boxed value. A
            // NativeCall `Pointer` unboxes to its address, which is what makes
            // pointer arithmetic expressible — `NativeHelpers::Pointer` builds
            // `.add`/`.succ`/`.pred` out of exactly that.
            "unbox_i" => {
                let v = crate::runtime::types::unwrap_varref_value(
                    args.first().cloned().unwrap_or(Value::NIL),
                );
                Ok(Value::int(
                    crate::runtime::nativecall::value_c_address(&v) as i64
                ))
            }
            // nqp::box_i($i, Type): the inverse. A `Pointer`/`Pointer[T]` target
            // yields a pointer at that address (`Pointer.new(0)` is a legitimate
            // defined value, so a zero address stays defined); anything else
            // boxes as a plain `Int`, which is what every non-pointer nqp target
            // amounts to here.
            "box_i" => {
                let n = args.first().map(crate::runtime::to_int).unwrap_or(0);
                let target = args
                    .get(1)
                    .map(|v| match v.view() {
                        ValueView::Package(name) => name.resolve().to_string(),
                        ValueView::Instance { class_name, .. } => class_name.resolve().to_string(),
                        _ => String::new(),
                    })
                    .unwrap_or_default();
                let short = crate::runtime::cstruct_layout::short_base_name(&target);
                if short == "Pointer" || short.starts_with("Pointer[") {
                    let of = short
                        .strip_prefix("Pointer[")
                        .and_then(|s| s.strip_suffix(']'));
                    let addr = n.max(0) as usize;
                    return Some(Ok(match of {
                        Some(of) => crate::runtime::nativecall::make_typed_pointer(addr, of),
                        None => crate::runtime::nativecall::make_pointer_object(addr),
                    }));
                }
                Ok(Value::int(n))
            }
            // nqp::setelems($buf, $n): resize a buffer to `$n` elements, the
            // extra ones zero. `NativeHelpers::Blob`'s `blob-allocate` is
            // `blob.new` followed by this, so a `Buf` out-parameter of a native
            // call is allocated through it.
            "setelems" => {
                let target = crate::runtime::types::unwrap_varref_value(
                    args.first().cloned().unwrap_or(Value::NIL),
                );
                let n = args.get(1).map(crate::runtime::to_int).unwrap_or(0).max(0) as usize;
                if let ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } = target.view()
                    && crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
                {
                    let resized = crate::value::value_buf::with_buf_elems_mut(&attributes, |e| {
                        e.resize(n, Value::int(0))
                    });
                    if resized.is_none() {
                        // A `Blob` type object carries no storage at all; give
                        // it some rather than silently answering an empty one.
                        let mut map = attributes.to_map();
                        crate::value::value_buf::set_buf_elems(
                            &mut map,
                            class_name,
                            vec![Value::int(0); n],
                        );
                        attributes.commit_attrs(map);
                    }
                    return Some(Ok(target.clone()));
                }
                // A plain (or native typed) array target: resize its items in
                // place — CBOR::Simple presizes `array[num32].new` this way.
                if let ValueView::Array(items, _) = target.view() {
                    // SAFETY: audited aliased in-place container write (see
                    // value::aliased_mut); no borrow into the node is live.
                    let data = unsafe { crate::value::gc_contents_mut(&items) };
                    data.items_mut().resize(n, Value::int(0));
                    return Some(Ok(target.clone()));
                }
                Ok(target)
            }
            _ => return None,
        })
    }
}
