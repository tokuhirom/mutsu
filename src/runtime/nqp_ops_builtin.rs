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
            // The typed variants coerce first: an nqp `int`/`num`/`str`
            // attribute holds a native value, and code that reads it back with
            // `getattr_i` expects one.
            "bindattr" | "bindattr_i" | "bindattr_n" | "bindattr_s" => {
                let obj = args.first().cloned().unwrap_or(Value::NIL);
                let attr = args.get(2).map(|v| v.to_string_value()).unwrap_or_default();
                let raw = args.get(3).cloned().unwrap_or(Value::NIL);
                let val = match op {
                    "bindattr_i" => Value::int(to_int(&raw)),
                    "bindattr_n" => Value::num(raw.to_f64()),
                    "bindattr_s" => Value::str(raw.to_string_value()),
                    _ => raw,
                };
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
            // nqp::create($type) — allocate an instance of `$type` with NO
            // constructor run: attributes stay uninitialized and `BUILD` is
            // never called, which is exactly Raku's `.CREATE`. nqp code uses
            // it both for a native array (`nqp::create(array[uint32])`) and to
            // hand-build an iterator (`nqp::create(self)` followed by
            // `bindattr`), so it must not go anywhere near `new`.
            "create" => {
                let ty = args.first().cloned().unwrap_or(Value::NIL);
                // A native array / Buf / Blob is allocated with its REPR's
                // empty storage, which for mutsu means `.new`: `CREATE` hands
                // back a value with no element storage at all, so the
                // `nqp::push_i` that invariably follows
                // (`nqp::strtocodes($s, NFC, nqp::create(array[uint32]))`)
                // had nothing to push onto. Everything else takes `CREATE`,
                // whose whole point here is to skip the constructor.
                let name = match ty.view() {
                    ValueView::Package(sym) => sym.resolve().to_string(),
                    _ => crate::runtime::utils::value_type_name(&ty).to_string(),
                };
                let method = if name.starts_with("array[")
                    || name == "array"
                    || crate::runtime::utils::is_buf_or_blob_class(&name)
                {
                    "new"
                } else {
                    "CREATE"
                };
                Ok(match self.call_method_with_values(ty, method, vec![]) {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                })
            }

            // nqp::getattr($obj, $class, '$!name') and the typed reads —
            // straight attribute access, with the class operand ignored
            // because mutsu stores one flat attribute map per instance rather
            // than a per-class slot table. (A private attribute of the same
            // name in two classes of one hierarchy would therefore collide;
            // that is the same limitation `$!name` access already has.)
            "getattr" | "getattr_i" | "getattr_n" | "getattr_s" => {
                let obj = args.first().cloned().unwrap_or(Value::NIL);
                let name = args.get(2).map(|v| v.to_string_value()).unwrap_or_default();
                let value = Self::nqp_attr_value(&obj, &name);
                Ok(match op {
                    "getattr_i" => Value::int(value.as_ref().map(to_int).unwrap_or(0)),
                    "getattr_n" => Value::num(value.as_ref().map(|v| v.to_f64()).unwrap_or(0.0)),
                    "getattr_s" => Value::str(
                        value
                            .as_ref()
                            .map(|v| v.to_string_value())
                            .unwrap_or_default(),
                    ),
                    _ => value.unwrap_or(Value::NIL),
                })
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

    /// The attribute-map key for an nqp `'$!name'` operand. nqp always spells
    /// the twigil; mutsu's instance maps are keyed by the bare name, so try
    /// both rather than assuming one (an attribute declared `@!items` is
    /// asked for as `'@!items'` and stored as `items`).
    fn nqp_attr_keys(name: &str) -> Vec<String> {
        let bare = name
            .strip_prefix("$!")
            .or_else(|| name.strip_prefix("@!"))
            .or_else(|| name.strip_prefix("%!"))
            .or_else(|| name.strip_prefix("&!"))
            .unwrap_or(name);
        if bare == name {
            vec![name.to_string()]
        } else {
            vec![bare.to_string(), name.to_string()]
        }
    }

    fn nqp_attr_value(obj: &Value, name: &str) -> Option<Value> {
        // `nqp::getattr($map, Map, '$!storage')` reaches for the hash a Map
        // wraps, so that nqp's bindkey/deletekey can build it in place. In
        // mutsu a Map/Hash IS that storage — there is no wrapper object with a
        // separate `$!storage` slot — so the hash answers for itself, and the
        // in-place mutations the caller then performs land on the same `Gc`
        // the Map value holds.
        if let ValueView::Hash(_) = obj.view() {
            return Some(obj.clone());
        }
        let ValueView::Instance { attributes, .. } = obj.view() else {
            return None;
        };
        let attrs = attributes.to_map();
        Self::nqp_attr_keys(name)
            .into_iter()
            .find_map(|k| attrs.get(&k).cloned())
    }
}
