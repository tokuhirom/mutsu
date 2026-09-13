//! The untyped list/hash `nqp::` ops, plus the two `p6*` bridge ops.
//!
//! Last link in the `nqp::` chain (`nqp_ops_str.rs` falls through to here).
//! Where `nqp_ops_text.rs` holds the *typed* native-list ops (`push_s`,
//! `atpos_i`, ...), this module holds their untyped twins (`nqp::push`,
//! `nqp::bindpos`) together with the queue ends (`pop_*`, `shift_*`) and the
//! ops that bridge the nqp level back to Raku values (`nqp::hash`,
//! `nqp::p6scalarwithvalue`, `nqp::p6bindattrinvres`).
//!
//! The immediate driver is upstream `JSON::Fast`, whose scanner and serializer
//! are written in these ops — see `docs/batteries/json-fast.md`.

use crate::runtime::{Interpreter, RuntimeError};
use crate::value::{Value, ValueView};

/// The attribute an `IterationBuffer` instance keeps its elements in. Spelled
/// literally in the constructor (`build_native_iterationbuffer_value`) and the
/// method dispatch; named here because the nqp ops below have to vivify it.
pub(crate) const ITERATION_BUFFER_ITEMS: &str = "__mutsu_iterationbuffer_items";

fn iarg(args: &[Value], i: usize) -> i64 {
    args.get(i).map(crate::runtime::to_int).unwrap_or(0)
}

/// The array a list-ish nqp value is backed by, as a `Value` that shares the
/// target's `Gc` node — so an in-place write through it is visible to every
/// other holder of the original.
///
/// A plain array answers for itself. A `Uni` answers with its codepoint array
/// — rakudo's `Uni` is a `uint32` VMArray and nqp code indexes, consumes and
/// splices one as exactly that. An `IterationBuffer` answers with its element
/// array, vivified when absent: `nqp::create(IterationBuffer)` runs `CREATE`,
/// which by definition installs no attributes, so the buffer nqp code then
/// pushes onto has no storage yet. A Buf/Blob answers `None` — its elements
/// live behind `value_buf` and [`Interpreter::nqp_with_elems_mut`] routes those
/// separately.
pub(crate) fn nqp_backing_array(v: &Value) -> Option<Value> {
    match v.view() {
        ValueView::Array(..) => Some(v.clone()),
        ValueView::Uni(uni) => Some(uni.codes.clone()),
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "IterationBuffer" => {
            if let Some(items) = attributes.as_map().get(ITERATION_BUFFER_ITEMS)
                && matches!(items.view(), ValueView::Array(..))
            {
                return Some(items.clone());
            }
            let fresh = Value::real_array(Vec::new());
            let mut updated = attributes.to_map();
            updated.insert(ITERATION_BUFFER_ITEMS.to_string(), fresh.clone());
            attributes.commit_attrs(updated);
            Some(fresh)
        }
        _ => None,
    }
}

/// Re-point an `IterationBuffer` at `array`'s node, so the two are one store
/// from here on. `false` when `storage` is not a buffer whose backing can be
/// swapped (a plain array cannot — nothing owns the `Value` slot to rewrite).
fn repoint_backing_array(storage: &Value, array: &Value) -> bool {
    if let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = storage.view()
        && class_name == "IterationBuffer"
        && matches!(array.view(), ValueView::Array(..))
    {
        let mut updated = attributes.to_map();
        updated.insert(ITERATION_BUFFER_ITEMS.to_string(), array.clone());
        attributes.commit_attrs(updated);
        return true;
    }
    false
}

impl Interpreter {
    /// Run `f` over a list-ish nqp value's elements in place. Covers a plain
    /// array, an `IterationBuffer` and a Buf/Blob; anything else is an error
    /// naming `op`.
    pub(crate) fn nqp_with_elems_mut<R>(
        op: &str,
        target: &Value,
        f: impl FnOnce(&mut Vec<Value>) -> R,
    ) -> Result<R, RuntimeError> {
        if let Some(array) = nqp_backing_array(target)
            && let ValueView::Array(items, _) = array.view()
        {
            // SAFETY: audited aliased in-place container write (see
            // value::aliased_mut and docs/gc-contents-mut-inventory.md) — `f`
            // is a pure element edit (push/pop/shift/index) that never
            // re-enters the interpreter, so no other borrow into the node is
            // live across it.
            let data = unsafe { crate::value::gc_contents_mut(&items) };
            return Ok(f(data.items_mut()));
        }
        if let ValueView::Instance { attributes, .. } = target.view()
            && let Some(r) = crate::value::value_buf::with_buf_elems_mut(&attributes, f)
        {
            return Ok(r);
        }
        Err(RuntimeError::new(format!(
            "nqp::{op}: expected a list, IterationBuffer or Buf/Blob, got {}",
            crate::runtime::value_type_name(target)
        )))
    }

    /// The elements of a list-ish nqp value, read-only.
    pub(crate) fn nqp_elems_of(target: &Value) -> Option<Vec<Value>> {
        if let Some(array) = nqp_backing_array(target) {
            return match array.view() {
                ValueView::Array(items, _) => Some(items.to_vec()),
                _ => None,
            };
        }
        if let ValueView::Instance { attributes, .. } = target.view() {
            return crate::value::value_buf::with_buf_elems(&attributes, |e| e.to_vec());
        }
        None
    }

    /// `nqp::bindattr($container, List, '$!reified', $storage)` and its Map
    /// `'$!storage'` twin: rakudo's List and Map are thin wrappers around a
    /// *separate* storage object, and nqp code installs one into the wrapper.
    /// mutsu has no wrapper — an Array/Hash IS its storage, which is already
    /// what `nqp_attr_value` answers a `'$!storage'` *read* with — so
    /// installing means the two values have to become ONE store: the container
    /// takes the storage's current contents, and the storage object is
    /// re-pointed at the container's node so later writes through either are
    /// seen by both.
    ///
    /// Both halves are load-bearing, because nqp code does it in both orders.
    /// `JSON::Fast`'s `hllize-list` installs an already-filled buffer into a
    /// fresh `List` (needs the contents copied in); its `parse-array` installs
    /// an *empty* buffer into `@result` and only then pushes onto the buffer
    /// (needs the re-point, or every element would land somewhere nothing
    /// reads).
    ///
    /// `true` when this was such a bind and it has been handled.
    pub(crate) fn nqp_bind_container_storage(
        attr_key: &str,
        container: &Value,
        storage: &Value,
    ) -> bool {
        if !matches!(attr_key, "reified" | "storage") {
            return false;
        }
        match container.view() {
            ValueView::Array(items, _) => {
                let Some(elems) = Self::nqp_elems_of(storage) else {
                    return false;
                };
                // SAFETY: audited aliased in-place container write (see
                // value::aliased_mut) — a plain element replacement with no
                // other borrow into the node live across it.
                let data = unsafe { crate::value::gc_contents_mut(&items) };
                *data.items_mut() = elems;
                repoint_backing_array(storage, container);
                true
            }
            ValueView::Hash(hash) => {
                let ValueView::Hash(src) = storage.view() else {
                    return false;
                };
                if crate::gc::Gc::ptr_eq(&hash, &src) {
                    // Already one store (the `'$!storage'` read handed the
                    // hash itself back) — nothing to install.
                    return true;
                }
                let entries: Vec<(String, Value)> =
                    src.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                // SAFETY: audited aliased in-place container write (see
                // value::aliased_mut); no borrow into the hash is live.
                let data = unsafe { crate::value::gc_contents_mut(&hash) };
                for (k, v) in entries {
                    data.map.insert(k, v);
                }
                true
            }
            _ => false,
        }
    }

    /// `nqp::splice($target, $source, $offset, $count)` over an ELEMENT store
    /// rather than a Buf's bytes. `None` when the target has no element store,
    /// leaving the byte path in `nqp_ops.rs` to answer.
    ///
    /// nqp code escapes text by splicing one native int list into another
    /// (`JSON::Fast`'s `str-escape` replaces a quote codepoint with the two
    /// codepoints `\` `"`), which the byte path cannot express.
    pub(crate) fn nqp_splice_elems(
        op: &str,
        target: &Value,
        source: &Value,
        offset: usize,
        count: usize,
    ) -> Option<Result<Value, RuntimeError>> {
        nqp_backing_array(target)?;
        let Some(src) = Self::nqp_elems_of(source) else {
            return Some(Err(RuntimeError::new(format!(
                "nqp::{op}: expected a list source, got {}",
                crate::runtime::value_type_name(source)
            ))));
        };
        Some(
            Self::nqp_with_elems_mut(op, target, |elems| {
                if elems.len() < offset {
                    elems.resize(offset, Value::int(0));
                }
                let upper = (offset + count).min(elems.len());
                elems.splice(offset..upper, src);
            })
            .map(|()| target.clone()),
        )
    }

    /// Try an untyped list/hash `nqp::` op. `None` means "not an op this table
    /// knows" — the caller then raises the loud unsupported-op error.
    pub(crate) fn call_nqp_op_list(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        Some(match op {
            // nqp::push($list, $value): append one element, untyped. The
            // typed twins (`push_s`/`push_i`/`push_n`) live in
            // `nqp_ops_text.rs`; this one stores the value as it came.
            "push" => {
                let target = args.first().cloned().unwrap_or(Value::NIL);
                let val = args.get(1).cloned().unwrap_or(Value::NIL);
                match Self::nqp_with_elems_mut(op, &target, |elems| elems.push(val)) {
                    Ok(()) => Ok(target),
                    Err(e) => Err(e),
                }
            }
            // nqp::pop($list) and its typed twins: remove and return the LAST
            // element. An empty list yields the type's zero rather than an
            // error, matching how `atpos_*` answers out of range.
            "pop" | "pop_s" | "pop_i" | "pop_n" => {
                match Self::nqp_with_elems_mut(
                    op,
                    &args.first().cloned().unwrap_or(Value::NIL),
                    |elems| elems.pop(),
                ) {
                    Ok(elem) => Ok(coerce_like(op, elem)),
                    Err(e) => Err(e),
                }
            }
            // nqp::shift($list) and its typed twins: remove and return the
            // FIRST element. `JSON::Fast`'s string scanner drives its whole
            // `Uni` of codepoints this way.
            "shift" | "shift_s" | "shift_i" | "shift_n" => {
                match Self::nqp_with_elems_mut(
                    op,
                    &args.first().cloned().unwrap_or(Value::NIL),
                    |elems| {
                        if elems.is_empty() {
                            None
                        } else {
                            Some(elems.remove(0))
                        }
                    },
                ) {
                    Ok(elem) => Ok(coerce_like(op, elem)),
                    Err(e) => Err(e),
                }
            }
            // nqp::bindpos($list, $i, $value): store at an index, untyped,
            // growing the list when the index is past the end. nqp code builds
            // sparse lookup tables this way — `JSON::Fast` indexes one by
            // codepoint to decode a hex digit — so the gaps have to read back
            // as something falsy, which is what `Value::NIL` gives.
            "bindpos" => {
                let target = args.first().cloned().unwrap_or(Value::NIL);
                let idx = iarg(args, 1).max(0) as usize;
                let val = args.get(2).cloned().unwrap_or(Value::NIL);
                let stored = val.clone();
                match Self::nqp_with_elems_mut(op, &target, |elems| {
                    if elems.len() <= idx {
                        elems.resize(idx + 1, Value::NIL);
                    }
                    elems[idx] = stored;
                }) {
                    Ok(()) => Ok(val),
                    Err(e) => Err(e),
                }
            }
            // nqp::chr($codepoint): the one-character string for a codepoint.
            // Lower level than Raku's `chr` (no `Cool` coercion), but it
            // rejects a non-scalar value the same way, because there is no
            // string that could hold one.
            "chr" => {
                let cp = iarg(args, 0);
                match u32::try_from(cp).ok().and_then(char::from_u32) {
                    Some(c) => Ok(Value::str(c.to_string())),
                    None => Err(RuntimeError::new(format!(
                        "nqp::chr: {cp} is not a valid Unicode codepoint"
                    ))),
                }
            }
            // nqp::hash('k', $v, ...): a fresh VM hash from alternating
            // key/value arguments — the associative twin of `nqp::list`. An
            // odd trailing argument binds to Nil, as nqp does.
            "hash" => {
                let mut map = std::collections::HashMap::new();
                for pair in args.chunks(2) {
                    let key = pair[0].to_string_value();
                    let val = pair.get(1).cloned().unwrap_or(Value::NIL);
                    map.insert(key, val);
                }
                Ok(Value::hash_with_data(Value::hash_arc(map)))
            }
            // nqp::p6scalarwithvalue($descriptor, $value): rakudo wraps a
            // value in a fresh `Scalar` carrying a container descriptor, which
            // is how an nqp-built Array/Hash gets assignable elements. mutsu's
            // containers store values directly and hand out an element
            // container on demand (ADR-0036/ADR-0045), so there is no
            // pre-wrapped Scalar to build and the descriptor is ignored.
            //
            // What the wrapping still has to reproduce is **itemization**: a
            // value bound in through raw `nqp::bindkey` never reaches the
            // store-side hook that itemizes an ordinary `%h<k> = [1,2]`, so
            // returning the bare value lost it. `JSON::Fast`'s `parse-obj`
            // builds every decoded object exactly that way, which made
            // `from-json('{"a":[1,2]}')<a>.raku` answer `[1, 2]` where rakudo
            // (running the same module) answers `$[1, 2]`.
            //
            // Which values itemize is measured against rakudo, not assumed:
            // `Array`, `Hash`, `Seq` and `List` do; `Range`, `Pair`, `Bool`,
            // `Int`, `Str` and a type object do not. That is narrower than
            // `Value::itemize_for_element_store`, which also itemizes every
            // `Range` variant — right for its own call sites, wrong here.
            "p6scalarwithvalue" => {
                let value = crate::runtime::types::unwrap_varref_value(
                    args.get(1).cloned().unwrap_or(Value::NIL),
                );
                Ok(match value.view() {
                    ValueView::Array(..) | ValueView::Hash(_) | ValueView::Seq(_) => value.item(),
                    _ => value,
                })
            }
            // nqp::p6bindattrinvres($obj, Type, '$!attr', $value): bind the
            // attribute and return the INVOCANT rather than the value, so a
            // mutating method stays chainable. The standard way nqp code
            // hands back a freshly populated object.
            "p6bindattrinvres" => {
                let obj = args.first().cloned().unwrap_or(Value::NIL);
                let attr = args.get(2).map(|v| v.to_string_value()).unwrap_or_default();
                let val = args.get(3).cloned().unwrap_or(Value::NIL);
                match Self::nqp_bindattr_value(op, &obj, &attr, val) {
                    Ok(()) => Ok(obj),
                    Err(e) => Err(e),
                }
            }

            _ => return None,
        })
    }
}

/// Coerce a popped/shifted element to the flavour the op name asks for. A
/// missing element (empty list) reads as the zero of that flavour.
fn coerce_like(op: &str, elem: Option<Value>) -> Value {
    match op.rsplit_once('_').map(|(_, t)| t) {
        Some("s") => Value::str(elem.map(|v| v.to_string_value()).unwrap_or_default()),
        Some("i") => Value::int(elem.as_ref().map(crate::runtime::to_int).unwrap_or(0)),
        Some("n") => Value::num(elem.as_ref().map(|v| v.to_f64()).unwrap_or(0.0)),
        _ => elem.unwrap_or(Value::NIL),
    }
}
