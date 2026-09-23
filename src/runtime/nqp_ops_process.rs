//! The process/introspection half of the `nqp::` value-op table.
//!
//! Split out of [`nqp_ops`](super::nqp_ops) (which stays focused on native
//! arithmetic, buffers and low-level file handles) to keep both files under the
//! 500-line limit. Reached from the same dispatch point: `call_nqp_op` falls
//! through to `call_nqp_op_process` before the loud unsupported-op error.
//!
//! The driver is rakudo's own `lib/Test.rakumod`, which mutsu runs verbatim as
//! the one `Test` provider there is (#7554, #7566): it needs exactly these ops.
//! Note that `can`, `join`, `split`
//! and `time` all collide with same-named Raku builtins of *different*
//! semantics, which is why they are implemented here under their full `nqp::`
//! name rather than by relaxing the aliasing guard in
//! `builtins_operators_fallback.rs`.

use crate::runtime::{Interpreter, IoHandleTarget, RuntimeError};
use crate::value::{Value, ValueView};

impl Interpreter {
    /// Try a process-level / introspection `nqp::` op. `None` means "not an op
    /// this table knows"; the caller then raises the unsupported-op error.
    pub(crate) fn call_nqp_op_process(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        Some(match op {
            // -- the process's standard handles --
            // These are the *process* streams, deliberately not the `$*OUT` /
            // `$*ERR` dynamic variables: a caller that swapped `$*OUT` for a
            // capture object still gets the real stdout here, which is the
            // whole reason nqp code reaches for them (Test.rakumod unbuffers
            // the real streams so TAP output cannot be reordered).
            // Cost: getstdout/getstderr/getstdin O(h), h = open IO handles (the handle table is scanned for the
            // lowest id of that target). MoarVM: O(1) -- see #9134.
            "getstdout" => Ok(self.std_handle(IoHandleTarget::Stdout)),
            "getstderr" => Ok(self.std_handle(IoHandleTarget::Stderr)),
            "getstdin" => Ok(self.std_handle(IoHandleTarget::Stdin)),

            // nqp::setbuffersizefh($fh, $size) — set the handle's output
            // buffer capacity (0 = unbuffered) and return the handle. Maps
            // onto the same state as Raku's `$fh.out-buffer = $size`, so any
            // pending bytes are flushed before the capacity changes.
            // Cost: O(1) plus flushing any pending buffered bytes.
            "setbuffersizefh" => {
                let fh = args.first().cloned().unwrap_or(Value::NIL);
                let size = args
                    .get(1)
                    .and_then(Self::parse_out_buffer_size)
                    .unwrap_or(0);
                match self.with_handle_mut(&fh, |state| state.out_buffer_setting(Some(Some(size))))
                {
                    Ok(_) => Ok(fh),
                    Err(e) => Err(e),
                }
            }

            // nqp::time — wall clock as an integer number of NANOseconds since
            // the epoch (MoarVM's `time`, which replaced the older float-valued
            // `time_n`).
            // Cost: O(1).
            "time" => Ok(Value::int(Self::epoch_nanos())),

            // nqp::eqaddr($a, $b) — object identity as an int 0/1. Same
            // relation as Raku's `=:=`, which is already identity over the
            // container-kind values and by-name over type objects.
            // Cost: O(1) (a user WHICH on an Instance compares memoized strings, O(len)).
            "eqaddr" => Ok(Value::int(i64::from(crate::runtime::values_identical(
                args.first().unwrap_or(&Value::NIL),
                args.get(1).unwrap_or(&Value::NIL),
            )))),

            // nqp::can($obj, $name) — int 0/1: does this object have a method
            // of that name (the low-level form of `$obj.^can($name)`).
            // Cost: O(d + m), d = MRO length walked by collect_can_methods, m = size of matching method bodies it
            // clones into Sub values. MoarVM: O(1) avg (method cache) -- see #9134.
            "can" => {
                let target = args.first().cloned().unwrap_or(Value::NIL);
                let name = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
                let found = !self.collect_can_methods(&target, &name).is_empty();
                Ok(Value::int(i64::from(found)))
            }

            // nqp::join($sep, $list) / nqp::split($sep, $str) — string join and
            // split over an nqp list. Plain literal separators, no regex and no
            // Raku `split` adverbs: `nqp::split("", $s)` yields the characters,
            // splitting the empty string yields the empty list, and every
            // separator occurrence produces a field (so trailing empties are
            // kept).
            // Cost: O(t), t = total chars of $sep and every element (all copied via to_string_value).
            "join" => {
                let sep = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let parts = args.get(1).map(Self::nqp_list_strings).unwrap_or_default();
                Ok(Value::str(parts.join(&sep)))
            }
            // Cost: O(n + m), n = chars of $str, m = chars of $sep (std str::split, two-way search); plus O(k) values produced.
            "split" => {
                let sep = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let target = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
                let parts: Vec<Value> = if target.is_empty() {
                    Vec::new()
                } else if sep.is_empty() {
                    target.chars().map(|c| Value::str(c.to_string())).collect()
                } else {
                    target
                        .split(&sep)
                        .map(|s| Value::str(s.to_string()))
                        .collect()
                };
                Ok(Value::array(parts))
            }

            // nqp::defined($v) / nqp::isconcrete($v) — int 0/1: false for a
            // type object or the VM null, true for everything else. nqp draws
            // a finer HLL-vs-REPR distinction between the two upstream, but
            // both collapse to the same test here, matching the raku-observed
            // behavior driving this op (Test::Async's `HubHOW` bundle
            // registry, #8024). `isconcrete_nd` is the no-decontainerize
            // sibling of `isconcrete`; operands are already decontainerized
            // once at the `call_nqp_op` boundary (`nqp_ops.rs`), so it shares
            // this implementation.
            // Cost: O(1).
            "defined" | "isconcrete" | "isconcrete_nd" => Ok(Value::int(i64::from(
                crate::runtime::types::value_is_defined(args.first().unwrap_or(&Value::NIL)),
            ))),

            // nqp::istrue($v) — int 0/1: nqp's own truthiness test. This is
            // the same predicate Raku's `so`/boolification uses internally,
            // exposed for nqp-level code that inspects a native attribute
            // directly (AttrX::Mooish's `composed` method boolifies a
            // `nqp::getattr_i` int this way rather than going through `?`).
            // Cost: O(1) for scalars (truthy() of a lazy list may reify its head).
            "istrue" => Ok(Value::int(i64::from(
                args.first().unwrap_or(&Value::NIL).truthy(),
            ))),

            // nqp::islist($v) — int 0/1: is this a raw nqp-level list (the
            // `list` op above builds one), as opposed to a boxed Raku Array
            // or other object. AttrX::Mooish::ClassHOW walks its own
            // BUILDPLAN-like task list this way to tell a sub-list task apart
            // from a Code task.
            //
            // Real nqp draws a REPR-level distinction here: a bare
            // `nqp::list()` answers true, a boxed `Array` answers false, even
            // though `nqp::list()` and a boxed `Array` are otherwise
            // interchangeable through `elems`/`atpos`/etc. mutsu represents
            // both the same way (`ValueView::Array`, matching `"list" =>
            // Value::array(...)` above), so this can't draw that line and
            // instead answers true for both. That over-answers for a real
            // Array passed to `nqp::islist` directly (rare — the op exists to
            // inspect nqp-level bookkeeping structures like BUILDPLAN, not
            // ordinary Raku data), so it stays a loud gap rather than a
            // silent one: TODO: track the raw-list/boxed-Array distinction
            // at the representation level rather than approximating it here.
            // Cost: O(1).
            "islist" => Ok(Value::int(i64::from(matches!(
                args.first().map(|v| v.view()),
                Some(ValueView::Array(..) | ValueView::Slip(_))
            )))),

            // nqp::hllize($v) — the HLL (Raku-level) box of an nqp-level
            // value. mutsu has no separate nqp/HLL value representation (see
            // `p6box_*`/`unbox_*` above), so every value here is already its
            // own HLL box and this is the identity function.
            // Cost: O(1).
            "hllize" => Ok(args.first().cloned().unwrap_or(Value::NIL)),

            // nqp::what($v) — the type object of $v, i.e. `$v.WHAT` at the
            // nqp level. Routed through the ordinary method dispatcher, which
            // already answers `WHAT` generically for every value shape.
            // Cost: O(1) plus a slow-path method dispatch.
            "what" => {
                let v = args.first().cloned().unwrap_or(Value::NIL);
                self.call_method_with_values(v, "WHAT", Vec::new())
            }

            // nqp::lock($lock) / nqp::unlock($lock) — the nqp-level entry
            // points to the same critical section `Lock`'s `.lock`/`.unlock`
            // methods use (`native_lock` in
            // `runtime/native_methods/concurrency.rs`); AttrX::Mooish takes
            // this path directly around a `Lock.new` attribute rather than
            // calling the methods.
            // Cost: O(1) plus a slow-path method dispatch (blocking time excluded).
            "lock" => {
                let lock = args.first().cloned().unwrap_or(Value::NIL);
                self.call_method_with_values(lock, "lock", Vec::new())
            }
            // Cost: O(1) plus a slow-path method dispatch.
            "unlock" => {
                let lock = args.first().cloned().unwrap_or(Value::NIL);
                self.call_method_with_values(lock, "unlock", Vec::new())
            }

            // nqp::list(...) — an untyped VM list; mutsu represents one as an
            // ordinary array, same as the typed `list_s`/`list_i`/`list_n`.
            // Cost: O(k), k = arguments copied into a fresh Vec.
            "list" => Ok(Value::array(args.to_vec())),

            // nqp::unshift(@l, $v) — the positional peer of
            // `push_s`/`push_i`/`push_n` (nqp_ops_text.rs): insert at the
            // front of an nqp list / native array in place, returning the
            // list.
            // Cost: O(e), e = elements of the list (Vec::insert(0) shifts every element; a Buf is decoded and
            // re-encoded whole), so n unshifts are O(n^2). MoarVM: O(1) amortized -- see #9121.
            "unshift" => {
                let target = args.first().cloned().unwrap_or(Value::NIL);
                let val = args.get(1).cloned().unwrap_or(Value::NIL);
                match target.view() {
                    ValueView::Array(items, _) => {
                        // SAFETY: audited aliased in-place container write
                        // (see value::aliased_mut) — the same pattern
                        // `push_elem` uses; no borrow into the node is live.
                        let data = unsafe { crate::value::gc_contents_mut(&items) };
                        data.items_mut().insert(0, val);
                        Ok(target)
                    }
                    ValueView::Instance { attributes, .. } => {
                        let stored = val;
                        let done =
                            crate::value::value_buf::with_buf_elems_mut(&attributes, |elems| {
                                elems.insert(0, stored)
                            });
                        match done {
                            Some(()) => Ok(target),
                            None => Err(RuntimeError::new(
                                "nqp::unshift: expected a Buf/Blob or array".to_string(),
                            )),
                        }
                    }
                    _ => Err(RuntimeError::new(
                        "nqp::unshift: expected a Buf/Blob or array".to_string(),
                    )),
                }
            }

            _ => return self.call_nqp_op_text(op, args),
        })
    }

    /// Wall clock in nanoseconds since the Unix epoch, saturating rather than
    /// wrapping (an `i64` of nanoseconds runs out in the year 2262).
    fn epoch_nanos() -> i64 {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| i64::try_from(d.as_nanos()).unwrap_or(i64::MAX))
            .unwrap_or(0)
    }

    /// The process-level handle for one of the standard streams: the handle
    /// registered by `init_io_environment`, found by target rather than by the
    /// `$*OUT`/`$*ERR`/`$*IN` dynamic variables (which a caller may have
    /// rebound). Lowest id wins, so it is the one created at startup.
    fn std_handle(&mut self, target: IoHandleTarget) -> Value {
        let existing = {
            let table = self.io_handles();
            table
                .map
                .iter()
                .filter(|(_, state)| state.target == target)
                .map(|(id, _)| *id)
                .min()
        };
        match existing {
            Some(id) => self.make_handle_instance(id),
            // No startup handle (a bare embedding of the interpreter): make one
            // rather than handing back a Nil the caller cannot use.
            None => {
                let mode = match target {
                    IoHandleTarget::Stdin => crate::runtime::IoHandleMode::Read,
                    _ => crate::runtime::IoHandleMode::Write,
                };
                self.create_handle(target, mode, None)
            }
        }
    }

    /// The elements of an nqp list as strings. nqp's `join` takes a VM list;
    /// mutsu represents one as an ordinary `Array`/`List` value, and a single
    /// non-list value counts as a one-element list.
    fn nqp_list_strings(value: &Value) -> Vec<String> {
        match value.view() {
            ValueView::Array(items, _) => items.iter().map(|v| v.to_string_value()).collect(),
            ValueView::Slip(items) => items.iter().map(|v| v.to_string_value()).collect(),
            _ => vec![value.to_string_value()],
        }
    }
}
