use super::*;

impl Interpreter {
    /// Whether `target` is a receiver for which the pure `.Set`/`.Bag`/`.Mix`/
    /// `.Map`/`.Hash`-family coercions can run natively in the VM. Two groups go
    /// native:
    ///   * list-like / QuantHash aggregates (List/Array/Seq/Slip/Hash/Set/Bag/Mix/
    ///     Pair/Range) — they flatten their elements; and
    ///   * plain `Cool` scalars (Str / Int / BigInt / Num / Rat / FatRat / Complex /
    ///     Bool) — each becomes a single-element collection via the coercion's
    ///     catch-all arm. The interpreter routes these exact scalars straight to the
    ///     same shared `builtins` impl with no special-casing (`Str.Set` →
    ///     `to_set` `other =>` arm = `Set(blue)`), so going native is behavior-invariant.
    ///
    /// `Instance` (`__baggy_data__` / Match captures / user coercion), `Package`
    /// (type objects — `.Map`/`.Hash` return the type object, handled by the
    /// interpreter), `Nil`, and `Junction` (autothread) receivers return `false`
    /// and fall through to the interpreter's richer handling.
    fn coerce_receiver_native_eligible(target: &Value) -> bool {
        matches!(
            target,
            Value::Array(..)
                | Value::Seq(_)
                | Value::Slip(_)
                | Value::Hash(_)
                | Value::Set(..)
                | Value::Bag(..)
                | Value::Mix(..)
                | Value::Pair(..)
                | Value::ValuePair(..)
                | Value::Str(_)
                | Value::Int(_)
                | Value::BigInt(_)
                | Value::Num(_)
                | Value::Rat(..)
                | Value::FatRat(..)
                | Value::Complex(..)
                | Value::Bool(_)
        ) || target.is_range()
    }

    /// Interpreter-native QuantHash coercion for `.Set`/`.Bag`/`.Mix`/`.SetHash`/
    /// `.BagHash`/`.MixHash` over a list-like aggregate or a plain `Cool` scalar
    /// (see [`coerce_receiver_native_eligible`](Self::coerce_receiver_native_eligible)).
    /// Delegates the element folding to the single authoritative pure
    /// implementation in `builtins::quanthash_coerce`, the same one the
    /// interpreter's `dispatch_method_by_name_2` uses, so the result is
    /// behavior-invariant. The `*Hash` variants flip the mutable flag exactly as
    /// the interpreter does. `.MixHash` embeds its type metadata directly in the
    /// `Value::Mix` Arc (container metadata has travelled in the value since #2952),
    /// so it is a pure value op like the others.
    ///
    /// Returns `None` (fall through to the interpreter) for `Instance`/`Package`/
    /// `Nil`/`Junction` receivers, leaving those rarer cases — `__baggy_data__`
    /// instances, type objects, user coercion, autothread — to the interpreter.
    pub(crate) fn try_native_quanthash_coerce(
        target: &Value,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(
            method,
            "Set" | "SetHash" | "Bag" | "BagHash" | "Mix" | "MixHash"
        ) {
            return None;
        }
        if !Self::coerce_receiver_native_eligible(target) {
            return None;
        }
        let target = target.clone();
        let result = match method {
            "Set" => crate::builtins::quanthash_coerce::to_set(target, "Set"),
            "SetHash" => {
                crate::builtins::quanthash_coerce::to_set(target, "SetHash").map(|r| match r {
                    Value::Set(items, _) => Value::Set(items, true),
                    other => other,
                })
            }
            "Bag" => crate::builtins::quanthash_coerce::to_bag(target, "Bag"),
            "BagHash" => {
                crate::builtins::quanthash_coerce::to_bag(target, "BagHash").map(|r| match r {
                    Value::Bag(items, _) => Value::Bag(items, true),
                    other => other,
                })
            }
            "Mix" => crate::builtins::quanthash_coerce::to_mix(target, "Mix"),
            "MixHash" => crate::builtins::quanthash_coerce::to_mixhash(target),
            _ => unreachable!(),
        };
        Some(result)
    }

    /// Interpreter-native `.Map` / `.Hash` coercion over a list-like aggregate or
    /// a plain `Cool` scalar (see
    /// [`coerce_receiver_native_eligible`](Self::coerce_receiver_native_eligible)),
    /// sharing the single `builtins::map_hash_coerce` impl the interpreter also
    /// uses. `.Map` decontainerizes and embeds the `Map` declared-type *in* the
    /// `Value::Hash` Arc (container metadata travels in the value since #2952 — no
    /// interpreter-owned side table), so both are pure value ops. A scalar (`42.Hash`)
    /// raises `X::Hash::Store::OddNumber` via the same `to_hash`, identical to the
    /// interpreter. Instance (Match / `__baggy_data__`) and Package (type object,
    /// where `.Map`/`.Hash` returns the type object) receivers fall through to the
    /// interpreter's richer handling. Lowercase `.hash` is intentionally not handled here.
    pub(crate) fn try_native_map_hash_coerce(
        target: &Value,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(method, "Map" | "Hash") {
            return None;
        }
        if !Self::coerce_receiver_native_eligible(target) {
            return None;
        }
        let target = target.clone();
        Some(match method {
            "Map" => crate::builtins::map_hash_coerce::to_map(target),
            "Hash" => crate::builtins::map_hash_coerce::to_hash(target, true),
            _ => unreachable!(),
        })
    }

    /// Interpreter-native `.IO` coercion over a `Cool` scalar receiver
    /// (`"path".IO`, `42.IO`): constructs an `IO::Path` from the receiver's string
    /// value via the single shared `make_io_path_instance` (which inherits `$*SPEC`
    /// / `$*CWD` from env — a normal native env read, identical to the
    /// interpreter's `dispatch_method_by_name` `.IO` arm). An `IO::Path`(-subclass)
    /// *type object* (`Value::Package`) returns itself (`IO::Path === IO::Path.IO`).
    ///
    /// Returns `None` (fall through to the interpreter) for `Instance` receivers
    /// (which may have a user `.IO` or be IO::Path/IO::Handle), non-IO `Package`
    /// type objects, and aggregate / `Junction` receivers (which autothread or
    /// have their own semantics) — only plain stringifiable scalars go native.
    pub(crate) fn try_native_io_coercion(
        &self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if method != "IO" || !args.is_empty() {
            return None;
        }
        match target {
            // `.IO` on an IO::Path (sub)class type object returns the type object
            // itself, so `IO::Path::Unix === IO::Path::Unix.IO`.
            Value::Package(name) => {
                let n = name.resolve();
                if n == "IO::Path" || n.starts_with("IO::Path::") {
                    Some(Ok(target.clone()))
                } else {
                    None
                }
            }
            // Plain stringifiable scalars coerce their string value to an IO::Path.
            Value::Str(_)
            | Value::Int(_)
            | Value::BigInt(_)
            | Value::Num(_)
            | Value::Rat(..)
            | Value::FatRat(..)
            | Value::Complex(..)
            | Value::Bool(_) => {
                let s = target.to_string_value();
                if s.contains('\0') {
                    return Some(Err(RuntimeError::new(
                        "X::IO::Null: Found null byte in pathname",
                    )));
                }
                Some(Ok(self.make_io_path_instance(&s)))
            }
            _ => None,
        }
    }

    /// Interpreter-native `.iterator` construction, mirroring
    /// `Interpreter::dispatch_iterator_method`: an already-built `Iterator`
    /// instance returns itself; a `Seq` falls through (consumed-state tracking +
    /// `squish` env mutation are interpreter-owned); any other receiver builds an
    /// `Iterator` instance via the single `builtins::iterator_construct` impl.
    /// Returns `None` to fall through to the interpreter.
    pub(crate) fn try_native_iterator_construct(target: &Value, method: &str) -> Option<Value> {
        if method != "iterator" {
            return None;
        }
        // Already an Iterator instance: identity (interpreter's early return).
        if let Value::Instance { class_name, .. } = target
            && class_name == "Iterator"
        {
            return Some(target.clone());
        }
        // Seq: consumed-state + `squish` env mutation are interpreter-owned.
        if matches!(target, Value::Seq(_)) {
            return None;
        }
        Some(crate::builtins::iterator_construct::build_iterator_instance(target))
    }

    /// Interpreter-native Iterator protocol over a self-contained, array-backed `Iterator`
    /// instance (`items` Array + `index` Int, no `squish_source` callbacks).
    /// Handles the index-advancing protocol methods `pull-one`/`skip-one`/
    /// `skip-at-least`/`skip-at-least-pull-one`/`sink-all`, mirroring the
    /// interpreter's mutating iterator dispatch in `methods_mut.rs` exactly,
    /// including the identity-based writeback (env + locals) so the receiver
    /// variable and any aliases observe the advance. Behavior-invariant.
    ///
    /// Returns `None` (fall through to the interpreter) for: non-`Iterator`
    /// receivers; squish iterators (which invoke user `as`/`with` callbacks);
    /// predictive / coroutine iterators (no concrete `items` array); the
    /// `push-*` family (it writes pulled elements into an external buffer arg,
    /// needing array-identity writeback handled by the interpreter); and
    /// `count-only`/`bool-only` (left to the interpreter's predictive handling).
    pub(super) fn try_native_iterator(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(
            method,
            "pull-one" | "skip-one" | "skip-at-least" | "skip-at-least-pull-one" | "sink-all"
        ) {
            return None;
        }
        let Value::Instance {
            class_name,
            attributes,
            ..
        } = target
        else {
            return None;
        };
        if class_name.resolve() != "Iterator"
            || attributes.contains_key("squish_source")
            || attributes.contains_key("is_lazy")
        {
            // squish iterators invoke user callbacks; lazy iterators (gather /
            // lazy Seq) pull through interpreter-owned coroutine state rather than
            // a materialized `items` snapshot — leave both to the interpreter.
            return None;
        }
        // Only a concrete array-backed iterator (excludes predictive/coroutine
        // iterators whose state lives off the instance). Snapshot the (cheap Arc
        // clone of the) backing array and the start index, then drop the read
        // guard before the writeback below write-locks the same cell.
        let (items, start_index) = {
            let map = attributes.as_map();
            let Some(Value::Array(items, ..)) = map.get("items") else {
                return None;
            };
            let start_index = match map.get("index") {
                Some(Value::Int(i)) if *i >= 0 => *i as usize,
                _ => 0,
            };
            (items.clone(), start_index)
        };
        let len = items.len();
        let mut index = start_index;
        let ret = match method {
            "pull-one" => {
                if index < len {
                    let out = items[index].clone();
                    index += 1;
                    out
                } else {
                    Value::str_from("IterationEnd")
                }
            }
            "sink-all" => {
                index = len;
                Value::str_from("IterationEnd")
            }
            "skip-one" => {
                if index < len {
                    index += 1;
                    Value::Bool(true)
                } else {
                    Value::Bool(false)
                }
            }
            "skip-at-least" => {
                let want = args.first().map(crate::runtime::to_int).unwrap_or(0).max(0) as usize;
                if len.saturating_sub(index) >= want {
                    index += want;
                    Value::Bool(true)
                } else {
                    index = len;
                    Value::Bool(false)
                }
            }
            "skip-at-least-pull-one" => {
                let want = args.first().map(crate::runtime::to_int).unwrap_or(0).max(0) as usize;
                if len.saturating_sub(index) >= want {
                    index += want;
                    if index < len {
                        let out = items[index].clone();
                        index += 1;
                        out
                    } else {
                        Value::str_from("IterationEnd")
                    }
                } else {
                    index = len;
                    Value::str_from("IterationEnd")
                }
            }
            _ => unreachable!(),
        };
        // Write the advanced index back by instance identity (env + locals), as
        // the interpreter's mutating iterator dispatch does, so the receiver
        // variable and aliases see the advance.
        if index != start_index {
            // Write through the shared cell in place: every alias of this
            // iterator instance (caller var, locals) sees the advance directly.
            attributes.insert("index".to_string(), Value::Int(index as i64));
        }
        Some(Ok(ret))
    }
}
