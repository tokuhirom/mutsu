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
            target.view(),
            ValueView::Array(..)
                | ValueView::Seq(_)
                | ValueView::Slip(_)
                | ValueView::Hash(_)
                | ValueView::Set(..)
                | ValueView::Bag(..)
                | ValueView::Mix(..)
                | ValueView::Pair(..)
                | ValueView::ValuePair(..)
                | ValueView::Str(_)
                | ValueView::Int(_)
                | ValueView::BigInt(_)
                | ValueView::Num(_)
                | ValueView::Rat(..)
                | ValueView::FatRat(..)
                | ValueView::Complex(..)
                | ValueView::Bool(_)
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
    /// `Mix` value's Arc (container metadata has travelled in the value since #2952),
    /// so it is a pure value op like the others.
    ///
    /// Returns `None` (fall through to the interpreter) for `Instance`/`Package`/
    /// `Nil`/`Junction` receivers, leaving those rarer cases — `__baggy_data__`
    /// instances, type objects, user coercion, autothread — to the interpreter.
    pub(crate) fn try_native_quanthash_coerce(
        &mut self,
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
        // The keying layer below is interpreter-free, so resolve any
        // user-defined `WHICH` identities of the elements first (see
        // `runtime::which_identity`).
        self.warm_which_identity(target);
        let target = target.clone();
        let result = match method {
            "Set" => crate::builtins::quanthash_coerce::to_set(target, "Set"),
            "SetHash" => crate::builtins::quanthash_coerce::to_set(target, "SetHash")
                .map(|r| r.quanthash_with_mutability(true)),
            "Bag" => crate::builtins::quanthash_coerce::to_bag(target, "Bag"),
            "BagHash" => crate::builtins::quanthash_coerce::to_bag(target, "BagHash")
                .map(|r| r.quanthash_with_mutability(true)),
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
    /// `Hash` value's Arc (container metadata travels in the value since #2952 — no
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
    /// *type object* (a `Package` value) returns itself (`IO::Path === IO::Path.IO`).
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
        match target.view() {
            // `.IO` on an IO::Path (sub)class type object returns the type object
            // itself, so `IO::Path::Unix === IO::Path::Unix.IO`.
            ValueView::Package(name) => {
                let n = name.resolve();
                if n == "IO::Path" || n.starts_with("IO::Path::") {
                    return Some(Ok(target.clone()));
                }
                crate::runtime::utils::dateish_io_concreteness_error(&n).map(Err)
            }
            // Plain stringifiable scalars coerce their string value to an IO::Path.
            ValueView::Str(_)
            | ValueView::Int(_)
            | ValueView::BigInt(_)
            | ValueView::Num(_)
            | ValueView::Rat(..)
            | ValueView::FatRat(..)
            | ValueView::Complex(..)
            | ValueView::Bool(_) => {
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
        if let ValueView::Instance { class_name, .. } = target.view()
            && class_name == "Iterator"
        {
            return Some(target.clone());
        }
        // Seq: consumed-state + `squish` env mutation are interpreter-owned.
        // HyperSeq/RaceSeq: single-iterator consumed-state is interpreter-owned too.
        if matches!(
            target.view(),
            ValueView::Seq(_) | ValueView::HyperSeq(_) | ValueView::RaceSeq(_)
        ) {
            return None;
        }
        Some(crate::builtins::iterator_construct::build_iterator_instance(target))
    }

    /// The Iterator protocol on a built-in `Iterator` instance, answered by the
    /// rows of the `Iterator` and `PredictiveIterator` roles found by their
    /// owner (ADR-11276 §9.126): the one implementation every dispatch path
    /// shares, which commits the advance through the instance's shared cell.
    /// `None` (fall through to the interpreter) for any other receiver or name.
    // Cost: O(1) to find the row; the row's own cost (see
    // `Interpreter::builtin_iterator_call`).
    pub(super) fn try_native_iterator(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        // Only a built-in iterator: the other opaque shapes (an enum member whose
        // enum composes a role of its own) answer through the by-name dispatch.
        if !matches!(
            target.receiver_shape()?,
            crate::value::DispatchShape::Iterator | crate::value::DispatchShape::PredictiveIterator
        ) {
            return None;
        }
        let owners = crate::builtins::method_table::opaque_owners_of(target)?;
        crate::builtins::method_table::invoke_owner(self, owners, method, args, || target.clone())
    }
}
