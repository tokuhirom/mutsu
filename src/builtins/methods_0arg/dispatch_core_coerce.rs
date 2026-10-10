/// Type coercion methods: self, clone, defined, DEFINITE, WHICH, Bool, Str, Int, UInt,
/// Num, Real, Numeric, Bridge
use crate::runtime;
use crate::symbol::Symbol;
use crate::value::str_numeric::str_numifies_to_complex;
use crate::value::value_buf::buf_len_or_zero;
use crate::value::{RuntimeError, Value, ValueView};

use super::parse_raku_int_from_str;
use crate::value::ValueMap;
use crate::value::types::is_stash_class_name;

/// Build the `X::Str::Numeric` attribute map for a string that cannot be
/// numified, deriving `pos`/`reason` from the same analyzer the numeric
/// operators use (so `"5 foo"` reports `trailing characters after number` at
/// pos 1, not a blanket "must begin with valid digits" at pos 0), and the
/// matching `source-indicator`.
fn str_numeric_exception_attrs(s: &str) -> ValueMap {
    let (pos, reason) = crate::runtime::str_numeric::str_numeric_failure(s).unwrap_or((
        0,
        "base-10 number must begin with valid digits or '.'".to_string(),
    ));
    let mut ex_attrs = ValueMap::default();
    ex_attrs.insert("source".to_string(), Value::str(s.to_string()));
    ex_attrs.insert("reason".to_string(), Value::str(reason.clone()));
    ex_attrs.insert("pos".to_string(), Value::int(pos as i64));
    let source_indicator = crate::runtime::str_numeric::build_source_indicator(s, pos);
    ex_attrs.insert(
        "source-indicator".to_string(),
        Value::str(source_indicator.clone()),
    );
    // Include the `⏏` position marker, matching Rakudo:
    // `Cannot convert string to number: trailing characters after number
    //  in '5⏏ foo' (indicated by ⏏)`.
    ex_attrs.insert(
        "message".to_string(),
        Value::str(format!(
            "Cannot convert string to number: {reason} {source_indicator}"
        )),
    );
    ex_attrs
}

/// Build a lazy `Failure` wrapping an `X::Str::Numeric` exception for a string
/// that cannot be numified — the shape raku's `.Int`/`.Num` produce on a bad
/// string (the exception only fires when the Failure is sunk/used).
pub(crate) fn str_numeric_failure(s: &str) -> Value {
    let ex = Value::make_instance(
        Symbol::intern("X::Str::Numeric"),
        str_numeric_exception_attrs(s),
    );
    let mut failure_attrs = std::collections::HashMap::new();
    failure_attrs.insert("exception".to_string(), ex);
    failure_attrs.insert("handled".to_string(), Value::FALSE);
    Value::make_instance(Symbol::intern("Failure"), failure_attrs)
}

/// The thrown `X::Str::Numeric` for a string that cannot be numified, for a
/// caller that coerces its argument itself (`(1..3).EXISTS-POS("a")`).
// Cost: O(d), d = chars of `s`.
pub(crate) fn str_numeric_error(s: &str) -> RuntimeError {
    let attrs = str_numeric_exception_attrs(s);
    let message = attrs
        .get("message")
        .map(|m| m.to_string_value())
        .unwrap_or_default();
    let mut err = RuntimeError::new(message);
    err.exception = Some(Box::new(Value::make_instance(
        Symbol::intern("X::Str::Numeric"),
        attrs,
    )));
    err
}

/// Render a Complex's literal form (`1+2i`, `1-2i`, `3.7+1e-20i`) for an
/// `X::Numeric::Real` message: its `.Str`, as raku prints the source value.
fn render_complex_literal(re: f64, im: f64) -> String {
    crate::value::format_complex(re, im)
}

/// Build the `X::Numeric::Real` exception raised when a Complex with a
/// non-zero imaginary part is coerced to a type that requires a Real value
/// (`.Int`, `.UInt`, `.Num`, `.Rat`, `.FatRat`, `.Real`, `.sign`, ...).
/// `target_type` is the type reported by `.target` — the type the coercion
/// actually attempted, e.g. `"Int"` for `.Int` — matching raku's per-method
/// message (`Cannot convert 1+2i to Int: imaginary part not zero`).
pub(crate) fn complex_not_real_exception(
    re: f64,
    im: f64,
    target_type: &str,
    source: &Value,
) -> Value {
    let mut attrs = std::collections::HashMap::new();
    attrs.insert(
        "message".to_string(),
        Value::str(format!(
            "Cannot convert {} to {target_type}: imaginary part not zero",
            render_complex_literal(re, im)
        )),
    );
    attrs.insert(
        "target".to_string(),
        Value::package(Symbol::intern(target_type)),
    );
    attrs.insert("source".to_string(), source.clone());
    Value::make_instance(Symbol::intern("X::Numeric::Real"), attrs)
}

/// The eager-throw form of [`complex_not_real_exception`], for coercions that
/// fail immediately rather than returning a lazy `Failure`.
pub(crate) fn complex_not_real_error(
    re: f64,
    im: f64,
    target_type: &str,
    source: &Value,
) -> RuntimeError {
    let msg = format!(
        "Cannot convert {} to {target_type}: imaginary part not zero",
        render_complex_literal(re, im)
    );
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(complex_not_real_exception(
        re,
        im,
        target_type,
        source,
    )));
    err
}

pub(super) fn dispatch(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    // A Range numerifies to its element count (`.elems`): `.Int`/`.Numeric`/
    // `.Real` yield that count as an `Int`, `.Num` as a `Num`. An infinite
    // range yields `Inf` for the real-valued coercions and fails for `.Int`
    // ("Cannot convert Inf to Int"). Handle this before the per-method arms,
    // which only know how to numerify scalar types.
    if target.is_range() && matches!(method, "Int" | "Numeric" | "Real" | "Num") {
        return Some(Some(
            crate::builtins::method_table::range::numeric_coercion(target, method),
        ));
    }
    // A lazy (infinite-backed) array numerifies to its element count, which it
    // cannot report: raku throws `X::Cannot::Lazy` (`Cannot .elems a lazy list`)
    // for every numeric coercion (`.Int`/`.Numeric`/`.Real`/`.Num`/prefix `+`).
    // Guard before the per-method arms, which would return the capped backing
    // length. (`.elems` itself is handled in `dispatch_core_numeric`.)
    if matches!(method, "Int" | "Numeric" | "Real" | "Num") && super::is_lazy_count_source(target) {
        return Some(super::range_elems_lazy_failure("elems"));
    }
    // `.Buf` / `.Blob` on any byte-string (Buf/Blob/utf8/blob8/...) returns a
    // value of the requested role carrying the same bytes. `utf8` (what
    // `.encode` produces) does Blob, so `.Buf`/`.Blob` reinterpret those bytes
    // as a plain `Buf[uint8]` / `Blob[uint8]`. Humming-Bird's HTTPServer uses
    // `"\r\n".encode.Buf` to build its constant delimiters.
    if matches!(method, "Buf" | "Blob")
        && let ValueView::Instance { class_name, .. } = target.view()
        && crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
    {
        // The `Blob`/`Buf` rows' implementation (`method_table::blob`).
        return Some(crate::builtins::method_table::blob::reinterpret(
            target, method,
        ));
    }
    // A numeric coercion method invoked on a *type object* of the same type
    // (`Int.Int`, `Num.Num`, `Complex.Complex`) is the identity coercion: raku
    // defines e.g. `method Int() { self }`, so for an undefined invocant (a
    // type object) the result is that same type object rather than a "No such
    // method" error. Zef's URI parser relies on `($auth<port> // Int).Int`
    // yielding the `Int` type object when the port is absent.
    //
    // Only `Int`/`Num`/`Complex` are handled here: raku returns the type object
    // cleanly for these, but `Rat.Rat`/`FatRat.FatRat` throw "must be an object
    // instance" (no `method Rat() { self }`), so those are left to fall through.
    if let ValueView::Package(name) = target.view()
        && matches!(method, "Int" | "Num" | "Complex")
        && (name.resolve() == method
            // `UInt.Int` also returns the invocant unchanged: UInt is a subset
            // of Int, so it inherits Int's `method Int() { self }`.
            || (method == "Int" && name.resolve() == "UInt"))
    {
        return Some(Some(Ok(target.clone())));
    }
    match method {
        "self" => {
            // For unhandled Failures, .self throws the exception
            if let ValueView::Instance {
                class_name,
                attributes,
                ..
            } = target.view()
                && class_name == "Failure"
                && !target.is_failure_handled()
                && let Some(ex) = attributes.as_map().get("exception")
            {
                let msg = ex.to_string_value();
                let mut err = crate::value::RuntimeError::new(msg);
                err.exception = Some(Box::new(ex.clone()));
                return Some(Some(Err(err)));
            }
            // `.self` hands out the *value*, not the container (unlike
            // `$x<>` -- both use `deitemize_element`, which already covers
            // Array/Hash/Scalar/ContainerRef/Slip): an itemized aggregate
            // must lose its `$` marker, matching `raku` (`{a=>1}.self.raku`
            // is `{:a(1)}`, not `${:a(1)}`). See issue #8490.
            Some(Some(Ok(target.clone().deitemize_element())))
        }
        "serial" => {
            // Any ordinary value is already its own serial (non-parallel) form,
            // so `.serial` returns the invocant's *value* (like `.self` --
            // see the comment there, and issue #8490's own note that
            // `.serial` shares `.self`'s model). Only a hyper/race pipeline
            // has a distinct serial form, which mutsu's hyper method
            // dispatch handles before reaching here.
            Some(Some(Ok(target.clone().deitemize_element())))
        }
        "clone" => {
            match target.view() {
                ValueView::Package(_) | ValueView::Nil => Some(Some(Ok(target.clone()))),
                // A lazy array (`my @a = 1, 2, 4 ... Inf`) clones to an
                // independent lazy list: separate cache/spec state, same
                // deterministic generator, so both keep reifying on demand.
                ValueView::LazyList(ll) => Some(Some(Ok(Value::lazy_list(crate::gc::Gc::new(
                    (**ll).clone(),
                ))))),
                // Clone the whole `ArrayData`, not just its elements, so
                // per-slot metadata (`initialized` deletion holes, `default`,
                // `shape`, element type) survives the copy — e.g.
                // `@a[3]:delete; my @b := @a.clone` keeps `@b[3]:exists` False
                // (S02-types/array.t test 108). Elements are `Value`-cloned
                // (shared handles), matching a shallow `.clone` — EXCEPT for a
                // shaped array, whose rows are its own storage: rakudo's
                // `.clone` gives independent containers per dimension
                // (rakudo#3334, S09-multidim/methods.t), and with in-place
                // element writes (container identity §3) shared rows would
                // alias `@a[1;1] = v` into the clone.
                ValueView::Array(items, kind) => {
                    let mut data = (**items).clone();
                    // The clone gets containers of its own: a slot promoted to a
                    // shared cell (a `for ... is rw` alias, a `:=` bind) must
                    // not keep aliasing the source (`my @c = @a.clone; @c[0] = 9`
                    // leaves `@a` alone, as in rakudo).
                    for item in data.live_mut().iter_mut() {
                        if item.is_container_ref() {
                            *item = item.deref_container();
                        }
                    }
                    if kind == crate::value::ArrayKind::Shaped || data.shape.is_some() {
                        fn clone_rows(v: &Value) -> Value {
                            match v.view() {
                                ValueView::Array(rows, k) => {
                                    let mut d = (**rows).clone();
                                    for item in d.live_mut().iter_mut() {
                                        *item = clone_rows(item);
                                    }
                                    Value::array_with_kind(crate::gc::Gc::new(d), k)
                                }
                                _ => v.clone(),
                            }
                        }
                        for item in data.live_mut().iter_mut() {
                            *item = clone_rows(item);
                        }
                    }
                    // Itemization is a property of the CONTAINER, not of the
                    // object, and `.clone` copies the object. So the clone comes
                    // back de-itemized, exactly as rakudo has it:
                    // `my $v = <a b c>; $v.raku` is `$("a", "b", "c")` but
                    // `$v.clone.raku` is `("a", "b", "c")` — which is why
                    // `my @a; @a = $v.clone` flattens where `@a = $v` does not.
                    // (Crane's `Crane::In.in(container, @path) = $value.clone`
                    // depends on exactly that: a List value must land in the
                    // target array as elements, not as one nested list.)
                    Some(Some(Ok(Value::array_with_kind(
                        crate::gc::Gc::new(data),
                        kind.decontainerize(),
                    ))))
                }
                ValueView::Hash(map) => Some(Some(Ok(Value::hash_with_data(Value::hash_arc(
                    (**map).clone(),
                ))))),
                // A Seq clone is a new eager Seq with the same elements.  A
                // Slip is likewise copied while retaining its itemization
                // marker; both are first-class values even though their
                // method surface is otherwise mostly provided by Mu.
                ValueView::Seq(items) => Some(Some(Ok(Value::seq(items.to_vec())))),
                ValueView::Slip(items) => Some(Some(Ok(
                    Value::slip(items.to_vec()).with_slip_itemized(target.slip_is_itemized())
                ))),
                ValueView::Set(data, mutable) => Some(Some(Ok(Value::set_parts(
                    crate::gc::Gc::new((**data).clone()),
                    mutable,
                )))),
                ValueView::Bag(data, mutable) => Some(Some(Ok(Value::bag_parts(
                    crate::gc::Gc::new((**data).clone()),
                    mutable,
                )))),
                ValueView::Mix(data, mutable) => Some(Some(Ok(Value::mix_parts(
                    crate::gc::Gc::new((**data).clone()),
                    mutable,
                )))),
                // The `Code.clone` row's implementation (`method_table::code`).
                ValueView::Sub(_) => Some(crate::builtins::method_table::code::pure_answer(
                    target, "clone",
                )),
                ValueView::Pair(key, value) => {
                    Some(Some(Ok(Value::pair(key.clone(), value.clone()))))
                }
                ValueView::ValuePair(key, value) => {
                    Some(Some(Ok(Value::value_pair(key.clone(), value.clone()))))
                }
                // Immutable value types: `.clone` (Mu.clone) yields a copy, which
                // for an immutable scalar is the value itself. These otherwise fell
                // through to the slow path and errored with NoSuchMethod.
                ValueView::Int(_)
                | ValueView::BigInt(_)
                | ValueView::Num(_)
                | ValueView::Str(_)
                | ValueView::Bool(_)
                | ValueView::Rat(..)
                | ValueView::FatRat(..)
                | ValueView::BigRat(..)
                | ValueView::Complex(..)
                | ValueView::Range(..)
                | ValueView::RangeExcl(..)
                | ValueView::RangeExclStart(..)
                | ValueView::RangeExclBoth(..)
                | ValueView::GenericRange { .. }
                | ValueView::Version { .. }
                | ValueView::Enum { .. } => Some(Some(Ok(target.clone()))),
                _ => Some(None), // fall through to slow path for instances etc.
            }
        }
        "defined" => {
            // Calling .defined on a Failure marks it as handled
            if let ValueView::Instance { class_name, .. } = target.view()
                && class_name == "Failure"
            {
                target.mark_failure_handled();
            }
            // For junctions, autothread .defined over eigenstates and collapse
            if let ValueView::Junction { kind, values } = target.view() {
                fn value_defined(v: &Value) -> bool {
                    match v.view() {
                        ValueView::Nil
                        | ValueView::Package(_)
                        | ValueView::ParametricRole { .. } => false,
                        ValueView::Slip(items) if items.is_empty() => false,
                        ValueView::Instance { class_name, .. } if class_name == "Failure" => false,
                        ValueView::Junction { kind, values } => {
                            let results: Vec<bool> = values.iter().map(value_defined).collect();
                            collapse_junction(&kind, &results)
                        }
                        _ => true,
                    }
                }
                fn collapse_junction(kind: &crate::value::JunctionKind, results: &[bool]) -> bool {
                    use crate::value::JunctionKind;
                    match kind {
                        JunctionKind::Any => results.iter().any(|&b| b),
                        JunctionKind::All => results.iter().all(|&b| b),
                        JunctionKind::One => results.iter().filter(|&&b| b).count() == 1,
                        JunctionKind::None => results.iter().all(|&b| !b),
                    }
                }
                let results: Vec<bool> = values.iter().map(value_defined).collect();
                let collapsed = collapse_junction(&kind, &results);
                Some(Some(Ok(Value::truth(collapsed))))
            } else {
                Some(Some(Ok(Value::truth(match target.view() {
                    ValueView::Nil | ValueView::Package(_) | ValueView::ParametricRole { .. } => {
                        false
                    }
                    ValueView::Slip(items) if items.is_empty() => false,
                    ValueView::Instance { class_name, .. } if class_name == "Failure" => false,
                    ValueView::VarRef { value, .. } => {
                        crate::runtime::types::value_is_defined(value)
                    }
                    _ => true,
                }))))
            }
        }
        // `.DEFINITE` is the concreteness primitive: True iff `target` is a
        // concrete instance rather than a type object. It is NOT `.defined`
        // (which Failure overrides to False) — a `Failure.new(...)` instance is
        // still definite, so it must NOT be special-cased here.
        "DEFINITE" => Some(Some(Ok(Value::truth(crate::runtime::value_is_definite(
            target,
        ))))),
        "WHICH" => Some(Some(Ok(super::which::which_of(target)))),
        "WHERE" => {
            // A NativeCall `Pointer` reports a *real, readable* address: bindings
            // legitimately walk the object's memory through it. `NativeHelpers`'
            // `MoarVM::Guts::REPRs` does exactly that to derive the offset of an
            // object's payload from its start, by scanning `.WHERE` for the
            // pointer value it just stored. See `native_object_where`.
            // The class is matched on its last `::` component: the prelude's
            // `Pointer` picks up the enclosing package when it is prepended
            // inside a module (`Probe::Pointer`), the same "one class, several
            // spellings" problem `cstruct_class_name` documents. Getting this
            // wrong is not a cosmetic miss — `.WHERE` would fall through to the
            // identity hash, and a binding that dereferences the result reads
            // wild memory.
            if let ValueView::Instance {
                class_name,
                attributes,
                ..
            } = target.view()
                && crate::qualified::last_segment(class_name).as_str() == "Pointer"
            {
                let addr = attributes
                    .as_map()
                    .get("address")
                    .map(|v| runtime::to_int(v) as usize)
                    .unwrap_or(0);
                return Some(Some(Ok(Value::int(
                    runtime::nativecall::native_object_where(addr) as i64,
                ))));
            }
            // Rakudo: `.WHERE` is the object's memory address as an Int.
            // mutsu's scalar values are unboxed (no stable address to report),
            // so derive a per-identity-stable Int from the WHICH identity
            // string instead: reference types embed their allocation address /
            // object id in it, value types their value. This preserves the
            // observable contract (same object => same WHERE, distinct
            // objects => distinct) without pinnable addresses.
            let which_str = match dispatch(target, "WHICH") {
                Some(Some(Ok(objat))) => match objat.view() {
                    ValueView::Instance { attributes, .. } => attributes.as_map().objat_which(),
                    _ => None,
                },
                _ => None,
            }?;
            use std::hash::{Hash, Hasher};
            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            which_str.hash(&mut hasher);
            Some(Some(Ok(Value::int((hasher.finish() >> 1) as i64))))
        }
        // Cost: O(1) for a Str invocant (emptiness test).
        // Cost: O(1) for an Array/List invocant (`truthy` tests emptiness only).
        "Bool" => {
            if matches!(target.view(), ValueView::Instance { .. })
                && (target.does_check("Real") || target.does_check("Numeric"))
            {
                Some(None)
            } else if matches!(
                target.view(),
                ValueView::Regex(_)
                    | ValueView::RegexWithAdverbs(..)
                    | ValueView::Routine { is_regex: true, .. }
            ) {
                // Regex.Bool needs to smartmatch against $_, which requires
                // runtime context. Fall through to the runtime handler.
                Some(None)
            } else {
                // Calling .Bool on a Failure marks it as handled
                if let ValueView::Instance { class_name, .. } = target.view()
                    && class_name == "Failure"
                {
                    target.mark_failure_handled();
                }
                Some(Some(Ok(Value::truth(target.truthy()))))
            }
        }
        "Str" | "Stringy" => Some(match target.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Failure" => {
                // Using a Failure in string context throws the wrapped exception.
                if let Some(ex) = attributes.as_map().get("exception") {
                    Some(Err(RuntimeError::from_exception_value(ex.clone())))
                } else {
                    Some(Err(RuntimeError::new("Failed")))
                }
            }
            ValueView::Package(_) | ValueView::Instance { .. } => None,
            ValueView::LazyList(_) => None, // fall through to runtime to force the list
            // A lazy (infinite-backed) array stringifies to a bounded `...`
            // placeholder rather than materializing its capped backing.
            ValueView::Array(_, crate::value::ArrayKind::Lazy) => Some(Ok(Value::str_from("..."))),
            ValueView::Enum { .. } => None, // fall through to enum dispatch for string enum support
            ValueView::Str(s) if s.as_str() == "IO::Special" => Some(Ok(Value::str_from(""))),
            ValueView::Rat(_, 0) | ValueView::FatRat(_, 0) => {
                // Zero-denominator Rat/FatRat .Str throws X::Numeric::DivideByZero
                None // fall through to runtime for exception with proper context
            }
            // A collection stringifies every element, so a zero-denominator
            // Rational inside it dies like its own `.Str` (GH #9608). The walk
            // runs twice only on the error path.
            ValueView::Array(..)
            | ValueView::Seq(..)
            | ValueView::Slip(..)
            | ValueView::Hash(..)
            | ValueView::Pair(..)
            | ValueView::ValuePair(..)
                if crate::runtime::utils::zero_denominator_rational_error(target).is_some() =>
            {
                crate::runtime::utils::zero_denominator_rational_error(target).map(Err)
            }
            // A list holding an `Instance` element needs the interpreter: the
            // element's class may define its own `Str`, which this pure
            // renderer cannot call (it would print the `ClassName()` fallback
            // -- `~@a` then disagreed with `@a.join("")` for the same array).
            // `dispatch_list_str_method` resolves the elements and re-renders.
            _ if crate::Interpreter::list_str_needs_interpreter(target) => None,
            // A `Regex` warns and yields the EMPTY string rather than its
            // source text, and the warning needs the interpreter -- see
            // `Interpreter::regex_str_coercion`.
            ValueView::Regex(_)
            | ValueView::RegexWithAdverbs(..)
            | ValueView::Routine { is_regex: true, .. } => None,
            // Cost: O(1) for a plain Str invocant (the value is shared, not
            // copied); O(n) otherwise, n = chars of the rendered string.
            _ if target.is_str_value() => Some(Ok(target.clone())),
            _ => Some(Ok(Value::str(target.to_string_value()))),
        }),
        "Int" => {
            // `.Int` on a type object: `Int.Int`/`UInt.Int` identity is handled
            // by the same-type check above; the concrete Cool types only define
            // `Int` multis with a `:D` invocant, so their type objects throw
            // X::Parameter::InvalidConcreteness. Every other type object (Any,
            // Mu, Cool, IntStr, user classes, roles) inherits Mu's coercion —
            // warn "uninitialized ... in numeric context" and return 0 — which
            // lives on the slow path so a user-defined `.Int` dispatches first.
            if let ValueView::Package(name) = target.view() {
                let n = name.resolve();
                return match n.as_str() {
                    "Num" | "Str" | "Rat" | "FatRat" | "Complex" => {
                        Some(Some(Err(RuntimeError::parameter_invalid_concreteness(
                            &n, &n, "Int", "self", true, true,
                        ))))
                    }
                    _ => Some(None),
                };
            }
            let result = match target.view() {
                // A real number: the numeric types' `Int` rows' implementation
                // (ADR-11276, `method_table::coerce`).
                ValueView::Int(_)
                | ValueView::BigInt(_)
                | ValueView::Num(_)
                | ValueView::Rat(..)
                | ValueView::FatRat(..)
                | ValueView::BigRat(..) => crate::builtins::method_table::coerce::int_of(target)?,
                // Cost: O(d^2), d = digits (num-bigint radix parse; a few O(n) copies first).
                ValueView::Str(s) => {
                    if s.trim().is_empty() {
                        // An empty or whitespace-only string coerces to 0, like
                        // `"".Numeric` (a defined-but-empty string, no warning).
                        Value::int(0)
                    } else if let Some(v) = parse_raku_int_from_str(&s) {
                        v
                    } else if str_numifies_to_complex(&s).is_some() {
                        // `"1+2i".Int` is `Complex.Int`: the runtime tests the
                        // imaginary part (`Interpreter::dispatch_complex_to_real`).
                        return Some(None);
                    } else if let Some(v) =
                        runtime::str_numeric::parse_raku_str_to_numeric(s.trim())
                            .as_ref()
                            .and_then(crate::builtins::method_table::coerce::int_of)
                    {
                        // raku's `Str.Int` parses via the full numeric grammar
                        // and truncates the result, so numeric string forms the
                        // strict integer parser above rejects — radix `:16<ff>`,
                        // rational `3/4`, ... — still coerce like their value.
                        v
                    } else {
                        // Return a Failure (lazy exception) instead of throwing.
                        return Some(Some(Ok(str_numeric_failure(&s))));
                    }
                }
                ValueView::Bool(b) => Value::int(if b { 1 } else { 0 }),
                // A Complex coerces to Int via its real part when its imaginary
                // part is `≅ 0` (`$*TOLERANCE`); otherwise it is not Real and
                // throws X::Numeric::Real. Reading the dynamic variable needs the
                // interpreter, so this falls through to
                // `Interpreter::dispatch_complex_to_real`.
                ValueView::Complex(..) => return Some(None),
                ValueView::Hash(h) => Value::int(h.len() as i64),
                ValueView::Array(items, ..) => Value::int(items.len() as i64),
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) => {
                    Value::int(buf_len_or_zero(&attributes) as i64)
                }
                // A StrDistance's `.Int` is the edit distance between its
                // before/after strings, matching `+$str-dist` / `.Numeric`.
                ValueView::Instance { class_name, .. } if class_name == "StrDistance" => {
                    Value::int(
                        super::dispatch_core_math::cool_instance_numeric(target).unwrap_or(0.0)
                            as i64,
                    )
                }
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        "UInt" => {
            // First coerce to Int, then check non-negative
            let int_result = match target.view() {
                ValueView::Int(i) => Some(Value::int(i)),
                ValueView::BigInt(_) => Some(target.clone()),
                ValueView::Num(f) if f.is_finite() => Some(Value::int(f.trunc() as i64)),
                ValueView::Rat(n, d) if d != 0 => Some(Value::int(n / d)),
                // A Complex coerces to UInt like `.Int` (`(5+0i).UInt` is 5): the
                // runtime checks its imaginary part against `$*TOLERANCE`.
                ValueView::Complex(..) => return Some(None),
                ValueView::Bool(b) => Some(Value::int(if b { 1 } else { 0 })),
                ValueView::Str(s) if s.trim().is_empty() => Some(Value::int(0)),
                // Cost: O(d^2), d = digits (num-bigint radix parse; a few O(n) copies first).
                ValueView::Str(s) => {
                    if let Some(v) = parse_raku_int_from_str(&s) {
                        Some(v)
                    } else if str_numifies_to_complex(&s).is_some() {
                        // As `.Int`: coerced as the `Complex` it numifies to.
                        return Some(None);
                    } else if let Some(v) =
                        runtime::str_numeric::parse_raku_str_to_numeric(s.trim())
                            .as_ref()
                            .and_then(crate::builtins::method_table::coerce::int_of)
                    {
                        // Same numeric-string forms as `.Int` (radix `:16<ff>`,
                        // rational `3/4`, ...): numify then truncate, then the
                        // non-negative check below applies.
                        Some(v)
                    } else {
                        // Invalid string: same X::Str::Numeric Failure (with the `⏏`
                        // position marker) as `.Int`, rather than a hand-rolled
                        // message that hard-codes the wrong reason and drops the marker.
                        return Some(Some(Ok(str_numeric_failure(&s))));
                    }
                }
                _ => None,
            };
            if let Some(int_val) = int_result {
                // Check non-negative
                let is_neg = match int_val.view() {
                    ValueView::Int(i) => i < 0,
                    ValueView::BigInt(n) => n.sign() == num_bigint::Sign::Minus,
                    _ => false,
                };
                if is_neg {
                    // Rakudo returns a soft X::OutOfRange Failure here, not a
                    // thrown exception — callers may test/reject it (e.g. a
                    // signature bind) without ever exploding.
                    let msg = format!(
                        "Coercion to UInt out of range. Is: {}, should be in 0..^Inf",
                        int_val.to_string_value()
                    );
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("message".to_string(), Value::str(msg));
                    attrs.insert(
                        "what".to_string(),
                        Value::str("Coercion to UInt".to_string()),
                    );
                    attrs.insert("got".to_string(), int_val);
                    attrs.insert("range".to_string(), Value::str("0..^Inf".to_string()));
                    let ex = Value::make_instance(Symbol::intern("X::OutOfRange"), attrs);
                    let mut failure_attrs = std::collections::HashMap::new();
                    failure_attrs.insert("exception".to_string(), ex);
                    failure_attrs.insert("handled".to_string(), Value::FALSE);
                    return Some(Some(Ok(Value::make_instance(
                        Symbol::intern("Failure"),
                        failure_attrs,
                    ))));
                }
                Some(Some(Ok(int_val)))
            } else {
                Some(None)
            }
        }
        // Cost: O(n), n = chars of the invocant.
        "Version" => match target.view() {
            // Cool.Version: Version.new(self.Str). A Version invocant is
            // already its own version.
            ValueView::Version { .. } => Some(Some(Ok(target.clone()))),
            ValueView::Str(_)
            | ValueView::Int(_)
            | ValueView::BigInt(_)
            | ValueView::Num(_)
            | ValueView::Rat(_, _)
            | ValueView::BigRat(_, _)
            | ValueView::FatRat(_, _)
            | ValueView::Bool(_) => {
                let s = target.to_string_value();
                if s.is_empty() {
                    Some(Some(Ok(Value::version(Vec::new(), false, false))))
                } else {
                    Some(Some(Ok(Value::version_from_str(&s))))
                }
            }
            _ => None,
        },
        "Num" => {
            // `.Num` on a concrete-only Cool type's type object dies with
            // X::Parameter::InvalidConcreteness; `Num.Num` is identity (below)
            // and the other type objects inherit Mu/Cool's warn-and-0.
            if let ValueView::Package(name) = target.view() {
                let n = name.resolve();
                let expected = match n.as_str() {
                    "Int" | "Str" | "Complex" => Some(n.as_str()),
                    "UInt" => Some("Int"),
                    "Rat" | "FatRat" => Some("Rational"),
                    _ => None,
                };
                if let Some(expected) = expected {
                    return Some(Some(Err(RuntimeError::parameter_invalid_concreteness(
                        expected, &n, "Num", "self", true, true,
                    ))));
                }
            }
            // A real number: the numeric types' `Num` rows' implementation
            // (ADR-11276, `method_table::coerce`).
            if let Some(result) = crate::builtins::method_table::coerce::num_of(target) {
                return Some(Some(Ok(result)));
            }
            let result = match target.view() {
                // Cost: O(d^2) for a d-digit integer string, O(n) otherwise (as `.Numeric`,
                // plus a trimmed copy).
                ValueView::Str(s) => {
                    let trimmed = s.trim();
                    if trimmed.is_empty() {
                        // An empty or whitespace-only string coerces to 0, like
                        // `"".Numeric`.
                        Value::num(0.0)
                    } else {
                        // Normalize U+2212 MINUS SIGN to ASCII hyphen-minus, then use
                        // the canonical Raku numeric parser (radix prefixes,
                        // underscores, rationals, strict Inf/NaN) so `.Num` agrees
                        // with `.Numeric`/`.Int`/prefix `+`. `.Num` always yields a Num.
                        let normalized = trimmed.replace('\u{2212}', "-");
                        if str_numifies_to_complex(&normalized).is_some() {
                            // As `.Int`: coerced as the `Complex` it numifies to.
                            return Some(None);
                        }
                        if let Some(v) =
                            crate::runtime::str_numeric::parse_raku_str_to_numeric(&normalized)
                        {
                            let f = v.to_f64();
                            // "-0"/"-0.0" parse as Int/Rat zero, which has no
                            // sign — but `.Num` must yield the IEEE negative
                            // zero (roast S32-num/negative-zero.t). Restore the
                            // sign from the source string when the magnitude is
                            // zero. (The scientific path, e.g. "-0e0", already
                            // returns a signed Num.)
                            let f = if f == 0.0
                                && f.is_sign_positive()
                                && normalized.starts_with('-')
                            {
                                -0.0
                            } else {
                                f
                            };
                            Value::num(f)
                        } else {
                            // An invalid string yields a lazy X::Str::Numeric Failure,
                            // mirroring `.Int` (not an eager X::AdHoc RuntimeError).
                            return Some(Some(Ok(str_numeric_failure(&s))));
                        }
                    }
                }
                ValueView::Bool(b) => Value::num(if b { 1.0 } else { 0.0 }),
                // The runtime checks the imaginary part against `$*TOLERANCE`
                // (`Interpreter::dispatch_complex_to_real`).
                ValueView::Complex(..) => return Some(None),
                ValueView::Array(items, ..) => Value::num(items.len() as f64),
                ValueView::Seq(items) => Value::num(items.len() as f64),
                ValueView::Slip(items) => Value::num(items.len() as f64),
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        "Real" => {
            // Calling .Real on a type object (Package) should warn and return zero
            if let ValueView::Package(name) = target.view() {
                let type_name = name.resolve();
                let zero = match type_name.as_str() {
                    "Int" | "IntStr" => Value::int(0),
                    "Rat" | "FatRat" | "RatStr" => Value::rat_raw(0, 1),
                    "Num" | "NumStr" | "Complex" | "ComplexStr" => Value::num(0.0),
                    _ => return Some(None),
                };
                let msg = format!(
                    "Use of uninitialized value of type {} in numeric context",
                    type_name
                );
                return Some(Some(Err(RuntimeError::warn_signal_with_resume(msg, zero))));
            }
            let result = match target.view() {
                ValueView::Int(i) => Value::int(i),
                ValueView::BigInt(_) => target.clone(),
                ValueView::Num(f) => Value::num(f),
                ValueView::Rat(n, d) => Value::rat_raw(n, d),
                ValueView::FatRat(n, d) => Value::fat_rat_raw(n, d),
                ValueView::BigRat(n, d) => Value::bigrat(n.clone(), d.clone()),
                ValueView::Bool(b) => Value::int(if b { 1 } else { 0 }),
                // A Complex is Real when its imaginary part is `≅ 0`; the runtime
                // reads `$*TOLERANCE` (`Interpreter::dispatch_complex_to_real`).
                ValueView::Complex(..) => return Some(None),
                // Cost: O(d^2) for a d-digit integer string, O(n) otherwise (as `.Numeric`).
                ValueView::Str(s) => {
                    // `.Real` yields the natural numeric type (Int/Rat/Num); use the
                    // canonical parser so radix prefixes and underscores work and the
                    // result agrees with `.Numeric`.
                    if str_numifies_to_complex(&s).is_some() {
                        // As `.Int`: coerced as the `Complex` it numifies to.
                        return Some(None);
                    }
                    if let Some(v) =
                        crate::runtime::str_numeric::parse_raku_str_to_numeric(s.trim())
                    {
                        v
                    } else {
                        // Same X::Str::Numeric Failure (typed, with the `⏏` marker)
                        // as `.Int`/`.Numeric`.
                        return Some(Some(Ok(str_numeric_failure(&s))));
                    }
                }
                ValueView::Array(items, ..) => Value::int(items.len() as i64),
                ValueView::Hash(h) => Value::int(h.len() as i64),
                ValueView::Seq(items) => Value::int(items.len() as i64),
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        "Numeric" => {
            // Calling .Numeric on a type object (Package) should warn and return zero
            if let ValueView::Package(name) = target.view() {
                let type_name = name.resolve();
                let zero = match type_name.as_str() {
                    "Int" | "IntStr" => Value::int(0),
                    "Rat" | "RatStr" => Value::rat_raw(0, 1),
                    "FatRat" => Value::fat_rat_raw(0, 1),
                    "Num" | "NumStr" => Value::num(0.0),
                    "Complex" | "ComplexStr" => Value::complex(0.0, 0.0),
                    _ => return Some(None),
                };
                let msg = format!(
                    "Use of uninitialized value of type {} in numeric context",
                    type_name
                );
                return Some(Some(Err(RuntimeError::warn_signal_with_resume(msg, zero))));
            }
            let result = match target.view() {
                ValueView::Int(i) => Value::int(i),
                ValueView::BigInt(_) => target.clone(),
                ValueView::Num(f) => Value::num(f),
                // Rational values stay exact under `.Numeric`; converting a
                // `Rat` through f64 loses large denominators (and turns
                // `1/100000` into the nearby `1/99999` when it is converted
                // back to Rat). `Numeric.Rat` is the identity in Rakudo.
                ValueView::Rat(_, _) | ValueView::FatRat(_, _) | ValueView::BigRat(_, _) => {
                    target.clone()
                }
                // Cost: O(d^2) for a d-digit integer string (num-bigint radix parse), O(n)
                // otherwise, n = chars of the invocant.
                ValueView::Str(s) => {
                    if let Some(v) = crate::runtime::str_numeric::parse_raku_str_to_numeric(&s) {
                        v
                    } else {
                        // Same X::Str::Numeric Failure (typed, with the `⏏` marker)
                        // as `.Int`.
                        return Some(Some(Ok(str_numeric_failure(&s))));
                    }
                }
                ValueView::Bool(b) => Value::int(if b { 1 } else { 0 }),
                // `.Numeric` preserves Complex values. Converting a Complex to
                // a real type belongs to `.Num`/`.Real`, where a non-zero
                // imaginary component is rejected; dropping that component
                // here made `Complex.Numeric.Rat` silently discard it.
                ValueView::Complex(_, _) => target.clone(),
                // Cost: O(1) (`.Numeric` / `+@a` of an array is its element count).
                ValueView::Array(items, ..) => Value::int(items.len() as i64),
                ValueView::Hash(h) => Value::int(h.len() as i64),
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) => {
                    Value::int(buf_len_or_zero(&attributes) as i64)
                }
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if is_stash_class_name(class_name.as_str()) => {
                    let count = match attributes.as_map().get("symbols").map(Value::view) {
                        Some(ValueView::Hash(map)) => map.len() as i64,
                        _ => 0,
                    };
                    Value::int(count)
                }
                // A StrDistance (`$str ~~ tr/a/b/`) numifies to the edit distance
                // between its before/after strings, so `+($str ~~ tr/old/new/)`
                // is the number of changes, not 0.
                ValueView::Instance { class_name, .. } if class_name == "StrDistance" => {
                    match super::dispatch_core_math::cool_instance_numeric(target) {
                        Some(n) if n.fract() == 0.0 && n.is_finite() => Value::int(n as i64),
                        Some(n) => Value::num(n),
                        None => Value::int(0),
                    }
                }
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        _ => None,
    }
}
