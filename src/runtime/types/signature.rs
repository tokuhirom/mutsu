use super::*;

pub(in crate::runtime) fn positional_values_from_unpack_target(value: &Value) -> Vec<Value> {
    // A variable passed by reference (e.g. `f(@a)` / `f($items)`) arrives as a
    // varref Capture wrapping the real value; unwrap before destructuring.
    if let Some((_, inner)) = varref_from_value(value) {
        return positional_values_from_unpack_target(&inner);
    }
    // `grep` promotes each matched source slot to a shared `ContainerRef` cell so a
    // writeback loop can mutate through it. A later destructuring bind of that element
    // (`@a.grep(...); @a.map(-> [$x, $y] {...})`) has to look through the cell, or it
    // sees one opaque value and reports "Too few positional arguments".
    if let ValueView::ContainerRef(cell) = value.view() {
        let inner = cell.lock().unwrap().clone();
        return positional_values_from_unpack_target(&inner);
    }
    match value.view() {
        ValueView::Capture { positional, .. } => (*positional).clone(),
        // For sub-signature destructuring, a list is always taken apart
        // element-wise even when itemized — e.g. `$(3, 4)` or an array variable
        // bound to a single destructuring positional via the single-argument
        // rule. `value_to_list` would otherwise return an itemized list as a
        // single opaque element, which fails the destructuring arity check.
        ValueView::Array(data, _) => data.items().clone(),
        ValueView::Seq(items) => items.to_vec(),
        ValueView::Slip(items) => (**items).clone(),
        _ => crate::runtime::value_to_list(value),
    }
}

/// Whether a sub-signature destructures a `Pair` purely by its named parts, as
/// `Pair (:key($k), :value($v))` does.
///
/// A `Pair`'s capture is `\(:key(…), :value(…))` — two named parts and no
/// positional one (`(2 => 'x').Capture.list` is `()` in rakudo) — but
/// `positional_values_from_unpack_target` reports the pair itself as a
/// one-element positional list, which the positional-destructure forms rely on.
/// So such a sub-signature leaves that element unconsumed by construction, and
/// the leftover-positional check has to sit this one out.
fn destructures_pair_by_name(value: &Value, sub_params: &[ParamDef]) -> bool {
    matches!(
        value.unwrap_varref().view(),
        ValueView::Pair(..) | ValueView::ValuePair(..)
    ) && !sub_params.is_empty()
        && sub_params.iter().all(|p| p.named)
}

pub(in crate::runtime) fn varref_from_value(value: &Value) -> Option<(String, Value)> {
    indexed_varref_from_value(value).map(|(name, inner, _)| (name, inner))
}

pub(in crate::runtime) fn indexed_varref_from_value(
    value: &Value,
) -> Option<(String, Value, Option<usize>)> {
    let (name, inner, index) = value.as_varref()?;
    Some((name.resolve(), inner.clone(), index.map(|i| i as usize)))
}

/// Wrap an integer value to fit within a native integer type's range
/// for function parameter binding.
/// For full-width native types (int/int64/uint/uint64), out-of-range values
/// cause an error (cannot unbox too-wide bigint into native integer).
pub(in crate::runtime) fn wrap_native_int_for_binding(
    constraint: &str,
    val: Value,
) -> Result<Value, crate::value::RuntimeError> {
    use crate::runtime::native_types;
    use num_bigint::BigInt as NumBigInt;
    use num_traits::ToPrimitive;

    let (base, _) = strip_type_smiley(constraint);
    if !native_types::is_native_int_type(base) {
        return Ok(val);
    }
    // Bool `does Int`: unbox it to 1/0 before native-int wrapping, so
    // `sub f(int $x); f(True)` binds 1 like raku.
    let val = native_types::unbox_bool_to_native_int(val);
    // Type objects cannot be unboxed to native types
    if let ValueView::Package(pkg_name) = val.view() {
        return Err(crate::value::RuntimeError::new(format!(
            "Cannot unbox a type object ({}) to {}.",
            pkg_name.resolve(),
            base
        )));
    }
    let big_val = match val.view() {
        ValueView::Int(n) => NumBigInt::from(n),
        ValueView::BigInt(n) => (**n).clone(),
        _ => return Ok(val),
    };
    if native_types::is_in_native_range(base, &big_val) {
        return Ok(val);
    }
    // Full-width signed native types don't wrap — they should throw on overflow.
    if matches!(base, "int" | "int64") {
        return Err(crate::value::RuntimeError::new(format!(
            "Cannot unbox {} bit wide bigint into native integer",
            big_val.bits()
        )));
    }
    // Full-width unsigned types: positive overflow throws, negative values wrap.
    if matches!(base, "uint" | "uint64") && big_val > NumBigInt::from(0u64) {
        return Err(crate::value::RuntimeError::new(format!(
            "Cannot unbox {} bit wide bigint into native integer",
            big_val.bits()
        )));
    }
    let wrapped = native_types::wrap_native_int(base, &big_val);
    Ok(wrapped
        .to_i64()
        .map(Value::int)
        .unwrap_or_else(|| Value::bigint(wrapped)))
}

/// Strip the [`Value::varref`] wrapper the caller tagged an lvalue argument
/// with, once the binder has taken the source name it needs. On the light-call
/// path this runs once per parameter, so it must not materialize the name.
pub(crate) fn unwrap_varref_value(value: Value) -> Value {
    match value.as_varref() {
        Some((_, inner, _)) => inner.clone(),
        None => value,
    }
}

/// Flatten a list of RAW caller arguments for `*@` (flattening slurpy)
/// parameter binding / a slurpy-taking builtin (`sprintf`, `catdir`, ...).
/// Every caller of this function passes un-pre-processed values (an
/// argument list, or a single argument wrapped in a one-element slice) --
/// preserve that contract when adding a new call site.
///
/// A **real Array** (the `[...]` constructor or an `@`-var) contributes its
/// OWN elements to `out`, one level, and no further: every element of a
/// real Array lives in an implicit Scalar container in Raku, so a nested
/// Array element is never itself decomposed (`f [[1,2],[3,4]]` is 2
/// elements, not 4; `f [1,[2,3]]` is `1, $[2,3]`, not `1, 2, 3`;
/// `sprintf("%d %d %d", @a)` where `@a = 1,2,3` still sees 3 elements).
/// A **List** (uncontainerized -- `(1,(2,3))`, Seq, Slip, Range) flattens
/// fully, recursively. An itemized container (`$(...)`, `$[...]`) is
/// preserved as a single element.
pub(crate) fn flatten_into_slurpy(values: &[Value], out: &mut Vec<Value>) {
    for val in values {
        match val.view() {
            ValueView::Array(_, kind) if kind.is_itemized() => {
                out.push(val.clone());
            }
            ValueView::Array(arr, kind) if kind.is_real_array() => {
                out.extend(arr.iter().cloned());
            }
            ValueView::Array(arr, _kind) => {
                flatten_into_slurpy(&arr, out);
            }
            ValueView::Seq(items) => {
                flatten_into_slurpy(&items, out);
            }
            ValueView::Slip(items) => {
                flatten_into_slurpy(&items, out);
            }
            ValueView::Range(a, b) => {
                let end = if b == i64::MAX {
                    b.min(a.saturating_add(crate::runtime::utils::MAX_LAZY_RANGE_PREFIX))
                } else {
                    b
                };
                if end >= a {
                    for i in a..=end {
                        out.push(Value::int(i));
                    }
                }
            }
            ValueView::RangeExcl(a, b) => {
                let end = if b == i64::MAX {
                    b.min(a.saturating_add(crate::runtime::utils::MAX_LAZY_RANGE_PREFIX))
                } else {
                    b
                };
                if end > a {
                    for i in a..end {
                        out.push(Value::int(i));
                    }
                }
            }
            ValueView::RangeExclStart(a, b) => {
                let start = a.saturating_add(1);
                let end = if b == i64::MAX {
                    b.min(start.saturating_add(crate::runtime::utils::MAX_LAZY_RANGE_PREFIX))
                } else {
                    b
                };
                if end >= start {
                    for i in start..=end {
                        out.push(Value::int(i));
                    }
                }
            }
            ValueView::RangeExclBoth(a, b) => {
                let start = a.saturating_add(1);
                let end = if b == i64::MAX {
                    b.min(start.saturating_add(crate::runtime::utils::MAX_LAZY_RANGE_PREFIX))
                } else {
                    b
                };
                if end > start {
                    for i in start..end {
                        out.push(Value::int(i));
                    }
                }
            }
            ValueView::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                // Only integer-endpoint ranges expand via the fast Int path. A
                // string range (`"a".."z"`) must expand as its character/string
                // sequence, so delegate to `value_to_list` which knows how (a
                // plain `to_int` would collapse both endpoints to 0).
                if matches!(
                    (start.as_ref().view(), end.as_ref().view()),
                    (ValueView::Int(_), ValueView::Int(_))
                ) {
                    // Both endpoints are `Int` (i64), which cannot represent
                    // an infinite range (that would be a `Num` Inf/Whatever
                    // endpoint, handled by the `value_to_list` fallback
                    // below) — so this range is always finite and expands
                    // in full, uncapped.
                    let a = crate::runtime::to_int(start);
                    let b = crate::runtime::to_int(end);
                    let s = if excl_start { a + 1 } else { a };
                    let e = if excl_end { b } else { b + 1 };
                    for i in s..e {
                        out.push(Value::int(i));
                    }
                } else {
                    out.extend(crate::runtime::utils::value_to_list(val));
                }
            }
            _ => {
                out.push(val.clone());
            }
        }
    }
}

pub(crate) fn make_varref_value(name: String, value: Value, source_index: Option<usize>) -> Value {
    Value::varref(Symbol::intern(&name), value, source_index.map(|i| i as u32))
}

/// Sentinel prefix marking a `*@v is rw`/`is raw` slurpy element writeback entry
/// inside the `rw_bindings` list. A slurpy `is raw`/`is rw` param aliases each
/// caller argument: when the body mutates `@v[i]` (or `for @v { $_++ }`), the
/// new value must flow back to the i-th caller source. A plain `(param, source)`
/// rw-binding can't express this (the whole slurpy array maps to many distinct
/// sources, possibly at distinct indices), so each element gets its own entry
/// whose `param_name` encodes the slurpy env key, the element index in the
/// slurpy array, and the index within the source (for an array source). The
/// `source_name` stays clean (`"a"`, `"@arr"`) so the call-site
/// `pending_rw_writeback_sources` collection refreshes the right caller slot.
const SLURPY_RW_MARKER: &str = "\u{1}S\u{1}";

/// Encode a slurpy element writeback into an `rw_bindings` param-name slot.
/// `slurpy_key` is the callee env key (`"@v"`), `elem_idx` the element's position
/// in the slurpy array, `src_idx` the index within an array source (`None` for a
/// scalar source).
pub(in crate::runtime) fn encode_slurpy_rw_param(
    slurpy_key: &str,
    elem_idx: usize,
    src_idx: Option<usize>,
) -> String {
    let src = match src_idx {
        Some(i) => i as i64,
        None => -1,
    };
    format!("{SLURPY_RW_MARKER}{slurpy_key}\u{1}{elem_idx}\u{1}{src}")
}

/// Decode a slurpy element writeback param-name. Returns
/// `(slurpy_key, elem_idx, src_idx)` or `None` if `param` is an ordinary
/// rw-binding param name.
pub(crate) fn decode_slurpy_rw_param(param: &str) -> Option<(&str, usize, Option<usize>)> {
    let rest = param.strip_prefix(SLURPY_RW_MARKER)?;
    // From the right: src, elem, key. The key (`"@v"`) never contains \u{1}.
    let mut parts = rest.rsplitn(3, '\u{1}');
    let src = parts.next()?;
    let elem = parts.next()?;
    let key = parts.next()?;
    let elem_idx: usize = elem.parse().ok()?;
    let src_idx = match src.parse::<i64>().ok()? {
        i if i >= 0 => Some(i as usize),
        _ => None,
    };
    Some((key, elem_idx, src_idx))
}

pub(in crate::runtime) fn sigilless_alias_key(name: &str) -> String {
    format!("__mutsu_sigilless_alias::{}", name)
}

pub(in crate::runtime) fn sigilless_readonly_key(name: &str) -> String {
    format!("__mutsu_sigilless_readonly::{}", name)
}

/// Collect the `Pair`/`ValuePair` elements of a list into a named map. Used
/// when a list of pairs is destructured by named sub-signature params.
fn pairs_in_list_to_named(items: &[Value]) -> std::collections::HashMap<String, Value> {
    let mut out = std::collections::HashMap::new();
    for item in items {
        match item.view() {
            ValueView::Pair(key, val) => {
                out.insert(key.clone(), val.clone());
            }
            ValueView::ValuePair(key, val) => {
                out.insert(key.to_string_value(), val.clone());
            }
            _ => {}
        }
    }
    out
}

pub(in crate::runtime) fn named_values_from_unpack_target(
    value: &Value,
) -> std::collections::HashMap<String, Value> {
    // Unwrap a varref Capture (a by-reference variable argument) to the real
    // value before extracting named entries.
    if let Some((_, inner)) = varref_from_value(value) {
        return named_values_from_unpack_target(&inner);
    }
    match value.view() {
        ValueView::Capture { named, .. } => (*named).clone(),
        ValueView::Hash(map) => map.map.clone(),
        ValueView::Pair(key, val) => {
            let mut out = std::collections::HashMap::new();
            out.insert(key.clone(), val.clone());
            out.insert("key".to_string(), Value::str(key.clone()));
            out.insert("value".to_string(), val.clone());
            out
        }
        // A `ValuePair` destructures exactly like a `Pair` — the two differ only
        // in whether the *call site* meant the pair as a named argument, which is
        // nothing to do with `Pair.Capture`. A hash entry (`%h.map(-> (:$k) {...})`)
        // arrives as a ValuePair, so without this arm it bound nothing.
        ValueView::ValuePair(key, val) => {
            let mut out = std::collections::HashMap::new();
            out.insert(key.to_string_value(), (*val).clone());
            out.insert("key".to_string(), (*key).clone());
            out.insert("value".to_string(), (*val).clone());
            out
        }
        // A list of Pairs (e.g. `(a => 1, b => 2)`) destructured by named
        // params `(:$a, :$b)` binds each pair's key to its value.
        ValueView::Array(data, _) => pairs_in_list_to_named(data.items()),
        ValueView::Seq(items) => pairs_in_list_to_named(&items),
        ValueView::Slip(items) => pairs_in_list_to_named(&items),
        ValueView::Instance { attributes, .. } => HashMap::from(&*attributes.as_map()),
        _ => std::collections::HashMap::new(),
    }
}

pub(in crate::runtime) fn extract_named_from_unpack_target(
    interpreter: &mut Interpreter,
    value: &Value,
    name: &str,
) -> Option<Value> {
    let named = named_values_from_unpack_target(value);
    let lookup_name = name
        .strip_prefix('$')
        .or_else(|| name.strip_prefix('@'))
        .or_else(|| name.strip_prefix('%'))
        .unwrap_or(name);
    if let Some(v) = named.get(name) {
        return Some(v.clone());
    }
    if let Some(v) = named.get(lookup_name) {
        return Some(v.clone());
    }
    interpreter
        .call_method_with_values(value.clone(), name, Vec::new())
        .ok()
}

/// Evaluate a subsignature parameter's `where` constraint against a candidate
/// value during multi dispatch.  Mirrors the top-level positional `where`
/// handling in `args_match_param_types`.
fn where_constraint_matches(
    interpreter: &mut Interpreter,
    where_expr: &Expr,
    pd: &ParamDef,
    candidate: &Value,
) -> bool {
    let saved = interpreter.env.clone();
    interpreter.env.insert("_".to_string(), candidate.clone());
    if !pd.name.is_empty() {
        interpreter.env.insert(pd.name.clone(), candidate.clone());
    }
    let ok = match where_expr {
        Expr::AnonSub { body, .. } => {
            // A `where { ... }` block may reference placeholder parameters
            // (e.g. `$^n`).  In a where clause there is a single value under
            // test, so bind every placeholder referenced in the block to the
            // candidate value (sigil-stripped to the raw `^name` env key). The
            // placeholder is read-only, so `where { $^x = ... }` dies.
            let mut ph_keys: Vec<String> = Vec::new();
            for ph in crate::ast::collect_placeholders(body)
                .iter()
                .chain(crate::ast::collect_where_assign_placeholders(body).iter())
            {
                let key = ph
                    .trim_start_matches(|c: char| "$@%&".contains(c))
                    .to_string();
                if !ph_keys.contains(&key) {
                    ph_keys.push(key);
                }
            }
            for key in &ph_keys {
                interpreter.env.insert(key.clone(), candidate.clone());
                interpreter.mark_readonly(key);
            }
            let r = interpreter
                .eval_block_value(body)
                .map(|v| v.truthy())
                .unwrap_or(false);
            for key in &ph_keys {
                interpreter.unmark_readonly(key);
            }
            r
        }
        Expr::MethodCall { target, .. } if matches!(target.as_ref(), Expr::Var(name) if name == "_") => {
            interpreter
                .eval_block_value(&[Stmt::Expr(where_expr.clone())])
                .map(|v| v.truthy())
                .unwrap_or(false)
        }
        expr => interpreter
            .eval_block_value(&[Stmt::Expr(expr.clone())])
            .map(|v| interpreter.smart_match(candidate, &v))
            .unwrap_or(false),
    };
    interpreter.env = saved;
    ok
}

pub(in crate::runtime) fn sub_signature_matches_value(
    interpreter: &mut Interpreter,
    sub_params: &[ParamDef],
    value: &Value,
) -> bool {
    let value = &interpreter.coerce_via_user_capture(value);
    let positional = positional_values_from_unpack_target(value);
    let mut positional_idx = 0usize;
    for pd in sub_params {
        if pd.slurpy {
            continue;
        }
        let mut candidate = if pd.named {
            extract_named_from_unpack_target(interpreter, value, &pd.name)
        } else if positional_idx < positional.len() {
            let v = positional[positional_idx].clone();
            positional_idx += 1;
            Some(v)
        } else {
            None
        };
        if candidate.is_none()
            && let Some(default) = &pd.default
        {
            candidate = interpreter
                .eval_block_value(&[Stmt::Expr(default.clone())])
                .ok();
        }
        let Some(candidate) = candidate else {
            // Optional params are OK without a value.  Named parameters are
            // optional unless explicitly marked required (`:$x!`), so a missing
            // optional named arg must not fail the match.
            if pd.optional_marker || pd.default.is_some() || (pd.named && !pd.required) {
                continue;
            }
            return false;
        };
        if let Some(constraint) = &pd.type_constraint
            && !interpreter.type_matches_value(constraint, &candidate)
        {
            return false;
        }
        // Sigil-based type constraints: an `@`-param requires a Positional
        // argument and a `%`-param requires an Associative one.  Without this a
        // scalar would wrongly bind to `@x`/`%h` during multi dispatch.
        if !pd.slurpy {
            if pd.name.starts_with('@')
                && !candidate.is_nil()
                && !interpreter.type_matches_value("Positional", &candidate)
            {
                return false;
            }
            if pd.name.starts_with('%')
                && !candidate.is_nil()
                && !interpreter.type_matches_value("Associative", &candidate)
            {
                return false;
            }
        }
        // Implicit Any constraint: untyped $ parameters reject Junction type objects
        if pd.type_constraint.is_none()
            && !pd.name.starts_with('@')
            && !pd.name.starts_with('%')
            && !pd.name.starts_with('&')
            && !pd.slurpy
            && !interpreter.type_matches_value("Any", &candidate)
        {
            return false;
        }
        // A named sub-param's parens are either a RENAME/alias target
        // (`:key($k)`, `:die(:$throw)` — plain inner params with no nested
        // signature of their own) or a genuine destructure (`:value((:key($d),
        // …))`). Only the latter takes the candidate apart; a rename binds the
        // whole candidate to the inner name and so always matches. Treating a
        // rename as a destructure asked the candidate for a positional element
        // it does not have (a `Pair`/`Array` value under `:value($rest)`), so
        // the candidate was rejected. `bind_sub_signature_from_value` already
        // draws this distinction; dispatch matching has to agree with it or a
        // signature that binds fine is never selected.
        if let Some(sub) = &pd.sub_signature
            && !(pd.named && sub.iter().all(|p| p.sub_signature.is_none()))
            && !sub_signature_matches_value(interpreter, sub, &candidate)
        {
            return false;
        }
        if let Some(where_expr) = &pd.where_constraint
            && !where_constraint_matches(interpreter, where_expr, pd, &candidate)
        {
            return false;
        }
    }
    // If there are unconsumed positional elements and no slurpy param, the match
    // fails — unless this is an all-named destructure of a Pair, which consumes
    // no positional element by construction (see `destructures_pair_by_name`).
    let has_slurpy = sub_params.iter().any(|p| !p.named && p.slurpy);
    if !has_slurpy
        && positional_idx < positional.len()
        && !destructures_pair_by_name(value, sub_params)
    {
        return false;
    }
    // Reject unexpected named arguments (for the multi-dispatch case where the
    // value is a Capture).  A named slurpy (`*%h`) accepts any named argument.
    // Read through the `VarRef` wrapper a variable argument arrives in, as the
    // positional/named extraction above already does — this used to skip any
    // `__mutsu_`-prefixed key instead, because a varref *was* a `Capture` whose
    // named map held the wrapper's magic keys.
    if let ValueView::Capture { named, .. } = value.unwrap_varref().view() {
        let has_named_slurpy = sub_params
            .iter()
            .any(|p| p.slurpy && (p.named || p.name.starts_with('%')));
        if !has_named_slurpy {
            for key in named.keys() {
                let consumed = sub_params.iter().any(|p| {
                    p.named && p.name.trim_start_matches(|c: char| "$@%&".contains(c)) == key
                });
                if !consumed {
                    return false;
                }
            }
        }
    }
    // Same rule for the Pair unpack: its capture is exactly `\(:key(…),
    // :value(…))`, so a sub-signature that names only one of the two leaves the
    // other unconsumed and does not match (rakudo rejects
    // `Pair (:key($k))` against `2 => 'x'` for that reason).
    if matches!(
        value.unwrap_varref().view(),
        ValueView::Pair(..) | ValueView::ValuePair(..)
    ) {
        let has_named_slurpy = sub_params
            .iter()
            .any(|p| p.slurpy && (p.named || p.name.starts_with('%')));
        if !has_named_slurpy && sub_params.iter().any(|p| p.named) {
            for key in ["key", "value"] {
                let consumed = sub_params.iter().any(|p| {
                    p.named && p.name.trim_start_matches(|c: char| "$@%&".contains(c)) == key
                });
                if !consumed {
                    return false;
                }
            }
        }
    }
    true
}

/// Bind a named parameter's sub-signature for renaming (e.g., `:a(:$b)`).
/// Unlike destructuring, the value is bound directly to each inner param.
/// Collect every caller-facing named-alias key in a (possibly nested) named
/// alias chain. `:variety(:style(:sort($x)))` yields `["style", "sort"]` — the
/// outermost name (`variety`) is the param's own match key and is handled by the
/// caller; every nested `:name(...)` level adds another key the caller may use.
pub(in crate::runtime) fn collect_nested_named_alias_keys(sub_params: &[ParamDef]) -> Vec<String> {
    let mut keys = Vec::new();
    let mut worklist: Vec<&ParamDef> = sub_params.iter().collect();
    let mut idx = 0;
    while idx < worklist.len() {
        let sp = worklist[idx];
        idx += 1;
        if sp.named {
            keys.push(sp.name.strip_prefix(':').unwrap_or(&sp.name).to_string());
        }
        if let Some(nested) = &sp.sub_signature {
            worklist.extend(nested.iter());
        }
    }
    keys
}

pub(in crate::runtime) fn bind_named_rename_sub_signature(
    interpreter: &mut Interpreter,
    sub_params: &[ParamDef],
    value: &Value,
) -> Result<(), RuntimeError> {
    for sub_pd in sub_params {
        if sub_pd.slurpy {
            continue;
        }
        let bind_name = &sub_pd.name;
        if bind_name.is_empty() {
            continue;
        }
        // An UNSUPPLIED renamed named param arrives here with the outer
        // param's default (a type object — the outer alias name has no
        // sigil, so `missing_optional_param_value` cannot see the leaf's).
        // A sigiled leaf must instead bind its sigil's empty container,
        // exactly like a plain `:%tls`: `:ssl(:tls(%tls-in))` unsupplied
        // binds `%tls-in = {}` (Cro::HTTP::Server.new), not an Associative
        // type error.
        if matches!(value.view(), ValueView::Package(_)) {
            if bind_name.starts_with('%') {
                let empty = Value::hash(std::collections::HashMap::new());
                if let Some(nested) = &sub_pd.sub_signature {
                    bind_named_rename_sub_signature(interpreter, nested, &empty)?;
                } else {
                    interpreter.bind_param_value(bind_name, empty);
                    interpreter.set_var_type_constraint(bind_name, sub_pd.type_constraint.clone());
                }
                continue;
            }
            if bind_name.starts_with('@') {
                let empty = Value::real_array(Vec::new());
                if let Some(nested) = &sub_pd.sub_signature {
                    bind_named_rename_sub_signature(interpreter, nested, &empty)?;
                } else {
                    interpreter.bind_param_value(bind_name, empty);
                    interpreter.set_var_type_constraint(bind_name, sub_pd.type_constraint.clone());
                }
                continue;
            }
        }
        if let Some(tc) = &sub_pd.type_constraint
            && !interpreter.type_matches_value(tc, value)
        {
            return Err(RuntimeError::typecheck_binding_parameter(
                bind_name,
                tc,
                crate::runtime::value_type_name(value),
                None,
            ));
        }
        // Sigil-based type check: %param requires Associative, @param requires Positional
        if bind_name.starts_with('%') && !matches!(value.view(), ValueView::Hash(..)) {
            return Err(RuntimeError::typecheck_binding_parameter(
                bind_name,
                "Associative",
                crate::runtime::value_type_name(value),
                None,
            ));
        }
        if bind_name.starts_with('@')
            && !matches!(value.view(), ValueView::Array(..) | ValueView::Nil)
        {
            return Err(RuntimeError::typecheck_binding_parameter(
                bind_name,
                "Positional",
                crate::runtime::value_type_name(value),
                None,
            ));
        }
        // A named alias can chain: `:type(:class($kind))` nests a further
        // rename under this alias. Only the LEAF of the chain (`$kind`, the one
        // with no further sub-signature) names a body variable; the intermediate
        // alias names (`type`, `class`) are caller-facing keys only and must NOT
        // be bound as body variables, else they shadow a same-named outer
        // constant/lexical (e.g. `:mil(:milli(:$millis))` with an outer
        // `constant milli`). Recurse to reach the leaf.
        if let Some(nested) = &sub_pd.sub_signature {
            bind_named_rename_sub_signature(interpreter, nested, value)?;
        } else {
            interpreter.bind_param_value(bind_name, value.clone());
            interpreter.set_var_type_constraint(bind_name, sub_pd.type_constraint.clone());
        }
    }
    Ok(())
}

/// Bind one destructured sub-parameter into the environment.
///
/// A sub-signature parameter is a **fresh per-invocation binding**, exactly like
/// a `my` declaration, so — while threads are live — it has to mask the
/// name-keyed `shared_vars` lane the same way `my` does
/// (`vm_var_assign_set_local.rs`). Without the mask, a `@`/`%` sub-parameter
/// captured by a spawned block is read out of that lane, which is seeded once
/// (`seed_if_absent`) and therefore frozen at the *first* spawn's value:
///
/// ```raku
/// await map -> [$a, @K] { start { "$a:{@K[0]}" } }, (1, (100,101)), (2, (200,201))
/// # was 1:100,2:100 — the second worker read the first iteration's @K
/// ```
///
/// `$` sub-parameters were already correct: the closure machinery owns them per
/// binding and keeps them off that lane entirely. `&` names are routines and
/// keep the lane; so do twigil'd forms, which share a name by design.
fn bind_sub_param_name(interpreter: &mut Interpreter, name: &str, value: Value) {
    // NOT gated on `shared_vars_active`: the FIRST spawn in a process consults
    // `param_bound_aggregates` before any thread exists, and gating left exactly
    // that spawn's binding to be seeded — and frozen — on the name lane
    // (`todo/tickets/shared-var-lane-freezes-a-reused-array-name.md`).
    if name.starts_with(['@', '%']) && Interpreter::is_plain_lexical_name(name) {
        interpreter
            .param_bound_aggregates
            .insert(name.to_string(), value.clone());
    }
    interpreter.env.insert(name.to_string(), value);
}

/// Drop the `Pair` elements of a *list* destructure target: binding a
/// sub-signature against a list goes through that list's `Capture`, and
/// `List.Capture` files every `Pair` element under the named lane, whatever
/// flavour it is (rakudo: `(1, x => 2).Capture` is `\(1, :x(2))`, and so is
/// `(1, $p).Capture` for a variable-held pair). So `sub bar(\p(Int $y, Str $s?,
/// *%h))` called as `bar((42, life => 40))` binds 42 to `$y`, leaves `$s`
/// undefined and hands `life` to `*%h` — it does NOT bind the pair to `$s`.
///
/// `named_values_from_unpack_target` already surfaces those pairs by name, so
/// this is only about not *also* offering them positionally. A target that IS
/// itself a pair keeps its single positional slot: it destructures by its own
/// key/value parts (`-> (:$key, :$value)`), which is a different rule.
fn drop_pairs_captured_as_named(value: &Value, positional: Vec<Value>) -> Vec<Value> {
    let unwrapped = value.unwrap_varref();
    if !matches!(
        unwrapped.view(),
        ValueView::Array(..) | ValueView::Seq(..) | ValueView::Slip(..)
    ) {
        return positional;
    }
    positional
        .into_iter()
        .filter(|v| {
            !matches!(
                v.unwrap_varref().view(),
                ValueView::Pair(..) | ValueView::ValuePair(..)
            )
        })
        .collect()
}

pub(in crate::runtime) fn bind_sub_signature_from_value(
    interpreter: &mut Interpreter,
    sub_params: &[ParamDef],
    value: &Value,
) -> Result<(), RuntimeError> {
    let value = &interpreter.coerce_via_user_capture(value);
    let positional =
        drop_pairs_captured_as_named(value, positional_values_from_unpack_target(value));
    let mut nested_positional_idx = 0usize;
    // Keys consumed by the non-slurpy named sub-params (outer name plus any
    // `:outer(:$inner)` alias names): a named slurpy (`*%rest`) receives only
    // the named arguments NOT bound to a named parameter.
    let consumed_named_keys: std::collections::HashSet<String> = sub_params
        .iter()
        .filter(|p| p.named && !p.slurpy)
        .flat_map(|p| {
            let strip = |n: &str| {
                n.trim_start_matches(|c: char| "$@%&:".contains(c))
                    .to_string()
            };
            let mut keys = vec![strip(&p.name)];
            if let Some(alias_params) = &p.sub_signature {
                keys.extend(
                    alias_params
                        .iter()
                        .filter(|a| a.named && !a.slurpy)
                        .map(|a| strip(&a.name)),
                );
            }
            keys
        })
        .collect();
    for sub_pd in sub_params {
        if sub_pd.slurpy {
            if sub_pd.name.starts_with('%') {
                let mut named = named_values_from_unpack_target(value);
                named.retain(|k, _| !consumed_named_keys.contains(k));
                if !sub_pd.name.is_empty() {
                    bind_sub_param_name(interpreter, &sub_pd.name, Value::hash(named));
                }
            } else if sub_pd.name.starts_with('@')
                || sub_pd.name.starts_with('$')
                || !sub_pd.name.is_empty()
                || sub_pd.sub_signature.is_some()
            {
                // Slurpy *@rest or *$rest: collect remaining positional values
                let remaining: Vec<Value> = positional[nested_positional_idx..].to_vec();
                nested_positional_idx = positional.len();
                let remaining_value = Value::array(remaining);
                if !sub_pd.name.is_empty() {
                    bind_sub_param_name(interpreter, &sub_pd.name, remaining_value.clone());
                }
                // A slurpy param may itself destructure the list it collects,
                // e.g. `*@ ($a, $b, $y, *@)`. Bind those inner params against the
                // collected list so nested slurpy destructuring reaches them.
                if let Some(nested) = &sub_pd.sub_signature {
                    bind_sub_signature_from_value(interpreter, nested, &remaining_value)?;
                }
            }
            continue;
        }
        let mut candidate = if sub_pd.named {
            let mut c = extract_named_from_unpack_target(interpreter, value, &sub_pd.name);
            // `:outer(:$inner)` alias: also accept the argument under an inner
            // name. Plain map lookup only — the method-call fallback in
            // `extract_named_from_unpack_target` must not fire for alias names
            // like `throw` or `warn`.
            if c.is_none()
                && let Some(alias_params) = &sub_pd.sub_signature
            {
                let named = named_values_from_unpack_target(value);
                for apd in alias_params.iter().filter(|a| a.named && !a.slurpy) {
                    let key = apd.name.trim_start_matches(|ch: char| "$@%&:".contains(ch));
                    if let Some(v) = named.get(key) {
                        c = Some(v.clone());
                        break;
                    }
                }
            }
            c
        } else if nested_positional_idx < positional.len() {
            let v = positional[nested_positional_idx].clone();
            nested_positional_idx += 1;
            Some(v)
        } else {
            None
        };
        if candidate.is_none()
            && let Some(default_expr) = &sub_pd.default
        {
            candidate = Some(interpreter.eval_block_value(&[Stmt::Expr(default_expr.clone())])?);
        }
        let Some(mut candidate) = candidate else {
            // If the param is required (not optional, no default), error
            if !sub_pd.optional_marker && sub_pd.default.is_none() && !sub_pd.named {
                return Err(RuntimeError::new(
                    "Too few positional arguments in sub-signature binding".to_string(),
                ));
            }
            // Explicitly bind optional params to their type object (Any/Mu,
            // or the constrained type) so that values from an outer scope
            // don't leak into recursive calls.
            if !sub_pd.name.is_empty() {
                let default_val = Interpreter::missing_optional_param_value(sub_pd);
                bind_sub_param_name(interpreter, &sub_pd.name, default_val.clone());
                // An unsupplied `:outer(:$inner)` alias must still declare the
                // inner names the body actually reads.
                if sub_pd.named
                    && let Some(alias_params) = &sub_pd.sub_signature
                {
                    for apd in alias_params.iter().filter(|a| a.named && !a.slurpy) {
                        if !apd.name.is_empty() {
                            bind_sub_param_name(interpreter, &apd.name, default_val.clone());
                        }
                    }
                }
            }
            continue;
        };
        let unwrapped_pair_value = if let ValueView::Pair(key, inner) = candidate.view() {
            let bind_name = sub_pd
                .name
                .strip_prefix('$')
                .or_else(|| sub_pd.name.strip_prefix('@'))
                .or_else(|| sub_pd.name.strip_prefix('%'))
                .unwrap_or(sub_pd.name.as_str());
            // Only unwrap a positional `key => val` pair whose key matches this
            // positional parameter's name. A NAMED sub-param's candidate already
            // came from `extract_named_from_unpack_target` (the source pair's
            // `.value`), so unwrapping it again would wrongly strip a value that
            // is itself a `Pair` — e.g. `Pair :value((...))` nested
            // destructuring or `:value($v)` where the value is a Pair (the
            // `ValuePair` form already skips this unwrap; this aligns the
            // string-key `Pair` form with it).
            if !sub_pd.named && bind_name == key {
                Some(inner.clone())
            } else {
                None
            }
        } else {
            None
        };
        if let Some(unwrapped) = unwrapped_pair_value {
            candidate = unwrapped;
        }
        // Sigil-based type check for a non-slurpy positional sub-param: a `@`
        // parameter requires a Positional, a `%` parameter an Associative. In a
        // destructure like `@a ($first, @rest)`, `@rest` is NOT slurpy, so it
        // binds the element at its position; a scalar there is a binding error
        // (e.g. `foo <1 2 3>` -> `@rest` gets the IntStr `2`).
        if !sub_pd.named {
            if sub_pd.name.starts_with('@')
                && !matches!(
                    candidate.view(),
                    ValueView::Array(..) | ValueView::Nil | ValueView::Slip(_) | ValueView::Seq(_)
                )
                && !candidate.is_range()
            {
                return Err(RuntimeError::typecheck_binding_parameter_value(
                    &sub_pd.name,
                    "Positional",
                    candidate,
                ));
            }
            // A `@` parameter listifies an Iterable that is not already
            // Positional, so a destructured `Seq` element binds as a `List` —
            // `-> [@a, $b] {...}` given `[(1,2).Seq, 9]` sees `@a.^name` as
            // `List` in Rakudo, and `@a` must stay usable after the Seq is
            // consumed. A `Range` is Positional already and binds unchanged.
            if sub_pd.name.starts_with('@')
                && let ValueView::Seq(items) = candidate.view()
            {
                candidate = Value::array(items.to_vec());
            }
            if sub_pd.name.starts_with('%')
                && !matches!(candidate.view(), ValueView::Hash(..) | ValueView::Nil)
            {
                return Err(RuntimeError::typecheck_binding_parameter_value(
                    &sub_pd.name,
                    "Associative",
                    candidate,
                ));
            }
        }
        if let Some(constraint) = &sub_pd.type_constraint {
            if let Some((target, source)) = parse_coercion_type(constraint) {
                // A `T(S)` parameter accepts a value already of type `T` as well as
                // an `S` (coerced via `.T`); only reject one that matches neither.
                if let Some(src) = source
                    && !interpreter.type_matches_value(src, &candidate)
                    && !interpreter.type_matches_value(target, &candidate)
                {
                    return Err(RuntimeError::new(format!(
                        "X::TypeCheck::Argument: Type check failed for {}: expected {}, got {}",
                        sub_pd.name,
                        constraint,
                        crate::runtime::value_type_name(&candidate)
                    )));
                }
                candidate = interpreter.try_coerce_value_for_constraint(constraint, candidate)?;
            } else if !interpreter.type_matches_value(constraint, &candidate) {
                return Err(RuntimeError::new(format!(
                    "X::TypeCheck::Argument: Type check failed for {}: expected {}, got {}",
                    sub_pd.name,
                    constraint,
                    crate::runtime::value_type_name(&candidate)
                )));
            }
        }
        let bind_alias_name = !(sub_pd.named && sub_pd.sub_signature.is_some());
        if !sub_pd.name.is_empty() && bind_alias_name {
            bind_sub_param_name(interpreter, &sub_pd.name, candidate.clone());
            // Neither shape of a destructured leaf ever got marked readonly:
            // a sigilless leaf (`sub f($ (\a, \b)) { a = 1 }`) is a TERM
            // binding like a top-level `sub f(\a) { a = 1 }` param, and a
            // `$`-sigiled leaf (`sub f($ ($x, $y)) { $x = 1 }`) is a readonly
            // ALIAS like any other non-`is rw` parameter. Both are marked at
            // the ordinary top-level binding site -- `MarkSigillessReadonly`
            // (a parser/compiler prologue, `compiler/helpers_sub_body.rs`)
            // for the former, `mark_readonly` (`binding_signature.rs`'s
            // "correct" branch) for the latter -- but a destructure leaf is
            // bound purely at RUNTIME, here, and neither mechanism reaches
            // it, so an assignment silently succeeded where Raku throws.
            if sub_pd.sigilless {
                // Mirror the exact env-key marker `Stmt::MarkSigillessReadonly`
                // compiles to, so `CheckReadOnly` treats this identically to a
                // top-level sigilless param or a `my \x = 5` bind (X::Assignment::RO,
                // "Cannot modify an immutable TYPE (VALUE)").
                interpreter.env.insert(
                    format!("__mutsu_sigilless_readonly::{}", sub_pd.name),
                    Value::TRUE,
                );
            } else if !sub_pd.name.starts_with('@')
                && !sub_pd.name.starts_with('%')
                && !sub_pd.name.starts_with('!')
                && !sub_pd.name.starts_with('.')
            {
                // `@`/`%` destructure leaves bind a writable container (same
                // exemption as top-level `@`/`%` params); `!`/`.` are
                // attribute-binding leaves, always writable. A plain `$`
                // leaf is readonly (X::AdHoc, "Cannot assign to a readonly
                // variable or a value") unless explicitly `is rw`/`is copy`/
                // `is raw`.
                let has_mutable_trait = sub_pd
                    .traits
                    .iter()
                    .any(|t| t == "rw" || t == "copy" || t == "raw");
                if has_mutable_trait {
                    interpreter.unmark_readonly(&sub_pd.name);
                } else {
                    interpreter.mark_readonly(&sub_pd.name);
                }
            }
        }
        if let Some(nested) = &sub_pd.sub_signature {
            // A named sub-param's parens are either a RENAME/alias target
            // (`:key($k)`, `:die(:$throw)` — plain inner params, no nested
            // signature) or a genuine destructure (`:value((:key($d), ...))` —
            // the inner param carries its own sub_signature). Rename binds the
            // whole candidate to each inner name; destructure recurses into it.
            let is_rename = sub_pd.named && nested.iter().all(|p| p.sub_signature.is_none());
            if is_rename {
                bind_named_rename_sub_signature(interpreter, nested, &candidate)?;
            } else {
                bind_sub_signature_from_value(interpreter, nested, &candidate)?;
            }
        }
    }
    // If there are unconsumed positional elements and no slurpy param, error
    let has_slurpy = sub_params.iter().any(|p| p.slurpy);
    let has_positional_params = sub_params.iter().any(|p| !p.named && !p.slurpy);
    if has_positional_params && !has_slurpy && nested_positional_idx < positional.len() {
        return Err(RuntimeError::new(
            "Too many positional arguments in sub-signature binding".to_string(),
        ));
    }
    Ok(())
}

pub(in crate::runtime) fn sub_signature_target_from_remaining_args(args: &[Value]) -> Value {
    // A single argument unpacks directly (e.g. `|(Int $x)` matching one value,
    // or `|(Pair $p (:$key, :$value))` matching one Pair whose `.key`/`.value`
    // accessors must remain reachable).  Only when multiple arguments remain do
    // we build a Capture, separating named (Pair) arguments into the named slot
    // so that subsignature named parameters can find them.
    if args.len() == 1 {
        return args[0].clone();
    }
    let mut positional = Vec::new();
    let mut named = std::collections::HashMap::new();
    for arg in args {
        match arg.view() {
            ValueView::Pair(key, val) => {
                named.insert(key.clone(), val.clone());
            }
            ValueView::ValuePair(key, val) => {
                named.insert(key.to_string_value(), val.clone());
            }
            _ => positional.push(arg.clone()),
        }
    }
    Value::capture(positional, named)
}

pub(in crate::runtime) fn callable_signature_info(
    interpreter: &mut Interpreter,
    value: &Value,
) -> Option<crate::value::signature::SigInfo> {
    use crate::value::signature::param_defs_to_sig_info;

    let callable = match value.view() {
        ValueView::WeakSub(weak) => weak.upgrade().map(Value::sub_value)?,
        _ => value.clone(),
    };
    if !interpreter.type_matches_value("Callable", &callable) {
        return None;
    }

    match callable.view() {
        ValueView::Sub(data) => {
            let return_type = interpreter
                .callable_return_type(&callable)
                .or_else(|| interpreter.routine_return_spec_by_name(&data.name.resolve()));
            Some(param_defs_to_sig_info(&data.param_defs, return_type))
        }
        ValueView::Routine { name, .. } => {
            let (params, param_defs) = interpreter.callable_signature(&callable);
            let defs = if !param_defs.is_empty() {
                param_defs
            } else {
                params
                    .into_iter()
                    .map(|name| ParamDef {
                        name,
                        default: None,
                        multi_invocant: true,
                        required: true,
                        named: false,
                        slurpy: false,
                        double_slurpy: false,
                        onearg: false,
                        sigilless: false,
                        type_constraint: None,
                        literal_value: None,
                        sub_signature: None,
                        where_constraint: None,
                        traits: Vec::new(),
                        optional_marker: false,
                        outer_sub_signature: None,
                        code_signature: None,
                        is_invocant: false,
                        shape_constraints: None,
                        block_param: false,
                    })
                    .collect::<Vec<_>>()
            };
            let return_type = interpreter.routine_return_spec_by_name(&name.resolve());
            Some(param_defs_to_sig_info(&defs, return_type))
        }
        _ => {
            let (params, param_defs) = interpreter.callable_signature(&callable);
            let defs = if !param_defs.is_empty() {
                param_defs
            } else {
                params
                    .into_iter()
                    .map(|name| ParamDef {
                        name,
                        default: None,
                        multi_invocant: true,
                        required: true,
                        named: false,
                        slurpy: false,
                        double_slurpy: false,
                        onearg: false,
                        sigilless: false,
                        type_constraint: None,
                        literal_value: None,
                        sub_signature: None,
                        where_constraint: None,
                        traits: Vec::new(),
                        optional_marker: false,
                        outer_sub_signature: None,
                        code_signature: None,
                        is_invocant: false,
                        shape_constraints: None,
                        block_param: false,
                    })
                    .collect::<Vec<_>>()
            };
            Some(param_defs_to_sig_info(
                &defs,
                interpreter.callable_return_type(&callable),
            ))
        }
    }
}

pub(in crate::runtime) fn code_signature_matches_value(
    interpreter: &mut Interpreter,
    expected_params: &[ParamDef],
    expected_return_type: &Option<String>,
    value: &Value,
) -> bool {
    use crate::value::signature::{param_defs_to_sig_info, signature_smartmatch};

    fn resolve_captured_constraint(interpreter: &Interpreter, constraint: &str) -> String {
        if let Some(captured) = constraint.strip_prefix("::")
            && let Some(ValueView::Package(name)) = interpreter.env.get(captured).map(Value::view)
        {
            return name.resolve();
        }
        if let Some(ValueView::Package(name)) = interpreter.env.get(constraint).map(Value::view) {
            return name.resolve();
        }
        constraint.to_string()
    }

    fn specialize_code_signature_params(
        interpreter: &Interpreter,
        params: &[ParamDef],
    ) -> Vec<ParamDef> {
        params
            .iter()
            .map(|p| {
                let mut next = p.clone();
                if let Some(tc) = &next.type_constraint {
                    next.type_constraint = Some(resolve_captured_constraint(interpreter, tc));
                }
                next
            })
            .collect()
    }

    // Demangle a `SigInfo`'s type-constraint strings the same way
    // `specialize_code_signature_params` does for `ParamDef`s — needed because
    // `callable_signature_info` (the ACTUAL callable's signature, e.g. the
    // `sub (Dog $x --> Bool) {...}` passed as an argument) returns `SigInfo`
    // directly, with its `Dog` type constraint still the bare name as written.
    // Without this, only the EXPECTED side (below) got remapped through a
    // lexical class's mangled storage name (ADR-0047 P1: `Foo\u{0}<decl-id>`),
    // so `Dog` (expected, remapped to `Dog\u{0}<id>`) could never string-equal
    // `Dog` (actual, left bare) even though they name the very same `my class
    // Dog` — every `Callable:(Dog --> ...)` signature constraint against a
    // lexical class failed to bind (`roast/S06-signature/closure-parameters.t`).
    fn specialize_sig_info(
        interpreter: &Interpreter,
        mut info: crate::value::signature::SigInfo,
    ) -> crate::value::signature::SigInfo {
        for p in &mut info.params {
            if let Some(tc) = &p.type_constraint {
                p.type_constraint = Some(resolve_captured_constraint(interpreter, tc));
            }
        }
        if let Some(rt) = &info.return_type {
            info.return_type = Some(resolve_captured_constraint(interpreter, rt));
        }
        info
    }

    let Some(actual_info) = callable_signature_info(interpreter, value) else {
        return false;
    };
    let actual_info = specialize_sig_info(interpreter, actual_info);
    let specialized_expected = specialize_code_signature_params(interpreter, expected_params);
    let specialized_return = expected_return_type
        .as_ref()
        .map(|rt| resolve_captured_constraint(interpreter, rt));
    let expected_info = param_defs_to_sig_info(&specialized_expected, specialized_return);
    // Function-type compatibility: candidate callable must be at least as general
    // as the required signature to safely accept all required calls.
    signature_smartmatch(&actual_info, &expected_info)
}
