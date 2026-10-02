use super::*;

/// `Date`/`DateTime` get their `.IO` from `Dateish`, whose signature takes a `Dateish:D`
/// invocant, so calling it on the type object is a concreteness error rather than a
/// stringification of `(Date)` into a path. Returns the error when `name` is one of them.
pub(crate) fn dateish_io_concreteness_error(name: &str) -> Option<RuntimeError> {
    if !matches!(name, "Date" | "DateTime") {
        return None;
    }
    Some(RuntimeError::parameter_invalid_concreteness(
        "Dateish", name, "IO", "self", true, // should_be_concrete
        true, // param_is_invocant
    ))
}

// Cost: O(n), n = operand count after slipping.
pub(crate) fn merge_junction(kind: JunctionKind, left: Value, right: Value) -> Value {
    // Infix junction operators (|, &, ^) always create a new junction
    // without flattening a junction operand. List-associative flattening is
    // handled at compile time via JunctionAnyN/AllN/OneN opcodes.
    Value::junction(kind, junction_operands(vec![left, right]))
}

/// The eigenstates an infix junction operator builds from its operands.
/// Rakudo's `infix:<|>`/`&`/`^` take `+values`, whose slurpy slips a
/// (non-itemized) `Slip` operand into the list: `|(1, 2) | 3` is
/// `any(1, 2, 3)`, not `any((1 2), 3)`.
// Cost: O(n), n = operand count after slipping.
pub(crate) fn junction_operands(values: Vec<Value>) -> Vec<Value> {
    if !values.iter().any(is_bare_slip) {
        return values;
    }
    let mut out = Vec::with_capacity(values.len());
    for v in values {
        match v.view() {
            ValueView::Slip(items) if !v.slip_is_itemized() => out.extend(items.iter().cloned()),
            _ => out.push(v),
        }
    }
    out
}

fn is_bare_slip(v: &Value) -> bool {
    matches!(v.view(), ValueView::Slip(_)) && !v.slip_is_itemized()
}

/// Build a structured X::Dynamic::NotFound RuntimeError.
/// Thrown when assigning to a dynamic variable (`$*x` / `@*x` / `%*x`) that is
/// not present anywhere in the dynamic scope. `display_name` is the full
/// sigil+twigil form (e.g. `$*an_undeclared_dynvar`).
pub(crate) fn dynamic_not_found_error(display_name: &str) -> RuntimeError {
    let msg = format!("Dynamic variable {} not found", display_name);
    let mut attrs = ValueMap::default();
    attrs.insert("name".to_string(), Value::str(display_name.to_string()));
    attrs.insert("symbol".to_string(), Value::str(display_name.to_string()));
    attrs.insert("message".to_string(), Value::str(msg));
    RuntimeError::typed("X::Dynamic::NotFound", attrs)
}

/// Whether `name` (an internal `*`-prefixed dynamic var name, e.g. `*TOLERANCE`,
/// or an array/hash form that keeps its sigil, e.g. `%*OPTS`) is a built-in
/// dynamic variable provided by the runtime. Built-in dynamics are always
/// "declared" by the setting, so reading or assigning one before any user
/// `my $*X` must never be treated as a genuinely undeclared dynamic var --
/// shared by the compiler's X::Dynamic::Postdeclaration check
/// (`Compiler::check_dynamic_var_decl_errors`) and the VM's read-side
/// X::Dynamic::NotFound fallback (`OpCode::GetGlobal`'s final miss branch),
/// so the one whitelist cannot drift between the two.
pub(crate) fn is_builtin_dynamic_var(name: &str) -> bool {
    let bare = name.trim_start_matches(['$', '@', '%', '&']);
    let Some(bare) = bare.strip_prefix('*') else {
        return false;
    };
    matches!(
        bare,
        "OUT"
            | "ERR"
            | "IN"
            | "ARGFILES"
            | "ARGS"
            | "SPEC"
            | "CWD"
            | "TMPDIR"
            | "HOME"
            | "EXECUTABLE"
            | "EXECUTABLE-NAME"
            | "PROGRAM"
            | "PROGRAM-NAME"
            | "DISTRO"
            | "PERL"
            | "RAKU"
            | "VM"
            | "KERNEL"
            | "PID"
            | "TOLERANCE"
            | "COLLATION"
            | "DEFAULT-READ-ELEMS"
            | "INIT-INSTANT"
            | "REPO"
            | "RAT-OVERFLOW"
            | "SCHEDULER"
            | "THREAD"
            | "SAMPLER"
            | "USER"
            | "GROUP"
            | "LANG"
    )
}

/// Build a structured X::Caller::NotDynamic RuntimeError.
/// Thrown when accessing a caller-frame lexical through `CALLER::` (either the
/// `$CALLER::x` symbolic form or the `CALLER::<$x>` stash-subscript form) when
/// that variable is not declared `is dynamic`. `name` is the bare variable name
/// without sigil; the reported `symbol` re-adds the `$` sigil.
pub(crate) fn caller_not_dynamic_error(name: &str) -> RuntimeError {
    let symbol = format!("${name}");
    let msg =
        format!("Cannot access '{symbol}' through CALLER, because it is not declared as dynamic");
    let mut attrs = ValueMap::default();
    attrs.insert("symbol".to_string(), Value::str(symbol));
    attrs.insert("message".to_string(), Value::str(msg));
    RuntimeError::typed("X::Caller::NotDynamic", attrs)
}

/// Compute the `.WHICH` string for a value, used as the internal key
/// in object hashes (`my %h{Any}`).
pub(crate) fn value_which_key(value: &Value) -> String {
    match value.view() {
        ValueView::Int(n) => format!("Int|{}", n),
        ValueView::BigInt(n) => format!("Int|{}", *n),
        ValueView::Num(n) => format!("Num|{}", n),
        ValueView::Str(s) => format!("Str|{}", *s),
        ValueView::Bool(b) => format!("Bool|{}", if b { 1 } else { 0 }),
        ValueView::Rat(n, d) => format!("Rat|{}/{}", n, d),
        ValueView::FatRat(n, d) => format!("FatRat|{}/{}", n, d),
        ValueView::BigRat(n, d) => {
            let flavour = if value.is_bigfatrat() {
                "FatRat"
            } else {
                "Rat"
            };
            format!("{}|{}/{}", flavour, n, d)
        }
        ValueView::Complex(r, i) => format!("Complex|{}+{}i", r, i),
        ValueView::Nil => format!("Nil|U{}", Symbol::intern("Nil").id()),
        ValueView::Package(name) => format!("{}|U{}", name.resolve(), name.id()),
        ValueView::CustomType(c) => format!("{}|U{}", c.name.resolve(), c.id),
        // A class may override `WHICH` to give its instances value semantics
        // (`Set(A.new(a=>5)) eqv Set(A.new(a=>5))`). Running that user method
        // needs the interpreter, so it deposits the answer on the instance and
        // we read it here; without an override the identity is the object's own
        // id. See `InstanceAttrs::which_memo`.
        ValueView::Instance {
            class_name,
            attributes,
            id,
        } => match value.user_which_memo() {
            Some(which) => which.to_string(),
            None => match class_name.resolve().as_str() {
                "Date" => {
                    let (year, month, day) =
                        crate::builtins::methods_0arg::temporal::date_attrs(&attributes.as_map());
                    format!(
                        "Date|{}",
                        crate::builtins::methods_0arg::temporal::daycount(year, month, day)
                    )
                }
                "DateTime" => {
                    let (year, month, day, hour, minute, second, timezone) =
                        crate::builtins::methods_0arg::temporal::datetime_attrs(
                            &attributes.as_map(),
                        );
                    format!(
                        "DateTime|{}",
                        crate::builtins::methods_0arg::temporal::format_datetime(
                            year, month, day, hour, minute, second, timezone,
                        )
                    )
                }
                // An ObjAt is keyed by the identity it carries -- the same
                // string its `.WHICH` reports -- so every `$o.WHICH` of one
                // object is one Set/Bag element.
                cn @ ("ObjAt" | "ValueObjAt") => format!(
                    "{}|{}",
                    cn,
                    attributes.as_map().objat_which().unwrap_or_default()
                ),
                // A user subclass of Version keeps its built Version in
                // `__mutsu_version_value`; like Version itself (`Version|1.0`)
                // its identity is the class plus the canonical string.
                _ if attributes.as_map().contains_key("__mutsu_version_value") => format!(
                    "{}|{}",
                    class_name.resolve(),
                    attributes
                        .as_map()
                        .get("__mutsu_version_value")
                        .map(Value::to_string_value)
                        .unwrap_or_default()
                ),
                _ => format!("{}|{}", value_type_name(value), id),
            },
        },
        // Same never-reused id as the `.WHICH` twin in
        // `builtins::methods_0arg::dispatch_core_coerce` -- an address is
        // unique only among LIVE objects, and this string outlives them.
        ValueView::Array(items, ..) => format!("Array|{}", items.which_id.get()),
        ValueView::Hash(map) => format!("Hash|{}", map.which_id.get()),
        // A code object is a reference type: its identity is its id, exactly
        // as its `.WHICH` (`Block|16`) reports. The text fallback below renders
        // every Block alike, so two different closures collided as one
        // object-hash key and as one curried-role argument.
        ValueView::Sub(sub_data) => format!("{}|{}", value_type_name(value), sub_data.id),
        // A regex is a code object too; its payload is shared by every alias
        // and minted afresh by every evaluation of its literal.
        ValueView::Regex(_) | ValueView::RegexWithAdverbs(_) => {
            format!("Regex|{}", value.regex_which_id().unwrap_or_default())
        }
        // A Pair with a plain string key and a ValuePair holding a Str key are
        // the same identity (`("x" => 1) === (:x(1))`), so both render the key
        // through its own `.WHICH` (`Pair|Str|x|Int|1`, raku's format).
        ValueView::Pair(k, v) => format!("Pair|Str|{}|{}", k, value_which_key(v)),
        ValueView::ValuePair(k, v) => format!("Pair|{}|{}", value_which_key(k), value_which_key(v)),
        ValueView::Enum { enum_type, key, .. } => {
            format!("{}|{}", enum_type.resolve(), key.resolve())
        }
        // A `but`-mixed value (e.g. `"quux" but $role`) keeps the identity of
        // its base value but is a DISTINCT object per mixed-in role set — raku's
        // `.WHICH` is `Str+{<role>}|quux`. Fold the (sorted) mixin type/role
        // names into the key so two different roles over the same base value are
        // distinct object-hash keys, while the same value+role collides.
        //
        // An allomorph (IntStr/NumStr/RatStr/ComplexStr — a numeric inner with a
        // preserved `Str` part) keys by BOTH halves, matching raku's
        // `IntStr|Int|1|Str|1`: `IntStr.new(1, "one")` and `IntStr.new(1, "1")`
        // are distinct identities, which the role-name fold alone would collapse.
        ValueView::Mixin(inner, mixins) => {
            if let Some(allo_name) = crate::value::types::allomorph_type_name(inner, mixins) {
                let str_part = mixins
                    .get("Str")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                return format!("{}|{}|Str|{}", allo_name, value_which_key(inner), str_part);
            }
            let mut roles: Vec<&str> = mixins.keys().map(|s| s.as_str()).collect();
            roles.sort_unstable();
            format!(
                "{}+{{{}}}|{}",
                value_type_name(inner),
                roles.join(","),
                value_which_key(inner)
            )
        }
        _ => {
            format!("{}|{}", value_type_name(value), value.to_string_value())
        }
    }
}
