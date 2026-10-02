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
