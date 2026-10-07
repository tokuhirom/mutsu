use super::*;
use crate::symbol::Symbol;

/// The "no such method" answer of `Metamodel::MethodContainer`'s `.^lookup`
/// and `.^find_method`: Rakudo hands back the **`Mu` type object**, not `Nil`.
/// `Int.^lookup("does-not-exist")` gists as `(Mu)` and `.defined` is `False`,
/// so a caller's `//` / boolean test behaves the same either way -- but
/// `.^name`, `.raku` and an `=== Mu` identity check do not, which is what the
/// `Metamodel/MethodContainer.rakudoc` example asserts.
pub(super) fn mop_absent_method() -> Value {
    Value::package(Symbol::intern("Mu"))
}

/// `.^add_method`/`.^add_multi_method`'s callable argument used to always be
/// a plain `Sub` (`X.^lookup('other')`'s old return shape). ADR-0019 Phase F
/// box F1 made `.^lookup`/`.^find_method` return a Method/Submethod
/// `Instance` instead (`todo/tickets/classhow-lookup-returns-sub-not-method-
/// instance.md`), so unwrap the `__mutsu_method_callable` attribute back to
/// that same `Sub` before the rest of this file's `ValueView::Sub` match --
/// any other value (a real closure literal, etc.) passes through unchanged.
pub(super) fn unwrap_method_instance_callable(value: &Value) -> Value {
    match value.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if matches!(class_name.as_str(), "Method" | "Submethod" | "Regex") => {
            let am = attributes.as_map();
            // A non-dispatcher candidate carries its callable directly. A
            // multi dispatcher has none of its own -- fall back to its first
            // candidate's callable as the carrier body, mirroring the old
            // Sub-shaped dispatcher (itself built from the first candidate
            // found); the lookup-tag rewrite below still marks the RESULT as
            // the dispatcher (no candidate_idx), not that specific candidate.
            let base_callable = am.get("__mutsu_method_callable").cloned().or_else(|| {
                am.get("candidates").and_then(|c| match c.view() {
                    ValueView::Array(items, _) => items.first().cloned(),
                    _ => None,
                })
            });
            let Some(base_callable) = base_callable else {
                return value.clone();
            };
            let callable = unwrap_method_instance_callable(&base_callable);
            // Port the Instance's `__mutsu_lookup_*` attributes back onto the
            // unwrapped Sub's `env` -- this function's callers (below) still
            // read them from `SubData::env` (predating ADR-0019 Phase F box
            // F1's move of that carrier data to Instance attributes) to
            // detect a multi-family alias (`^add_method(name,
            // X.^lookup('other'))` cloning the whole candidate family, not
            // just one carrier candidate). Cleared first, not merely
            // overwritten, since a dispatcher's own attrs carry no
            // `__mutsu_lookup_candidate_idx` but its fallback candidate's
            // unwrapped env still has one from its own unwrap above.
            if let ValueView::Sub(data) = callable.view() {
                let mut new_data: crate::value::SubData = (**data).clone();
                for key in [
                    "__mutsu_lookup_class",
                    "__mutsu_lookup_method",
                    "__mutsu_lookup_candidate_idx",
                ] {
                    new_data.env.remove(key);
                    if let Some(v) = am.get(key) {
                        new_data.env.insert(key.to_string(), v.clone());
                    }
                }
                return Value::sub_value(crate::gc::Gc::new(new_data));
            }
            callable
        }
        _ => value.clone(),
    }
}

/// `Metamodel::Naming.shortname`: the type name with every `Foo::` package
/// qualifier dropped, including inside `[...]` type args -- `Foo::Bar` ->
/// `Bar`, `R[M2::N]` -> `R[N]`. Non-identifier suffixes (`<anon|1>`,
/// `Int:D`, `+{Role}`) pass through unchanged.
pub(super) fn shorten_type_name(name: &str) -> String {
    let is_ident = |c: char| c.is_alphanumeric() || c == '_' || c == '-' || c == '\'';
    let chars: Vec<char> = name.chars().collect();
    let mut out = String::new();
    let mut i = 0;
    while i < chars.len() {
        if chars[i] == ':' && i + 2 < chars.len() && chars[i + 1] == ':' && is_ident(chars[i + 2]) {
            // Drop the qualifier segment just emitted along with the `::`.
            while out.chars().next_back().is_some_and(is_ident) {
                out.pop();
            }
            i += 2;
            continue;
        }
        out.push(chars[i]);
        i += 1;
    }
    out
}

/// The element type `.^array_type` reports for the type named `type_name`.
///
/// A parameterised container names its element type outright
/// (`Buf[uint64]` -> `uint64`, `array[num32]` -> `num32`,
/// `CArray[int32]` -> `int32`). An unparameterised byte-buffer type carries its
/// element width in its own name (`utf16` -> `uint16`), and a bare `Buf`/`Blob`
/// is `uint8`. Anything else is `Mu`, which is what Rakudo's `ClassHOW` answers
/// for a type that is not an array (`Str.^array_type` is `Mu`).
pub(super) fn array_element_type_name(type_name: &str) -> &str {
    if let Some(inner) = type_name
        .split_once('[')
        .and_then(|(_, rest)| rest.strip_suffix(']'))
    {
        return inner;
    }
    match type_name {
        "Buf" | "Blob" | "buf8" | "blob8" | "utf8" | "utf8-c8" => "uint8",
        "buf16" | "blob16" | "utf16" => "uint16",
        "buf32" | "blob32" | "utf32" => "uint32",
        "buf64" | "blob64" => "uint64",
        _ => "Mu",
    }
}

impl Interpreter {
    /// Whether `value` is a genuine role reference: a role's own type object
    /// (`ValueView::Package` whose name is a role, not a class — including a
    /// role that happens to ALSO have been punned to a class of the same
    /// name, since a bareword role mention always stays `Package`, never the
    /// `Mixin` `Interpreter::punned_role_type_object` builds), a
    /// parameterised role (`ValueView::ParametricRole`), or one of
    /// `.^candidates`' own per-candidate `Instance` objects.
    ///
    /// `.^candidates` (and any other role-group-only MOP method) is only
    /// defined on `ParametricRoleGroupHOW`/`ParametricRoleHOW`/`CurriedRoleHOW`,
    /// never on `ClassHOW` — so a punned role's class (a `Mixin`) or an
    /// ordinary class (a `Package` whose name is not a role) must NOT match,
    /// and instead fall through to the `X::Method::NotFound` default at the
    /// bottom of `dispatch_classhow_method`, matching Rakudo
    /// (`R.^pun.^candidates` throws; `R.^candidates` answers `((R))`).
    pub(crate) fn is_role_reference_value(&self, value: &Value) -> bool {
        match value.view() {
            ValueView::Package(name) => self.is_role_type_name(&name.resolve()),
            ValueView::ParametricRole { .. } => true,
            ValueView::Instance { attributes, .. } => {
                attributes.as_map().contains_key("__mutsu_role_base_name")
            }
            _ => false,
        }
    }

    /// Resolve a nominalizable type name to its nominal base type
    /// (`^nominalize`): strip `:D`/`:U`/`:_` definiteness, unwrap a coercion
    /// type (`Int(Rat)` -> `Int`), and walk a subset chain to the first
    /// non-subset base. Plain nominal types return themselves.
    pub(crate) fn nominalize_type_name(&self, name: &str) -> String {
        let mut current = name.to_string();
        loop {
            let stripped = current
                .strip_suffix(":D")
                .or_else(|| current.strip_suffix(":U"))
                .or_else(|| current.strip_suffix(":_"));
            if let Some(s) = stripped {
                current = s.to_string();
                continue;
            }
            if crate::runtime::types::is_coercion_constraint(&current)
                && let Some((target, _)) = crate::runtime::types::parse_coercion_type(&current)
            {
                current = target.to_string();
                continue;
            }
            if let Some(subset) = self.registry().subsets.get(&current) {
                let base = subset.base.clone();
                if !base.is_empty() && base != current {
                    current = base;
                    continue;
                }
            }
            return current;
        }
    }

    pub(super) fn dispatch_classhow_method(
        &mut self,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // `Metamodel::DefiniteHOW`'s own two metamethods (ADR-0069).
        if let Some(result) = self.dispatch_definitehow_method(method, &args) {
            return result;
        }
        // `Metamodel::EnumHOW`'s own metamethods.
        if let Some(result) = self.dispatch_enumhow_method(method, &args) {
            return result;
        }
        // The metamethods are rows of the `Metamodel::*HOW` owners (ADR-11276
        // slice 3G); the args carry the type object first, then the call's own.
        if let Some(result) = crate::builtins::method_table::invoke_owner_raw(
            self,
            crate::builtins::method_table::MOP_OWNERS,
            method,
            &args,
        ) {
            return result;
        }
        let type_name = args
            .first()
            .map(|a| self.mop_receiver_owner(a))
            .unwrap_or_default();
        Err(RuntimeError::meta_method_not_found(method, &type_name))
    }
}
