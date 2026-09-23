//! Parameter and declared-type admission for [`super::TrirCompiler`]:
//! which signatures and variable types TRIR proves, and how each binds.

use super::{TrKind, TrParam, TrParamCheck, TrirCompiler};
use crate::ast::ParamDef;

/// Whether a parameter's recorded name is a plain `$`-scalar lexical.
///
/// `ParamDef::name` carries the spelling with the `$` stripped but every
/// other marker intact, so this is the gate that keeps out an ATTRIBUTIVE
/// parameter (`$!t` arrives as `"!t"`, and binding it must reach `self`'s
/// attribute cell, which only the general binder does — `sub s($!t) {}` with
/// an empty body is otherwise a perfectly provable TRIR routine that silently
/// discards its argument), a `@`/`%`/`&` container, a `*`-slurpy, a dynamic
/// (`*foo`), a compiler variable (`?FILE`), and the anonymous `_`.
fn plain_scalar_param_name(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !(first.is_alphabetic() || first == '_') {
        return false;
    }
    // `$self` is the reserved invocant lexical (ADR-0061), not an ordinary
    // parameter name.
    if name == "_" || name == "self" || name.starts_with("__") {
        return false;
    }
    name.chars()
        .all(|c| c.is_alphanumeric() || c == '_' || c == '-' || c == '\'')
}

/// The native scalar spellings TRIR gives a native slot. Everything else —
/// including the boxed nominal types `Int`/`Str`/`Num` — is a boxed slot,
/// because a boxed parameter legitimately accepts a bare type object and a
/// native one must reject it (`FastParamType`'s own note).
pub(super) fn native_kind_of(tc: Option<&str>) -> Option<TrKind> {
    match tc {
        Some("int") => Some(TrKind::Int),
        Some("num") => Some(TrKind::Num),
        _ => None,
    }
}

/// The key a sigilless parameter's slot is bound under.
pub(super) fn sigilless_key(name: &str) -> String {
    format!("\\{name}")
}

/// A boxed parameter's nominal constraint as a bind-time check: a plain,
/// capitalized type name, optionally qualified, with at most a `:D`/`:U`
/// smiley. Anything else — a coercion type `Str()`, a parameterization
/// `Array[Int]`, a lowercase native (`uint32`) the boxed bank cannot
/// narrow — declines.
fn nominal_check(tc: &str) -> Option<TrParamCheck> {
    let (base, smiley) = crate::runtime::types::strip_type_smiley(tc);
    let defined = match smiley {
        None | Some(":_") => None,
        Some(":D") => Some(true),
        Some(":U") => Some(false),
        Some(_) => return None,
    };
    // Every `::`-separated segment starts with a capital letter; checked
    // character by character (a colon pair must be followed by one).
    let bytes = base.as_bytes();
    let mut plain = bytes.first().is_some_and(u8::is_ascii_uppercase);
    let mut i = 0;
    while plain && i < bytes.len() {
        let c = bytes[i];
        if c == b':' {
            plain = bytes.get(i + 1) == Some(&b':')
                && bytes.get(i + 2).is_some_and(u8::is_ascii_uppercase);
            i += 2;
            continue;
        }
        plain = c.is_ascii_alphanumeric() || c == b'_' || c == b'-';
        i += 1;
    }
    plain.then(|| TrParamCheck {
        base: base.to_string(),
        defined,
    })
}

/// The width of a sized native integer type the int bank can hold exactly:
/// `(bits, signed)`. `uint`/`uint64` range past `i64` and decline, as does
/// every non-integer type.
pub(super) fn sized_int_width(tc: &str) -> Option<(u8, bool, &'static str)> {
    Some(match tc {
        "int8" => (8, true, "int8"),
        "int16" => (16, true, "int16"),
        "int32" => (32, true, "int32"),
        "uint8" => (8, false, "uint8"),
        "byte" => (8, false, "byte"),
        "uint16" => (16, false, "uint16"),
        "uint32" => (32, false, "uint32"),
        _ => return None,
    })
}

impl TrirCompiler<'_> {
    pub(super) fn declare_params(&mut self, param_defs: &[ParamDef]) -> Option<()> {
        for pd in param_defs {
            // ADR-0110 §4.4: positional scalars only, no slurpy/named/where/
            // sub-signature/default/capture/attributive forms.
            let shape_reason = if pd.named {
                "named"
            } else if pd.slurpy || pd.double_slurpy || pd.onearg {
                "slurpy"
            } else if pd.optional_marker || pd.default.is_some() {
                "optional or defaulted"
            } else if pd.sub_signature.is_some() || pd.outer_sub_signature.is_some() {
                "destructuring"
            } else if pd.where_constraint.is_some() {
                "where-constrained"
            } else if pd.code_signature.is_some() {
                "code-signature"
            } else if pd.type_capture.is_some() {
                "type-capturing"
            } else if pd.literal_value.is_some() {
                "literal"
            } else if pd.shape_constraints.is_some() {
                "shaped"
            } else if pd.is_invocant {
                "invocant"
            } else if !pd.trait_args.is_empty() {
                "trait-argument"
            } else {
                "not a plain $ lexical"
            };
            if pd.named
                || pd.slurpy
                || pd.double_slurpy
                || pd.onearg
                || pd.is_invocant
                || pd.optional_marker
                || pd.default.is_some()
                || pd.sub_signature.is_some()
                || pd.outer_sub_signature.is_some()
                || pd.where_constraint.is_some()
                || pd.code_signature.is_some()
                || pd.type_capture.is_some()
                || pd.literal_value.is_some()
                || pd.shape_constraints.is_some()
                || !pd.trait_args.is_empty()
                || !plain_scalar_param_name(&pd.name)
            {
                let n = pd.name.clone();
                self.note_decline(|| format!("parameter ${n} is {shape_reason}"));
                return None;
            }
            let is_rw = pd.traits.iter().any(|t| t == "rw");
            // `is rw` is the only trait Stage 1 understands; `is copy`,
            // `is raw` and every custom trait decline.
            if pd.traits.iter().any(|t| t != "rw") {
                let t = pd.traits.join(" ");
                self.note_decline(|| format!("parameter trait is {t}"));
                return None;
            }
            let tc = pd.type_constraint.as_deref();
            let mut check = None;
            let kind = match native_kind_of(tc) {
                // A sigilless parameter binds without a container, which a
                // boxed slot already is; a native one would need the
                // general binder's native coercion instead.
                Some(_) if pd.sigilless => {
                    self.note_decline(|| "a native sigilless parameter".to_string());
                    return None;
                }
                Some(k) => k,
                None if tc.is_none() || tc == Some("str") => TrKind::Obj,
                // A nominal constraint is checked at bind time with the
                // general binder's own type test (`bind_ro_param`); a failed
                // check declines the call, so the untyped path raises.
                None => match tc.and_then(nominal_check) {
                    Some(c) => {
                        check = Some(c);
                        TrKind::Obj
                    }
                    None => {
                        let t = tc.unwrap_or("").to_string();
                        self.note_decline(|| format!("parameter type {t}"));
                        return None;
                    }
                },
            };
            // Only a native parameter may be `is rw` here: a boxed one would
            // need a real container, which is exactly what the untyped path
            // already does well (ADR-0109).
            if is_rw && !kind.is_native() {
                return None;
            }
            // A sigilless parameter is read as a bareword term (`codes`), so it
            // lives under its own key: a bareword `pos` next to a `$pos`
            // parameter is a call to the routine `pos`, not the variable.
            let slot = if pd.sigilless {
                if is_rw {
                    return None;
                }
                self.alloc(&sigilless_key(&pd.name), kind)
            } else {
                self.alloc(&pd.name, kind)
            };
            let type_name = match tc {
                Some("int") => "int",
                Some("num") => "num",
                Some("str") => "str",
                _ => "",
            };
            self.params.push(TrParam {
                slot,
                kind,
                is_rw,
                type_name,
                check,
            });
        }
        Some(())
    }
}
