//! The type aliases a routine declaration's signature names, read from the
//! declaring frame. An imported type term (`CArray`, `size_t` from NativeCall)
//! is a pad term of the importing unit (ADR-8751), which the env-based alias
//! resolution (`declared_type_alias_target`) cannot see, so a declaration reads
//! them here, where its frame is live.

use super::*;

impl Interpreter {
    /// The `(alias, aliased type)` pairs for the identifiers a signature's type
    /// spellings name that are `constant`/imported type terms of the frame
    /// `code` runs: its parameters' types, those of nested signatures and
    /// `&callback (...)` signatures, and `--> T`.
    // Cost: O(p + n), p = parameters including nested signatures, n = bytes
    // of their type spellings.
    pub(super) fn frame_signature_type_aliases(
        &self,
        code: &CompiledCode,
        param_defs: &[crate::ast::ParamDef],
        return_type: Option<&str>,
    ) -> Vec<(String, String)> {
        let mut spellings: Vec<&str> = Vec::new();
        signature_type_spellings(param_defs, &mut spellings);
        spellings.extend(return_type);
        let mut aliases: Vec<(String, String)> = Vec::new();
        for spelling in spellings {
            for ident in spelling
                .split(|c: char| !(c.is_alphanumeric() || matches!(c, '_' | '-' | ':')))
                .filter(|ident| {
                    ident.starts_with(|c: char| c.is_alphabetic() || c == '_')
                        && !ident.contains(':')
                })
            {
                if aliases.iter().any(|(alias, _)| alias == ident) {
                    continue;
                }
                if let Some(value) =
                    self.locals_get_by_name(code, &crate::runtime::term_names::term_key(ident))
                    && let ValueView::Package(target) = value.deref_container().view()
                {
                    aliases.push((ident.to_string(), target.resolve()));
                }
            }
        }
        aliases
    }
}

/// Every type spelling a signature names: its parameters' types and those of
/// their nested signatures and `&callback (...)` signatures (with their `--> T`).
// Cost: O(p), p = parameters including nested signatures.
fn signature_type_spellings<'a>(defs: &'a [crate::ast::ParamDef], out: &mut Vec<&'a str>) {
    for pd in defs {
        out.extend(pd.type_constraint.as_deref());
        if let Some(sig) = pd.sub_signature.as_deref() {
            signature_type_spellings(sig, out);
        }
        if let Some((sig, ret)) = &pd.code_signature {
            signature_type_spellings(sig, out);
            out.extend(ret.as_deref());
        }
    }
}
