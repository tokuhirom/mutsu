use super::*;
use crate::rakuast::compound::op::COMPOUND_ASSIGN_OPS;

pub(crate) fn parse_compound_assign_op(input: &str) -> Option<(&str, CompoundAssignOp)> {
    // Unicode arithmetic-operator aliases whose compound-assignment forms mirror
    // the ASCII ops: `×=` (U+00D7) == `*=`, `÷=` (U+00F7) == `/=`. Raku accepts
    // these; the multiplicative parser leaves `×`/`÷` before `=` alone so the
    // lvalue reaches here (mirroring the `*=` guard).
    //
    // A user-declared `infix:<×>`/`infix:<÷>` takes the alias back, exactly as
    // it does for the bare operator in `parse_multiplicative_op`: rakudo makes
    // `×=` the assignment metaop over `infix:<×>` *as spelled*, so it reaches
    // the user's candidate while `*=` (a different name) does not. Declining
    // here hands the spelling to `parse_custom_compound_assign_op`, which
    // already builds that metaop for any user symbol infix (`⋅=`).
    for (sym, op, bare) in [
        ("\u{00D7}=", CompoundAssignOp::Mul, "\u{00D7}"),
        ("\u{00F7}=", CompoundAssignOp::Div, "\u{00F7}"),
    ] {
        if let Some(stripped) = input.strip_prefix(sym)
            && !crate::parser::stmt::simple::is_user_defined_infix(bare)
        {
            return Some((stripped, op));
        }
    }
    for op in COMPOUND_ASSIGN_OPS {
        if let Some(stripped) = input.strip_prefix(op.symbol()) {
            return Some((stripped, *op));
        }
    }
    // Z-prefixed compound assignments (Z+=, Z//=, ...) are handled by
    // parse_meta_compound_assign_op, not here.
    None
}

pub(crate) fn parse_custom_compound_assign_op(input: &str) -> Option<(&str, String)> {
    // Try word-like custom operators (alphabetic/underscore start)
    let mut chars = input.char_indices();
    let (_, first) = chars.next()?;
    if first.is_alphabetic() || first == '_' {
        let mut end = first.len_utf8();
        for (idx, ch) in chars {
            if ch.is_alphanumeric() || ch == '_' || ch == '-' {
                end = idx + ch.len_utf8();
            } else {
                break;
            }
        }
        let name = &input[..end];
        if !matches!(
            name,
            "if" | "unless"
                | "for"
                | "while"
                | "until"
                | "given"
                | "when"
                | "with"
                | "without"
                | "not"
        ) {
            let rest = &input[end..];
            if let Some(rest) = rest.strip_prefix('=') {
                return Some((rest, name.to_string()));
            }
        }
    }

    // Try user-declared symbol infix operators (e.g. ⋅= for infix:<⋅>)
    if let Some((symbol, len)) =
        crate::parser::stmt::simple::match_user_declared_infix_symbol_op(input)
    {
        let rest = &input[len..];
        if let Some(rest) = rest.strip_prefix('=') {
            return Some((rest, symbol));
        }
    }

    None
}

pub(crate) fn parse_meta_compound_assign_op(input: &str) -> Option<(&str, String, String)> {
    let (meta, after_meta) = if let Some(rest) = input.strip_prefix('R') {
        ("R", rest)
    } else if let Some(rest) = input.strip_prefix('X') {
        ("X", rest)
    } else {
        let rest = input.strip_prefix('Z')?;
        ("Z", rest)
    };
    if after_meta.starts_with('=')
        && !after_meta.starts_with("==")
        && !after_meta.starts_with("=>")
        && !after_meta.starts_with("=:=")
    {
        return Some((&after_meta[1..], meta.to_string(), "=".to_string()));
    }
    let (rest, op) = parse_compound_assign_op(after_meta)?;
    let op_name = op
        .symbol()
        .strip_suffix('=')
        .unwrap_or(op.symbol())
        .to_string();
    Some((rest, meta.to_string(), op_name))
}
