//! Rakudo's `Parameter` flag word: the `$!flags` attribute and the
//! `nqp::const::SIG_ELEM_*` bits that make it up.
//!
//! A `Parameter` keeps all its boolean properties in one `Int` bit field.
//! Ecosystem code (`Deps`' `is injected` trait) reads and writes it through
//! the MOP (`$p.^attributes.first(*.name eq '$!flags')`), so a `Parameter`
//! carries a `flags` attribute alongside the per-property attributes
//! `build_parameter_attrs` sets. The values are rakudo's own, checked against
//! `raku -e 'use nqp; say nqp::const::SIG_ELEM_...'`.

pub(crate) const BIND_CAPTURE: i64 = 1;
pub(crate) const BIND_PRIVATE_ATTR: i64 = 2;
pub(crate) const BIND_PUBLIC_ATTR: i64 = 4;
pub(crate) const SLURPY_POS: i64 = 8;
pub(crate) const SLURPY_NAMED: i64 = 16;
pub(crate) const SLURPY_LOL: i64 = 32;
pub(crate) const INVOCANT: i64 = 64;
pub(crate) const MULTI_INVOCANT: i64 = 128;
pub(crate) const IS_RW: i64 = 256;
pub(crate) const IS_COPY: i64 = 512;
pub(crate) const IS_RAW: i64 = 1024;
pub(crate) const IS_OPTIONAL: i64 = 2048;
pub(crate) const ARRAY_SIGIL: i64 = 4096;
pub(crate) const HASH_SIGIL: i64 = 8192;
pub(crate) const DEFAULT_FROM_OUTER: i64 = 16384;
pub(crate) const IS_CAPTURE: i64 = 32768;
pub(crate) const UNDEFINED_ONLY: i64 = 65536;
pub(crate) const DEFINED_ONLY: i64 = 131072;
pub(crate) const DEFAULT_IS_LITERAL: i64 = 1048576;
pub(crate) const SLURPY_ONEARG: i64 = 16777216;
pub(crate) const CODE_SIGIL: i64 = 33554432;
pub(crate) const IS_COERCIVE: i64 = 67108864;
pub(crate) const IS_ITEM: i64 = 268435456;
/// `SIG_ELEM_IS_NOT_READONLY`: any of `is rw` / `is copy` / `is raw`.
pub(crate) const IS_NOT_READONLY: i64 = IS_RW | IS_COPY | IS_RAW;
/// `SIG_ELEM_IS_SLURPY`: any slurpy kind.
pub(crate) const IS_SLURPY: i64 = SLURPY_POS | SLURPY_NAMED | SLURPY_LOL | SLURPY_ONEARG;
/// `SIG_ELEM_IS_NOT_POSITIONAL`: a slurpy or a named parameter.
pub(crate) const IS_NOT_POSITIONAL: i64 = 16810040;

/// The value of `nqp::const::SIG_ELEM_<name>`, `name` without the prefix.
// Cost: O(1).
pub(crate) fn sig_elem_const(name: &str) -> Option<i64> {
    Some(match name.strip_prefix("SIG_ELEM_")? {
        "BIND_CAPTURE" => BIND_CAPTURE,
        "BIND_PRIVATE_ATTR" => BIND_PRIVATE_ATTR,
        "BIND_PUBLIC_ATTR" => BIND_PUBLIC_ATTR,
        "SLURPY_POS" => SLURPY_POS,
        "SLURPY_NAMED" => SLURPY_NAMED,
        "SLURPY_LOL" => SLURPY_LOL,
        "INVOCANT" => INVOCANT,
        "MULTI_INVOCANT" => MULTI_INVOCANT,
        "IS_RW" => IS_RW,
        "IS_COPY" => IS_COPY,
        "IS_RAW" => IS_RAW,
        "IS_OPTIONAL" => IS_OPTIONAL,
        "ARRAY_SIGIL" => ARRAY_SIGIL,
        "HASH_SIGIL" => HASH_SIGIL,
        "DEFAULT_FROM_OUTER" => DEFAULT_FROM_OUTER,
        "IS_CAPTURE" => IS_CAPTURE,
        "UNDEFINED_ONLY" => UNDEFINED_ONLY,
        "DEFINED_ONLY" => DEFINED_ONLY,
        "DEFAULT_IS_LITERAL" => DEFAULT_IS_LITERAL,
        "SLURPY_ONEARG" => SLURPY_ONEARG,
        "CODE_SIGIL" => CODE_SIGIL,
        "IS_COERCIVE" => IS_COERCIVE,
        "IS_ITEM" => IS_ITEM,
        "IS_NOT_READONLY" => IS_NOT_READONLY,
        "IS_SLURPY" => IS_SLURPY,
        "IS_NOT_POSITIONAL" => IS_NOT_POSITIONAL,
        _ => return None,
    })
}

/// What [`flags_word`] reads off a parameter: the properties the word encodes,
/// gathered by the caller from the declaration.
#[derive(Default)]
pub(crate) struct FlagInputs<'a> {
    pub(crate) sigil: char,
    /// `.name`'s twigil: `"!"`, `"."` or `""`.
    pub(crate) twigil: &'a str,
    pub(crate) invocant: bool,
    pub(crate) multi_invocant: bool,
    pub(crate) rw: bool,
    pub(crate) copy: bool,
    pub(crate) raw: bool,
    pub(crate) optional: bool,
    pub(crate) capture: bool,
    pub(crate) slurpy_pos: bool,
    pub(crate) slurpy_named: bool,
    pub(crate) slurpy_lol: bool,
    pub(crate) slurpy_onearg: bool,
    /// `:D`, `:U` or anything else (`:_`, none).
    pub(crate) definedness: &'a str,
    pub(crate) coercive: bool,
    pub(crate) default_is_literal: bool,
}

/// The `$!flags` word of a parameter.
// Cost: O(1).
pub(crate) fn flags_word(f: &FlagInputs<'_>) -> i64 {
    let mut word = 0;
    let mut set = |on: bool, bit: i64| {
        if on {
            word |= bit;
        }
    };
    set(f.twigil == "!", BIND_PRIVATE_ATTR);
    set(f.twigil == ".", BIND_PUBLIC_ATTR);
    set(f.slurpy_pos, SLURPY_POS);
    set(f.slurpy_named, SLURPY_NAMED);
    set(f.slurpy_lol, SLURPY_LOL);
    set(f.slurpy_onearg, SLURPY_ONEARG);
    set(f.invocant, INVOCANT);
    set(f.multi_invocant, MULTI_INVOCANT);
    set(f.rw, IS_RW);
    set(f.copy, IS_COPY);
    set(f.raw, IS_RAW);
    set(f.optional, IS_OPTIONAL);
    set(f.sigil == '@', ARRAY_SIGIL);
    set(f.sigil == '%', HASH_SIGIL);
    set(f.sigil == '&', CODE_SIGIL);
    set(f.capture, IS_CAPTURE);
    set(f.definedness == ":U", UNDEFINED_ONLY);
    set(f.definedness == ":D", DEFINED_ONLY);
    set(f.coercive, IS_COERCIVE);
    set(f.default_is_literal, DEFAULT_IS_LITERAL);
    word
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn composite_constants_match_rakudo() {
        assert_eq!(IS_NOT_READONLY, 1792);
        assert_eq!(IS_SLURPY, 16777272);
        assert_eq!(sig_elem_const("SIG_ELEM_IS_OPTIONAL"), Some(2048));
        assert_eq!(sig_elem_const("SIG_ELEM_NOPE"), None);
    }

    #[test]
    fn optional_array_named_word() {
        let f = FlagInputs {
            sigil: '@',
            multi_invocant: true,
            optional: true,
            ..Default::default()
        };
        assert_eq!(flags_word(&f), 6272);
    }
}
