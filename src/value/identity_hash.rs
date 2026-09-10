//! `Hash for Value` — the *declaration identity* hash the AST fingerprints
//! ([`crate::ast::function_body_fingerprint`] and friends) are built on.
//!
//! # Why this exists
//!
//! `Expr::Literal` and `ParamDef::literal_value` hold a `Value`, so the derived
//! `Hash` on `Expr` / `Stmt` needs one here. Before
//! [#7822](https://github.com/tokuhirom/mutsu/issues/7822) the fingerprints
//! hashed a `Debug` *rendering* of the AST instead, which dragged the whole
//! `core::fmt` machinery (`DebugStruct::field`, `DebugSet::entry`,
//! `format_inner`) behind every routine the compiler touched.
//!
//! # The contract — identity, NOT value equality
//!
//! This hash answers "were these two literals parsed from the same source
//! shape", nothing more. It is deliberately **inconsistent** with
//! [`PartialEq for Value`](crate::value::Value), which is Raku's `eqv`-flavoured
//! *value* equality: that impl makes `Int(1) == Num(1.0)` and
//! `Array([1]) == Seq([1])`, and this hash separates them. Two consequences,
//! both intended:
//!
//! - Hashing `NaN` by bit pattern, and separating `0.0` from `-0.0`, is
//!   *correct* here. Two source texts that lex to different bits are different
//!   declarations.
//! - `Value` must never gain an `Eq` impl. `HashMap`/`HashSet` require
//!   `K: Eq + Hash`, so the missing `Eq` is what mechanically prevents anyone
//!   from keying a map on this hash and silently getting Raku's value equality
//!   wrong. If you need a value-equality hash, write a separate one — do not
//!   make `Value: Eq`.
//!
//! # Structure only
//!
//! Fingerprints are compared across separately-parsed copies of the same
//! source, so nothing here may depend on an address or on an allocation
//! identity. `Symbol` is fine: it hashes an interning id, which is a pure
//! function of the symbol's text within a process, and no fingerprint is ever
//! serialized (`FunctionDef::body_fp_cache` and `CompiledRoutineMetadata` are
//! both in-process only).
//!
//! # The `Debug` fallback
//!
//! The variants a parser can actually plant in an AST — the numeric and string
//! immediates, ranges, pairs, versions, `Nil`/`*`/`**` — are hashed
//! structurally. The live-object variants (`Sub`, `Instance`, `Promise`,
//! `Proxy`, the `Gc`-backed containers, ...) fall back to streaming their
//! `Debug` rendering into the hasher, exactly as the whole AST used to. They
//! are unreachable from a literal in practice, so the fallback costs nothing on
//! any hot path, and several of them (`HashData::which_id`, `ArrayData`'s
//! `WhichId`) are not structurally comparable across copies anyway — the
//! fallback is the honest answer for those, not a shortcut.

use super::{Value, ValueView};
use std::hash::{Hash, Hasher};

/// A `fmt::Write` sink that streams formatted bytes straight into a `Hasher`,
/// so `write!(.., "{:?}", x)` hashes the `Debug` rendering without ever
/// allocating an intermediate `String`.
struct HashWrite<'a, H: Hasher>(&'a mut H);

impl<H: Hasher> std::fmt::Write for HashWrite<'_, H> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.0.write(s.as_bytes());
        Ok(())
    }
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let view = self.view();
        // Tags every variant, including the ones that fall through to the
        // `Debug` arm, so a structurally-hashed payload can never collide with
        // a differently-tagged one carrying the same bytes.
        std::mem::discriminant(&view).hash(state);
        match view {
            ValueView::Int(i) => i.hash(state),
            ValueView::BigInt(n) => (**n).hash(state),
            // Bit pattern, not numeric value — see the module docs.
            ValueView::Num(f) => f.to_bits().hash(state),
            ValueView::Str(s) => (**s).hash(state),
            ValueView::Bool(b) => b.hash(state),
            ValueView::Range(a, b)
            | ValueView::RangeExcl(a, b)
            | ValueView::RangeExclStart(a, b)
            | ValueView::RangeExclBoth(a, b)
            | ValueView::Rat(a, b)
            | ValueView::FatRat(a, b) => {
                a.hash(state);
                b.hash(state);
            }
            ValueView::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                (**start).hash(state);
                (**end).hash(state);
                excl_start.hash(state);
                excl_end.hash(state);
            }
            ValueView::BigRat(num, den) => {
                num.hash(state);
                den.hash(state);
            }
            ValueView::Complex(re, im) => {
                re.to_bits().hash(state);
                im.to_bits().hash(state);
            }
            ValueView::CompUnitDepSpec { short_name } => short_name.hash(state),
            ValueView::Package(name) => name.hash(state),
            ValueView::Routine {
                package,
                name,
                is_regex,
            } => {
                package.hash(state);
                name.hash(state);
                is_regex.hash(state);
            }
            ValueView::Pair(key, value) => {
                key.hash(state);
                value.hash(state);
            }
            ValueView::ValuePair(key, value) => {
                key.hash(state);
                value.hash(state);
            }
            ValueView::Regex(pattern) => (**pattern).hash(state),
            ValueView::Junction { kind, values } => {
                std::mem::discriminant(&kind).hash(state);
                (**values).hash(state);
            }
            ValueView::Slip(items) => (**items).hash(state),
            ValueView::Version {
                parts,
                plus,
                minus,
                text,
            } => {
                parts.hash(state);
                plus.hash(state);
                minus.hash(state);
                text.hash(state);
            }
            ValueView::ParametricRole {
                base_name,
                type_args,
            } => {
                base_name.hash(state);
                type_args.hash(state);
            }
            ValueView::Scalar(inner) => inner.hash(state),
            // Unit variants: the discriminant above is the whole identity.
            ValueView::Nil | ValueView::Whatever | ValueView::HyperWhatever => {}
            // Live-object / `Gc`-backed variants — see the module docs.
            _ => {
                use std::fmt::Write as _;
                let _ = write!(HashWrite(state), "{self:?}");
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::hash_map::DefaultHasher;

    fn fp(v: &Value) -> u64 {
        let mut h = DefaultHasher::new();
        v.hash(&mut h);
        h.finish()
    }

    #[test]
    fn equal_literals_hash_equal() {
        assert_eq!(fp(&Value::int(42)), fp(&Value::int(42)));
        assert_eq!(
            fp(&Value::str("hello".to_string())),
            fp(&Value::str("hello".to_string()))
        );
        assert_eq!(fp(&Value::num(1.5)), fp(&Value::num(1.5)));
    }

    #[test]
    fn distinct_literals_hash_distinct() {
        assert_ne!(fp(&Value::int(42)), fp(&Value::int(43)));
        assert_ne!(
            fp(&Value::str("a".to_string())),
            fp(&Value::str("b".to_string()))
        );
    }

    /// The identity contract: `Int(1) == Num(1.0)` under `PartialEq`, but they
    /// are different declarations, so they must hash apart.
    #[test]
    fn identity_hash_is_not_value_equality() {
        assert_eq!(Value::int(1), Value::num(1.0));
        assert_ne!(fp(&Value::int(1)), fp(&Value::num(1.0)));
    }

    /// `NaN` is hashed by bit pattern, so it is stable — unlike its `PartialEq`
    /// treatment, which is a special case rather than a bitwise one.
    #[test]
    fn nan_hashes_stably_and_signed_zeros_differ() {
        assert_eq!(fp(&Value::num(f64::NAN)), fp(&Value::num(f64::NAN)));
        assert_ne!(fp(&Value::num(0.0)), fp(&Value::num(-0.0)));
    }
}
