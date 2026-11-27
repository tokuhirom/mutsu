//! The enums the core declares (`Order`, `Endian`, `ProtocolFamily`,
//! `Signal`, `SeekType`, `FileChangeEvent`, `PromiseStatus`), as the one
//! table every layer that names them reads, and their names as constants of
//! the env's base tiers, built on first lookup (#19197, the rule of
//! ADR-19206).
//!
//! Every scope sees each enum's type name (`Order`), each variant bare
//! (`Less`) and each variant qualified (`Order::Less`). `Interpreter::new`
//! used to intern all ~120 of those names and build a value for each, ~10% of
//! a `say "hello"`'s instructions, for constants most programs never name.
//!
//! Now nothing is built at start-up. A name is recognized when it is interned:
//! [`is_name`] sets two bits of its symbol flags, one saying an env lookup may
//! find it, and one saying it is a built-in enum constant. A base tier that
//! misses such a symbol asks [`get`], which builds the value on its first
//! lookup into a slot the whole process shares. The values are plain enum
//! data, so the slots hold nothing the collector has to trace.

use std::sync::OnceLock;

use crate::symbol::{Symbol, SymbolMemo};
use crate::value::{EnumValue, Value};

/// How an enum's type name reads as a term.
#[derive(Clone, Copy)]
enum TypeTerm {
    /// The type name as a `Str`.
    Str,
    /// A package object (`ProtocolFamily`).
    Package,
}

/// A core enum: its name, how its type name reads, and its variants in
/// declaration order.
pub(crate) struct BuiltinEnum {
    pub(crate) name: &'static str,
    type_term: TypeTerm,
    pub(crate) variants: &'static [(&'static str, i64)],
}

impl BuiltinEnum {
    /// The variants as the `(name, value)` list the registry's enum types and
    /// the type-constraint tables hold.
    // Cost: O(v), v = the enum's variants.
    pub(crate) fn variants_owned(&self) -> Vec<(String, EnumValue)> {
        self.variants
            .iter()
            .map(|(name, value)| ((*name).to_string(), EnumValue::Int(*value)))
            .collect()
    }

    /// Variant `index`'s value.
    // Cost: O(1).
    pub(crate) fn variant_value(&self, index: usize) -> Value {
        let (name, value) = self.variants[index];
        Value::enum_parts(
            Symbol::intern(self.name),
            Symbol::intern(name),
            EnumValue::Int(value),
            index,
        )
    }

    /// The index of the variant called `name`.
    // Cost: O(v), v = the enum's variants.
    pub(crate) fn variant_index(&self, name: &str) -> Option<usize> {
        self.variants
            .iter()
            .position(|(variant, _)| *variant == name)
    }

    // Cost: O(1).
    fn type_value(&self) -> Value {
        match self.type_term {
            TypeTerm::Str => Value::str_from(self.name),
            TypeTerm::Package => Value::package(Symbol::intern(self.name)),
        }
    }
}

pub(crate) const ORDER: BuiltinEnum = BuiltinEnum {
    name: "Order",
    type_term: TypeTerm::Str,
    variants: &[("Less", -1), ("Same", 0), ("More", 1)],
};

pub(crate) const ENDIAN: BuiltinEnum = BuiltinEnum {
    name: "Endian",
    type_term: TypeTerm::Str,
    variants: &[("NativeEndian", 0), ("LittleEndian", 1), ("BigEndian", 2)],
};

pub(crate) const PROTOCOL_FAMILY: BuiltinEnum = BuiltinEnum {
    name: "ProtocolFamily",
    type_term: TypeTerm::Package,
    variants: &[
        ("PF_UNSPEC", 0),
        ("PF_INET", 1),
        ("PF_INET6", 2),
        ("PF_LOCAL", 3),
        ("PF_UNIX", 3),
        ("PF_MAX", 4),
    ],
};

/// Every signal MoarVM lists, including the ones this platform lacks (value
/// 0), so `Signal.keys` is complete as in Rakudo.
pub(crate) const SIGNAL: BuiltinEnum = BuiltinEnum {
    name: "Signal",
    type_term: TypeTerm::Str,
    variants: &crate::signal_table::SIGNALS,
};

/// The `SeekType` enum used by `IO::Handle.seek` (and user IO classes that
/// subclass it, e.g. `IO::Blob`). A real enum makes `SeekType:D` parameter
/// and default type checks pass and the values smartmatch `SeekType`.
pub(crate) const SEEK_TYPE: BuiltinEnum = BuiltinEnum {
    name: "SeekType",
    type_term: TypeTerm::Str,
    variants: &[
        ("SeekFromBeginning", 0),
        ("SeekFromCurrent", 1),
        ("SeekFromEnd", 2),
    ],
};

pub(crate) const FILE_CHANGE_EVENT: BuiltinEnum = BuiltinEnum {
    name: "FileChangeEvent",
    type_term: TypeTerm::Str,
    variants: &[("FileChanged", 1), ("FileRenamed", 2)],
};

/// `Promise.status` answers one of these, so `$p.status == Kept` compares
/// enum values numerically and `when Kept` smartmatches the enum.
pub(crate) const PROMISE_STATUS: BuiltinEnum = BuiltinEnum {
    name: "PromiseStatus",
    type_term: TypeTerm::Str,
    variants: &[("Planned", 0), ("Kept", 1), ("Broken", 2)],
};

/// The core enums. No two of them share a variant name (a unit test pins
/// it), so a bare variant names exactly one constant.
pub(crate) const ENUMS: [&BuiltinEnum; 7] = [
    &ORDER,
    &ENDIAN,
    &PROTOCOL_FAMILY,
    &SIGNAL,
    &SEEK_TYPE,
    &FILE_CHANGE_EVENT,
    &PROMISE_STATUS,
];

/// The most variants an enum has (`Signal`'s 35), plus the type name's slot.
const SLOTS_PER_ENUM: usize = 36;

/// A constant: enum `enum_index`, and slot 0 for its type name or `v + 1` for
/// its variant `v`.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct Constant {
    enum_index: usize,
    slot: usize,
}

impl Constant {
    // Cost: O(1).
    fn encode(self) -> u16 {
        // + 1: 0 is "not a constant" in the memo.
        (self.enum_index * SLOTS_PER_ENUM + self.slot + 1) as u16
    }

    // Cost: O(1).
    fn decode(code: u16) -> Option<Self> {
        let n = (code as usize).checked_sub(1)?;
        Some(Self {
            enum_index: n / SLOTS_PER_ENUM,
            slot: n % SLOTS_PER_ENUM,
        })
    }
}

/// The enum whose type name is `name`.
// Cost: O(|name|).
fn enum_named(name: &str) -> Option<usize> {
    ENUMS.iter().position(|e| e.name == name)
}

/// The constant spelled `name`, if any.
// Cost: O(e + v) string compares, e = enums, v = variants of one enum; a name
// that does not start with an ASCII capital is rejected at once.
fn constant(name: &str) -> Option<Constant> {
    if !name.as_bytes().first().is_some_and(u8::is_ascii_uppercase) {
        return None;
    }
    if let Some((type_name, variant)) = name.split_once("::") {
        let enum_index = enum_named(type_name)?;
        let v = ENUMS[enum_index].variant_index(variant)?;
        return Some(Constant {
            enum_index,
            slot: v + 1,
        });
    }
    if let Some(enum_index) = enum_named(name) {
        return Some(Constant {
            enum_index,
            slot: 0,
        });
    }
    variant_named(name).map(|(enum_index, v)| Constant {
        enum_index,
        slot: v + 1,
    })
}

/// The enum and variant index of the bare variant `name`.
// Cost: O(V) string compares, V = all variants (about 55); a `SIG` name
// searches `Signal` only.
fn variant_named(name: &str) -> Option<(usize, usize)> {
    if name.starts_with("SIG") {
        let signal = enum_named(SIGNAL.name)?;
        return SIGNAL.variant_index(name).map(|v| (signal, v));
    }
    ENUMS
        .iter()
        .enumerate()
        .find_map(|(e, en)| en.variant_index(name).map(|v| (e, v)))
}

/// Whether `name` is a constant of the base tiers. Called once per symbol, by
/// the symbol flags' computation.
// Cost: as `constant`.
pub(crate) fn is_name(name: &str) -> bool {
    constant(name).is_some()
}

/// Whether `name` is a variant of a core enum (`Same`, `Kept`,
/// `LittleEndian`, `SIGINT`), which every scope reads as a bare term.
// Cost: as `variant_named`.
pub(crate) fn is_variant(name: &str) -> bool {
    variant_named(name).is_some()
}

/// The value of the core enum variant spelled `name` (`Same` is
/// `Order::Same`), or `None` when no core enum has such a variant.
// Cost: as `variant_named`, plus the value's construction.
pub(crate) fn variant_value(name: &str) -> Option<Value> {
    variant_named(name).map(|(e, v)| ENUMS[e].variant_value(v))
}

/// Each symbol's constant, memoized (0: none).
static CONSTANT_OF: SymbolMemo = SymbolMemo::new();

/// Each constant's value, built on its first lookup.
static VALUES: [[OnceLock<Value>; SLOTS_PER_ENUM]; ENUMS.len()] =
    [const { [const { OnceLock::new() }; SLOTS_PER_ENUM] }; ENUMS.len()];

/// Every constant's symbol and slot, interned on the first whole-tier walk.
static ALL: OnceLock<Box<[(Symbol, Constant)]>> = OnceLock::new();

// Cost: O(1) after the constant's first lookup.
fn value_of(c: Constant) -> &'static Value {
    VALUES[c.enum_index][c.slot].get_or_init(|| {
        let e = ENUMS[c.enum_index];
        match c.slot {
            0 => e.type_value(),
            slot => e.variant_value(slot - 1),
        }
    })
}

/// The value of the constant `key`, if it is one: what a base tier answers
/// for a name its map does not hold.
// Cost: O(1), one flag load, for a symbol that is not a constant; for one that
// is, O(1) after its first lookup in the process.
#[inline]
pub(crate) fn get(key: Symbol) -> Option<&'static Value> {
    if key.flags() & crate::symbol::flags::BUILTIN_ENUM_CONST == 0 {
        return None;
    }
    lookup(key)
}

// Cost: O(1) after the symbol's first lookup.
#[cold]
fn lookup(key: Symbol) -> Option<&'static Value> {
    let code = CONSTANT_OF.get(key, |name| constant(name).map_or(0, Constant::encode));
    Constant::decode(code).map(value_of)
}

/// Whether `key` is a constant.
// Cost: O(1).
#[inline]
pub(crate) fn contains(key: Symbol) -> bool {
    key.flags() & crate::symbol::flags::BUILTIN_ENUM_CONST != 0
}

// Cost: O(1) after the first call, which interns every constant's name.
fn all() -> &'static [(Symbol, Constant)] {
    ALL.get_or_init(|| {
        let mut out = Vec::new();
        for (enum_index, e) in ENUMS.iter().enumerate() {
            out.push((
                Symbol::intern(e.name),
                Constant {
                    enum_index,
                    slot: 0,
                },
            ));
            for (v, (variant, _)) in e.variants.iter().enumerate() {
                let c = Constant {
                    enum_index,
                    slot: v + 1,
                };
                out.push((Symbol::intern(variant), c));
                out.push((Symbol::intern(&format!("{}::{variant}", e.name)), c));
            }
        }
        out.into_boxed_slice()
    })
}

/// Every constant's symbol (interning them all on the first call).
// Cost: O(1) to create; walking it is O(C), C = constants.
pub(crate) fn keys() -> impl Iterator<Item = &'static Symbol> {
    all().iter().map(|(sym, _)| sym)
}

/// Every constant and its value, building them all.
// Cost: O(1) to create; walking it is O(C), plus each first build.
pub(crate) fn iter() -> impl Iterator<Item = (&'static Symbol, &'static Value)> {
    all().iter().map(|(sym, c)| (sym, value_of(*c)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn variant_names_are_unique_across_the_core_enums() {
        let mut seen = std::collections::HashSet::new();
        for e in ENUMS {
            assert!(e.variants.len() < SLOTS_PER_ENUM, "{}", e.name);
            assert!(seen.insert(e.name), "{}", e.name);
            for (variant, _) in e.variants {
                assert!(seen.insert(*variant), "{variant} is not unique");
            }
        }
    }

    #[test]
    fn every_spelling_names_its_constant() {
        for (e, en) in ENUMS.iter().enumerate() {
            assert_eq!(
                constant(en.name),
                Some(Constant {
                    enum_index: e,
                    slot: 0
                })
            );
            for (v, (variant, _)) in en.variants.iter().enumerate() {
                let c = Some(Constant {
                    enum_index: e,
                    slot: v + 1,
                });
                assert_eq!(constant(variant), c, "{variant}");
                assert_eq!(constant(&format!("{}::{variant}", en.name)), c);
            }
        }
        for name in [
            "less",
            "Order::",
            "Order::Nope",
            "Nope::Less",
            "SIGNOPE",
            "Ordered",
        ] {
            assert_eq!(constant(name), None, "{name}");
        }
        assert_eq!(
            keys().count(),
            7 + 2 * ENUMS.iter().map(|e| e.variants.len()).sum::<usize>()
        );
    }

    #[test]
    fn a_constant_is_flagged_and_built_on_lookup() {
        let less = Symbol::intern("Order::Less");
        assert!(contains(less));
        assert!(crate::symbol::maybe_env_key(less));
        let v = get(less).expect("constant");
        assert_eq!(v.to_string_value(), "Less");
        assert!(std::ptr::eq(v, get(Symbol::intern("Less")).unwrap()));
        assert!(get(Symbol::intern("Ordinary")).is_none());
        assert_eq!(
            get(Symbol::intern("ProtocolFamily")).map(|v| v.to_string_value()),
            Some(Value::package(Symbol::intern("ProtocolFamily")).to_string_value())
        );
    }
}
