//! Custom serde serialization for Value.
//!
//! Only a subset of Value variants can appear in AST literals. Non-serializable
//! variants (Sub, WeakSub, Promise, Channel, LazyList, Proxy, CustomType, etc.)
//! are rejected at serialization time with an error.

use super::{ArrayKind, EnumValue, JunctionKind, Value, VersionPart};
use crate::symbol::Symbol;
use num_bigint::BigInt as NumBigInt;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

/// A serializable representation of Value.
/// Only includes variants that can appear in parsed AST literals.
#[derive(Serialize, Deserialize)]
enum SerValue {
    Int(i64),
    BigInt(NumBigInt),
    Num(f64),
    Str(String),
    Bool(bool),
    Range(i64, i64),
    RangeExcl(i64, i64),
    RangeExclStart(i64, i64),
    RangeExclBoth(i64, i64),
    GenericRange {
        start: Box<SerValue>,
        end: Box<SerValue>,
        excl_start: bool,
        excl_end: bool,
    },
    Array(Vec<SerValue>, ArrayKind),
    Hash(HashMap<String, SerValue>),
    Rat(i64, i64),
    FatRat(i64, i64),
    BigRat(NumBigInt, NumBigInt),
    Complex(f64, f64),
    Set(HashSet<String>),
    Bag(HashMap<String, i64>),
    Mix(HashMap<String, f64>),
    CompUnitDepSpec {
        short_name: Symbol,
    },
    Package(Symbol),
    Routine {
        package: Symbol,
        name: Symbol,
        is_regex: bool,
    },
    Pair(String, Box<SerValue>),
    ValuePair(Box<SerValue>, Box<SerValue>),
    Enum {
        enum_type: Symbol,
        key: Symbol,
        value: EnumValue,
        index: usize,
    },
    Regex(String),
    RegexWithAdverbs {
        pattern: String,
        global: bool,
        exhaustive: bool,
        overlap: bool,
        repeat: Option<usize>,
        nth: Option<String>,
        perl5: bool,
        pos: bool,
        continue_: bool,
        ignore_case: bool,
        sigspace: bool,
        samecase: bool,
        samespace: bool,
    },
    Junction {
        kind: SerJunctionKind,
        values: Vec<SerValue>,
    },
    Seq(Vec<SerValue>),
    Slip(Vec<SerValue>),
    Version {
        parts: Vec<VersionPart>,
        plus: bool,
        minus: bool,
    },
    Instance {
        class_name: Symbol,
        attributes: HashMap<String, SerValue>,
        id: u64,
    },
    Mixin(Box<SerValue>, HashMap<String, SerValue>),
    Capture {
        positional: Vec<SerValue>,
        named: HashMap<String, SerValue>,
    },
    Uni {
        form: String,
        text: String,
    },
    ParametricRole {
        base_name: Symbol,
        type_args: Vec<SerValue>,
    },
    Scalar(Box<SerValue>),
    Nil,
    Whatever,
    HyperWhatever,
}

#[derive(Serialize, Deserialize)]
enum SerJunctionKind {
    Any,
    All,
    One,
    None,
}

fn value_to_ser(v: &Value) -> Result<SerValue, String> {
    match v {
        Value::Int(n) => Ok(SerValue::Int(*n)),
        Value::BigInt(n) => Ok(SerValue::BigInt((**n).clone())),
        Value::Num(n) => Ok(SerValue::Num(*n)),
        Value::Str(s) => Ok(SerValue::Str((**s).clone())),
        Value::Bool(b) => Ok(SerValue::Bool(*b)),
        Value::Range(a, b) => Ok(SerValue::Range(*a, *b)),
        Value::RangeExcl(a, b) => Ok(SerValue::RangeExcl(*a, *b)),
        Value::RangeExclStart(a, b) => Ok(SerValue::RangeExclStart(*a, *b)),
        Value::RangeExclBoth(a, b) => Ok(SerValue::RangeExclBoth(*a, *b)),
        Value::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => Ok(SerValue::GenericRange {
            start: Box::new(value_to_ser(start)?),
            end: Box::new(value_to_ser(end)?),
            excl_start: *excl_start,
            excl_end: *excl_end,
        }),
        Value::Array(items, kind) => {
            let ser_items: Result<Vec<_>, _> = items.iter().map(value_to_ser).collect();
            Ok(SerValue::Array(ser_items?, *kind))
        }
        Value::Hash(map) => {
            let ser_map: Result<HashMap<_, _>, _> = map
                .iter()
                .map(|(k, v)| value_to_ser(v).map(|sv| (k.clone(), sv)))
                .collect();
            Ok(SerValue::Hash(ser_map?))
        }
        Value::Rat(n, d) => Ok(SerValue::Rat(*n, *d)),
        Value::FatRat(n, d) => Ok(SerValue::FatRat(*n, *d)),
        Value::BigRat(n, d) => Ok(SerValue::BigRat(n.clone(), d.clone())),
        Value::Complex(r, i) => Ok(SerValue::Complex(*r, *i)),
        Value::Set(s) => Ok(SerValue::Set((**s).clone())),
        Value::Bag(b) => Ok(SerValue::Bag((**b).clone())),
        Value::Mix(m) => Ok(SerValue::Mix((**m).clone())),
        Value::CompUnitDepSpec { short_name } => Ok(SerValue::CompUnitDepSpec {
            short_name: *short_name,
        }),
        Value::Package(s) => Ok(SerValue::Package(*s)),
        Value::Routine {
            package,
            name,
            is_regex,
        } => Ok(SerValue::Routine {
            package: *package,
            name: *name,
            is_regex: *is_regex,
        }),
        Value::Pair(k, v) => Ok(SerValue::Pair(k.clone(), Box::new(value_to_ser(v)?))),
        Value::ValuePair(k, v) => Ok(SerValue::ValuePair(
            Box::new(value_to_ser(k)?),
            Box::new(value_to_ser(v)?),
        )),
        Value::Enum {
            enum_type,
            key,
            value,
            index,
        } => Ok(SerValue::Enum {
            enum_type: *enum_type,
            key: *key,
            value: value.clone(),
            index: *index,
        }),
        Value::Regex(s) => Ok(SerValue::Regex((**s).clone())),
        Value::RegexWithAdverbs {
            pattern,
            global,
            exhaustive,
            overlap,
            repeat,
            nth,
            perl5,
            pos,
            continue_,
            ignore_case,
            sigspace,
            samecase,
            samespace,
        } => Ok(SerValue::RegexWithAdverbs {
            pattern: (**pattern).clone(),
            global: *global,
            exhaustive: *exhaustive,
            overlap: *overlap,
            repeat: *repeat,
            nth: nth.as_ref().map(|v| (**v).clone()),
            perl5: *perl5,
            pos: *pos,
            continue_: *continue_,
            ignore_case: *ignore_case,
            sigspace: *sigspace,
            samecase: *samecase,
            samespace: *samespace,
        }),
        Value::Junction { kind, values } => {
            let ser_kind = match kind {
                JunctionKind::Any => SerJunctionKind::Any,
                JunctionKind::All => SerJunctionKind::All,
                JunctionKind::One => SerJunctionKind::One,
                JunctionKind::None => SerJunctionKind::None,
            };
            let ser_values: Result<Vec<_>, _> = values.iter().map(value_to_ser).collect();
            Ok(SerValue::Junction {
                kind: ser_kind,
                values: ser_values?,
            })
        }
        Value::Seq(items) => {
            let ser_items: Result<Vec<_>, _> = items.iter().map(value_to_ser).collect();
            Ok(SerValue::Seq(ser_items?))
        }
        Value::Slip(items) => {
            let ser_items: Result<Vec<_>, _> = items.iter().map(value_to_ser).collect();
            Ok(SerValue::Slip(ser_items?))
        }
        Value::Version { parts, plus, minus } => Ok(SerValue::Version {
            parts: parts.clone(),
            plus: *plus,
            minus: *minus,
        }),
        Value::Instance {
            class_name,
            attributes,
            id,
        } => {
            let ser_attrs: Result<HashMap<_, _>, _> = attributes
                .iter()
                .map(|(k, v)| value_to_ser(v).map(|sv| (k.clone(), sv)))
                .collect();
            Ok(SerValue::Instance {
                class_name: *class_name,
                attributes: ser_attrs?,
                id: *id,
            })
        }
        Value::Mixin(inner, overrides) => {
            let ser_overrides: Result<HashMap<_, _>, _> = overrides
                .iter()
                .map(|(k, v)| value_to_ser(v).map(|sv| (k.clone(), sv)))
                .collect();
            Ok(SerValue::Mixin(
                Box::new(value_to_ser(inner)?),
                ser_overrides?,
            ))
        }
        Value::Capture { positional, named } => {
            let ser_pos: Result<Vec<_>, _> = positional.iter().map(value_to_ser).collect();
            let ser_named: Result<HashMap<_, _>, _> = named
                .iter()
                .map(|(k, v)| value_to_ser(v).map(|sv| (k.clone(), sv)))
                .collect();
            Ok(SerValue::Capture {
                positional: ser_pos?,
                named: ser_named?,
            })
        }
        Value::Uni { form, text } => Ok(SerValue::Uni {
            form: form.clone(),
            text: text.clone(),
        }),
        Value::ParametricRole {
            base_name,
            type_args,
        } => {
            let ser_args: Result<Vec<_>, _> = type_args.iter().map(value_to_ser).collect();
            Ok(SerValue::ParametricRole {
                base_name: *base_name,
                type_args: ser_args?,
            })
        }
        Value::Scalar(inner) => Ok(SerValue::Scalar(Box::new(value_to_ser(inner)?))),
        Value::Nil => Ok(SerValue::Nil),
        Value::Whatever => Ok(SerValue::Whatever),
        Value::HyperWhatever => Ok(SerValue::HyperWhatever),
        // Non-serializable variants
        Value::Sub(_)
        | Value::WeakSub(_)
        | Value::LazyList(_)
        | Value::Promise(_)
        | Value::Channel(_)
        | Value::Proxy { .. }
        | Value::CustomType { .. }
        | Value::CustomTypeInstance { .. } => Err(format!(
            "cannot serialize Value variant: {:?}",
            std::mem::discriminant(v)
        )),
    }
}

fn ser_to_value(sv: SerValue) -> Value {
    match sv {
        SerValue::Int(n) => Value::Int(n),
        SerValue::BigInt(n) => Value::BigInt(Arc::new(n)),
        SerValue::Num(n) => Value::Num(n),
        SerValue::Str(s) => Value::Str(Arc::new(s)),
        SerValue::Bool(b) => Value::Bool(b),
        SerValue::Range(a, b) => Value::Range(a, b),
        SerValue::RangeExcl(a, b) => Value::RangeExcl(a, b),
        SerValue::RangeExclStart(a, b) => Value::RangeExclStart(a, b),
        SerValue::RangeExclBoth(a, b) => Value::RangeExclBoth(a, b),
        SerValue::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => Value::GenericRange {
            start: Arc::new(ser_to_value(*start)),
            end: Arc::new(ser_to_value(*end)),
            excl_start,
            excl_end,
        },
        SerValue::Array(items, kind) => Value::Array(
            Arc::new(items.into_iter().map(ser_to_value).collect()),
            kind,
        ),
        SerValue::Hash(map) => Value::Hash(Arc::new(
            map.into_iter().map(|(k, v)| (k, ser_to_value(v))).collect(),
        )),
        SerValue::Rat(n, d) => Value::Rat(n, d),
        SerValue::FatRat(n, d) => Value::FatRat(n, d),
        SerValue::BigRat(n, d) => Value::BigRat(n, d),
        SerValue::Complex(r, i) => Value::Complex(r, i),
        SerValue::Set(s) => Value::Set(Arc::new(s)),
        SerValue::Bag(b) => Value::Bag(Arc::new(b)),
        SerValue::Mix(m) => Value::Mix(Arc::new(m)),
        SerValue::CompUnitDepSpec { short_name } => Value::CompUnitDepSpec { short_name },
        SerValue::Package(s) => Value::Package(s),
        SerValue::Routine {
            package,
            name,
            is_regex,
        } => Value::Routine {
            package,
            name,
            is_regex,
        },
        SerValue::Pair(k, v) => Value::Pair(k, Box::new(ser_to_value(*v))),
        SerValue::ValuePair(k, v) => {
            Value::ValuePair(Box::new(ser_to_value(*k)), Box::new(ser_to_value(*v)))
        }
        SerValue::Enum {
            enum_type,
            key,
            value,
            index,
        } => Value::Enum {
            enum_type,
            key,
            value,
            index,
        },
        SerValue::Regex(s) => Value::Regex(Arc::new(s)),
        SerValue::RegexWithAdverbs {
            pattern,
            global,
            exhaustive,
            overlap,
            repeat,
            nth,
            perl5,
            pos,
            continue_,
            ignore_case,
            sigspace,
            samecase,
            samespace,
        } => Value::RegexWithAdverbs {
            pattern: Arc::new(pattern),
            global,
            exhaustive,
            overlap,
            repeat,
            nth: nth.map(Arc::new),
            perl5,
            pos,
            continue_,
            ignore_case,
            sigspace,
            samecase,
            samespace,
        },
        SerValue::Junction { kind, values } => {
            let jk = match kind {
                SerJunctionKind::Any => JunctionKind::Any,
                SerJunctionKind::All => JunctionKind::All,
                SerJunctionKind::One => JunctionKind::One,
                SerJunctionKind::None => JunctionKind::None,
            };
            Value::Junction {
                kind: jk,
                values: Arc::new(values.into_iter().map(ser_to_value).collect()),
            }
        }
        SerValue::Seq(items) => Value::Seq(Arc::new(items.into_iter().map(ser_to_value).collect())),
        SerValue::Slip(items) => {
            Value::Slip(Arc::new(items.into_iter().map(ser_to_value).collect()))
        }
        SerValue::Version { parts, plus, minus } => Value::Version { parts, plus, minus },
        SerValue::Instance {
            class_name,
            attributes,
            id,
        } => Value::Instance {
            class_name,
            attributes: Arc::new(crate::value::InstanceAttrs::new(
                class_name,
                attributes
                    .into_iter()
                    .map(|(k, v)| (k, ser_to_value(v)))
                    .collect(),
                id,
                true,
            )),
            id,
        },
        SerValue::Mixin(inner, overrides) => Value::Mixin(
            Arc::new(ser_to_value(*inner)),
            Arc::new(
                overrides
                    .into_iter()
                    .map(|(k, v)| (k, ser_to_value(v)))
                    .collect(),
            ),
        ),
        SerValue::Capture { positional, named } => Value::Capture {
            positional: positional.into_iter().map(ser_to_value).collect(),
            named: named
                .into_iter()
                .map(|(k, v)| (k, ser_to_value(v)))
                .collect(),
        },
        SerValue::Uni { form, text } => Value::Uni { form, text },
        SerValue::ParametricRole {
            base_name,
            type_args,
        } => Value::ParametricRole {
            base_name,
            type_args: type_args.into_iter().map(ser_to_value).collect(),
        },
        SerValue::Scalar(inner) => Value::Scalar(Box::new(ser_to_value(*inner))),
        SerValue::Nil => Value::Nil,
        SerValue::Whatever => Value::Whatever,
        SerValue::HyperWhatever => Value::HyperWhatever,
    }
}

impl Serialize for Value {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let sv = value_to_ser(self).map_err(serde::ser::Error::custom)?;
        sv.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Value {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let sv = SerValue::deserialize(deserializer)?;
        Ok(ser_to_value(sv))
    }
}
