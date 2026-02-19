use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicU64, Ordering};

use crate::ast::Stmt;
use num_bigint::BigInt as NumBigInt;
use num_traits::{ToPrimitive, Zero};
use std::cell::RefCell;
use std::rc::Rc;

static INSTANCE_ID_COUNTER: AtomicU64 = AtomicU64::new(1);

pub(crate) fn next_instance_id() -> u64 {
    INSTANCE_ID_COUNTER.fetch_add(1, Ordering::Relaxed)
}

#[derive(Debug, Clone, PartialEq)]
pub enum JunctionKind {
    Any,
    All,
    One,
    None,
}

fn gcd(mut a: i64, mut b: i64) -> i64 {
    a = a.abs();
    b = b.abs();
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

/// Apply tclc (titlecase first char, lowercase rest) to a string.
pub fn tclc_str(s: &str) -> String {
    let mut result = String::new();
    let mut first = true;
    for ch in s.chars() {
        if first {
            result.push_str(&crate::builtins::unicode_titlecase_first(ch));
            first = false;
        } else {
            for c in ch.to_lowercase() {
                result.push(c);
            }
        }
    }
    result
}

/// Apply wordcase to a string: find words matching <ident>+ % <[ - ' ]>
/// and apply tclc to each word. Non-word characters pass through unchanged.
pub fn wordcase_str(s: &str) -> String {
    let chars: Vec<char> = s.chars().collect();
    let len = chars.len();
    let mut result = String::new();
    let mut i = 0;

    while i < len {
        if chars[i].is_alphabetic() || chars[i] == '_' {
            let word_start = i;
            // Consume first ident: [alpha|_] \w*
            i += 1;
            while i < len && (chars[i].is_alphanumeric() || chars[i] == '_') {
                i += 1;
            }
            // Try to consume more idents separated by - or '
            loop {
                if i < len
                    && (chars[i] == '-' || chars[i] == '\'')
                    && i + 1 < len
                    && (chars[i + 1].is_alphabetic() || chars[i + 1] == '_')
                {
                    i += 1; // consume separator
                    i += 1; // consume first char of next ident
                    while i < len && (chars[i].is_alphanumeric() || chars[i] == '_') {
                        i += 1;
                    }
                } else {
                    break;
                }
            }
            let word: String = chars[word_start..i].iter().collect();
            result.push_str(&tclc_str(&word));
        } else {
            result.push(chars[i]);
            i += 1;
        }
    }
    result
}

pub fn format_complex(r: f64, i: f64) -> String {
    fn fmt_num(v: f64) -> String {
        if v.fract() == 0.0 && v.is_finite() {
            format!("{}", v as i64)
        } else {
            format!("{}", v)
        }
    }
    if i == 0.0 {
        format!("{}+0i", fmt_num(r))
    } else if i < 0.0 {
        format!("{}{}i", fmt_num(r), fmt_num(i))
    } else {
        format!("{}+{}i", fmt_num(r), fmt_num(i))
    }
}

pub fn make_rat(num: i64, den: i64) -> Value {
    if den == 0 {
        if num == 0 {
            return Value::Rat(0, 0); // NaN
        } else if num > 0 {
            return Value::Rat(1, 0); // Inf
        } else {
            return Value::Rat(-1, 0); // -Inf
        }
    }
    let g = gcd(num, den);
    let (mut n, mut d) = (num / g, den / g);
    if d < 0 {
        n = -n;
        d = -d;
    }
    Value::Rat(n, d)
}

#[allow(private_interfaces)]
#[derive(Debug, Clone)]
pub enum Value {
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
        start: Box<Value>,
        end: Box<Value>,
        excl_start: bool,
        excl_end: bool,
    },
    Array(Vec<Value>),
    Hash(HashMap<String, Value>),
    Rat(i64, i64),
    FatRat(i64, i64),
    Complex(f64, f64),
    Set(HashSet<String>),
    Bag(HashMap<String, i64>),
    Mix(HashMap<String, f64>),
    CompUnitDepSpec {
        short_name: String,
    },
    Package(String),
    Routine {
        package: String,
        name: String,
    },
    Pair(String, Box<Value>),
    Enum {
        enum_type: String,
        key: String,
        value: i64,
        index: usize,
    },
    Regex(String),
    Sub {
        package: String,
        name: String,
        params: Vec<String>,
        body: Vec<Stmt>,
        env: HashMap<String, Value>,
        id: u64,
    },
    Instance {
        class_name: String,
        attributes: HashMap<String, Value>,
        id: u64,
    },
    Junction {
        kind: JunctionKind,
        values: Vec<Value>,
    },
    Slip(Vec<Value>),
    LazyList(Rc<LazyList>),
    Version {
        parts: Vec<VersionPart>,
        plus: bool,
        minus: bool,
    },
    Nil,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum VersionPart {
    Num(i64),
    Str(String),
    Whatever,
}

#[derive(Debug, Clone)]
pub(crate) struct LazyList {
    pub(crate) body: Vec<Stmt>,
    pub(crate) env: HashMap<String, Value>,
    pub(crate) cache: RefCell<Option<Vec<Value>>>,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::BigInt(a), Value::BigInt(b)) => a == b,
            (Value::BigInt(a), Value::Int(b)) | (Value::Int(b), Value::BigInt(a)) => {
                *a == NumBigInt::from(*b)
            }
            (Value::Num(a), Value::Num(b)) => (a.is_nan() && b.is_nan()) || a == b,
            (Value::Int(a), Value::Num(b)) => (*a as f64) == *b,
            (Value::Num(a), Value::Int(b)) => *a == (*b as f64),
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Range(a1, b1), Value::Range(a2, b2)) => a1 == a2 && b1 == b2,
            (Value::RangeExcl(a1, b1), Value::RangeExcl(a2, b2)) => a1 == a2 && b1 == b2,
            (Value::RangeExclStart(a1, b1), Value::RangeExclStart(a2, b2)) => a1 == a2 && b1 == b2,
            (Value::RangeExclBoth(a1, b1), Value::RangeExclBoth(a2, b2)) => a1 == a2 && b1 == b2,
            (
                Value::GenericRange {
                    start: s1,
                    end: e1,
                    excl_start: es1,
                    excl_end: ee1,
                },
                Value::GenericRange {
                    start: s2,
                    end: e2,
                    excl_start: es2,
                    excl_end: ee2,
                },
            ) => es1 == es2 && ee1 == ee2 && s1 == s2 && e1 == e2,
            (Value::Array(a), Value::Array(b)) => a == b,
            (Value::Hash(a), Value::Hash(b)) => a == b,
            (Value::Rat(a1, b1), Value::Rat(a2, b2)) => {
                if *b1 == 0 && *b2 == 0 && *a1 == 0 && *a2 == 0 {
                    return false; // NaN != NaN
                }
                a1 == a2 && b1 == b2
            }
            (Value::Rat(n, d), Value::Int(i)) | (Value::Int(i), Value::Rat(n, d)) => {
                *d != 0 && *n == *i * *d
            }
            (Value::Rat(n, d), Value::Num(f)) | (Value::Num(f), Value::Rat(n, d)) => {
                if *d == 0 {
                    return false;
                }
                (*n as f64 / *d as f64) == *f
            }
            (Value::Complex(r1, i1), Value::Complex(r2, i2)) => {
                let f_eq = |a: &f64, b: &f64| (a.is_nan() && b.is_nan()) || a == b;
                f_eq(r1, r2) && f_eq(i1, i2)
            }
            (Value::Complex(r, i), Value::Int(n)) | (Value::Int(n), Value::Complex(r, i)) => {
                *i == 0.0 && *r == *n as f64
            }
            (Value::Complex(r, i), Value::Num(f)) | (Value::Num(f), Value::Complex(r, i)) => {
                *i == 0.0 && *r == *f
            }
            (Value::FatRat(a1, b1), Value::FatRat(a2, b2)) => a1 == a2 && b1 == b2,
            (Value::Set(a), Value::Set(b)) => a == b,
            (Value::Bag(a), Value::Bag(b)) => a == b,
            (Value::Mix(a), Value::Mix(b)) => a == b,
            (
                Value::CompUnitDepSpec { short_name: a },
                Value::CompUnitDepSpec { short_name: b },
            ) => a == b,
            (Value::Package(a), Value::Package(b)) => a == b,
            (Value::Pair(ak, av), Value::Pair(bk, bv)) => ak == bk && av == bv,
            (
                Value::Enum {
                    enum_type: at,
                    key: ak,
                    ..
                },
                Value::Enum {
                    enum_type: bt,
                    key: bk,
                    ..
                },
            ) => at == bt && ak == bk,
            (Value::Regex(a), Value::Regex(b)) => a == b,
            (
                Value::Routine {
                    package: ap,
                    name: an,
                },
                Value::Routine {
                    package: bp,
                    name: bn,
                },
            ) => ap == bp && an == bn,
            (
                Value::Instance {
                    class_name: a,
                    attributes: aa,
                    ..
                },
                Value::Instance {
                    class_name: b,
                    attributes: ba,
                    ..
                },
            ) => a == b && aa == ba,
            (
                Value::Junction {
                    kind: ak,
                    values: av,
                },
                Value::Junction {
                    kind: bk,
                    values: bv,
                },
            ) => ak == bk && av == bv,
            (Value::Sub { id: a, .. }, Value::Sub { id: b, .. }) => a == b,
            (Value::LazyList(a), Value::LazyList(b)) => Rc::ptr_eq(a, b),
            (
                Value::Version {
                    parts: ap,
                    plus: aplus,
                    minus: aminus,
                },
                Value::Version {
                    parts: bp,
                    plus: bplus,
                    minus: bminus,
                },
            ) => {
                if aplus != bplus || aminus != bminus {
                    return false;
                }
                // Normalize trailing zeroes: compare after stripping trailing Num(0)
                let a_norm = Self::version_strip_trailing_zeros(ap);
                let b_norm = Self::version_strip_trailing_zeros(bp);
                a_norm == b_norm
            }
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }
}

impl Value {
    /// Type-strict value equivalence (Raku `eqv` operator).
    /// Unlike PartialEq (used for `==`), this does NOT allow cross-type comparisons.
    /// Two values are eqv only if they are the same type AND have the same value.
    pub(crate) fn eqv(&self, other: &Self) -> bool {
        match (self, other) {
            // Arrays: recursively use eqv for elements
            (Value::Array(a), Value::Array(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.eqv(y))
            }
            // Hashes: recursively use eqv for values
            (Value::Hash(a), Value::Hash(b)) => {
                a.len() == b.len() && a.iter().all(|(k, v)| b.get(k).is_some_and(|bv| v.eqv(bv)))
            }
            // Pairs: recursively use eqv for values
            (Value::Pair(ak, av), Value::Pair(bk, bv)) => ak == bk && av.eqv(bv),
            // Slips: recursively use eqv for elements
            (Value::Slip(a), Value::Slip(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.eqv(y))
            }
            // Same-type scalar comparisons delegate to PartialEq
            (Value::Int(_), Value::Int(_))
            | (Value::Num(_), Value::Num(_))
            | (Value::Str(_), Value::Str(_))
            | (Value::Bool(_), Value::Bool(_))
            | (Value::Rat(_, _), Value::Rat(_, _))
            | (Value::FatRat(_, _), Value::FatRat(_, _))
            | (Value::Complex(_, _), Value::Complex(_, _))
            | (Value::Set(_), Value::Set(_))
            | (Value::Bag(_), Value::Bag(_))
            | (Value::Mix(_), Value::Mix(_))
            | (Value::Enum { .. }, Value::Enum { .. })
            | (Value::Regex(_), Value::Regex(_))
            | (Value::Routine { .. }, Value::Routine { .. })
            | (Value::Sub { .. }, Value::Sub { .. })
            | (Value::Instance { .. }, Value::Instance { .. })
            | (Value::Range(_, _), Value::Range(_, _))
            | (Value::RangeExcl(_, _), Value::RangeExcl(_, _))
            | (Value::RangeExclStart(_, _), Value::RangeExclStart(_, _))
            | (Value::RangeExclBoth(_, _), Value::RangeExclBoth(_, _))
            | (Value::GenericRange { .. }, Value::GenericRange { .. })
            | (Value::LazyList(_), Value::LazyList(_))
            | (Value::Version { .. }, Value::Version { .. })
            | (Value::Nil, Value::Nil)
            | (Value::Package(_), Value::Package(_))
            | (Value::CompUnitDepSpec { .. }, Value::CompUnitDepSpec { .. })
            | (Value::Junction { .. }, Value::Junction { .. }) => self == other,
            // Cross-type comparisons always return false for eqv
            _ => false,
        }
    }

    pub(crate) fn make_instance(class_name: String, attributes: HashMap<String, Value>) -> Self {
        Value::Instance {
            class_name,
            attributes,
            id: next_instance_id(),
        }
    }

    pub(crate) fn version_strip_trailing_zeros(parts: &[VersionPart]) -> Vec<VersionPart> {
        let mut v: Vec<VersionPart> = parts.to_vec();
        while matches!(v.last(), Some(VersionPart::Num(0))) {
            v.pop();
        }
        if v.is_empty() {
            vec![VersionPart::Num(0)]
        } else {
            v
        }
    }

    pub(crate) fn is_range(&self) -> bool {
        matches!(
            self,
            Value::Range(_, _)
                | Value::RangeExcl(_, _)
                | Value::RangeExclStart(_, _)
                | Value::RangeExclBoth(_, _)
                | Value::GenericRange { .. }
        )
    }

    /// Check if this value is a numeric type (Int, Num, Rat, FatRat, BigInt).
    pub(crate) fn is_numeric(&self) -> bool {
        matches!(
            self,
            Value::Int(_)
                | Value::BigInt(_)
                | Value::Num(_)
                | Value::Rat(_, _)
                | Value::FatRat(_, _)
        )
    }

    /// Convert a numeric value to f64.
    pub(crate) fn to_f64(&self) -> f64 {
        match self {
            Value::Int(i) => *i as f64,
            Value::BigInt(n) => n.to_f64().unwrap_or(0.0),
            Value::Num(f) => *f,
            Value::Rat(n, d) => {
                if *d != 0 {
                    *n as f64 / *d as f64
                } else if *n == 0 {
                    f64::NAN
                } else if *n > 0 {
                    f64::INFINITY
                } else {
                    f64::NEG_INFINITY
                }
            }
            Value::FatRat(n, d) => {
                if *d != 0 {
                    *n as f64 / *d as f64
                } else if *n == 0 {
                    f64::NAN
                } else if *n > 0 {
                    f64::INFINITY
                } else {
                    f64::NEG_INFINITY
                }
            }
            Value::Bool(b) => {
                if *b {
                    1.0
                } else {
                    0.0
                }
            }
            Value::Str(s) => s.trim().parse::<f64>().unwrap_or(0.0),
            _ => 0.0,
        }
    }

    pub(crate) fn truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Int(i) => *i != 0,
            Value::BigInt(n) => !n.is_zero(),
            Value::Num(f) => *f != 0.0 && !f.is_nan(),
            Value::Str(s) => !s.is_empty(),
            Value::Range(_, _) => true,
            Value::RangeExcl(_, _) => true,
            Value::RangeExclStart(_, _) => true,
            Value::RangeExclBoth(_, _) => true,
            Value::GenericRange { .. } => true,
            Value::Array(items) => !items.is_empty(),
            Value::Hash(items) => !items.is_empty(),
            Value::Rat(n, _) => *n != 0,
            Value::FatRat(_, _) => true,
            Value::Complex(r, i) => *r != 0.0 || *i != 0.0,
            Value::Set(s) => !s.is_empty(),
            Value::Bag(b) => !b.is_empty(),
            Value::Mix(m) => !m.is_empty(),
            Value::Pair(_, _) => true,
            Value::Enum { .. } => true,
            Value::CompUnitDepSpec { .. } => true,
            Value::Package(_) => false,
            Value::Routine { .. } => true,
            Value::Sub { .. } => true,
            Value::Instance {
                class_name,
                attributes,
                ..
            } => {
                if class_name == "Proc" {
                    // Proc is truthy when exitcode == 0
                    return match attributes.get("exitcode") {
                        Some(Value::Int(code)) => *code == 0,
                        _ => false,
                    };
                }
                true
            }
            Value::Junction { kind, values } => match kind {
                JunctionKind::Any => values.iter().any(|v| v.truthy()),
                JunctionKind::All => values.iter().all(|v| v.truthy()),
                JunctionKind::One => values.iter().filter(|v| v.truthy()).count() == 1,
                JunctionKind::None => values.iter().all(|v| !v.truthy()),
            },
            Value::Slip(items) => !items.is_empty(),
            Value::LazyList(_) => true,
            Value::Regex(_) => true,
            Value::Version { .. } => true,
            Value::Nil => false,
        }
    }

    /// Check if this value is an instance of the given type name (Raku `isa` operator).
    pub(crate) fn isa_check(&self, type_name: &str) -> bool {
        let my_type = match self {
            Value::Int(_) | Value::BigInt(_) => "Int",
            Value::Num(_) => "Num",
            Value::Str(_) => "Str",
            Value::Bool(_) => "Bool",
            Value::Rat(_, _) => "Rat",
            Value::FatRat(_, _) => "FatRat",
            Value::Complex(_, _) => "Complex",
            Value::Array(_) | Value::LazyList(_) => "Array",
            Value::Hash(_) => "Hash",
            Value::Set(_) => "Set",
            Value::Bag(_) => "Bag",
            Value::Mix(_) => "Mix",
            Value::Pair(_, _) => "Pair",
            Value::Range(_, _)
            | Value::RangeExcl(_, _)
            | Value::RangeExclStart(_, _)
            | Value::RangeExclBoth(_, _)
            | Value::GenericRange { .. } => "Range",
            Value::Nil => "Nil",
            Value::Instance { class_name, .. } => class_name.as_str(),
            Value::Package(name) => name.as_str(),
            Value::Enum { enum_type, .. } => enum_type.as_str(),
            Value::Sub { .. } | Value::Routine { .. } => "Sub",
            Value::Regex(_) => "Regex",
            Value::Junction { .. } => "Junction",
            Value::Version { .. } => "Version",
            Value::Slip(_) => "Slip",
            Value::CompUnitDepSpec { .. } => "CompUnit::DependencySpecification",
        };
        if my_type == type_name {
            return true;
        }
        // Check type hierarchy
        match type_name {
            "Any" => true,
            "Mu" => true,
            "Cool" => matches!(
                self,
                Value::Int(_)
                    | Value::BigInt(_)
                    | Value::Num(_)
                    | Value::Str(_)
                    | Value::Bool(_)
                    | Value::Rat(_, _)
                    | Value::FatRat(_, _)
                    | Value::Complex(_, _)
                    | Value::Array(_)
                    | Value::Hash(_)
            ),
            "Numeric" => matches!(
                self,
                Value::Int(_)
                    | Value::BigInt(_)
                    | Value::Num(_)
                    | Value::Rat(_, _)
                    | Value::FatRat(_, _)
                    | Value::Complex(_, _)
            ),
            "Real" => matches!(
                self,
                Value::Int(_)
                    | Value::BigInt(_)
                    | Value::Num(_)
                    | Value::Rat(_, _)
                    | Value::FatRat(_, _)
            ),
            "Int" => matches!(self, Value::Bool(_)),
            "Stringy" => matches!(self, Value::Str(_)),
            _ => false,
        }
    }

    /// Check if this value does (composes) the given role name.
    pub(crate) fn does_check(&self, role_name: &str) -> bool {
        // For now, delegate to isa_check since we don't track roles separately
        self.isa_check(role_name)
    }

    pub(crate) fn to_string_value(&self) -> String {
        match self {
            Value::Int(i) => i.to_string(),
            Value::BigInt(n) => n.to_string(),
            Value::Num(f) => {
                if f.is_nan() {
                    "NaN".to_string()
                } else if f.is_infinite() {
                    if *f > 0.0 {
                        "Inf".to_string()
                    } else {
                        "-Inf".to_string()
                    }
                } else if f.fract() == 0.0 && f.is_finite() {
                    format!("{}", *f as i64)
                } else {
                    format!("{}", f)
                }
            }
            Value::Str(s) => s.clone(),
            Value::Bool(true) => "True".to_string(),
            Value::Bool(false) => "False".to_string(),
            Value::Range(a, b) => format!("{}..{}", a, b),
            Value::RangeExcl(a, b) => format!("{}..^{}", a, b),
            Value::RangeExclStart(a, b) => format!("^{}..{}", a, b),
            Value::RangeExclBoth(a, b) => format!("^{}..^{}", a, b),
            Value::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                // String ranges expand to space-separated elements
                if let (Value::Str(a), Value::Str(b)) = (start.as_ref(), end.as_ref())
                    && a.len() == 1
                    && b.len() == 1
                {
                    let s = a.chars().next().unwrap() as u32;
                    let e = b.chars().next().unwrap() as u32;
                    let s = if *excl_start { s + 1 } else { s };
                    let items: Vec<String> = if *excl_end {
                        (s..e)
                            .filter_map(char::from_u32)
                            .map(|c| c.to_string())
                            .collect()
                    } else {
                        (s..=e)
                            .filter_map(char::from_u32)
                            .map(|c| c.to_string())
                            .collect()
                    };
                    return items.join(" ");
                }
                let prefix = if *excl_start { "^" } else { "" };
                let sep = if *excl_end { "..^" } else { ".." };
                format!(
                    "{}{}{}{}",
                    prefix,
                    start.to_string_value(),
                    sep,
                    end.to_string_value()
                )
            }
            Value::Array(items) => items
                .iter()
                .map(|v| v.to_string_value())
                .collect::<Vec<_>>()
                .join(" "),
            Value::LazyList(_) => "LazyList".to_string(),
            Value::Hash(items) => items
                .iter()
                .map(|(k, v)| format!("{}\t{}", k, v.to_string_value()))
                .collect::<Vec<_>>()
                .join("\n"),
            Value::Rat(n, d) => {
                if *d == 0 {
                    if *n == 0 {
                        "NaN".to_string()
                    } else if *n > 0 {
                        "Inf".to_string()
                    } else {
                        "-Inf".to_string()
                    }
                } else if *n % *d == 0 {
                    // Exact integer: Rat(10, 2) => "5"
                    format!("{}", *n / *d)
                } else {
                    let whole = *n as f64 / *d as f64;
                    // Check if it can be represented as exact decimal
                    let mut dd = *d;
                    while dd % 2 == 0 {
                        dd /= 2;
                    }
                    while dd % 5 == 0 {
                        dd /= 5;
                    }
                    if dd == 1 {
                        // Exact decimal representation
                        let s = format!("{}", whole);
                        if s.contains('.') {
                            s
                        } else {
                            format!("{}.0", whole)
                        }
                    } else {
                        format!("{:.6}", whole)
                    }
                }
            }
            Value::FatRat(a, b) => format!("{}/{}", a, b),
            Value::Complex(r, i) => format_complex(*r, *i),
            Value::Set(s) => {
                let mut keys: Vec<&String> = s.iter().collect();
                keys.sort();
                format!(
                    "set({})",
                    keys.iter()
                        .map(|k| k.as_str())
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            }
            Value::Bag(b) => {
                let mut keys: Vec<(&String, &i64)> = b.iter().collect();
                keys.sort_by_key(|(k, _)| (*k).clone());
                format!(
                    "bag({})",
                    keys.iter()
                        .map(|(k, v)| format!("{}({})", k, v))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Value::Mix(m) => {
                let mut keys: Vec<(&String, &f64)> = m.iter().collect();
                keys.sort_by_key(|(k, _)| (*k).clone());
                format!(
                    "mix({})",
                    keys.iter()
                        .map(|(k, v)| format!("{}({})", k, v))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Value::Pair(k, v) => format!("{}\t{}", k, v.to_string_value()),
            Value::Enum { key, .. } => key.clone(),
            Value::CompUnitDepSpec { short_name } => {
                format!("CompUnit::DependencySpecification({})", short_name)
            }
            Value::Package(s) => format!("({})", s),
            Value::Routine { package, name } => format!("{}::{}", package, name),
            Value::Sub { name, .. } => name.clone(),
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "IO::Path" => attributes
                .get("path")
                .map(|v: &Value| v.to_string_value())
                .unwrap_or_else(|| format!("{}()", class_name)),
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Exception" => attributes
                .get("message")
                .map(|v: &Value| v.to_string_value())
                .unwrap_or_else(|| format!("{}()", class_name)),
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "ObjAt" => attributes
                .get("WHICH")
                .map(|v: &Value| v.to_string_value())
                .unwrap_or_else(|| format!("{}()", class_name)),
            Value::Instance { class_name, .. } => format!("{}()", class_name),
            Value::Junction { kind, values } => {
                let kind_str = match kind {
                    JunctionKind::Any => "any",
                    JunctionKind::All => "all",
                    JunctionKind::One => "one",
                    JunctionKind::None => "none",
                };
                let elems = values
                    .iter()
                    .map(|v| v.to_string_value())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", kind_str, elems)
            }
            Value::Regex(pattern) => format!("/{}/", pattern),
            Value::Version { parts, plus, minus } => {
                let s = Self::version_parts_to_string(parts);
                if *plus {
                    format!("{}+", s)
                } else if *minus {
                    format!("{}-", s)
                } else {
                    s
                }
            }
            Value::Slip(items) => items
                .iter()
                .map(|v| v.to_string_value())
                .collect::<Vec<_>>()
                .join(" "),
            Value::Nil => String::new(),
        }
    }

    pub(crate) fn version_parts_to_string(parts: &[VersionPart]) -> String {
        parts
            .iter()
            .map(|p| match p {
                VersionPart::Num(n) => n.to_string(),
                VersionPart::Str(s) => s.clone(),
                VersionPart::Whatever => "*".to_string(),
            })
            .collect::<Vec<_>>()
            .join(".")
    }

    /// Parse a version string into parts, handling:
    /// - `.`, `-`, `+`, `/` as separators
    /// - `_` as a separator (becomes a separate part if alone)
    /// - Transitions between digits and non-digit/non-separator chars create implicit splits
    /// - Leading zeros stripped from numeric parts
    pub(crate) fn parse_version_string(s: &str) -> (Vec<VersionPart>, bool, bool) {
        let mut plus = false;
        let mut minus = false;
        let mut raw = s;
        if let Some(stripped) = raw.strip_suffix('+') {
            plus = true;
            raw = stripped;
        } else if let Some(stripped) = raw.strip_suffix('-') {
            minus = true;
            raw = stripped;
        }

        let mut parts = Vec::new();
        let mut current = String::new();
        let mut is_digit_run = false;

        for ch in raw.chars() {
            match ch {
                '.' | '-' | '+' | '/' => {
                    if !current.is_empty() {
                        parts.push(Self::make_version_part(&current, is_digit_run));
                        current.clear();
                    }
                    is_digit_run = false;
                }
                '_' => {
                    if !current.is_empty() {
                        parts.push(Self::make_version_part(&current, is_digit_run));
                        current.clear();
                    }
                    // _ by itself is a "whatever" separator part
                    parts.push(VersionPart::Str("_".to_string()));
                    is_digit_run = false;
                }
                c if c.is_ascii_digit() => {
                    if !current.is_empty() && !is_digit_run {
                        // Transition from alpha to digit
                        parts.push(Self::make_version_part(&current, false));
                        current.clear();
                    }
                    current.push(c);
                    is_digit_run = true;
                }
                c => {
                    if !current.is_empty() && is_digit_run {
                        // Transition from digit to alpha
                        parts.push(Self::make_version_part(&current, true));
                        current.clear();
                    }
                    current.push(c);
                    is_digit_run = false;
                }
            }
        }
        if !current.is_empty() {
            parts.push(Self::make_version_part(&current, is_digit_run));
        }

        if parts.is_empty() {
            parts.push(VersionPart::Num(0));
        }

        (parts, plus, minus)
    }

    fn make_version_part(s: &str, is_digit: bool) -> VersionPart {
        if s == "*" {
            VersionPart::Whatever
        } else if is_digit {
            VersionPart::Num(s.parse::<i64>().unwrap_or(0))
        } else {
            VersionPart::Str(s.to_string())
        }
    }

    /// Convert a Value to a num_bigint::BigInt for arbitrary-precision arithmetic.
    pub(crate) fn to_bigint(&self) -> NumBigInt {
        match self {
            Value::Int(i) => NumBigInt::from(*i),
            Value::BigInt(n) => n.clone(),
            Value::Num(f) => NumBigInt::from(*f as i64),
            Value::Rat(n, d) => {
                if *d != 0 {
                    NumBigInt::from(n / d)
                } else {
                    NumBigInt::from(0)
                }
            }
            Value::Str(s) => s
                .parse::<NumBigInt>()
                .unwrap_or_else(|_| NumBigInt::from(0)),
            _ => NumBigInt::from(0),
        }
    }

    /// Create a Value from a BigInt, normalizing to Int(i64) when possible.
    pub(crate) fn from_bigint(n: NumBigInt) -> Value {
        if let Some(i) = n.to_i64() {
            Value::Int(i)
        } else {
            Value::BigInt(n)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeErrorCode {
    ParseUnparsed,
    ParseExpected,
    ParseGeneric,
}

impl std::fmt::Display for RuntimeErrorCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            RuntimeErrorCode::ParseUnparsed => "PARSE_UNPARSED",
            RuntimeErrorCode::ParseExpected => "PARSE_EXPECTED",
            RuntimeErrorCode::ParseGeneric => "PARSE_GENERIC",
        };
        write!(f, "{}", name)
    }
}

impl RuntimeErrorCode {
    pub fn is_parse(self) -> bool {
        matches!(
            self,
            RuntimeErrorCode::ParseUnparsed
                | RuntimeErrorCode::ParseExpected
                | RuntimeErrorCode::ParseGeneric
        )
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    pub message: String,
    pub code: Option<RuntimeErrorCode>,
    pub line: Option<usize>,
    pub column: Option<usize>,
    pub hint: Option<String>,
    pub return_value: Option<Value>,
    pub is_last: bool,
    pub is_next: bool,
    pub is_redo: bool,
    pub is_proceed: bool,
    pub is_succeed: bool,
    pub is_fail: bool,
    pub label: Option<String>,
}

impl RuntimeError {
    pub(crate) fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            code: None,
            line: None,
            column: None,
            hint: None,
            return_value: None,
            is_last: false,
            is_next: false,
            is_redo: false,
            is_proceed: false,
            is_succeed: false,
            is_fail: false,
            label: None,
        }
    }

    pub(crate) fn with_location(
        message: impl Into<String>,
        code: RuntimeErrorCode,
        line: usize,
        column: usize,
    ) -> Self {
        Self {
            message: message.into(),
            code: Some(code),
            line: Some(line),
            column: Some(column),
            hint: None,
            return_value: None,
            is_last: false,
            is_next: false,
            is_redo: false,
            is_proceed: false,
            is_succeed: false,
            is_fail: false,
            label: None,
        }
    }

    pub(crate) fn last_signal() -> Self {
        Self {
            message: "X::ControlFlow".to_string(),
            code: None,
            line: None,
            column: None,
            hint: None,
            return_value: None,
            is_last: true,
            is_next: false,
            is_redo: false,
            is_proceed: false,
            is_succeed: false,
            is_fail: false,
            label: None,
        }
    }

    pub(crate) fn next_signal() -> Self {
        Self {
            message: "X::ControlFlow".to_string(),
            code: None,
            line: None,
            column: None,
            hint: None,
            return_value: None,
            is_last: false,
            is_next: true,
            is_redo: false,
            is_proceed: false,
            is_succeed: false,
            is_fail: false,
            label: None,
        }
    }

    pub(crate) fn redo_signal() -> Self {
        Self {
            message: "X::ControlFlow".to_string(),
            code: None,
            line: None,
            column: None,
            hint: None,
            return_value: None,
            is_last: false,
            is_next: false,
            is_redo: true,
            is_proceed: false,
            is_succeed: false,
            is_fail: false,
            label: None,
        }
    }

    pub(crate) fn proceed_signal() -> Self {
        Self {
            message: String::new(),
            code: None,
            line: None,
            column: None,
            hint: None,
            return_value: None,
            is_last: false,
            is_next: false,
            is_redo: false,
            is_proceed: true,
            is_succeed: false,
            is_fail: false,
            label: None,
        }
    }

    pub(crate) fn succeed_signal() -> Self {
        Self {
            message: String::new(),
            code: None,
            line: None,
            column: None,
            hint: None,
            return_value: None,
            is_last: false,
            is_next: false,
            is_redo: false,
            is_proceed: false,
            is_succeed: true,
            is_fail: false,
            label: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::RuntimeErrorCode;

    #[test]
    fn runtime_error_code_display_names_are_stable() {
        assert_eq!(
            RuntimeErrorCode::ParseUnparsed.to_string(),
            "PARSE_UNPARSED"
        );
        assert_eq!(
            RuntimeErrorCode::ParseExpected.to_string(),
            "PARSE_EXPECTED"
        );
        assert_eq!(RuntimeErrorCode::ParseGeneric.to_string(), "PARSE_GENERIC");
    }

    #[test]
    fn runtime_error_code_parse_classification() {
        assert!(RuntimeErrorCode::ParseUnparsed.is_parse());
        assert!(RuntimeErrorCode::ParseExpected.is_parse());
        assert!(RuntimeErrorCode::ParseGeneric.is_parse());
    }
}
