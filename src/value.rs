use std::collections::{HashMap, HashSet};

use crate::ast::Stmt;
use std::cell::RefCell;
use std::rc::Rc;

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
    Num(f64),
    Str(String),
    Bool(bool),
    Range(i64, i64),
    RangeExcl(i64, i64),
    RangeExclStart(i64, i64),
    RangeExclBoth(i64, i64),
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
        param: Option<String>,
        body: Vec<Stmt>,
        env: HashMap<String, Value>,
    },
    Instance {
        class_name: String,
        attributes: HashMap<String, Value>,
    },
    Junction {
        kind: JunctionKind,
        values: Vec<Value>,
    },
    LazyList(Rc<LazyList>),
    Nil,
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
            (Value::Num(a), Value::Num(b)) => a == b,
            (Value::Int(a), Value::Num(b)) => (*a as f64) == *b,
            (Value::Num(a), Value::Int(b)) => *a == (*b as f64),
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Range(a1, b1), Value::Range(a2, b2)) => a1 == a2 && b1 == b2,
            (Value::RangeExcl(a1, b1), Value::RangeExcl(a2, b2)) => a1 == a2 && b1 == b2,
            (Value::RangeExclStart(a1, b1), Value::RangeExclStart(a2, b2)) => a1 == a2 && b1 == b2,
            (Value::RangeExclBoth(a1, b1), Value::RangeExclBoth(a2, b2)) => a1 == a2 && b1 == b2,
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
            (Value::Complex(r1, i1), Value::Complex(r2, i2)) => r1 == r2 && i1 == i2,
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
                },
                Value::Instance {
                    class_name: b,
                    attributes: ba,
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
            (Value::LazyList(a), Value::LazyList(b)) => Rc::ptr_eq(a, b),
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }
}

impl Value {
    pub(crate) fn truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Int(i) => *i != 0,
            Value::Num(f) => *f != 0.0 && !f.is_nan(),
            Value::Str(s) => !s.is_empty(),
            Value::Range(_, _) => true,
            Value::RangeExcl(_, _) => true,
            Value::RangeExclStart(_, _) => true,
            Value::RangeExclBoth(_, _) => true,
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
            Value::Package(_) => true,
            Value::Routine { .. } => true,
            Value::Sub { .. } => true,
            Value::Instance { .. } => true,
            Value::Junction { kind, values } => match kind {
                JunctionKind::Any => values.iter().any(|v| v.truthy()),
                JunctionKind::All => values.iter().all(|v| v.truthy()),
                JunctionKind::One => values.iter().filter(|v| v.truthy()).count() == 1,
                JunctionKind::None => values.iter().all(|v| !v.truthy()),
            },
            Value::LazyList(_) => true,
            Value::Regex(_) => true,
            Value::Nil => false,
        }
    }

    pub(crate) fn to_string_value(&self) -> String {
        match self {
            Value::Int(i) => i.to_string(),
            Value::Num(f) => {
                if f.fract() == 0.0 && f.is_finite() {
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
            } if class_name == "IO::Path" => attributes
                .get("path")
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
            Value::Nil => "Nil".to_string(),
        }
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    pub message: String,
    pub return_value: Option<Value>,
    pub is_last: bool,
    pub is_next: bool,
    pub is_redo: bool,
    pub is_proceed: bool,
    pub is_succeed: bool,
    pub label: Option<String>,
}

impl RuntimeError {
    pub(crate) fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            return_value: None,
            is_last: false,
            is_next: false,
            is_redo: false,
            is_proceed: false,
            is_succeed: false,
            label: None,
        }
    }

    pub(crate) fn return_val(value: Value) -> Self {
        Self {
            message: String::new(),
            return_value: Some(value),
            is_last: false,
            is_next: false,
            is_redo: false,
            is_proceed: false,
            is_succeed: false,
            label: None,
        }
    }

    pub(crate) fn last_signal() -> Self {
        Self {
            message: String::new(),
            return_value: None,
            is_last: true,
            is_next: false,
            is_redo: false,
            is_proceed: false,
            is_succeed: false,
            label: None,
        }
    }

    pub(crate) fn next_signal() -> Self {
        Self {
            message: String::new(),
            return_value: None,
            is_last: false,
            is_next: true,
            is_redo: false,
            is_proceed: false,
            is_succeed: false,
            label: None,
        }
    }

    pub(crate) fn redo_signal() -> Self {
        Self {
            message: String::new(),
            return_value: None,
            is_last: false,
            is_next: false,
            is_redo: true,
            is_proceed: false,
            is_succeed: false,
            label: None,
        }
    }

    pub(crate) fn proceed_signal() -> Self {
        Self {
            message: String::new(),
            return_value: None,
            is_last: false,
            is_next: false,
            is_redo: false,
            is_proceed: true,
            is_succeed: false,
            label: None,
        }
    }

    pub(crate) fn succeed_signal() -> Self {
        Self {
            message: String::new(),
            return_value: None,
            is_last: false,
            is_next: false,
            is_redo: false,
            is_proceed: false,
            is_succeed: true,
            label: None,
        }
    }
}
