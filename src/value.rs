use std::collections::HashMap;

use crate::ast::Stmt;

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
    FatRat(i64, i64),
    CompUnitDepSpec { short_name: String },
    Package(String),
    Routine { package: String, name: String },
    Pair(String, Box<Value>),
    Sub {
        package: String,
        name: String,
        param: Option<String>,
        body: Vec<Stmt>,
        env: HashMap<String, Value>,
    },
    Nil,
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
            (Value::FatRat(a1, b1), Value::FatRat(a2, b2)) => a1 == a2 && b1 == b2,
            (Value::CompUnitDepSpec { short_name: a }, Value::CompUnitDepSpec { short_name: b }) => a == b,
            (Value::Package(a), Value::Package(b)) => a == b,
            (Value::Pair(ak, av), Value::Pair(bk, bv)) => ak == bk && av == bv,
            (Value::Routine { package: ap, name: an }, Value::Routine { package: bp, name: bn }) => ap == bp && an == bn,
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
            Value::FatRat(_, _) => true,
            Value::Pair(_, _) => true,
            Value::CompUnitDepSpec { .. } => true,
            Value::Package(_) => true,
            Value::Routine { .. } => true,
            Value::Sub { .. } => true,
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
            Value::Hash(items) => items
                .iter()
                .map(|(k, v)| format!("{}\t{}", k, v.to_string_value()))
                .collect::<Vec<_>>()
                .join("\n"),
            Value::FatRat(a, b) => format!("{}/{}", a, b),
            Value::Pair(k, v) => format!("{}\t{}", k, v.to_string_value()),
            Value::CompUnitDepSpec { short_name } => format!("CompUnit::DependencySpecification({})", short_name),
            Value::Package(s) => s.clone(),
            Value::Routine { package, name } => format!("{}::{}", package, name),
            Value::Sub { name, .. } => name.clone(),
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
}

impl RuntimeError {
    pub(crate) fn new(message: impl Into<String>) -> Self {
        Self { message: message.into(), return_value: None, is_last: false, is_next: false }
    }

    pub(crate) fn return_val(value: Value) -> Self {
        Self { message: String::new(), return_value: Some(value), is_last: false, is_next: false }
    }

    pub(crate) fn last_signal() -> Self {
        Self { message: String::new(), return_value: None, is_last: true, is_next: false }
    }

    pub(crate) fn next_signal() -> Self {
        Self { message: String::new(), return_value: None, is_last: false, is_next: true }
    }
}
