use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicU64, Ordering};

use crate::ast::Stmt;
use num_bigint::BigInt as NumBigInt;
use num_traits::{ToPrimitive, Zero};
use std::sync::{Arc, Condvar, Mutex, Weak};

mod display;
mod error;
mod types;

pub use display::{tclc_str, wordcase_str};
pub use error::{RuntimeError, RuntimeErrorCode};
// SubData is re-exported so callers can destructure Value::Sub(data)

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

#[derive(Debug, Clone)]
pub struct SubData {
    pub package: String,
    pub name: String,
    pub params: Vec<String>,
    pub(crate) body: Vec<Stmt>,
    pub env: HashMap<String, Value>,
    pub id: u64,
}

fn gcd(mut a: i64, mut b: i64) -> i64 {
    a = a.wrapping_abs();
    b = b.wrapping_abs();
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
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
    /// The bool flag distinguishes Array (`true`, from `[...]`) from List (`false`, from `(,)`).
    Array(Arc<Vec<Value>>, bool),
    Hash(Arc<HashMap<String, Value>>),
    Rat(i64, i64),
    FatRat(i64, i64),
    Complex(f64, f64),
    Set(Arc<HashSet<String>>),
    Bag(Arc<HashMap<String, i64>>),
    Mix(Arc<HashMap<String, f64>>),
    CompUnitDepSpec {
        short_name: String,
    },
    Package(String),
    Routine {
        package: String,
        name: String,
    },
    Pair(String, Box<Value>),
    /// Pair with a non-string key (preserves the original key type for `.key`)
    ValuePair(Box<Value>, Box<Value>),
    Enum {
        enum_type: String,
        key: String,
        value: i64,
        index: usize,
    },
    Regex(String),
    Sub(Arc<SubData>),
    /// A weak reference to a Sub (used for &?BLOCK self-references to break cycles).
    /// Upgrade to the strong value when accessed; returns Nil if expired.
    WeakSub(Weak<SubData>),
    Instance {
        class_name: String,
        attributes: Arc<HashMap<String, Value>>,
        id: u64,
    },
    Junction {
        kind: JunctionKind,
        values: Arc<Vec<Value>>,
    },
    Seq(Arc<Vec<Value>>),
    Slip(Arc<Vec<Value>>),
    LazyList(Arc<LazyList>),
    Version {
        parts: Vec<VersionPart>,
        plus: bool,
        minus: bool,
    },
    Promise(SharedPromise),
    Channel(SharedChannel),
    Nil,
    HyperWhatever,
    /// A value with mixin overrides from the `but` operator.
    /// Inner value is the original; the HashMap maps type names (e.g. "Bool") to override values.
    Mixin(Box<Value>, HashMap<String, Value>),
    /// A Capture: positional args + named args
    Capture {
        positional: Vec<Value>,
        named: HashMap<String, Value>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum VersionPart {
    Num(i64),
    Str(String),
    Whatever,
}

#[derive(Debug)]
pub(crate) struct LazyList {
    pub(crate) body: Vec<Stmt>,
    pub(crate) env: HashMap<String, Value>,
    pub(crate) cache: Mutex<Option<Vec<Value>>>,
}

impl Clone for LazyList {
    fn clone(&self) -> Self {
        Self {
            body: self.body.clone(),
            env: self.env.clone(),
            cache: Mutex::new(self.cache.lock().unwrap().clone()),
        }
    }
}

// --- SharedPromise: thread-safe promise state ---

#[derive(Debug)]
struct PromiseState {
    status: String, // "Planned", "Kept", "Broken"
    result: Value,
    output: String, // captured stdout from thread
    stderr_output: String,
    class_name: String, // "Promise" or subclass name
}

#[derive(Debug, Clone)]
pub(crate) struct SharedPromise {
    inner: Arc<(Mutex<PromiseState>, Condvar)>,
}

impl SharedPromise {
    pub(crate) fn new() -> Self {
        Self::new_with_class("Promise".to_string())
    }

    pub(crate) fn new_with_class(class_name: String) -> Self {
        Self {
            inner: Arc::new((
                Mutex::new(PromiseState {
                    status: "Planned".to_string(),
                    result: Value::Nil,
                    output: String::new(),
                    stderr_output: String::new(),
                    class_name,
                }),
                Condvar::new(),
            )),
        }
    }

    #[allow(dead_code)]
    pub(crate) fn new_kept(result: Value) -> Self {
        Self {
            inner: Arc::new((
                Mutex::new(PromiseState {
                    status: "Kept".to_string(),
                    result,
                    output: String::new(),
                    stderr_output: String::new(),
                    class_name: "Promise".to_string(),
                }),
                Condvar::new(),
            )),
        }
    }

    pub(crate) fn class_name(&self) -> String {
        let (lock, _) = &*self.inner;
        lock.lock().unwrap().class_name.clone()
    }

    pub(crate) fn keep(&self, result: Value, output: String, stderr: String) {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        state.status = "Kept".to_string();
        state.result = result;
        state.output = output;
        state.stderr_output = stderr;
        cvar.notify_all();
    }

    /// Try to keep; returns Err(current_status) if already kept/broken.
    pub(crate) fn try_keep(&self, result: Value) -> Result<(), String> {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        if state.status != "Planned" {
            return Err(state.status.clone());
        }
        state.status = "Kept".to_string();
        state.result = result;
        cvar.notify_all();
        Ok(())
    }

    pub(crate) fn break_with(&self, error: Value, output: String, stderr: String) {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        state.status = "Broken".to_string();
        state.result = error;
        state.output = output;
        state.stderr_output = stderr;
        cvar.notify_all();
    }

    /// Try to break; returns Err(current_status) if already kept/broken.
    pub(crate) fn try_break(&self, error: Value) -> Result<(), String> {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        if state.status != "Planned" {
            return Err(state.status.clone());
        }
        state.status = "Broken".to_string();
        state.result = error;
        cvar.notify_all();
        Ok(())
    }

    /// Check if promise is resolved (Kept or Broken).
    pub(crate) fn is_resolved(&self) -> bool {
        let (lock, _) = &*self.inner;
        lock.lock().unwrap().status != "Planned"
    }

    pub(crate) fn status(&self) -> String {
        let (lock, _) = &*self.inner;
        lock.lock().unwrap().status.clone()
    }

    pub(crate) fn wait(&self) -> (Value, String, String) {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        while state.status == "Planned" {
            state = cvar.wait(state).unwrap();
        }
        (
            state.result.clone(),
            state.output.clone(),
            state.stderr_output.clone(),
        )
    }

    pub(crate) fn result_blocking(&self) -> Value {
        self.wait().0
    }
}

impl PartialEq for SharedPromise {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.inner, &other.inner)
    }
}

// --- SharedChannel: thread-safe channel ---

#[derive(Debug)]
struct ChannelState {
    queue: std::collections::VecDeque<Value>,
    closed: bool,
}

#[derive(Debug, Clone)]
pub(crate) struct SharedChannel {
    inner: Arc<(Mutex<ChannelState>, Condvar)>,
}

impl SharedChannel {
    pub(crate) fn new() -> Self {
        Self {
            inner: Arc::new((
                Mutex::new(ChannelState {
                    queue: std::collections::VecDeque::new(),
                    closed: false,
                }),
                Condvar::new(),
            )),
        }
    }

    pub(crate) fn send(&self, value: Value) {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        state.queue.push_back(value);
        cvar.notify_one();
    }

    pub(crate) fn receive(&self) -> Value {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        loop {
            if let Some(val) = state.queue.pop_front() {
                return val;
            }
            if state.closed {
                return Value::Nil;
            }
            state = cvar.wait(state).unwrap();
        }
    }

    pub(crate) fn poll(&self) -> Option<Value> {
        let (lock, _) = &*self.inner;
        let mut state = lock.lock().unwrap();
        state.queue.pop_front()
    }

    pub(crate) fn close(&self) {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        state.closed = true;
        cvar.notify_all();
    }

    pub(crate) fn closed(&self) -> bool {
        let (lock, _) = &*self.inner;
        lock.lock().unwrap().closed
    }
}

impl PartialEq for SharedChannel {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.inner, &other.inner)
    }
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
            (Value::Array(a, ..), Value::Array(b, ..)) => a == b,
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
            (Value::ValuePair(ak, av), Value::ValuePair(bk, bv)) => ak == bk && av == bv,
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
            (Value::Sub(a), Value::Sub(b)) => a.id == b.id,
            (Value::LazyList(a), Value::LazyList(b)) => Arc::ptr_eq(a, b),
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
            (Value::Promise(a), Value::Promise(b)) => a == b,
            (Value::Channel(a), Value::Channel(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }
}

impl Value {
    // ---- Arc-wrapping convenience constructors ----
    pub fn array(items: Vec<Value>) -> Self {
        Value::Array(Arc::new(items), false)
    }
    /// Create a true Array value (from [...] literals).
    pub fn real_array(items: Vec<Value>) -> Self {
        Value::Array(Arc::new(items), true)
    }
    pub fn hash(map: HashMap<String, Value>) -> Self {
        Value::Hash(Arc::new(map))
    }
    pub fn set(s: HashSet<String>) -> Self {
        Value::Set(Arc::new(s))
    }
    pub fn bag(m: HashMap<String, i64>) -> Self {
        Value::Bag(Arc::new(m))
    }
    pub fn mix(m: HashMap<String, f64>) -> Self {
        Value::Mix(Arc::new(m))
    }
    pub fn slip(items: Vec<Value>) -> Self {
        Value::Slip(Arc::new(items))
    }
    pub fn junction(kind: JunctionKind, values: Vec<Value>) -> Self {
        Value::Junction {
            kind,
            values: Arc::new(values),
        }
    }

    /// Create a new Sub value wrapping the given SubData in an Arc.
    pub(crate) fn make_sub(
        package: String,
        name: String,
        params: Vec<String>,
        body: Vec<Stmt>,
        env: HashMap<String, Value>,
    ) -> Self {
        Value::Sub(Arc::new(SubData {
            package,
            name,
            params,
            body,
            env,
            id: next_instance_id(),
        }))
    }

    /// Create a new Sub value with an explicit id.
    pub(crate) fn make_sub_with_id(
        package: String,
        name: String,
        params: Vec<String>,
        body: Vec<Stmt>,
        env: HashMap<String, Value>,
        id: u64,
    ) -> Self {
        Value::Sub(Arc::new(SubData {
            package,
            name,
            params,
            body,
            env,
            id,
        }))
    }

    /// Access SubData fields if this is a Sub (or upgraded WeakSub).
    #[allow(dead_code)]
    pub(crate) fn as_sub(&self) -> Option<&SubData> {
        match self {
            Value::Sub(data) => Some(data),
            _ => None,
        }
    }

    /// Upgrade a WeakSub to a Sub, or return Nil if expired.
    #[allow(dead_code)]
    pub(crate) fn upgrade_weak(&self) -> Value {
        match self {
            Value::WeakSub(weak) => match weak.upgrade() {
                Some(strong) => Value::Sub(strong),
                None => Value::Nil,
            },
            other => other.clone(),
        }
    }

    pub(crate) fn make_instance(class_name: String, attributes: HashMap<String, Value>) -> Self {
        Value::Instance {
            class_name,
            attributes: Arc::new(attributes),
            id: next_instance_id(),
        }
    }

    /// Create an Instant value from the current system time.
    pub(crate) fn make_instant_now() -> Self {
        let secs = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_secs_f64())
            .unwrap_or(0.0);
        let mut attrs = HashMap::new();
        attrs.insert("value".to_string(), Value::Num(secs));
        Value::make_instance("Instant".to_string(), attrs)
    }

    /// Create a Match object with positional captures.
    pub(crate) fn make_match_object_with_captures(
        matched: String,
        from: i64,
        to: i64,
        positional: &[String],
    ) -> Self {
        let mut attrs = HashMap::new();
        attrs.insert("str".to_string(), Value::Str(matched));
        attrs.insert("from".to_string(), Value::Int(from));
        attrs.insert("to".to_string(), Value::Int(to));
        let caps: Vec<Value> = positional.iter().map(|s| Value::Str(s.clone())).collect();
        attrs.insert("list".to_string(), Value::array(caps));
        Value::make_instance("Match".to_string(), attrs)
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
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Instant" => {
                attributes.get("value").map(|v| v.to_f64()).unwrap_or(0.0)
            }
            _ => 0.0,
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

// Compile-time assertions that Value is Send + Sync
const _: fn() = || {
    fn assert_send_sync<T: Send + Sync>() {}
    assert_send_sync::<Value>();
};
