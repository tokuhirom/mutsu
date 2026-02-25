#![allow(clippy::result_large_err)]
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::io::{Read, Seek, SeekFrom, Write};
use std::net::ToSocketAddrs;
#[cfg(unix)]
use std::os::unix::fs::{self as unix_fs, PermissionsExt};
#[cfg(windows)]
use std::os::windows::fs as windows_fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use crate::ast::{Expr, FunctionDef, ParamDef, PhaserKind, Stmt};
use crate::opcode::{CompiledCode, OpCode};
use crate::parse_dispatch;
use crate::value::{
    JunctionKind, LazyList, RuntimeError, SharedChannel, SharedPromise, Value, make_rat,
};
use num_traits::Signed;

mod accessors;
mod builtins;
mod builtins_coerce;
mod builtins_collection;
mod builtins_io;
mod builtins_string;
mod builtins_system;
mod call_helpers;
mod calls;
mod class;
mod dispatch;
mod handle;
mod io;
mod methods;
mod methods_mut;
mod methods_trans;
mod native_io;
mod native_methods;
mod ops;
mod regex;
mod regex_parse;
mod registration;
mod resolution;
mod run;
mod seq_helpers;
mod sequence;
mod sprintf;
mod subtest;
mod system;
mod test_functions;
pub(crate) mod types;
mod unicode;
pub(crate) mod utils;

pub(crate) use utils::*;

use self::unicode::{check_unicode_property, check_unicode_property_with_args};

#[derive(Clone)]
struct ClassDef {
    parents: Vec<String>,
    attributes: Vec<(String, bool, Option<Expr>, bool)>, // (name, is_public, default, is_rw)
    methods: HashMap<String, Vec<MethodDef>>,            // name -> overloads
    native_methods: HashSet<String>,
    mro: Vec<String>,
}

#[derive(Debug, Clone)]
struct RoleDef {
    attributes: Vec<(String, bool, Option<Expr>, bool)>,
    methods: HashMap<String, Vec<MethodDef>>,
    is_stub_role: bool,
}

#[derive(Debug, Clone)]
struct SubsetDef {
    base: String,
    predicate: Option<Expr>,
}

#[derive(Debug, Clone)]
struct MethodDef {
    params: Vec<String>,
    param_defs: Vec<ParamDef>,
    body: Vec<Stmt>,
    is_rw: bool,
    is_private: bool,
    return_type: Option<String>,
}

#[derive(Debug, Clone)]
struct MethodDispatchFrame {
    receiver_class: String,
    invocant: Value,
    args: Vec<Value>,
    remaining: Vec<(String, MethodDef)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IoHandleTarget {
    Stdout,
    Stderr,
    Stdin,
    ArgFiles,
    File,
    Socket,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IoHandleMode {
    Read,
    Write,
    Append,
    ReadWrite,
}

#[derive(Debug)]
struct IoHandleState {
    target: IoHandleTarget,
    mode: IoHandleMode,
    path: Option<String>,
    line_separators: Vec<Vec<u8>>,
    encoding: String,
    file: Option<fs::File>,
    socket: Option<std::net::TcpStream>,
    closed: bool,
    #[allow(dead_code)]
    bin: bool,
}

#[derive(Clone)]
struct RegexPattern {
    tokens: Vec<RegexToken>,
    anchor_start: bool,
    anchor_end: bool,
    ignore_case: bool,
    ignore_mark: bool,
}

#[derive(Clone, Default)]
struct RegexCaptures {
    named: HashMap<String, Vec<String>>,
    positional: Vec<String>,
    matched: String,
    from: usize,
    to: usize,
    capture_start: Option<usize>,
    capture_end: Option<usize>,
}

#[derive(Clone)]
struct RegexToken {
    atom: RegexAtom,
    quant: RegexQuant,
    named_capture: Option<String>,
    ratchet: bool,
}

#[derive(Clone)]
enum RegexAtom {
    Literal(char),
    Named(String),
    Any,
    CharClass(CharClass),
    Newline,
    NotNewline,
    Group(RegexPattern),
    CaptureGroup(RegexPattern),
    Alternation(Vec<RegexPattern>),
    ZeroWidth,
    CodeAssertion {
        code: String,
        negated: bool,
    },
    UnicodeProp {
        name: String,
        negated: bool,
        args: Option<String>,
    },
    UnicodePropAssert {
        name: String,
        negated: bool,
    }, // zero-width assertion
    CaptureStartMarker,
    CaptureEndMarker,
    /// Combined character class: <+ xdigit - lower>, matches positive AND NOT negative
    CompositeClass {
        positive: Vec<ClassItem>,
        negative: Vec<ClassItem>,
    },
}

#[derive(Clone)]
enum RegexQuant {
    One,
    ZeroOrMore,
    OneOrMore,
    ZeroOrOne,
}

#[derive(Clone)]
struct CharClass {
    negated: bool,
    items: Vec<ClassItem>,
}

#[derive(Clone)]
enum ClassItem {
    Range(char, char),
    Char(char),
    Digit,
    Word,
    Space,
    NamedBuiltin(String),
    UnicodePropItem { name: String, negated: bool },
}

pub struct Interpreter {
    env: HashMap<String, Value>,
    output: String,
    stderr_output: String,
    warn_output: String,
    warn_suppression_depth: usize,
    test_state: Option<TestState>,
    subtest_depth: usize,
    halted: bool,
    exit_code: i64,
    bailed_out: bool,
    functions: HashMap<String, FunctionDef>,
    proto_functions: HashMap<String, FunctionDef>,
    token_defs: HashMap<String, Vec<FunctionDef>>,
    lib_paths: Vec<String>,
    handles: HashMap<usize, IoHandleState>,
    next_handle_id: usize,
    program_path: Option<String>,
    current_package: String,
    routine_stack: Vec<(String, String)>,
    method_class_stack: Vec<String>,
    pending_call_arg_sources: Option<Vec<Option<String>>>,
    test_pending_callsite_line: Option<i64>,
    test_assertion_line_stack: Vec<i64>,
    block_stack: Vec<Value>,
    doc_comments: HashMap<String, String>,
    type_metadata: HashMap<String, HashMap<String, Value>>,
    when_matched: bool,
    gather_items: Vec<Vec<Value>>,
    enum_types: HashMap<String, Vec<(String, i64)>>,
    classes: HashMap<String, ClassDef>,
    hidden_classes: HashSet<String>,
    hidden_defer_parents: HashMap<String, HashSet<String>>,
    class_trusts: HashMap<String, HashSet<String>>,
    roles: HashMap<String, RoleDef>,
    role_parents: HashMap<String, Vec<String>>,
    role_type_params: HashMap<String, Vec<String>>,
    subsets: HashMap<String, SubsetDef>,
    proto_subs: HashSet<String>,
    proto_tokens: HashSet<String>,
    proto_dispatch_stack: Vec<(String, Vec<Value>)>,
    end_phasers: Vec<(Vec<Stmt>, HashMap<String, Value>)>,
    chroot_root: Option<PathBuf>,
    loaded_modules: HashSet<String>,
    module_load_stack: Vec<String>,
    /// When true, `is export` trait is ignored (used by `need` to load without importing).
    pub(crate) suppress_exports: bool,
    pub(crate) newline_mode: NewlineMode,
    /// Stack of snapshots for lexical import scoping.
    /// Each entry saves (function_keys, class_names, newline_mode, strict_mode) before a block with `use`.
    import_scope_stack: Vec<(HashSet<String>, HashSet<String>, NewlineMode, bool)>,
    pub(crate) strict_mode: bool,
    state_vars: HashMap<String, Value>,
    /// Variable dynamic-scope metadata used by `.VAR.dynamic`.
    var_dynamic_flags: HashMap<String, bool>,
    let_saves: Vec<(String, Value)>,
    pub(super) supply_emit_buffer: Vec<Vec<Value>>,
    /// Shared variables between threads. When `start` spawns a thread,
    /// `@` variables are stored here so both parent and child can see mutations.
    shared_vars: Arc<Mutex<HashMap<String, Value>>>,
    /// Registry of encodings (both built-in and user-registered).
    /// Each entry maps a canonical name to an EncodingEntry.
    encoding_registry: Vec<EncodingEntry>,
    /// When set, pseudo-method names (DEFINITE, WHAT, etc.) bypass native fast path.
    /// Used for quoted method calls like `."DEFINITE"()`.
    pub(crate) skip_pseudo_method_native: Option<String>,
    /// Stack of remaining multi dispatch candidates for callsame/nextsame/nextcallee.
    /// Each entry is (remaining_candidates, original_args).
    multi_dispatch_stack: Vec<(Vec<FunctionDef>, Vec<Value>)>,
    method_dispatch_stack: Vec<MethodDispatchFrame>,
    /// Names suppressed by `anon class`. These bare words should error as undeclared.
    suppressed_names: HashSet<String>,
}

/// An entry in the encoding registry.
#[derive(Debug, Clone)]
pub(crate) struct EncodingEntry {
    /// Canonical encoding name.
    pub name: String,
    /// Alternative names for this encoding.
    pub alternative_names: Vec<String>,
    /// If Some, this is a user-registered encoding (the Value is the type object).
    pub user_type: Option<Value>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum NewlineMode {
    Lf,
    Cr,
    Crlf,
}

pub(crate) struct SubtestContext {
    parent_test_state: Option<TestState>,
    parent_output: String,
    parent_halted: bool,
}

pub(crate) type RoutineRegistrySnapshot = (
    HashMap<String, FunctionDef>,
    HashMap<String, FunctionDef>,
    HashMap<String, Vec<FunctionDef>>,
    HashSet<String>,
    HashSet<String>,
);

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter {
    pub fn new() -> Self {
        let mut env = HashMap::new();
        env.insert("*PID".to_string(), Value::Int(std::process::id() as i64));
        env.insert("@*ARGS".to_string(), Value::array(Vec::new()));
        env.insert("*INIT-INSTANT".to_string(), Value::make_instant_now());
        env.insert(
            "*SCHEDULER".to_string(),
            Value::make_instance("ThreadPoolScheduler".to_string(), HashMap::new()),
        );
        let mut classes = HashMap::new();
        classes.insert(
            "Mu".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Mu".to_string()],
            },
        );
        classes.insert(
            "Any".to_string(),
            ClassDef {
                parents: vec!["Mu".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Any".to_string(), "Mu".to_string()],
            },
        );
        classes.insert(
            "Promise".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["keep", "result", "status", "then"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["Promise".to_string()],
            },
        );
        classes.insert(
            "Channel".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["send", "receive", "close", "closed"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["Channel".to_string()],
            },
        );
        classes.insert(
            "Supply".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "emit",
                    "tap",
                    "act",
                    "decode",
                    "repeated",
                    "do",
                    "reverse",
                    "split",
                    "tail",
                    "min",
                    "collate",
                    "Supply",
                    "Promise",
                    "schedule-on",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["Supply".to_string()],
            },
        );
        classes.insert(
            "utf8".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["utf8".to_string()],
            },
        );
        classes.insert(
            "utf16".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["utf16".to_string()],
            },
        );
        classes.insert(
            "Supplier".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["emit", "done", "quit", "Supply"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["Supplier".to_string()],
            },
        );
        classes.insert(
            "Proc::Async".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "start",
                    "command",
                    "started",
                    "stdout",
                    "stderr",
                    "kill",
                    "write",
                    "close-stdin",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["Proc::Async".to_string()],
            },
        );
        classes.insert(
            "Proc".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "exitcode", "signal", "command", "pid", "Numeric", "Int", "Bool", "Str", "gist",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["Proc".to_string()],
            },
        );
        classes.insert(
            "Tap".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["cancel"].iter().map(|s| s.to_string()).collect(),
                mro: vec!["Tap".to_string()],
            },
        );
        classes.insert(
            "Scheduler".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["cue"].iter().map(|s| s.to_string()).collect(),
                mro: vec!["Scheduler".to_string()],
            },
        );
        classes.insert(
            "ThreadPoolScheduler".to_string(),
            ClassDef {
                parents: vec!["Scheduler".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["cue"].iter().map(|s| s.to_string()).collect(),
                mro: vec!["ThreadPoolScheduler".to_string()],
            },
        );
        classes.insert(
            "CurrentThreadScheduler".to_string(),
            ClassDef {
                parents: vec!["Scheduler".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["cue"].iter().map(|s| s.to_string()).collect(),
                mro: vec!["CurrentThreadScheduler".to_string()],
            },
        );
        classes.insert(
            "Cancellation".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["cancel"].iter().map(|s| s.to_string()).collect(),
                mro: vec!["Cancellation".to_string()],
            },
        );
        classes.insert(
            "Lock".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["protect", "lock", "unlock", "condition"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["Lock".to_string()],
            },
        );
        classes.insert(
            "Lock::ConditionVariable".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["wait", "signal", "signal_all"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["Lock::ConditionVariable".to_string()],
            },
        );
        classes.insert(
            "IO::Path".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "Str",
                    "gist",
                    "raku",
                    "perl",
                    "IO",
                    "basename",
                    "dirname",
                    "parent",
                    "sibling",
                    "child",
                    "add",
                    "extension",
                    "absolute",
                    "relative",
                    "resolve",
                    "volume",
                    "is-absolute",
                    "is-relative",
                    "e",
                    "f",
                    "d",
                    "l",
                    "r",
                    "w",
                    "x",
                    "mode",
                    "s",
                    "z",
                    "modified",
                    "accessed",
                    "changed",
                    "lines",
                    "words",
                    "slurp",
                    "open",
                    "copy",
                    "rename",
                    "move",
                    "chmod",
                    "mkdir",
                    "rmdir",
                    "dir",
                    "spurt",
                    "unlink",
                    "starts-with",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["IO::Path".to_string()],
            },
        );
        classes.insert(
            "IO::Handle".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "close", "get", "getc", "lines", "words", "read", "write", "print", "say",
                    "put", "flush", "seek", "tell", "eof", "encoding", "opened", "slurp", "Supply",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["IO::Handle".to_string()],
            },
        );
        classes.insert(
            "Backtrace".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Backtrace".to_string()],
            },
        );
        classes.insert(
            "IO::Pipe".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["slurp", "Str", "gist"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["IO::Pipe".to_string()],
            },
        );
        classes.insert(
            "IO::Socket::INET".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["close", "getpeername"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["IO::Socket::INET".to_string()],
            },
        );
        classes.insert(
            "Distro".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "name",
                    "auth",
                    "desc",
                    "release",
                    "path-sep",
                    "is-win",
                    "version",
                    "signature",
                    "gist",
                    "Str",
                    "raku",
                    "perl",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["Distro".to_string()],
            },
        );
        classes.insert(
            "Perl".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "DISTROnames",
                    "compiler",
                    "backend",
                    "name",
                    "auth",
                    "version",
                    "signature",
                    "desc",
                    "gist",
                    "raku",
                    "Str",
                    "release",
                    "codename",
                    "id",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["Perl".to_string()],
            },
        );
        classes.insert(
            "Compiler".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "name",
                    "auth",
                    "version",
                    "signature",
                    "desc",
                    "gist",
                    "raku",
                    "Str",
                    "release",
                    "codename",
                    "id",
                    "backend",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["Compiler".to_string()],
            },
        );
        classes.insert(
            "Encoding::Builtin".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["name", "alternative-names", "encoder", "decoder"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["Encoding::Builtin".to_string()],
            },
        );
        classes.insert(
            "Encoding::Encoder".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["encode-chars"].iter().map(|s| s.to_string()).collect(),
                mro: vec!["Encoding::Encoder".to_string()],
            },
        );
        classes.insert(
            "Encoding::Decoder".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["decode-chars"].iter().map(|s| s.to_string()).collect(),
                mro: vec!["Encoding::Decoder".to_string()],
            },
        );
        classes.insert(
            "Encoding::Registry".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["find", "register"].iter().map(|s| s.to_string()).collect(),
                mro: vec!["Encoding::Registry".to_string()],
            },
        );
        classes.insert(
            "Pod::Block".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Pod::Block".to_string()],
            },
        );
        classes.insert(
            "Pod::Block::Comment".to_string(),
            ClassDef {
                parents: vec!["Pod::Block".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Pod::Block::Comment".to_string(), "Pod::Block".to_string()],
            },
        );
        classes.insert(
            "Pod::Block::Table".to_string(),
            ClassDef {
                parents: vec!["Pod::Block".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Pod::Block::Table".to_string(), "Pod::Block".to_string()],
            },
        );
        classes.insert(
            "X::AdHoc".to_string(),
            ClassDef {
                parents: vec!["Exception".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["X::AdHoc".to_string(), "Exception".to_string()],
            },
        );
        classes.insert(
            "X::TypeCheck::Binding".to_string(),
            ClassDef {
                parents: vec!["Exception".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["X::TypeCheck::Binding".to_string(), "Exception".to_string()],
            },
        );
        classes.insert(
            "X::TypeCheck::Binding::Parameter".to_string(),
            ClassDef {
                parents: vec!["X::TypeCheck::Binding".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec![
                    "X::TypeCheck::Binding::Parameter".to_string(),
                    "X::TypeCheck::Binding".to_string(),
                    "Exception".to_string(),
                ],
            },
        );
        classes.insert(
            "X::TypeCheck::Argument".to_string(),
            ClassDef {
                parents: vec!["Exception".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec![
                    "X::TypeCheck::Argument".to_string(),
                    "Exception".to_string(),
                ],
            },
        );
        classes.insert(
            "X::TypeCheck::Assignment".to_string(),
            ClassDef {
                parents: vec!["Exception".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec![
                    "X::TypeCheck::Assignment".to_string(),
                    "Exception".to_string(),
                ],
            },
        );
        let mut interpreter = Self {
            env,
            output: String::new(),
            stderr_output: String::new(),
            warn_output: String::new(),
            warn_suppression_depth: 0,
            test_state: None,
            subtest_depth: 0,
            halted: false,
            exit_code: 0,
            bailed_out: false,
            functions: HashMap::new(),
            proto_functions: HashMap::new(),
            token_defs: HashMap::new(),
            lib_paths: Vec::new(),
            handles: HashMap::new(),
            next_handle_id: 1,
            program_path: None,
            current_package: "GLOBAL".to_string(),
            routine_stack: Vec::new(),
            method_class_stack: Vec::new(),
            pending_call_arg_sources: None,
            test_pending_callsite_line: None,
            test_assertion_line_stack: Vec::new(),
            block_stack: Vec::new(),
            doc_comments: HashMap::new(),
            type_metadata: HashMap::new(),
            when_matched: false,
            gather_items: Vec::new(),
            enum_types: HashMap::new(),
            classes,
            hidden_classes: HashSet::new(),
            hidden_defer_parents: HashMap::new(),
            class_trusts: HashMap::new(),
            roles: {
                let mut roles = HashMap::new();
                roles.insert(
                    "Encoding".to_string(),
                    RoleDef {
                        attributes: Vec::new(),
                        methods: HashMap::new(),
                        is_stub_role: false,
                    },
                );
                roles
            },
            role_parents: HashMap::new(),
            role_type_params: HashMap::new(),
            subsets: HashMap::new(),
            proto_subs: HashSet::new(),
            proto_tokens: HashSet::new(),
            proto_dispatch_stack: Vec::new(),
            end_phasers: Vec::new(),
            chroot_root: None,
            loaded_modules: HashSet::new(),
            module_load_stack: Vec::new(),
            suppress_exports: false,
            newline_mode: NewlineMode::Lf,
            import_scope_stack: Vec::new(),
            strict_mode: false,
            state_vars: HashMap::new(),
            var_dynamic_flags: HashMap::new(),
            let_saves: Vec::new(),
            supply_emit_buffer: Vec::new(),
            shared_vars: Arc::new(Mutex::new(HashMap::new())),
            encoding_registry: Self::builtin_encodings(),
            skip_pseudo_method_native: None,
            multi_dispatch_stack: Vec::new(),
            method_dispatch_stack: Vec::new(),
            suppressed_names: HashSet::new(),
        };
        interpreter.init_io_environment();
        interpreter.init_order_enum();
        interpreter.init_endian_enum();
        interpreter.env.insert("Any".to_string(), Value::Nil);
        interpreter
    }

    fn builtin_encodings() -> Vec<EncodingEntry> {
        vec![
            EncodingEntry {
                name: "utf-8".to_string(),
                alternative_names: vec!["utf8".to_string()],
                user_type: None,
            },
            EncodingEntry {
                name: "ascii".to_string(),
                alternative_names: Vec::new(),
                user_type: None,
            },
            EncodingEntry {
                name: "iso-8859-1".to_string(),
                alternative_names: vec!["latin-1".to_string()],
                user_type: None,
            },
            EncodingEntry {
                name: "utf-16".to_string(),
                alternative_names: vec!["utf16".to_string()],
                user_type: None,
            },
            EncodingEntry {
                name: "utf-16le".to_string(),
                alternative_names: vec![
                    "utf16le".to_string(),
                    "utf16-le".to_string(),
                    "utf-16-le".to_string(),
                ],
                user_type: None,
            },
            EncodingEntry {
                name: "utf-16be".to_string(),
                alternative_names: vec![
                    "utf16be".to_string(),
                    "utf16-be".to_string(),
                    "utf-16-be".to_string(),
                ],
                user_type: None,
            },
            EncodingEntry {
                name: "windows-932".to_string(),
                alternative_names: vec!["windows932".to_string()],
                user_type: None,
            },
            EncodingEntry {
                name: "windows-1251".to_string(),
                alternative_names: vec!["windows1251".to_string()],
                user_type: None,
            },
            EncodingEntry {
                name: "windows-1252".to_string(),
                alternative_names: vec!["windows1252".to_string()],
                user_type: None,
            },
        ]
    }

    /// Find an encoding by name (case-insensitive). Returns the entry index if found.
    pub(crate) fn find_encoding(&self, name: &str) -> Option<&EncodingEntry> {
        let name_fc = name.to_lowercase();
        self.encoding_registry.iter().find(|e| {
            e.name.to_lowercase() == name_fc
                || e.alternative_names
                    .iter()
                    .any(|alt| alt.to_lowercase() == name_fc)
        })
    }

    /// Register a user-defined encoding. Returns Ok(()) on success,
    /// or the conflicting name on failure.
    pub(crate) fn register_encoding(&mut self, entry: EncodingEntry) -> Result<(), String> {
        // Check for conflicts
        let name_fc = entry.name.to_lowercase();
        for existing in &self.encoding_registry {
            if existing.name.to_lowercase() == name_fc {
                return Err(entry.name.clone());
            }
            if existing
                .alternative_names
                .iter()
                .any(|alt| alt.to_lowercase() == name_fc)
            {
                return Err(entry.name.clone());
            }
        }
        for alt in &entry.alternative_names {
            let alt_fc = alt.to_lowercase();
            for existing in &self.encoding_registry {
                if existing.name.to_lowercase() == alt_fc {
                    return Err(alt.clone());
                }
                if existing
                    .alternative_names
                    .iter()
                    .any(|a| a.to_lowercase() == alt_fc)
                {
                    return Err(alt.clone());
                }
            }
        }
        self.encoding_registry.push(entry);
        Ok(())
    }

    pub(crate) fn suppress_name(&mut self, name: &str) {
        self.suppressed_names.insert(name.to_string());
    }

    pub(crate) fn is_name_suppressed(&self, name: &str) -> bool {
        self.suppressed_names.contains(name)
    }

    pub fn set_pid(&mut self, pid: i64) {
        self.env.insert("*PID".to_string(), Value::Int(pid));
    }

    pub fn set_program_path(&mut self, path: &str) {
        self.program_path = Some(path.to_string());
        self.env
            .insert("*PROGRAM".to_string(), self.make_io_path_instance(path));
        self.env
            .insert("*PROGRAM-NAME".to_string(), Value::Str(path.to_string()));
    }

    pub fn set_args(&mut self, args: Vec<Value>) {
        self.env.insert("@*ARGS".to_string(), Value::array(args));
    }

    pub fn add_lib_path(&mut self, path: String) {
        if !path.is_empty() {
            self.lib_paths.push(path);
        }
    }

    /// Save current function/class keys for lexical import scoping.
    pub(crate) fn push_import_scope(&mut self) {
        let func_keys: HashSet<String> = self.functions.keys().cloned().collect();
        let class_keys: HashSet<String> = self.classes.keys().cloned().collect();
        self.import_scope_stack
            .push((func_keys, class_keys, self.newline_mode, self.strict_mode));
    }

    /// Restore function/class registries to the last saved snapshot,
    /// removing any entries added since the push.
    pub(crate) fn pop_import_scope(&mut self) {
        if let Some((func_snapshot, class_snapshot, newline_mode, strict_mode)) =
            self.import_scope_stack.pop()
        {
            self.functions.retain(|key, _| func_snapshot.contains(key));
            self.classes.retain(|key, _| class_snapshot.contains(key));
            self.newline_mode = newline_mode;
            self.strict_mode = strict_mode;
        }
    }

    pub(crate) fn use_module(&mut self, module: &str) -> Result<(), RuntimeError> {
        if self.loaded_modules.contains(module) {
            if module == "strict" {
                self.strict_mode = true;
            }
            return Ok(());
        }
        if self.module_load_stack.iter().any(|m| m == module) {
            let mut chain = self.module_load_stack.clone();
            chain.push(module.to_string());
            return Err(RuntimeError::new(format!(
                "circular module dependency detected: {}",
                chain.join(" -> ")
            )));
        }
        self.module_load_stack.push(module.to_string());

        let result = if module == "Test"
            || matches!(
                module,
                "strict"
                    | "warnings"
                    | "MONKEY-SEE-NO-EVAL"
                    | "MONKEY-TYPING"
                    | "nqp"
                    | "MONKEY"
                    | "newline"
            ) {
            Ok(())
        } else if module == "Test::Tap" {
            // Handle Test::Tap as built-in
            Ok(())
        } else if module.starts_with("Test::") {
            // Load Test:: submodules from source as regular modules.
            // Parse errors should propagate like other `use` failures.
            // Missing helper modules remain non-fatal for compatibility.
            match self.load_module(module) {
                Ok(()) => Ok(()),
                Err(err) if err.message.starts_with("Module not found:") => Ok(()),
                Err(err) => Err(err),
            }
        } else {
            self.load_module(module)
        };

        self.module_load_stack.pop();
        if result.is_ok() {
            if module == "strict" {
                self.strict_mode = true;
            }
            self.loaded_modules.insert(module.to_string());
        }
        result
    }

    /// Load a module without importing its exports (Raku `need` keyword).
    pub(crate) fn need_module(&mut self, module: &str) -> Result<(), RuntimeError> {
        if self.loaded_modules.contains(module) {
            return Ok(());
        }
        if self.module_load_stack.iter().any(|m| m == module) {
            let mut chain = self.module_load_stack.clone();
            chain.push(module.to_string());
            return Err(RuntimeError::new(format!(
                "circular module dependency detected: {}",
                chain.join(" -> ")
            )));
        }
        self.module_load_stack.push(module.to_string());
        let saved = self.suppress_exports;
        self.suppress_exports = true;
        let result = self.load_module(module);
        self.suppress_exports = saved;
        self.module_load_stack.pop();
        if result.is_ok() {
            self.loaded_modules.insert(module.to_string());
        }
        result
    }

    pub(crate) fn no_module(&mut self, module: &str) -> Result<(), RuntimeError> {
        if module == "strict" {
            self.strict_mode = false;
        }
        Ok(())
    }

    pub fn output(&self) -> &str {
        &self.output
    }

    pub fn exit_code(&self) -> i64 {
        self.exit_code
    }

    pub(crate) fn is_halted(&self) -> bool {
        self.halted
    }

    pub(crate) fn write_warn_to_stderr(&mut self, message: &str) {
        let msg = format!("{}\n", message);
        self.stderr_output.push_str(&msg);
        self.warn_output.push_str(&msg);
        eprint!("{}", msg);
    }

    pub(crate) fn push_warn_suppression(&mut self) {
        self.warn_suppression_depth += 1;
    }

    pub(crate) fn pop_warn_suppression(&mut self) {
        self.warn_suppression_depth = self.warn_suppression_depth.saturating_sub(1);
    }

    pub(crate) fn warning_suppressed(&self) -> bool {
        self.warn_suppression_depth > 0
    }

    pub(crate) fn env(&self) -> &HashMap<String, Value> {
        &self.env
    }

    pub(crate) fn env_insert(&mut self, key: String, value: Value) {
        self.env.insert(key, value);
    }

    fn normalize_var_meta_name(name: &str) -> &str {
        name.trim_start_matches(['$', '@', '%', '&'])
    }

    pub(crate) fn set_var_dynamic(&mut self, name: &str, dynamic: bool) {
        let key = Self::normalize_var_meta_name(name).to_string();
        self.var_dynamic_flags.insert(key, dynamic);
    }

    pub(crate) fn is_var_dynamic(&self, name: &str) -> bool {
        self.var_dynamic_flags
            .get(Self::normalize_var_meta_name(name))
            .copied()
            .unwrap_or(false)
    }

    pub(crate) fn has_class(&self, name: &str) -> bool {
        self.classes.contains_key(name)
    }

    /// Create a lightweight clone of this interpreter for use in a spawned thread.
    /// Shares function/class/role/enum definitions but starts with fresh output and test state.
    /// Array (`@`) variables are shared between parent and child via `shared_vars`
    /// so that mutations (push, pop, etc.) are visible across threads.
    pub(crate) fn clone_for_thread(&mut self) -> Self {
        // Copy @ variables into shared_vars so both parent and child see mutations
        let shared = Arc::clone(&self.shared_vars);
        {
            let mut sv = shared.lock().unwrap();
            for (key, val) in &self.env {
                if key.starts_with('@') {
                    sv.insert(key.clone(), val.clone());
                }
            }
        }
        Self {
            env: self.env.clone(),
            output: String::new(),
            stderr_output: String::new(),
            warn_output: String::new(),
            warn_suppression_depth: 0,
            test_state: None,
            subtest_depth: 0,
            halted: false,
            exit_code: 0,
            bailed_out: false,
            functions: self.functions.clone(),
            proto_functions: self.proto_functions.clone(),
            token_defs: self.token_defs.clone(),
            lib_paths: self.lib_paths.clone(),
            handles: HashMap::new(),
            next_handle_id: 1,
            program_path: self.program_path.clone(),
            current_package: self.current_package.clone(),
            routine_stack: Vec::new(),
            method_class_stack: Vec::new(),
            pending_call_arg_sources: None,
            test_pending_callsite_line: None,
            test_assertion_line_stack: Vec::new(),
            block_stack: Vec::new(),
            doc_comments: HashMap::new(),
            type_metadata: self.type_metadata.clone(),
            when_matched: false,
            gather_items: Vec::new(),
            enum_types: self.enum_types.clone(),
            classes: self.classes.clone(),
            hidden_classes: self.hidden_classes.clone(),
            hidden_defer_parents: self.hidden_defer_parents.clone(),
            class_trusts: self.class_trusts.clone(),
            roles: self.roles.clone(),
            role_parents: self.role_parents.clone(),
            role_type_params: self.role_type_params.clone(),
            subsets: self.subsets.clone(),
            proto_subs: self.proto_subs.clone(),
            proto_tokens: self.proto_tokens.clone(),
            proto_dispatch_stack: Vec::new(),
            end_phasers: Vec::new(),
            chroot_root: self.chroot_root.clone(),
            loaded_modules: self.loaded_modules.clone(),
            module_load_stack: Vec::new(),
            suppress_exports: false,
            newline_mode: self.newline_mode,
            import_scope_stack: Vec::new(),
            strict_mode: self.strict_mode,
            state_vars: HashMap::new(),
            var_dynamic_flags: self.var_dynamic_flags.clone(),
            let_saves: Vec::new(),
            supply_emit_buffer: Vec::new(),
            shared_vars: Arc::clone(&self.shared_vars),
            encoding_registry: self.encoding_registry.clone(),
            skip_pseudo_method_native: None,
            multi_dispatch_stack: Vec::new(),
            method_dispatch_stack: Vec::new(),
            suppressed_names: self.suppressed_names.clone(),
        }
    }

    /// Read a shared array variable. If the variable is in shared_vars, return
    /// the shared version (which may have been mutated by another thread).
    pub(crate) fn get_shared_var(&self, key: &str) -> Option<Value> {
        if key.starts_with('@') {
            let sv = self.shared_vars.lock().unwrap();
            sv.get(key).cloned()
        } else {
            None
        }
    }

    /// Write a shared array variable. Updates both the local env and shared_vars.
    pub(crate) fn set_shared_var(&mut self, key: &str, value: Value) {
        self.env.insert(key.to_string(), value.clone());
        if key.starts_with('@') {
            let mut sv = self.shared_vars.lock().unwrap();
            if sv.contains_key(key) {
                sv.insert(key.to_string(), value);
            }
        }
    }

    /// Sync shared variables back from shared_vars into the local env.
    /// Called after await/sleep to pick up mutations from other threads.
    pub(crate) fn sync_shared_vars_to_env(&mut self) {
        let sv = self.shared_vars.lock().unwrap();
        for (key, val) in sv.iter() {
            self.env.insert(key.clone(), val.clone());
        }
    }

    pub(crate) fn merge_sigilless_alias_writes(
        &self,
        saved_env: &mut HashMap<String, Value>,
        current_env: &HashMap<String, Value>,
    ) {
        for (key, alias) in current_env {
            if !key.starts_with("__mutsu_sigilless_alias::") {
                continue;
            }
            let Value::Str(alias_name) = alias else {
                continue;
            };
            if let Some(value) = current_env.get(alias_name).cloned() {
                saved_env.insert(alias_name.clone(), value);
            }
        }
    }
}

#[derive(Debug, Default)]
struct TestState {
    planned: Option<usize>,
    ran: usize,
    failed: usize,
    force_todo: Vec<(usize, usize)>,
}

impl TestState {
    fn new() -> Self {
        Self {
            planned: None,
            ran: 0,
            failed: 0,
            force_todo: Vec::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Interpreter;
    use std::fs;
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn say_and_math() {
        let mut interp = Interpreter::new();
        let output = interp.run("say 1 + 2; say 3 * 4;").unwrap();
        assert_eq!(output, "3\n12\n");
    }

    #[test]
    fn variables_and_concat() {
        let mut interp = Interpreter::new();
        let output = interp
            .run("my $x = 2; $x = $x + 3; say \"hi\" ~ $x;")
            .unwrap();
        assert_eq!(output, "hi5\n");
    }

    #[test]
    fn if_else() {
        let mut interp = Interpreter::new();
        let output = interp
            .run("my $x = 1; if $x == 1 { say \"yes\"; } else { say \"no\"; }")
            .unwrap();
        assert_eq!(output, "yes\n");
    }

    #[test]
    fn while_loop() {
        let mut interp = Interpreter::new();
        let output = interp
            .run("my $x = 0; while $x < 3 { say $x; $x = $x + 1; }")
            .unwrap();
        assert_eq!(output, "0\n1\n2\n");
    }

    #[test]
    fn use_module_with_parse_error_raises_exception() {
        let mut interp = Interpreter::new();
        let uniq = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("mutsu-badmod-{}", uniq));
        fs::create_dir_all(&dir).unwrap();
        let mod_path = dir.join("Bad.rakumod");
        fs::write(&mod_path, "unit module Bad;\nsub broken( { }\n").unwrap();

        let program = format!("use lib '{}'; use Bad;", dir.to_string_lossy());
        let err = interp.run(&program).unwrap_err();
        assert!(err.message.contains("Failed to parse module 'Bad'"));
        assert!(err.message.contains("parse error"));

        let _ = fs::remove_file(mod_path);
        let _ = fs::remove_dir(dir);
    }

    #[test]
    fn use_lib_empty_string_raises_libempty_exception() {
        let mut interp = Interpreter::new();
        let err = interp.run("use lib '';").unwrap_err();
        assert!(err.message.contains("X::LibEmpty"));
    }

    #[test]
    fn circular_module_dependency_is_reported() {
        let mut interp = Interpreter::new();
        let uniq = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("mutsu-circularmod-{}", uniq));
        fs::create_dir_all(&dir).unwrap();
        let a_path = dir.join("A.rakumod");
        let b_path = dir.join("B.rakumod");
        fs::write(&a_path, "unit class A; use B").unwrap();
        fs::write(&b_path, "unit class B; use A").unwrap();

        let program = format!("use lib '{}'; use A;", dir.to_string_lossy());
        let err = interp.run(&program).unwrap_err();
        assert!(err.message.to_lowercase().contains("circular"));

        let _ = fs::remove_file(a_path);
        let _ = fs::remove_file(b_path);
        let _ = fs::remove_dir(dir);
    }

    #[test]
    fn is_run_honors_compiler_include_paths() {
        let mut interp = Interpreter::new();
        let uniq = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("mutsu-is-run-inc-{}", uniq));
        fs::create_dir_all(&dir).unwrap();
        let m_path = dir.join("M.rakumod");
        fs::write(&m_path, "unit module M;\nsub hi is export { 42 }\n").unwrap();

        let escaped_dir = dir
            .to_string_lossy()
            .replace('\\', "\\\\")
            .replace('"', "\\\"");
        let program = format!(
            "use Test; use lib \"roast/packages/Test-Helpers\"; use Test::Util; \
             plan 1; \
             is_run \"use M; say hi\", :compiler-args[\"-I\", \"{}\"], {{ :out(\"42\\n\"), :status(0) }}, \"is_run uses -I\";",
            escaped_dir
        );
        let output = interp.run(&program).unwrap();
        assert!(output.contains("ok 1 - is_run uses -I"));

        let _ = fs::remove_file(m_path);
        let _ = fs::remove_dir(dir);
    }

    #[test]
    fn unit_module_applies_to_following_declarations() {
        let mut interp = Interpreter::new();
        let uniq = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("mutsu-unit-mod-scope-{}", uniq));
        fs::create_dir_all(&dir).unwrap();
        let m_path = dir.join("Shadow.rakumod");
        fs::write(&m_path, "unit module Shadow;\nour $debug = 1;\n").unwrap();

        let program = format!(
            "use lib '{}'; use Shadow; say $Shadow::debug;",
            dir.to_string_lossy()
        );
        let output = interp.run(&program).unwrap();
        assert_eq!(output, "1\n");

        let _ = fs::remove_file(m_path);
        let _ = fs::remove_dir(dir);
    }

    #[test]
    fn like_supports_case_insensitive_quote_word_regex() {
        let mut interp = Interpreter::new();
        let output = interp
            .run("use Test; plan 1; like \"circular module\", /:i «circular»/, \"regex\";")
            .unwrap();
        assert!(output.contains("ok 1 - regex"));
    }
}
