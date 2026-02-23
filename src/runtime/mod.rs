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
    attributes: Vec<(String, bool, Option<Expr>)>, // (name, is_public, default)
    methods: HashMap<String, Vec<MethodDef>>,      // name -> overloads
    native_methods: HashSet<String>,
    mro: Vec<String>,
}

#[derive(Debug, Clone)]
struct RoleDef {
    attributes: Vec<(String, bool, Option<Expr>)>,
    methods: HashMap<String, Vec<MethodDef>>,
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
}

#[derive(Clone, Default)]
struct RegexCaptures {
    named: HashMap<String, String>,
    positional: Vec<String>,
    matched: String,
    from: usize,
    to: usize,
}

#[derive(Clone)]
struct RegexToken {
    atom: RegexAtom,
    quant: RegexQuant,
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
    test_state: Option<TestState>,
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
    block_stack: Vec<Value>,
    doc_comments: HashMap<String, String>,
    type_metadata: HashMap<String, HashMap<String, Value>>,
    when_matched: bool,
    gather_items: Vec<Vec<Value>>,
    enum_types: HashMap<String, Vec<(String, i64)>>,
    classes: HashMap<String, ClassDef>,
    roles: HashMap<String, RoleDef>,
    subsets: HashMap<String, SubsetDef>,
    proto_subs: HashSet<String>,
    proto_tokens: HashSet<String>,
    proto_dispatch_stack: Vec<(String, Vec<Value>)>,
    end_phasers: Vec<(Vec<Stmt>, HashMap<String, Value>)>,
    chroot_root: Option<PathBuf>,
    loaded_modules: HashSet<String>,
    /// When true, `is export` trait is ignored (used by `need` to load without importing).
    pub(crate) suppress_exports: bool,
    state_vars: HashMap<String, Value>,
    let_saves: Vec<(String, Value)>,
    pub(super) supply_emit_buffer: Vec<Vec<Value>>,
    /// Shared variables between threads. When `start` spawns a thread,
    /// `@` variables are stored here so both parent and child can see mutations.
    shared_vars: Arc<Mutex<HashMap<String, Value>>>,
    /// Registry of encodings (both built-in and user-registered).
    /// Each entry maps a canonical name to an EncodingEntry.
    encoding_registry: Vec<EncodingEntry>,
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
        let mut classes = HashMap::new();
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
                native_methods: ["emit", "tap", "repeated", "do", "reverse", "Supply"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["Supply".to_string()],
            },
        );
        classes.insert(
            "Supplier".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["emit", "done", "Supply"]
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
                native_methods: HashSet::new(),
                mro: vec!["Tap".to_string()],
            },
        );
        classes.insert(
            "ThreadPoolScheduler".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["ThreadPoolScheduler".to_string()],
            },
        );
        classes.insert(
            "CurrentThreadScheduler".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["CurrentThreadScheduler".to_string()],
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
                    "flush", "seek", "tell", "eof", "encoding", "opened", "slurp", "Supply",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["IO::Handle".to_string()],
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
        let mut interpreter = Self {
            env,
            output: String::new(),
            stderr_output: String::new(),
            warn_output: String::new(),
            test_state: None,
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
            block_stack: Vec::new(),
            doc_comments: HashMap::new(),
            type_metadata: HashMap::new(),
            when_matched: false,
            gather_items: Vec::new(),
            enum_types: HashMap::new(),
            classes,
            roles: {
                let mut roles = HashMap::new();
                roles.insert(
                    "Encoding".to_string(),
                    RoleDef {
                        attributes: Vec::new(),
                        methods: HashMap::new(),
                    },
                );
                roles
            },
            subsets: HashMap::new(),
            proto_subs: HashSet::new(),
            proto_tokens: HashSet::new(),
            proto_dispatch_stack: Vec::new(),
            end_phasers: Vec::new(),
            chroot_root: None,
            loaded_modules: HashSet::new(),
            suppress_exports: false,
            state_vars: HashMap::new(),
            let_saves: Vec::new(),
            supply_emit_buffer: Vec::new(),
            shared_vars: Arc::new(Mutex::new(HashMap::new())),
            encoding_registry: Self::builtin_encodings(),
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

    pub(crate) fn use_module(&mut self, module: &str) -> Result<(), RuntimeError> {
        self.loaded_modules.insert(module.to_string());
        if module == "Test"
            || matches!(
                module,
                "strict" | "warnings" | "MONKEY-SEE-NO-EVAL" | "MONKEY-TYPING" | "nqp" | "MONKEY"
            )
        {
            return Ok(());
        }
        // Handle Test::Tap as built-in
        if module == "Test::Tap" {
            return Ok(());
        }
        // Load Test:: submodules from source as regular modules.
        // Parse errors should propagate like other `use` failures.
        // Missing helper modules remain non-fatal for compatibility.
        if module.starts_with("Test::") {
            return match self.load_module(module) {
                Ok(()) => Ok(()),
                Err(err) if err.message.starts_with("Module not found:") => Ok(()),
                Err(err) => Err(err),
            };
        }
        self.load_module(module)
    }

    /// Load a module without importing its exports (Raku `need` keyword).
    pub(crate) fn need_module(&mut self, module: &str) -> Result<(), RuntimeError> {
        self.loaded_modules.insert(module.to_string());
        let saved = self.suppress_exports;
        self.suppress_exports = true;
        let result = self.load_module(module);
        self.suppress_exports = saved;
        result
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

    pub(crate) fn env(&self) -> &HashMap<String, Value> {
        &self.env
    }

    pub(crate) fn env_insert(&mut self, key: String, value: Value) {
        self.env.insert(key, value);
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
            test_state: None,
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
            block_stack: Vec::new(),
            doc_comments: HashMap::new(),
            type_metadata: self.type_metadata.clone(),
            when_matched: false,
            gather_items: Vec::new(),
            enum_types: self.enum_types.clone(),
            classes: self.classes.clone(),
            roles: self.roles.clone(),
            subsets: self.subsets.clone(),
            proto_subs: self.proto_subs.clone(),
            proto_tokens: self.proto_tokens.clone(),
            proto_dispatch_stack: Vec::new(),
            end_phasers: Vec::new(),
            chroot_root: self.chroot_root.clone(),
            loaded_modules: self.loaded_modules.clone(),
            suppress_exports: false,
            state_vars: HashMap::new(),
            let_saves: Vec::new(),
            supply_emit_buffer: Vec::new(),
            shared_vars: Arc::clone(&self.shared_vars),
            encoding_registry: self.encoding_registry.clone(),
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
}
