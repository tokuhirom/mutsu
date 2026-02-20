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
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use crate::ast::{Expr, FunctionDef, ParamDef, PhaserKind, Stmt};
use crate::opcode::{CompiledCode, OpCode};
use crate::parse_dispatch;
use crate::value::{JunctionKind, LazyList, RuntimeError, Value, make_rat, next_instance_id};
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

use self::unicode::check_unicode_property;

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
    Digit,
    Word,
    Space,
    Newline,
    NotNewline,
    Group(RegexPattern),
    Alternation(Vec<RegexPattern>),
    ZeroWidth,
    UnicodeProp {
        name: String,
        negated: bool,
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
    test_state: Option<TestState>,
    halted: bool,
    bailed_out: bool,
    functions: HashMap<String, FunctionDef>,
    token_defs: HashMap<String, Vec<FunctionDef>>,
    lib_paths: Vec<String>,
    handles: HashMap<usize, IoHandleState>,
    next_handle_id: usize,
    program_path: Option<String>,
    current_package: String,
    routine_stack: Vec<(String, String)>,
    block_stack: Vec<Value>,
    doc_comments: HashMap<String, String>,
    when_matched: bool,
    gather_items: Vec<Vec<Value>>,
    enum_types: HashMap<String, Vec<(String, i64)>>,
    classes: HashMap<String, ClassDef>,
    roles: HashMap<String, RoleDef>,
    subsets: HashMap<String, SubsetDef>,
    proto_subs: HashSet<String>,
    proto_tokens: HashSet<String>,
    end_phasers: Vec<(Vec<Stmt>, HashMap<String, Value>)>,
    chroot_root: Option<PathBuf>,
    loaded_modules: HashSet<String>,
    state_vars: HashMap<String, Value>,
    let_saves: Vec<(String, Value)>,
    pub(super) supply_emit_buffer: Vec<Vec<Value>>,
}

pub(crate) struct SubtestContext {
    parent_test_state: Option<TestState>,
    parent_output: String,
    parent_halted: bool,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter {
    pub fn new() -> Self {
        let mut env = HashMap::new();
        env.insert("*PID".to_string(), Value::Int(std::process::id() as i64));
        env.insert("@*ARGS".to_string(), Value::Array(Vec::new()));
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
                native_methods: ["emit", "tap", "repeated", "do", "Supply"]
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
                native_methods: ["start", "command", "started", "stdout", "stderr"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["Proc::Async".to_string()],
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
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["Compiler".to_string()],
            },
        );
        let mut interpreter = Self {
            env,
            output: String::new(),
            stderr_output: String::new(),
            test_state: None,
            halted: false,
            bailed_out: false,
            functions: HashMap::new(),
            token_defs: HashMap::new(),
            lib_paths: Vec::new(),
            handles: HashMap::new(),
            next_handle_id: 1,
            program_path: None,
            current_package: "GLOBAL".to_string(),
            routine_stack: Vec::new(),
            block_stack: Vec::new(),
            doc_comments: HashMap::new(),
            when_matched: false,
            gather_items: Vec::new(),
            enum_types: HashMap::new(),
            classes,
            roles: HashMap::new(),
            subsets: HashMap::new(),
            proto_subs: HashSet::new(),
            proto_tokens: HashSet::new(),
            end_phasers: Vec::new(),
            chroot_root: None,
            loaded_modules: HashSet::new(),
            state_vars: HashMap::new(),
            let_saves: Vec::new(),
            supply_emit_buffer: Vec::new(),
        };
        interpreter.init_io_environment();
        interpreter.init_order_enum();
        interpreter.env.insert("Any".to_string(), Value::Nil);
        interpreter
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
        self.env.insert("@*ARGS".to_string(), Value::Array(args));
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
        // Try to load Test:: submodules from source; fall back to no-op if unavailable
        if module.starts_with("Test::") {
            if self.load_module(module).is_err() {
                return Ok(());
            }
            return Ok(());
        }
        self.load_module(module)
    }

    pub fn output(&self) -> &str {
        &self.output
    }

    pub(crate) fn is_halted(&self) -> bool {
        self.halted
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
}
