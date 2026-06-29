#![allow(clippy::result_large_err)]
use crate::symbol::Symbol;
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
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex, RwLock};
use std::thread;

static ROLE_ID_COUNTER: AtomicU64 = AtomicU64::new(1);

pub(crate) fn next_role_id() -> u64 {
    ROLE_ID_COUNTER.fetch_add(1, Ordering::Relaxed)
}
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use crate::ast::{Expr, FunctionDef, ParamDef, PhaserKind, Stmt};
use crate::env::Env;
use crate::opcode::{CompiledCode, CompiledFunction};
use crate::parse_dispatch;
use crate::value::{
    ArrayKind, EnumValue, JunctionKind, LazyList, RuntimeError, SharedChannel, SharedPromise,
    Value, make_rat, take_pending_instance_destroys,
};
use num_traits::Signed;

/// Flatten arguments for `append` using Raku's "one-arg rule":
/// if exactly one non-itemized Array/List argument is passed, its elements
/// are flattened into the result. With multiple arguments, each is appended as-is.
pub(crate) fn flatten_append_args(args: Vec<Value>) -> Vec<Value> {
    if args.len() == 1 {
        match &args[0] {
            Value::Array(vals, kind) if !kind.is_itemized() => vals.to_vec(),
            Value::Seq(vals) => vals.to_vec(),
            Value::Hash(map) => {
                // Flatten hash into key-value pairs
                let mut result = Vec::new();
                for (k, v) in map.iter() {
                    result.push(Value::Pair(k.clone(), Box::new(v.clone())));
                }
                result
            }
            _ => args,
        }
    } else {
        args
    }
}

/// Split a string by commas while respecting bracket/paren depth.
/// Returns the trimmed, non-empty parts.
fn split_balanced_comma_list(input: &str) -> Vec<String> {
    let mut args = Vec::new();
    let mut depth = 0i32;
    let mut start = 0;
    for (i, ch) in input.char_indices() {
        match ch {
            '(' | '[' => depth += 1,
            ')' | ']' => depth -= 1,
            ',' if depth == 0 => {
                let part = input[start..i].trim();
                if !part.is_empty() {
                    args.push(part.to_string());
                }
                start = i + 1;
            }
            _ => {}
        }
    }
    let last = input[start..].trim();
    if !last.is_empty() {
        args.push(last.to_string());
    }
    args
}

/// Get the current process ID (returns 0 on WASM where process IDs don't exist).
fn current_process_id() -> i64 {
    #[cfg(not(target_arch = "wasm32"))]
    {
        std::process::id() as i64
    }
    #[cfg(target_arch = "wasm32")]
    {
        0
    }
}

/// Get the local timezone offset in seconds (west-negative, east-positive).
/// Returns 0 (UTC) on WASM or if the offset cannot be determined.
pub(crate) fn local_timezone_offset_secs() -> i64 {
    #[cfg(all(not(target_arch = "wasm32"), feature = "native"))]
    {
        // Use libc::localtime_r to retrieve the tm_gmtoff field which gives
        // the UTC offset in seconds for the current local timezone.
        unsafe {
            let now = libc::time(std::ptr::null_mut());
            let mut tm: libc::tm = std::mem::zeroed();
            libc::localtime_r(&now, &mut tm);
            tm.tm_gmtoff
        }
    }
    #[cfg(not(all(not(target_arch = "wasm32"), feature = "native")))]
    {
        0
    }
}

type ProtectBlockCacheEntry = (
    Arc<CompiledCode>,
    Arc<HashMap<String, CompiledFunction>>,
    Arc<Vec<(usize, String)>>,
    Arc<Vec<(usize, String)>>,
    Arc<Vec<String>>,
);
type ProtectBlockCache = HashMap<u64, ProtectBlockCacheEntry>;

mod accessors;
mod accessors_misc;
mod accessors_resolve;
mod accessors_stack;
mod accessors_stash;
mod accessors_state;
mod builtins;
mod builtins_atomic;
mod builtins_atomic_cas;
mod builtins_atomic_shared;
mod builtins_coerce;
pub(crate) mod builtins_collection;
mod builtins_collection_classify;
mod builtins_collection_deepmap;
mod builtins_collection_extrema;
mod builtins_collection_listops;
mod builtins_collection_mapgrep;
mod builtins_collection_minmax;
mod builtins_control_flow;
mod builtins_dispatch_next;
mod builtins_eval_misc;
mod builtins_feed;
mod builtins_io;
mod builtins_io_dir;
mod builtins_io_fs;
mod builtins_io_stream;
mod builtins_lvalue;
mod builtins_multidim;
mod builtins_multidim_assign;
mod builtins_multidim_ops;
mod builtins_multidim_subscript;
mod builtins_operators_coerce;
mod builtins_operators_fallback;
mod builtins_operators_infix;
mod builtins_operators_repeat;
pub(crate) mod builtins_reduce;
mod builtins_string;
pub(crate) mod builtins_system;
mod builtins_system_async;
mod builtins_system_proc;
mod builtins_system_require;
mod builtins_system_run;
mod builtins_unbase;
mod call_helpers;
mod calls;
mod class;
mod class_dispatch;
mod class_introspection;
pub(crate) mod deprecation;
pub(crate) mod did_you_mean;
mod dispatch;
mod dispatch_candidates;
mod dispatch_proto;
mod dispatch_proto_call;
mod dispatch_proto_rewrite;
mod dispatch_resolve;
mod eval_check;
mod handle;
mod handle_io;
mod handle_open;
mod handle_read;
mod handle_read_chars;
mod io;
mod io_doc;
mod io_env;
mod io_handles;
mod io_pod;
mod io_pod_blocks;
mod io_pod_config;
mod io_pod_entries;
mod io_pod_table;
mod io_sysinfo;
pub(crate) mod json;
mod lock_reentry;
mod main_args;
mod metamodel;
mod methods;
mod methods_aggregate_ctor;
mod methods_call_dispatch;
mod methods_call_helpers;
mod methods_classhow;
mod methods_classhow_attribute;
mod methods_classhow_builtin_methods;
mod methods_classhow_dispatch;
mod methods_classhow_lookup;
mod methods_classhow_method_obj;
mod methods_classhow_mro;
mod methods_classhow_parents;
mod methods_collection;
pub(crate) mod methods_collection_ops;
mod methods_dispatch_match;
mod methods_dispatch_match2;
mod methods_dispatch_match3;
mod methods_dispatch_new;
mod methods_distribution;
mod methods_distribution_cur_inst;
mod methods_distribution_cur_resolve;
mod methods_distribution_helpers;
mod methods_enum_dispatch;
mod methods_format;
mod methods_grammar;
mod methods_instance_ops;
mod methods_introspect;
mod methods_io_dispatch;
mod methods_match_dispatch;
mod methods_mixin_dispatch;
mod methods_mut;
mod methods_mut_dispatch;
mod methods_mut_hash;
mod methods_mut_method_lvalue;
mod methods_mut_proxy;
mod methods_mut_rw_attr;
mod methods_mut_substr_buf;
mod methods_native_bypass;
mod methods_object;
mod methods_object_attr_constraints;
mod methods_object_default_ctor;
mod methods_object_dispatch_new;
mod methods_object_native_ctors_buf_num;
mod methods_object_native_ctors_io;
mod methods_object_native_ctors_misc;
mod methods_object_native_ctors_temporal;
mod methods_pick_roll;
mod methods_promise;
mod methods_promise_class;
mod methods_qualified;
mod methods_quanthash_ctor;
mod methods_seq_dispatch;
pub(crate) mod methods_signature;
mod methods_signature_candidates;
pub(crate) mod methods_signature_errors;
mod methods_signature_shaped;
mod methods_string;
mod methods_string_codec;
mod methods_string_encoding;
mod methods_string_index;
mod methods_string_search;
mod methods_string_subst_repl;
mod methods_string_substr;
mod methods_sub;
mod methods_supply_dispatch;
mod methods_temporal;
mod methods_trans;
mod methods_type_coerce;
mod methods_walk;
mod native_io;
mod native_io_special;
pub(crate) mod native_methods;
mod native_proc_async;
mod native_supplier_methods;
mod native_supply_dispatch;
mod native_supply_methods;
mod native_supply_mut_methods;
pub(crate) mod native_types;
pub(crate) mod nativecall;
mod ops_bits;
mod ops_compare;
mod ops_reduction;
mod ops_set;
mod output_sink;
pub(crate) mod phasers;
mod react_died;
pub(crate) mod regex;
pub(crate) mod regex_parse;
mod regex_parse_charclass;
mod regex_parse_core;
mod regex_parse_ltm;
mod regex_parse_modifier;
mod registration;
mod registration_class;
mod registration_class_attr;
mod registration_class_augment;
mod registration_class_decl;
mod registration_role;
pub(crate) mod registration_sub;
mod registry;
pub(crate) mod resolution;
mod resolution_call_sub;
mod resolution_eval;
mod resolution_lazy;
pub(crate) mod resolution_map_grep;
mod resolution_map_grep_rw;
mod resolution_method;
mod resolution_private_method;
mod run;
mod run_dist;
mod run_modules;
mod run_prelude;
mod run_roast_preprocess;
mod runtime_caller_env;
mod runtime_class_query;
mod runtime_container;
mod runtime_encoding;
mod runtime_init;
mod runtime_module;
mod runtime_module_exports;
mod runtime_output;
mod runtime_shared_vars;
mod runtime_thread;
mod runtime_var_meta;
mod seq_helpers;
mod sequence;
mod signal_watcher;
pub(super) mod sprintf;
mod sprintf_helpers;
mod sprintf_validate;
pub(crate) mod str_numeric;
pub(crate) mod subtest;
mod supply_classify;
mod supply_promise;
mod supply_transform;
mod system;
mod system_eval_names;
mod system_eval_redecl;
mod system_eval_string;
mod system_eval_vars;
mod system_introspect;
mod tap_state;
mod test_functions;
pub(crate) mod types;
mod unicode;
pub(crate) mod utf8_c8;
pub(crate) mod utils;
pub(crate) mod value_iterator;
pub(crate) use self::output_sink::OutputSink;
#[allow(unused_imports)]
pub(crate) use self::output_sink::{OutputSinkReadGuard, OutputSinkWriteGuard};
pub(crate) use self::registration_class::ClassDeclModifiers;
pub(crate) use self::registry::Registry;
pub(crate) use self::tap_state::{TapState, TestState, TodoRange};

pub(crate) use utils::*;

// Re-export thread utility functions for VM access
pub(crate) use methods_collection_ops::{current_mutsu_thread_id, is_initial_thread};

use self::unicode::{check_unicode_property, check_unicode_property_with_args};

pub(super) type ClassAttributeDef = (
    String,
    bool,
    Option<Expr>,
    bool,
    Option<Option<String>>,
    char,
    Option<Expr>,
);

/// Kind of declaration a doc comment is attached to.
#[derive(Clone, Debug, Default, PartialEq)]
pub(crate) enum DocDeclKind {
    #[default]
    Package, // class, module, package, grammar, role, enum, subset
    Sub,         // sub, method, submethod
    GrammarRule, // token, rule, regex (inside grammar)
    Attr,        // has $.attr
    Param,       // documented parameter
}

/// A declarator doc comment with leading (#|) and trailing (#=) parts.
#[derive(Clone, Debug, Default)]
pub(crate) struct DocComment {
    pub leading: Option<String>,
    pub trailing: Option<String>,
    /// The name of the thing this comment is attached to (for WHEREFORE).
    pub wherefore_name: String,
    /// Kind of declaration.
    pub kind: DocDeclKind,
    /// Whether this is a proto declaration (affects WHEREFORE type in $=pod).
    pub is_proto: bool,
    /// Optional return type for subs (e.g., `anon Str sub {}` has return_type "Str").
    pub return_type: Option<String>,
    /// Source line number (1-based) where the declaration appears.
    pub source_line: Option<u32>,
    /// For Sub kind: what type to use in $=pod WHEREFORE (e.g. "Method", "Submethod").
    /// None means use default logic (Sub/Routine).
    pub callable_type_override: Option<String>,
}

impl DocComment {
    fn contents(&self) -> String {
        match (&self.leading, &self.trailing) {
            (Some(l), Some(t)) => format!("{}\n{}", l, t),
            (Some(l), None) => l.clone(),
            (None, Some(t)) => t.clone(),
            (None, None) => String::new(),
        }
    }
}

#[derive(Clone, Default)]
pub(crate) struct ClassDef {
    parents: Vec<String>,
    // (name, is_public, default, is_rw, is_required, sigil, where_constraint)
    attributes: Vec<ClassAttributeDef>,
    attribute_types: HashMap<String, String>, // attr_name -> type constraint
    attribute_smileys: HashMap<String, String>, // attr_name -> smiley ("D", "U", "_")
    attribute_built: HashMap<String, bool>,
    /// Attributes declared with `has $x` (no twigil) — the bare name is an alias
    /// for `$!x` inside class methods.
    alias_attributes: HashSet<String>,
    methods: HashMap<String, Vec<MethodDef>>, // name -> overloads
    native_methods: HashSet<String>,
    mro: Vec<String>,
    /// Attribute var names (e.g. "!foo") that have `handles *` wildcard delegation.
    wildcard_handles: Vec<String>,
    /// Class-level attributes declared with `our $.x` or `my $.x` (shared across instances).
    /// Maps attribute name to its current value.
    class_level_attrs: HashMap<String, Value>,
}

#[derive(Debug, Clone)]
pub(crate) struct RoleDef {
    pub(crate) attributes: Vec<ClassAttributeDef>,
    pub(crate) methods: HashMap<String, Vec<MethodDef>>,
    pub(crate) is_stub_role: bool,
    pub(crate) is_hidden: bool,
    /// Whether this role was declared with `is rw` or `also is rw`.
    /// Used during `register_role_decl` to compute effective is_rw for attributes.
    #[allow(dead_code)]
    pub(crate) is_rw: bool,
    /// Captured environment for evaluating attribute defaults in closures.
    pub(crate) captured_env: Option<HashMap<String, Value>>,
    /// Attribute var names (e.g. "!foo") that have `handles *` wildcard delegation.
    pub(crate) wildcard_handles: Vec<String>,
    /// Unique identifier for this role definition instance, used to distinguish
    /// different lexical roles with the same name.
    pub(crate) role_id: u64,
    /// Attribute conflicts detected during role-to-role composition.
    /// Each entry is (attr_name, declaring_role, conflicting_role).
    pub(crate) attribute_conflicts: Vec<(String, String, String)>,
    /// Attribute names declared directly in this role's body (not inherited
    /// via `does`). Used to disambiguate diamond composition (where the same
    /// attribute reaches via two paths from a shared ancestor) from a real
    /// attribute conflict.
    pub(crate) own_attribute_names: HashSet<String>,
    /// Body statements deferred until composition time (for parameterized roles).
    /// These are non-method/non-attribute statements that may reference type parameters
    /// and must be re-executed for each class composition with concrete type bindings.
    pub(crate) deferred_body_stmts: Vec<Stmt>,
    /// Unknown lowercase trait names deferred for custom `trait_mod:<is>` dispatch.
    pub(crate) deferred_custom_traits: Vec<String>,
}

#[derive(Debug, Clone)]
pub(crate) struct RoleCandidateDef {
    type_params: Vec<String>,
    type_param_defs: Vec<ParamDef>,
    role_def: RoleDef,
    /// Parent classes/roles declared via `is` on this candidate.
    parents: Vec<String>,
    /// Language version (e.g. "6.c") captured at registration time.
    language_version: String,
}

#[derive(Debug, Clone)]
pub(crate) struct SubsetDef {
    base: String,
    predicate: Option<Expr>,
    version: String,
}

#[derive(Debug, Clone)]
pub(crate) struct MethodDef {
    pub(crate) params: Vec<String>,
    pub(crate) param_defs: Vec<ParamDef>,
    /// Method body AST. Wrapped in Arc to make MethodDef clones O(1) since
    /// the body is never mutated after construction and can be large.
    pub(crate) body: std::sync::Arc<Vec<Stmt>>,
    pub(crate) is_rw: bool,
    pub(crate) is_private: bool,
    pub(crate) is_multi: bool,
    pub(crate) is_my: bool,
    /// Role where this method was originally declared when composed into a class.
    pub(crate) role_origin: Option<String>,
    /// The deepest/original role where this method was first defined (for diamond detection).
    pub(crate) original_role: Option<String>,
    pub(crate) return_type: Option<String>,
    pub(crate) compiled_code: Option<std::sync::Arc<crate::opcode::CompiledCode>>,
    /// Delegation info: (attribute_var_name, target_method_name).
    /// When set, the method forwards the call (with all args) to the named method
    /// on the object stored in the given attribute.
    pub(crate) delegation: Option<(String, String)>,
    /// `is default` trait — this candidate is preferred when multi dispatch ties.
    pub(crate) is_default: bool,
    /// `is DEPRECATED` message: None = not deprecated.
    pub(crate) deprecated_message: Option<String>,
    /// Whether this is a submethod (not inherited by subclasses).
    pub(crate) is_submethod: bool,
}

/// Invocant context for an active `proto method` `{*}` dispatch.
#[derive(Debug, Clone)]
pub(crate) struct ProtoMethodCtx {
    pub(crate) invocant: Value,
}

/// One entry of `multi_dispatch_stack`: (function_name, remaining_candidates,
/// original_args, first_candidate_rw_params). See the field doc on
/// `Interpreter::multi_dispatch_stack`.
type MultiDispatchEntry = (
    String,
    Vec<Arc<FunctionDef>>,
    Vec<Value>,
    Vec<(usize, String)>,
);

#[derive(Debug, Clone)]
struct MethodDispatchFrame {
    receiver_class: String,
    invocant: Value,
    args: Vec<Value>,
    remaining: Vec<(String, MethodDef)>,
    /// The FIRST (winning) candidate's scalar `is rw`/`is raw` positional params
    /// as (positional_arg_index, sigil-less_param_name). Stays fixed across the
    /// MRO chain so a `nextsame`+rw redispatch can forward the rw param's current
    /// value and route the next candidate's writeback through it (§D capstone).
    rw_params: Vec<(usize, String)>,
}

/// Frame for navigating through wrapper chain during callsame/callwith.
#[derive(Debug, Clone)]
pub(crate) struct WrapDispatchFrame {
    /// The sub id being wrapped (to prevent re-entrant wrap dispatch).
    pub(crate) sub_id: u64,
    /// Remaining callables: inner wrappers then original sub. Next to call is first.
    pub(crate) remaining: Vec<Value>,
    /// Original call arguments.
    pub(crate) args: Vec<Value>,
}

#[derive(Debug, Clone)]
pub(crate) struct SquishIteratorMeta {
    pub(crate) source_items: Vec<Value>,
    pub(crate) as_func: Option<Value>,
    pub(crate) with_func: Option<Value>,
    pub(crate) revert_values: HashMap<String, Value>,
    pub(crate) revert_remove: Vec<String>,
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

/// Abstraction over TCP and UNIX socket streams so socket I/O code is shared.
#[derive(Debug)]
enum SocketStream {
    Tcp(std::net::TcpStream),
    #[cfg(unix)]
    Unix(std::os::unix::net::UnixStream),
}

impl std::io::Read for SocketStream {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        match self {
            SocketStream::Tcp(s) => s.read(buf),
            #[cfg(unix)]
            SocketStream::Unix(s) => s.read(buf),
        }
    }
}

impl SocketStream {
    fn try_clone(&self) -> std::io::Result<Self> {
        match self {
            SocketStream::Tcp(s) => Ok(SocketStream::Tcp(s.try_clone()?)),
            #[cfg(unix)]
            SocketStream::Unix(s) => Ok(SocketStream::Unix(s.try_clone()?)),
        }
    }

    fn peer_addr(&self) -> std::io::Result<String> {
        match self {
            SocketStream::Tcp(s) => s.peer_addr().map(|a| a.to_string()),
            #[cfg(unix)]
            SocketStream::Unix(s) => s.peer_addr().map(|a| {
                a.as_pathname()
                    .map_or("(unnamed)".to_string(), |p| p.display().to_string())
            }),
        }
    }
}

impl std::io::Write for SocketStream {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            SocketStream::Tcp(s) => s.write(buf),
            #[cfg(unix)]
            SocketStream::Unix(s) => s.write(buf),
        }
    }
    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            SocketStream::Tcp(s) => s.flush(),
            #[cfg(unix)]
            SocketStream::Unix(s) => s.flush(),
        }
    }
}

/// Abstraction over TCP and UNIX socket listeners.
#[derive(Debug)]
enum SocketListener {
    Tcp(std::net::TcpListener),
    #[cfg(unix)]
    Unix(std::os::unix::net::UnixListener),
}

impl SocketListener {
    fn accept(&self) -> std::io::Result<SocketStream> {
        match self {
            SocketListener::Tcp(l) => {
                let (stream, _addr) = l.accept()?;
                Ok(SocketStream::Tcp(stream))
            }
            #[cfg(unix)]
            SocketListener::Unix(l) => {
                let (stream, _addr) = l.accept()?;
                Ok(SocketStream::Unix(stream))
            }
        }
    }

    fn try_clone(&self) -> std::io::Result<Self> {
        match self {
            SocketListener::Tcp(l) => Ok(SocketListener::Tcp(l.try_clone()?)),
            #[cfg(unix)]
            SocketListener::Unix(l) => Ok(SocketListener::Unix(l.try_clone()?)),
        }
    }
}

/// Opaque payload attached to a `SharedPromise` so that a worker thread
/// can hand newly-opened IO handles back to the awaiting interpreter.
#[derive(Debug)]
pub(crate) struct ThreadPromisePayload {
    pub(crate) new_handles: Vec<(usize, IoHandleState)>,
    pub(crate) next_handle_id: usize,
}

#[derive(Debug)]
pub(crate) struct IoHandleState {
    target: IoHandleTarget,
    mode: IoHandleMode,
    path: Option<String>,
    line_separators: Vec<Vec<u8>>,
    line_chomp: bool,
    encoding: String,
    file: Option<fs::File>,
    socket: Option<SocketStream>,
    listener: Option<SocketListener>,
    closed: bool,
    out_buffer_capacity: Option<usize>,
    out_buffer_pending: Vec<u8>,
    #[allow(dead_code)]
    bin: bool,
    nl_out: String,
    bytes_written: i64,
    /// Whether any read/seek operation has been performed on this handle.
    /// Used to implement Raku's eof semantics: a freshly opened file at
    /// position 0 with size 0 returns False for .eof until a read is attempted.
    read_attempted: bool,
    /// Whether the UTF-16 BOM has been written for this handle.
    /// Used to ensure we only write one BOM at the start of a utf16 stream.
    utf16_bom_written: bool,
    /// For utf16 auto-detect: the detected endianness after reading BOM.
    /// None = not yet detected, Some(true) = big-endian, Some(false) = little-endian.
    utf16_detected_be: Option<bool>,
    /// For ArgFiles: index into @*ARGS tracking which file we're reading
    argfiles_index: usize,
    /// For ArgFiles: currently open file reader (buffered)
    argfiles_reader: Option<std::io::BufReader<fs::File>>,
    /// For ArgFiles created via `IO::ArgFiles.new(@files)`: the explicit file
    /// list to read from, overriding the global `@*ARGS`. None = use `@*ARGS`.
    argfiles_paths: Option<Vec<String>>,
    /// Buffered words not yet yielded by `read_word_from_handle_value`. A single
    /// line read can produce many words; the leftovers live here until consumed.
    pending_words: std::collections::VecDeque<String>,
    /// When set, the handle is closed automatically the moment word iteration
    /// reaches EOF (Raku's `words($fh, :close)` close-on-exhaust semantics).
    close_on_word_exhaust: bool,
}

#[derive(Clone)]
struct RegexPattern {
    tokens: Vec<RegexToken>,
    anchor_start: bool,
    anchor_end: bool,
    ignore_case: bool,
    ignore_mark: bool,
}

/// Context stored for each code block encountered during regex matching.
#[derive(Clone)]
pub(crate) struct CodeBlockContext {
    pub(crate) code: String,
    pub(crate) named: HashMap<String, Vec<String>>,
    pub(crate) matched_so_far: String,
    pub(crate) positional: Vec<String>,
}

/// A single entry in a quantified capture list: (matched_text, from, to, subcaptures).
pub(crate) type QuantifiedCaptureEntry = (String, usize, usize, Option<RegexCaptures>);

/// Prefix marking a `named_subcaps` entry as a *silent action capture*: the
/// match of a silent subrule (`<.foo>`) that is hidden from `.hash` but whose
/// grammar action method (and its descendants') must still fire. The prefix is a
/// control character that can never appear in a real capture name, so marker
/// entries never collide with user captures and are trivially filtered.
pub(crate) const SILENT_ACTION_MARKER_PREFIX: &str = "\u{1}silent\u{1}";

#[derive(Clone, Default)]
pub(crate) struct RegexCaptures {
    pub(crate) named: HashMap<String, Vec<String>>,
    /// Nested sub-captures for named subrule matches. Key is capture name,
    /// value is inner captures from the subrule (parallel to entries in `named`).
    pub(crate) named_subcaps: HashMap<String, Vec<RegexCaptures>>,
    pub(crate) positional: Vec<String>,
    /// Nested sub-captures for positional capture groups. Each entry corresponds
    /// to the same index in `positional` and holds inner captures from nested groups.
    pub(crate) positional_subcaps: Vec<Option<RegexCaptures>>,
    /// When a capture group is quantified (e.g. `(\w)+`), this parallel vec
    /// stores the list of all iteration matches for that positional index.
    /// When `Some`, the positional slot should be rendered as an Array of Match objects.
    pub(crate) positional_quantified: Vec<Option<Vec<QuantifiedCaptureEntry>>>,
    /// Character offsets (start, end) for each entry in `positional`.
    pub(crate) positional_offsets: Vec<(usize, usize)>,
    /// Marks a positional slot as an *unmatched optional* capture (`(x)?` that
    /// matched zero times) which must render as `Nil` (not an empty Match). Only
    /// the zero-match reservation arms set entries here; they pad with `false` up
    /// to the current `positional` length before pushing `true`, so matched
    /// captures (which never touch this vec) stay aligned. The Match builder reads
    /// it via `.get(i)` — a missing/`false` entry renders normally.
    pub(crate) positional_nil: Vec<bool>,
    /// Unnamed capture slots by capture index (for $0, $1, ...), where `None`
    /// represents an unmatched capture.
    pub(crate) positional_slots: Vec<Option<(String, usize, usize)>>,
    pub(crate) matched: String,
    pub(crate) from: usize,
    pub(crate) to: usize,
    pub(crate) capture_start: Option<usize>,
    pub(crate) capture_end: Option<usize>,
    /// Starting position of the match in the input (character index).
    /// Set at the beginning of regex matching to allow code blocks to compute
    /// the matched-so-far text.
    pub(crate) match_from: usize,
    /// Code blocks encountered during matching (code + captures at that point).
    /// Executed after match for side effects.
    /// Each entry: (code, named_captures, matched_so_far, positional_captures)
    pub(crate) code_blocks: Vec<CodeBlockContext>,
    /// Variables declared via `:my $var = expr;` inside regex.
    /// These are made available to `<{ code }>` closures.
    pub(crate) regex_vars: HashMap<String, Value>,
    /// The winning :sym<> variant name, if this match was from a protoregex.
    pub(crate) sym: Option<String>,
    /// Named captures from quantified tokens — always stored as arrays in Match.
    pub(crate) named_quantified: HashSet<String>,
    /// For aliased captures like `<str=.str_escape>`, maps capture name to
    /// original rule name for grammar action dispatch.
    pub(crate) capture_alias_map: HashMap<String, String>,
    /// The original rule name when this capture was stored under an alias.
    pub(crate) action_name: Option<String>,
    /// Hash captures from `%<name>=(...)` aliasing in regex.
    pub(crate) hash_captures: HashMap<String, Vec<(String, Option<String>)>>,
}

#[derive(Clone)]
struct RegexToken {
    atom: RegexAtom,
    quant: RegexQuant,
    named_capture: Option<String>,
    /// Secondary named capture for capturing subrule aliases like `$<alias>=<builtin_class>`.
    /// When set, the matched text is also stored under this name (the original rule name).
    secondary_named_capture: Option<String>,
    /// Hash aliasing: `%<name>=(...)` captures build a hash
    hash_capture: Option<String>,
    /// Array-sigil capture alias (`@<name>=(...)`): forces the named capture
    /// into list context, so even a single (non-quantified) match yields a
    /// one-element List rather than a bare Match. Mirrors Raku's `@`-sigil
    /// declaration semantics for hypothetical capture variables.
    force_list_capture: bool,
    ratchet: bool,
    /// Frugal (non-greedy) quantifier modifier: `*?`, `+?`, `??`
    frugal: bool,
    /// Separator for `%` / `%%` quantifiers, e.g. `<thing> +% ','`. When present,
    /// the quantified atom is matched with `separator.atom` interleaved between
    /// iterations. `allow_trailing` is true for `%%` (an optional trailing
    /// separator is permitted). The separator's own captures are appended as
    /// positional/named captures after the main atom's, matching Raku semantics.
    separator: Option<Box<RegexSeparatorSpec>>,
}

#[derive(Clone)]
struct RegexSeparatorSpec {
    /// The separator sub-pattern (matched between iterations). Holding a full
    /// pattern preserves named captures, quantifiers, and other structure of
    /// complex separators such as `$<delim>=<[a..z]>*`.
    pattern: RegexPattern,
    allow_trailing: bool,
}

#[derive(Clone)]
enum RegexAtom {
    Literal(char),
    Named(String),
    Any,
    CharClass(CharClass),
    /// `<.ws>` — Raku's word-boundary-aware whitespace rule:
    /// requires `\s+` between word characters, `\s*` otherwise.
    WsRule,
    Newline,
    NotNewline,
    Group(RegexPattern),
    CaptureGroup(RegexPattern),
    Alternation(Vec<RegexPattern>),
    SequentialAlternation(Vec<RegexPattern>),
    /// Conjunction: all branches must match at the same position; longest match wins
    Conjunction(Vec<RegexPattern>),
    ZeroWidth,
    CodeAssertion {
        code: String,
        negated: bool,
        is_assertion: bool,
    },
    /// `<{ code }>` — closure interpolation: evaluate code and match result as regex
    ClosureInterpolation {
        code: String,
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
    /// `:my $var = expr;` — variable declaration inside a regex
    VarDecl {
        code: String,
    },
    /// Combined character class: <+ xdigit - lower>, matches positive AND NOT negative
    CompositeClass {
        positive: Vec<ClassItem>,
        negative: Vec<ClassItem>,
    },
    /// Lookaround assertion: <?before pattern>, <!before pattern>,
    /// <?after pattern>, <!after pattern>
    Lookaround {
        pattern: RegexPattern,
        negated: bool,
        is_behind: bool,
    },
    /// `<<` or `«` — left word boundary assertion (zero-width)
    LeftWordBoundary,
    /// `>>` or `»` — right word boundary assertion (zero-width)
    RightWordBoundary,
    /// `^^` — start of line assertion (zero-width)
    StartOfLine,
    /// `$$` — end of line assertion (zero-width)
    EndOfLine,
    /// `$0`, `$1`, etc. — backreference to positional capture group
    Backref(usize),
    /// `$<name>` — backreference to named capture group
    NamedBackref(String),
    /// `<?same>` / `<!same>` — zero-width assertion: adjacent chars are same/different
    SameAssertion {
        negated: bool,
    },
    /// `<at(N)>` — zero-width assertion: match at position N
    AtPosition(usize),
    /// Internal marker used while rewriting `left ~ goal inner`.
    TildeMarker,
    /// Goal matching produced by `~`: match `inner` first, then `goal`,
    /// but preserve capture order as written (`goal` before `inner`).
    GoalMatch {
        goal: RegexPattern,
        inner: RegexPattern,
        goal_text: String,
    },
}

#[derive(Clone)]
enum RegexQuant {
    One,
    ZeroOrMore,
    OneOrMore,
    ZeroOrOne,
    /// `** min..max` — repeat exactly min to max times (max=None means unbounded)
    Repeat(usize, Option<usize>),
    /// `** {code}` — repeat count determined at runtime by evaluating code block
    RepeatCode(String),
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
    NegDigit,
    Word,
    NegWord,
    Space,
    NegSpace,
    HorizSpace,
    NegHorizSpace,
    VertSpace,
    NegVertSpace,
    NotNewline,
    NamedBuiltin(String),
    UnicodePropItem { name: String, negated: bool },
}

/// Entry in the callframe stack, tracking state for each call frame.
#[derive(Clone)]
pub(crate) struct CallFrameEntry {
    pub file: String,
    pub line: i64,
    pub code: Option<Value>,
    pub env: Env,
}

/// Entry in the routine stack, tracking the call chain for backtraces.
#[derive(Clone, Debug)]
pub(crate) struct RoutineFrame {
    pub package: String,
    pub name: String,
    pub line: Option<u32>,
    pub file: Option<String>,
    pub is_method: bool,
    /// Whether this frame is a block/closure (not a named routine).
    pub is_block: bool,
}

/// CompUnit::Repository::Installation runtime state. Boxed inside `Interpreter`
/// (see the `cur_repo` field) to keep it off the inline struct that is moved by
/// value into nested on-stack VMs.
#[derive(Default, Clone)]
pub(crate) struct CurRepoState {
    /// `$*REPO.loaded` units, keyed by repository prefix.
    loaded: HashMap<String, Vec<Value>>,
    /// Symbols loaded by `$*REPO.need(...)` but not yet published into GLOBAL.
    /// `::('Foo')` treats these as unknown until `merge-symbols` un-hides them.
    pending_global_symbols: HashSet<String>,
}

pub struct Interpreter {
    env: Env,
    /// Program output sink — stdout/stderr buffers, the immediate-flush flag,
    /// and thread-clone interleaving. Lifted behind `Arc<RwLock<…>>` (PR-B) so
    /// the VM and the Interpreter can reach it as peers, exactly like
    /// `io_handles` (③後段/④; see `docs/vm-output-ownership.md`). Access through
    /// the `output_sink()` / `output_sink_mut()` guarded accessors.
    output_sink: Arc<RwLock<OutputSink>>,
    warn_output: String,
    warn_suppression_depth: usize,
    /// All TAP / `Test` module runtime state (counter, subtest stack, bail-out).
    /// See [`TapState`] — extracted out of this struct so its ownership can later
    /// move (lever B). Access only through `self.tap`'s methods.
    tap: TapState,
    halted: bool,
    exit_code: i64,
    /// When true, `exit` sets the `halted` flag instead of calling
    /// `std::process::exit()`.  Used by in-process `is_run` so that
    /// the nested interpreter does not kill the parent process.
    pub(crate) nested_mode: bool,
    /// NativeCall (`is native`) sub descriptors, keyed by sub name. Populated at
    /// declaration; a call to a name present here is routed through C FFI
    /// instead of running the (`{ * }`) Raku body.
    pub(crate) native_call_specs: HashMap<String, nativecall::NativeCallSpec>,
    operator_assoc: HashMap<String, String>,
    /// Operator sub names (infix:<..>, prefix:<..>, etc.) that have been
    /// imported into the current lexical scope via `use Module`. Used to
    /// preseed the parser when EVAL is called so that imported operators
    /// remain visible, but non-exported operators from loaded modules do not.
    pub(crate) imported_operator_names: HashSet<String>,
    lib_paths: Vec<String>,
    /// Open IO handles (files/sockets/listeners) shared between the VM and the
    /// Interpreter behind transitional `Arc<RwLock>` scaffolding. Snapshot-cloned
    /// per thread (see [`io_handles`] module docs and `clone_for_thread`).
    io_handles: Arc<RwLock<io_handles::IoHandleTable>>,
    program_path: Option<String>,
    /// Name of the package currently in scope (e.g. `GLOBAL`, `Foo::Bar`),
    /// used to build fully-qualified names during function/method dispatch and
    /// declaration. Held behind transitional `Arc<RwLock>` scaffolding so the VM
    /// can read/write it through its own handle (mirroring `io_handles` /
    /// `registry`) rather than bouncing through `self.interpreter`. Snapshot-cloned
    /// per thread (see `clone_for_thread`). Accessed only via
    /// `current_package()` / `set_current_package()`, which read-clone / write the
    /// lock and never hold the guard across user-code re-entry.
    current_package: Arc<RwLock<String>>,
    routine_stack: Vec<RoutineFrame>,
    callframe_stack: Vec<CallFrameEntry>,
    method_class_stack: Vec<String>,
    /// The class whose instance is currently being constructed, set only while
    /// evaluating typed-attribute default type objects so a suppressed nested
    /// class name resolves within its owning class (see `resolve_suppressed_type`).
    constructing_class: Option<String>,
    pending_call_arg_sources: Option<Vec<Option<String>>>,
    test_pending_callsite_line: Option<i64>,
    /// Number of active CONTROL handlers in the current VM stack. Tracked
    /// on the interpreter (rather than per-VM) so that nested VMs (e.g.
    /// EVAL) can observe handlers installed by the outer VM and propagate
    /// warn/control signals appropriately.
    pub(crate) control_handler_depth: u32,
    test_assertion_line_stack: Vec<i64>,
    block_stack: Vec<Value>,
    doc_comments: HashMap<String, DocComment>,
    /// Ordered list of doc comments for $=pod
    doc_comment_list: Vec<DocComment>,
    /// Cache for .WHY results so identity checks (=:=) work
    why_cache: HashMap<String, Value>,
    type_metadata: HashMap<String, HashMap<String, Value>>,
    when_matched: bool,
    gather_items: Vec<Vec<Value>>,
    gather_take_limits: Vec<Option<usize>>,
    block_scope_depth: usize,
    /// Declaration registry (enums/subsets/... — migrated group-by-group, PLAN.md ②),
    /// shared with the VM behind `Arc<RwLock>`. See [`Registry`] and `src/runtime/registry.rs`.
    /// Lock discipline: never hold a guard across user-code re-entry (deadlock).
    registry: Arc<RwLock<Registry>>,
    /// Active `{*}` proto dispatch frames: (proto_name, args, method_ctx).
    /// `method_ctx` is `Some` when the active proto is a `proto method` body, so
    /// `{*}` redispatches to a multi *method* candidate on the invocant rather
    /// than a proto sub candidate.
    proto_dispatch_stack: Vec<(String, Vec<Value>, Option<ProtoMethodCtx>)>,
    /// One-shot guard set by a proto method's `{*}` redispatch: the next method
    /// call of this name bypasses proto-body interception (so it reaches the
    /// real multi candidate). Recursive calls from within the candidate, which
    /// happen after this flag is consumed, run the proto body again (matching raku).
    pub(crate) proto_method_skip: Option<String>,
    pending_dispatch_error: Option<RuntimeError>,
    end_phasers: Vec<(Vec<Stmt>, Env)>,
    /// Tracks END phaser site_ids to ensure each is registered only once.
    end_phaser_sites: HashSet<u64>,
    chroot_root: Option<PathBuf>,
    loaded_modules: HashSet<String>,
    need_hidden_classes: HashSet<String>,
    /// CompUnit::Repository::Installation state (`.loaded` units and the symbols
    /// pulled in by `.need` but not yet merged into GLOBAL).
    ///
    /// Boxed: the whole `Interpreter` is moved by value into a `VM` that lives on
    /// the stack (see `run_block_raw`), and nested module loads stack full copies,
    /// so keeping rarely-used state off the inline struct preserves stack budget.
    cur_repo: Box<CurRepoState>,
    /// Classes/roles hidden from package stash lookups (e.g. `Example2::.keys`).
    /// Populated when a `use X::Y` loads modules whose dependency chain neither
    /// declares a class matching the module name nor includes a `package X {}`
    /// declaration, hiding transitive dependencies from the namespace stash.
    package_stash_hidden: HashSet<String>,
    /// Package names declared via `package X {}` during the current module
    /// loading chain. Saved/restored around each top-level `use_module_with_tags`
    /// call so it only contains packages from the current loading chain.
    pub(crate) chain_declared_packages: HashSet<String>,
    /// Maps module names to the set of packages declared during their loading.
    /// Used to propagate package declarations when a module is re-used.
    module_packages: HashMap<String, HashSet<String>>,
    closure_env_overrides: HashMap<u64, Env>,
    /// PredictiveIterator backing a `Seq.new(iterator)`, keyed by the Seq's
    /// Arc pointer (`seq_id`). Kept off the scoped `env` so the association
    /// survives sub/block returns between Seq creation and `.tail`/`.Numeric`
    /// (an env-keyed side table was lost on scope exit).
    /// TODO: entries are never reclaimed; acceptable as predictive Seqs are rare.
    predictive_seq_iters: HashMap<usize, Value>,
    protect_block_cache: ProtectBlockCache,
    /// Compiled bytecode for subset `where` predicates, keyed by subset name.
    /// A subset's predicate is a fixed `Expr`, so it is compiled once and reused
    /// across all type checks instead of recompiling + cloning the entire
    /// function/proto registry on every check (the old `eval_block_value` path).
    /// Cleared per-name on subset redeclaration; starts empty per thread (the
    /// cache is a pure recomputable optimization). See `type_matches_value`.
    subset_predicate_cache: HashMap<String, SubsetPredicateCompiled>,
    private_zeroarg_method_cache: HashMap<(String, String), Option<(String, MethodDef)>>,
    module_load_stack: Vec<String>,
    /// The current distribution context ($?DISTRIBUTION).
    pub(crate) current_distribution: Option<Value>,
    /// Maps package names to their distribution context.
    /// Populated during module loading so OTF compilation can resolve $?DISTRIBUTION.
    pub(crate) package_distributions: HashMap<String, Value>,
    /// Exported subroutine symbols by package and export tag.
    exported_subs: HashMap<String, HashMap<String, HashSet<String>>>,
    /// Exported variable/constant symbols by package and export tag.
    exported_vars: HashMap<String, HashMap<String, HashSet<String>>>,
    /// Trait-modified routine values (e.g. a sub with a custom `is` trait that
    /// mixed a role into it) keyed by package and routine name. Captured at
    /// `is export` registration time so `import` can restore the `&name` env
    /// binding with the role mixed in, rather than just the plain FunctionDef.
    exported_sub_values: HashMap<String, HashMap<String, Value>>,
    /// Mirrored export tables for modules declared with `unit module X`
    /// when the actual runtime package registration used "GLOBAL".
    /// Populated during `load_module` so that `import_module` can perform
    /// tag validation and raise `X::Import::NoSuchTag` for bad tags.
    unit_module_exported_subs: HashMap<String, HashMap<String, HashSet<String>>>,
    /// Stack of unit-module names currently being loaded; used by
    /// `register_exported_sub` to mirror GLOBAL registrations into
    /// `unit_module_exported_subs`.
    unit_module_loading_stack: Vec<String>,
    /// When true, `is export` trait is ignored (used by `need` to load without importing).
    pub(crate) suppress_exports: bool,
    /// When true, rw routine calls should not auto-FETCH Proxy return values.
    pub(crate) in_lvalue_assignment: bool,
    /// When true, a role call with non-matching args returns a Pair instead of
    /// throwing X::Coerce::Impossible. Set during the RHS evaluation of `does`
    /// so that `$x does Role("arg")` works as a role application.
    pub(crate) in_does_rhs: bool,
    /// When set, `does` on a routine parameter inside trait_mod:<is> will
    /// store the resulting Mixin value for writeback to the outer scope.
    pub(crate) trait_mod_writeback_key: Option<String>,
    /// The captured Mixin value from a trait_mod `does` writeback.
    pub(crate) trait_mod_writeback_value: Option<Value>,
    /// When true, hash indexing with a missing key autovivifies (creates an
    /// empty Hash entry and returns it).  Set during reduce with `is raw`
    /// callbacks so that container semantics are preserved.
    pub(crate) hash_autovivify: bool,
    pub(crate) newline_mode: NewlineMode,
    /// Stack of snapshots for lexical import scoping.
    /// Each entry saves (function_keys, class_names, newline_mode, strict_mode, fatal_mode)
    /// before a block with `use`.
    import_scope_stack: Vec<ImportScopeSnapshot>,
    pub(crate) strict_mode: bool,
    pub(crate) fatal_mode: bool,
    /// Persistent store for `our`-scoped variables.  Values are saved here
    /// by `SetGlobal` so they survive block-scope restoration (which only
    /// preserves env keys that existed before the block).
    our_vars: HashMap<String, Value>,
    /// Package-block `my` lexicals, keyed by package name then env var name.
    /// A named sub defined in a `package Foo { my $x = ...; sub f { $x } }` block
    /// closes over `$x`, but mutsu's registry subs have no per-sub closure env and
    /// resolve free vars from the call-time env; the block scope is dropped on exit
    /// (`exec_package_scope_op`) so a by-name/exported call (`Foo::f`, an exported
    /// `MAIN`) can no longer see `$x`. After the block runs, its `my` lexicals are
    /// recorded here keyed by the package, and a `GetGlobal` miss falls back to
    /// `package_lexicals[current_package]`. This fires ONLY inside that package's
    /// subs (where `current_package == Foo`), so it does not leak the lexical to
    /// bare references after the block (which run under `GLOBAL`).
    pub(crate) package_lexicals: HashMap<String, HashMap<String, Value>>,
    state_vars: HashMap<String, Value>,
    /// Per-closure-instance captured-variable state, keyed by
    /// (closure instance id, captured variable Symbol). This is the hot
    /// closure-call persistence store (loaded/saved on every closure call for
    /// its free variables); a typed key avoids the per-call
    /// `format!("__mutsu_closure_cap::{id}::{name}")` String allocation and the
    /// String hashing that dominated the closure dispatch profile.
    closure_captured_state: HashMap<(u64, Symbol), Value>,
    once_values: HashMap<String, Value>,
    once_scope_stack: Vec<u64>,
    next_once_scope_id: u64,
    /// Variable dynamic-scope metadata used by `.VAR.dynamic`.
    var_dynamic_flags: HashMap<String, bool>,
    /// Stack of caller environments for $CALLER:: / $DYNAMIC:: resolution.
    /// Each entry is a snapshot of the env at the point a sub/function was called.
    caller_env_stack: Vec<Env>,
    /// Variable binding aliases: maps target name -> source name.
    /// When target is read, the value of source is returned instead.
    /// Set up by $CALLER::target := $source binding.
    var_bindings: HashMap<String, String>,
    /// `use variables :D/:U/:_` pragma — applies default smiley to unsmiley'd type constraints.
    /// Empty string means no pragma active.
    pub(crate) variables_pragma: String,
    /// `use attributes :D/:U/:_` pragma — applies default smiley to unsmiley'd attribute type constraints.
    /// Empty string means no pragma active.
    pub(crate) attributes_pragma: String,
    /// Variable type constraints used to enforce typed re-assignment across closures.
    var_type_constraints: HashMap<String, String>,
    /// Monotonic flag: set once any `atomicint` variable / atomic storage has been
    /// registered in this interpreter (or inherited from a parent thread). The
    /// per-`GetGlobal`/`GetLocal` atomic-variable check is expensive (a `format!`
    /// plus two `var_type_constraint` lookups, each itself a `format!`), yet
    /// atomics are exotic; when this flag is clear the entire check is skipped,
    /// which removes that cost from the hot variable-read path. Never cleared, so
    /// a program that stops using an atomic still resolves correctly.
    atomic_var_seen: bool,
    /// Variable default values set by `is default(...)` trait.
    var_defaults: HashMap<String, Value>,
    // Array/Hash element defaults are embedded in `ArrayData.default` /
    // `HashData.default`.
    /// Optional hash key type constraints (e.g. `%h{Str}`).
    var_hash_key_constraints: HashMap<String, String>,
    // Array/Hash/Set/Bag/Mix type metadata and object-hash original keys are
    // embedded in their backing data structs (ArrayData/HashData/SetData/
    // BagData/MixData) — no side tables.
    /// Type metadata for instance values keyed by stable instance id. Lifted
    /// behind `Arc<RwLock>` (the same shared-handle playbook used for
    /// `current_package` / `io_handles`) so the VM and Interpreter can reach it
    /// as peers and CP-3 can fold it by handle transfer rather than ownership
    /// reasoning. Like those handles it is a *per-thread snapshot*, not
    /// live-shared: `clone_for_thread` deep-copies the map into a fresh `Arc`,
    /// so the lock never contends across threads. Collapses to a plain VM field
    /// once the Interpreter execution path is removed (PLAN.md ④/⑤).
    instance_type_metadata: Arc<RwLock<HashMap<u64, ContainerTypeInfo>>>,
    let_saves: Vec<(String, Value, bool)>,
    pub(super) supply_emit_buffer: Vec<Vec<Value>>,
    pub(super) supply_emit_timed_buffer: Vec<Vec<(Value, std::time::Instant)>>,
    /// Active streaming consumers for on-demand `supply { ... }` bodies driven by
    /// `react`. When a stream consumer is registered for an emitter's
    /// `supplier_id`, `emit` delivers the value to the consumer callback
    /// synchronously (instead of buffering into `supply_emit_buffer`), so an
    /// infinite synchronous body (`supply { loop { emit(...) } }`) can be
    /// terminated by the consumer's `done` on emit-to-dead-consumer.
    pub(super) supply_stream_consumers: Vec<crate::runtime::subtest::StreamConsumer>,
    /// Shared variables between threads. When `start` spawns a thread,
    /// variables are stored here so both parent and child can see mutations.
    shared_vars: Arc<RwLock<HashMap<String, Value>>>,
    /// True when this interpreter participates in cross-thread variable sharing.
    /// Set by `clone_for_thread` on both parent and child.
    pub(crate) shared_vars_active: bool,
    /// True once any sigilless attribute alias (`has $x`) has been materialized.
    /// Sigilless attributes are read/written through a bare `Var("x")` that is
    /// disambiguated only by the runtime `__mutsu_sigilless_alias::` table, so
    /// the cell-direct read/write routing must consult that table. This flag
    /// gates that extra lookup so programs without sigilless attributes (the vast
    /// majority) pay nothing on the hot variable-read path. Process-sticky: set
    /// true on first use, never reset (Phase 3 Stage 2c (ii)).
    pub(crate) sigilless_attrs_active: bool,
    /// Keys in shared_vars that were explicitly updated (not just initialized by
    /// `clone_for_thread`). `sync_shared_vars_to_env` only syncs these keys so
    /// that function parameters aren't overwritten with stale values.
    shared_vars_dirty: Arc<RwLock<HashSet<String>>>,
    /// Registry of encodings (both built-in and user-registered).
    /// Each entry maps a canonical name to an EncodingEntry.
    encoding_registry: Vec<EncodingEntry>,
    /// When set, pseudo-method names (DEFINITE, WHAT, etc.) bypass native fast path.
    /// Used for quoted method calls like `."DEFINITE"()`.
    pub(crate) skip_pseudo_method_native: Option<String>,
    /// Set by multi-method resolution when two or more candidates are equally
    /// specific (an ambiguous dispatch). Consumed by the caller to raise an
    /// `X::Multi::Ambiguous` error instead of silently picking one.
    pub(crate) dispatch_ambiguous: bool,
    /// Pending Proxy subclass attribute reference for writeback on mutating methods.
    /// Set when reading a Proxy subclass attribute; consumed by subsequent .push/.pop etc.
    pub(crate) pending_proxy_subclass_attr: Option<(crate::value::ProxySubclassAttrs, String)>,
    /// Stack of remaining multi dispatch candidates for callsame/nextsame/nextcallee.
    /// Each entry is (function_name, remaining_candidates, original_args,
    /// first_candidate_rw_params). The 4th element lists the FIRST (winning,
    /// compiled) candidate's scalar `is rw`/`is raw` positional params as
    /// (positional_arg_index, sigil-less_param_name); it stays fixed across the
    /// redispatch chain so a `nextsame`+rw redispatch can (a) pass the rw param's
    /// CURRENT value to the next candidate and (b) write the chain's final value
    /// back into the first candidate's VM local slot, instead of the first
    /// candidate's exit flush clobbering it with its own stale value (§D capstone).
    multi_dispatch_stack: Vec<MultiDispatchEntry>,
    method_dispatch_stack: Vec<MethodDispatchFrame>,
    /// Stack of samewith dispatch contexts.
    /// Each entry is (function_or_method_name, optional_invocant).
    /// Pushed whenever a multi sub or multi method is entered, popped on exit.
    samewith_context_stack: Vec<(String, Option<Value>)>,
    /// Wrap chains: sub_id -> stack of (handle_id, wrapper_sub). Outermost is last.
    wrap_chains: HashMap<u64, Vec<(u64, Value)>>,
    /// Maps sub_id to function name for named call wrap chain lookup.
    wrap_sub_names: HashMap<u64, String>,
    /// Maps function name to the Sub value that was wrapped. Used to get the right sub_id
    /// when dispatching named function calls through the wrap chain.
    wrap_name_to_sub: HashMap<String, Value>,
    /// Maps function name to the callable_id at the time wrap was first called.
    /// Used to detect sub redefinition (e.g. `sub foo` in a new block).
    wrap_callable_ids: HashMap<String, Option<i64>>,
    /// Counter for generating unique wrap handle IDs.
    wrap_handle_counter: u64,
    /// Stack of wrap dispatch frames for callsame/callwith inside wrappers.
    wrap_dispatch_stack: Vec<WrapDispatchFrame>,
    /// Method-level wrap chains: (class_name, method_name, candidate_index) ->
    /// stack of (handle_id, wrapper_sub).
    method_wrap_chains: HashMap<(String, String, usize), Vec<(u64, Value)>>,
    /// Names suppressed by `anon class`. These bare words should error as undeclared.
    suppressed_names: HashSet<String>,
    /// Bare enum variant names poisoned by redeclaration from different enums.
    /// Maps bare name -> latest enum package name.
    poisoned_enum_aliases: HashMap<String, String>,
    /// Per-scope stack of bare enum names introduced, for cleanup on scope exit.
    enum_scope_names: Vec<Vec<String>>,
    /// Fully-qualified names of `my`-scoped classes/subs inside packages.
    /// These should NOT appear in the parent package's stash.
    my_scoped_package_items: HashSet<String>,
    /// Stack of lexically-scoped class names per block scope depth.
    /// When a block scope exits, classes registered in that scope get suppressed.
    lexical_class_scopes: Vec<Vec<String>>,
    /// Last expression value from VM execution, used by REPL for auto-display.
    pub(crate) last_value: Option<Value>,
    /// Pending env updates from regex code blocks, to be synced to VM locals.
    pub(crate) pending_local_updates: Vec<(String, Value)>,
    /// Set of variable names that are readonly (default parameter binding).
    readonly_vars: HashSet<String>,
    /// Metadata for Seq values produced by `squish` with callbacks, used to
    /// provide callback-aware iterator behavior.
    pub(crate) squish_iterator_meta: HashMap<usize, SquishIteratorMeta>,
    /// Metadata for custom types created by Metamodel::Primitives.create_type.
    pub(crate) custom_type_data: HashMap<u64, CustomTypeData>,
    /// Rebless mapping: instance_id -> new HOW value.
    /// Used by Metamodel::Primitives.rebless to track reblessed objects.
    pub(crate) rebless_map: HashMap<u64, Value>,
    /// Value set by `make()` inside grammar action methods.
    /// Persists across env save/restore in method dispatch.
    pub(crate) action_made: Option<Value>,
    /// The `:actions` object of an in-progress `Grammar.parse`, if any. Set for
    /// the duration of a parse so that `<?{ ... }>` code assertions can run the
    /// relevant action method on a just-matched named capture and expose its
    /// `.made` result during parsing (raku runs actions incrementally at reduce
    /// time; mutsu otherwise only runs them post-parse). Saved/restored around
    /// nested/re-entrant parses.
    pub(crate) current_grammar_actions: Option<Value>,
    /// Pending error from regex security validation, to be propagated by the caller.
    #[allow(dead_code)]
    pending_regex_error: Option<RuntimeError>,
    /// When true, module precompilation cache is enabled.
    precomp_enabled: bool,
    /// When true, `augment class` is allowed (set by `use MONKEY-TYPING` or `use MONKEY`).
    monkey_typing: bool,

    // === Merged VM execution registers (CP-3 collapse: the bytecode VM was
    // dissolved into the Interpreter; these were the per-execution fields of the
    // former `VM` struct). The Interpreter IS the bytecode VM now. ===
    pub(crate) stack: Vec<Value>,
    pub(crate) locals: Vec<Value>,
    /// Current frame's captured upvalue array, indexed by the running
    /// `CompiledCode::upvalue_syms` order. Read by `GetUpvalue(i)`. Set from
    /// `SubData::upvalues` on closure entry and saved/restored across call frames
    /// alongside `locals`. A `None` entry (or out-of-range index) makes
    /// `GetUpvalue` fall back to a by-name env read. Empty for non-closure frames.
    pub(crate) upvalues: Vec<Option<Value>>,
    pub(crate) in_smartmatch_rhs: bool,
    pub(crate) transliterate_in_smartmatch: bool,
    pub(crate) substitution_in_smartmatch: bool,
    pub(crate) last_topic_value: Option<Value>,
    pub(crate) topic_save_stack: Vec<Value>,
    pub(crate) container_ref_var: Option<String>,
    pub(crate) container_ref_reversed: bool,
    pub(crate) topic_source_var: Option<String>,
    /// The `@`/`%` source variable when `$_` is a whole-container topic
    /// (`given @a` / `with %h`), where `$_` aliases the entire container. A `.=`
    /// metaop on the topic (`TopicDotAssign`) writes the reassigned `$_` straight
    /// through to this source with container-assignment coercion. Distinct from
    /// `topic_source_var`, which a `for @a` element loop also sets but where `$_`
    /// is a single element (handled by the per-element writeback, not this).
    pub(crate) topic_container_source: Option<String>,
    pub(crate) element_source: Option<(String, Value, bool)>,
    pub(crate) quanthash_bind_params: Vec<String>,
    pub(crate) for_param_restore_stack: Vec<(String, Option<Value>)>,
    pub(crate) call_frames: Vec<crate::vm::VmCallFrame>,
    /// Active CONTROL handlers on the dynamic call stack (one per executing
    /// `CONTROL { }` block). Kept in lock-step with `control_handler_depth` so
    /// a `warn` raised deep inside a protected body can find the innermost
    /// handler via `.last()` and, if it is `resume_safe`, run it inline at the
    /// raise site (cross-frame resumable warn). See `vm::ControlHandlerEntry`.
    pub(crate) control_handlers: Vec<crate::vm::ControlHandlerEntry>,
    /// Address of the `CompiledCode` of the bytecode frame currently executing
    /// in `exec_one` (set at the top of every dispatch). Used by the lazy-force
    /// machinery to reconcile the *caller's* local slots from env after a reify
    /// mutated a captured-outer lexical (Slice F: the lazy body runs at reify
    /// time, deep inside an op handler, so its captured-outer write reaches env
    /// but not the caller slot under reverse-sync OFF). Stored as an address
    /// (not a raw pointer) so the interpreter stays `Send` for worker threads;
    /// it is only dereferenced synchronously within the same call tree, where
    /// the pointed-to `CompiledCode` is an ancestor stack frame and therefore
    /// alive. `0` before any frame runs. Reset across thread clones.
    pub(crate) current_code: usize,
    /// When `Some`, a *carrier* (EVAL / interpreter fallback) is running and
    /// every by-name env write through `set_env_with_main_alias` logs its name
    /// here. On carrier return, exactly these names are written back into the
    /// caller's slots (`writeback_carrier_writes`). See docs/vm-single-store.md
    /// Slice B.
    pub(crate) carrier_writes: Option<std::collections::HashSet<String>>,
    pub(crate) method_dispatch_pure: bool,
    pub(crate) resume_ip: Option<usize>,
    pub(crate) bind_context: bool,
    pub(crate) scalar_bind_context: bool,
    pub(crate) bound_decont_active: bool,
    pub(crate) rebind_context: bool,
    pub(crate) constant_context: bool,
    /// Slice 2a (`docs/scalar-array-sharing.md`): set by `MarkArrayShareContext`
    /// just before a `SetLocal` for `$scalar = @arr` / `$scalar = %hash`. Tells
    /// the assignment to promote the source container to a shared `ContainerRef`
    /// cell (raku reference semantics) rather than snapshotting it.
    pub(crate) array_share_context: bool,
    /// Slice 2a/2b: the source variable name whose container the upcoming
    /// `SetLocal`/`AssignExpr` should share (set by `MarkArrayShareSource`).
    /// `@z`/`%h` for a whole-container RHS (`$n = @z`), or a scalar name for a
    /// chained share (`$r = $q`); the runtime only shares when that source holds
    /// a container/`ContainerRef` (so a plain `$x = $y` stays a copy).
    pub(crate) array_share_source: Option<String>,
    /// Slice 2a: cheap gate — `true` once any `__mutsu_array_share::` marker has
    /// been set, so the `SetLocal` write-through fast path only pays the marker
    /// lookup when at least one `=`-array-shared scalar exists.
    pub(crate) array_share_active: bool,
    /// Slice 2b: set by `MarkElementShare` to flag the upcoming
    /// `IndexAssignExprNamed` as a `=`-reference share of an array/hash element
    /// (vs a true `:=` bind). Consumed by `exec_index_assign_expr_named_op`,
    /// which marks the written element `__mutsu_elem_share::` after the store.
    pub(crate) element_share_pending: bool,
    pub(crate) explicit_initializer_context: bool,
    pub(crate) vardecl_context: bool,
    /// Set by `MarkShapedDeclContext` before a `SetLocal` whose `my @a[N]` /
    /// `my @a[N;M] = ...` declaration is itself shaped — so the assignment KEEPS
    /// the shape instead of dropping it as a value copy (`my @u = @shaped` does).
    pub(crate) shaped_decl_context: bool,
    /// Slice F (env<->locals coherence): the caller-variable *source* names that
    /// the most recent compiled-function return wrote back via an `is rw` /
    /// `is raw` / aliased-container parameter (`apply_rw_bindings_to_env`). The
    /// writeback mutates the caller's variable in `env` by name; the call-site op
    /// (which holds the caller's `code`) drains this list and writes each value
    /// straight through to the caller's local slot, so the slot stays coherent
    /// without the reverse `sync_locals_from_env` pull.
    pub(crate) pending_rw_writeback_sources: Vec<String>,
    /// Like `pending_rw_writeback_sources` but for writes that target a *caller
    /// frame's* lexical by name (`callframe(d).my.<$x> = v` / `$CALLER::x = v`).
    /// These differ in two ways: (1) the target slot lives several frames up, not
    /// in the immediate caller, so a source unmatched at one call site must be
    /// RETAINED (not dropped) until it reaches the frame that owns the slot — an
    /// intervening *deeper* call (the writer making another call before returning)
    /// must not consume it; (2) the value is read from env at drain time, same as
    /// the rw list. Drained at the same call sites, with retain-on-miss semantics.
    pub(crate) pending_caller_var_writeback: Vec<String>,
    pub(crate) local_bind_pairs: Vec<(usize, usize)>,
    pub(crate) otf_compile_cache: HashMap<u64, CompiledFunction>,
    pub(crate) state_scope_id: Option<u64>,
    #[allow(clippy::type_complexity)]
    pub(crate) fn_resolve_cache: HashMap<(Symbol, usize, Vec<String>), (String, u64, String)>,
    pub(crate) fn_resolve_gen: u64,
    pub(crate) fn_resolve_cache_gen: u64,
    pub(crate) multi_candidates_cache: HashMap<Symbol, bool>,
    pub(crate) multi_candidates_cache_gen: u64,
    pub(crate) light_call_cache: HashMap<Symbol, (String, u64)>,
    pub(crate) light_call_cache_gen: u64,
    pub(crate) pos_light_call_cache: HashMap<Symbol, (String, u64)>,
    pub(crate) pos_light_call_cache_gen: u64,
    /// Bare names that appear as a `&`-sigil parameter in some registered sub
    /// (e.g. `foo` from `sub callit(&foo) {...}`). A call to such a name may be
    /// shadowed by a lexical `&name` binding in the current frame, so the
    /// name-keyed light-call caches must be bypassed for it (the slow path's
    /// `lexical_override` check resolves the correct callable). Populated at sub
    /// registration; checked cheaply (guarded by `is_empty()`) on each call.
    pub(crate) amp_param_shadowed_names: std::collections::HashSet<Symbol>,
    /// Fingerprint of the sub declaration currently installed under each
    /// `package::name` (single, non-multi) routine key. A re-executed
    /// `RegisterSub` whose compile-time fingerprint matches the installed one is
    /// an idempotent no-op (see [`crate::ast::sub_registration_fingerprint`]),
    /// so the registrar can return early without re-deriving the FunctionDef and
    /// without invalidating the resolution caches. Entries are best-effort: a
    /// miss simply takes the full registration path.
    pub(crate) registered_fn_fingerprints: HashMap<Symbol, u64>,
    /// Derive-once cache: a declaration is parsed into a `FunctionDef` exactly
    /// once, then shared. Keyed by the routine's fully-qualified name
    /// (`package::name`), the value is `(declaration fingerprint, Arc<FunctionDef>)`.
    /// A `my sub` inside a routine is removed from the registry when the routine
    /// returns (lexical-scope snapshot/restore) and re-installed on the next call;
    /// without this cache that re-install would re-run the full AST→FunctionDef
    /// derivation (auto-signature scan, validation, body clone) every call. With
    /// it, the re-install is a cheap `Arc` clone of the already-derived definition.
    /// The key is the FQ name (not the fingerprint) so two distinct subs that share
    /// an identical body but differ in name never alias; the stored fingerprint is
    /// re-checked on lookup so a redefined body at the same name re-derives.
    pub(crate) prepared_fn_defs: HashMap<Symbol, (u64, Arc<FunctionDef>)>,
    pub(crate) method_resolve_cache:
        rustc_hash::FxHashMap<(Symbol, Symbol), crate::vm::MethodResolveEntry>,
    #[allow(clippy::type_complexity)]
    pub(crate) last_method_resolve: Option<(Symbol, Symbol, String, Arc<MethodDef>)>,
    pub(crate) fast_method_cache:
        rustc_hash::FxHashMap<(Symbol, Symbol), crate::vm::FastMethodCacheEntry>,
    /// Sound multi-method resolution cache (§B): for a multi whose dispatch is
    /// purely type+arity based (no `where` / literal / subset / `:D`/`:U` smiley /
    /// coercion candidate), the resolved candidate is a function of the receiver
    /// class + method + the runtime types of the positional args, so it is cached
    /// here keyed on `(class, method, arg-type-keys)`. Cleared with the other
    /// method caches when the registry changes.
    #[allow(clippy::type_complexity)]
    pub(crate) multi_resolve_cache:
        rustc_hash::FxHashMap<(Symbol, Symbol, Vec<Symbol>), Option<(String, Arc<MethodDef>)>>,
    /// Memoized `(class, method) -> is this multi's dispatch type+arity deterministic`
    /// (i.e. cacheable in `multi_resolve_cache`). Computed once by scanning the MRO
    /// candidates for value-dependent constraints.
    pub(crate) multi_type_cacheable: rustc_hash::FxHashMap<(Symbol, Symbol), bool>,
    /// Memoized `(class, method) -> does this name have >= 2 structural dispatch
    /// candidates across the MRO` (counting overloads BEFORE arg-matching).
    /// `false` means the name resolves to at most one candidate, so
    /// `push_method_dispatch_frame` can skip the per-call `resolve_all_methods_with_owner`
    /// MRO walk + MethodDef clones entirely (a single/zero candidate never produces
    /// a deferral frame regardless of args — arg-matching only reduces the count).
    /// Structural (registry-shape) only, so it is sound to key on `(class, method)`
    /// and is cleared with the other method caches on any registry change.
    pub(crate) dispatch_multi_candidate: rustc_hash::FxHashMap<(Symbol, Symbol), bool>,
    /// Memoized structural fingerprint of a method body, keyed by the *pointer*
    /// of its `Arc<Vec<Stmt>>` body. `function_body_fingerprint` Debug-traverses
    /// the whole body AST, which dominated the method-redispatch hot path
    /// (`build_remaining` / `prepare_method_dispatch_frame`, reached by every
    /// `nextsame`/`samewith` and multi-method call) — perf showed ~8% of a
    /// samewith-tight-loop in SipHash-over-Debug. A `MethodDef` clone shares its
    /// body `Arc`, and two *distinct* methods always have distinct body `Arc`s
    /// (clones are the only way to share one, and clones carry identical
    /// params/param_defs), so the body-`Arc` pointer uniquely identifies the
    /// `(params, param_defs, body)` tuple the fingerprint covers. The cache holds
    /// a strong `Arc` clone of each body so the pointer can never be freed and
    /// reused under a stale entry — it needs no invalidation and is bounded by
    /// the number of distinct method bodies in the program.
    pub(crate) method_body_fp_cache: rustc_hash::FxHashMap<usize, (Arc<Vec<Stmt>>, u64)>,
    /// Same memoization as `method_body_fp_cache`, but for `Arc<FunctionDef>`
    /// (the multi *function* redispatch path: `push_multi_dispatch_frame` runs
    /// `function_body_fingerprint` over every candidate on every multi call to
    /// pick/skip the winner). Keyed by the `Arc<FunctionDef>` pointer, which
    /// uniquely identifies the def the fingerprint covers (clones share the Arc;
    /// distinct defs have distinct Arcs). Holds a strong `Arc` clone so the
    /// pointer can never free+reuse under a stale entry — no invalidation needed.
    pub(crate) func_def_fp_cache: rustc_hash::FxHashMap<usize, (Arc<FunctionDef>, u64)>,
    /// Sound multi-*function* resolution cache — the function-dispatch analogue of
    /// `multi_resolve_cache`. For a multi sub whose dispatch is purely type+arity
    /// based (no `where` / literal / subset / `:D`/`:U` smiley / coercion
    /// candidate), the winning candidate is a function of `(package, name,
    /// positional arg types)`, so it is cached here. Keyed on
    /// `(package_sym, name_sym, arg-type-keys)`. Cleared with the other dispatch
    /// caches on any registry change.
    #[allow(clippy::type_complexity)]
    pub(crate) func_multi_resolve_cache:
        rustc_hash::FxHashMap<(Symbol, Symbol, Vec<Symbol>), Option<Arc<FunctionDef>>>,
    /// Memoized `(package, name) -> is this multi sub's dispatch type+arity
    /// deterministic` (i.e. cacheable in `func_multi_resolve_cache`). The
    /// function analogue of `multi_type_cacheable`.
    pub(crate) func_multi_type_cacheable: rustc_hash::FxHashMap<(Symbol, Symbol), bool>,
    /// Names of classes the user declared with a `class`/`role`/`grammar`/`enum`
    /// statement (`register_class_decl`). For such a class the collected public-
    /// attribute list is authoritative: a `.name` accessor resolves ONLY for a
    /// declared public `has $.name`; an undeclared name (e.g. an unknown named arg
    /// `.new` accepted and stored) falls through to X::Method::NotFound (Rakudo:
    /// `class C {}; C.new(x=>3).x` dies). Native/built-in objects (Parameter,
    /// Signature, exception types, ...) are NOT here — their attributes live only
    /// in the stored map and are not collected — so the accessor fallback still
    /// reads them.
    pub(crate) user_declared_classes: std::collections::HashSet<String>,
    pub(crate) block_declared_vars: Vec<HashSet<String>>,
    pub(crate) loop_local_vars: Vec<HashSet<String>>,
    pub(crate) loop_local_saved_env: Vec<HashMap<String, Value>>,
    pub(crate) loop_cond_active: bool,
    pub(crate) outer_scope_locals: Vec<Vec<Value>>,
    /// Stack of captured ENTER-phaser values for blocks whose textually-last
    /// statement is an ENTER phaser (its entry-time value becomes the block
    /// result). Pushed by `PushEnterResult` in the ENTER section and popped by
    /// `LoadEnterResult` at the end of the block body.
    pub(crate) enter_result_stack: Vec<Value>,
    pub(crate) pending_alias_bind_names: Vec<(String, String)>,
    pub(crate) otf_call_cache: HashMap<Symbol, CompiledFunction>,
    pub(crate) otf_call_cache_gen: u64,
    pub(crate) check_phaser_depth: u32,
    pub(crate) gather_for_loop_resume: Option<crate::value::ForLoopResumeState>,
    pub(crate) rw_map_topic_capture: Option<Value>,
}

/// Metadata stored per custom type created by Metamodel::Primitives.
#[derive(Debug, Clone)]
pub(crate) struct CustomTypeData {
    /// Type checking cache: list of types this type accepts.
    pub(crate) type_check_cache: Option<Vec<Value>>,
    /// Whether the type check cache is authoritative (no fallback to HOW.type_check).
    pub(crate) authoritative: bool,
    /// Whether to call HOW.accepts_type for smartmatch checks.
    pub(crate) call_accepts: bool,
    /// Whether compose_type has been called.
    pub(crate) composed: bool,
    /// Whether this type was created with :mixin flag.
    #[allow(dead_code)]
    pub(crate) is_mixin: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ContainerTypeInfo {
    pub(crate) value_type: String,
    pub(crate) key_type: Option<String>,
    pub(crate) declared_type: Option<String>,
}

/// Compiled bytecode for a subset `where` predicate (the predicate body plus any
/// nested compiled functions), shared via `Arc` so a single compilation is
/// reused across every type check. Keyed by subset name in
/// `Interpreter::subset_predicate_cache`.
type SubsetPredicateCompiled = Arc<(
    crate::opcode::CompiledCode,
    HashMap<String, crate::opcode::CompiledFunction>,
)>;

/// Read a value's container type metadata. Array/Hash/Set/Bag/Mix carry it
/// embedded in their backing data struct (travels across copy-on-write);
/// `Instance` values look it up in the shared `instance_type_metadata` side
/// table by id. This free function is the single implementation shared by
/// `Interpreter::container_type_metadata` and the VM's peer-handle native read
/// (CP-3 Track 1: removes the interpreter bounce for `Instance` type-meta
/// reads). It touches no `env`, so neither caller needs an env loan.
pub(crate) fn container_type_metadata_with(
    value: &Value,
    instance_meta: &Arc<RwLock<HashMap<u64, ContainerTypeInfo>>>,
) -> Option<ContainerTypeInfo> {
    // Embedded-metadata readers for Set/Bag/Mix (mirrors `hashdata_type_info`).
    macro_rules! embedded_type_info {
        ($data:ident) => {
            if $data.has_type_meta() {
                Some(ContainerTypeInfo {
                    value_type: $data.value_type.clone().unwrap_or_default(),
                    key_type: $data.key_type.clone(),
                    declared_type: $data.declared_type.clone(),
                })
            } else {
                None
            }
        };
    }
    match value {
        Value::Array(items, ..) => embedded_type_info!(items),
        Value::Mix(items, _) => embedded_type_info!(items),
        Value::Set(items, _) => embedded_type_info!(items),
        Value::Bag(items, _) => embedded_type_info!(items),
        Value::Hash(items) => Interpreter::hashdata_type_info(items),
        Value::Instance { id, .. } => instance_meta.read().unwrap().get(id).cloned(),
        Value::Mixin(inner, _) => container_type_metadata_with(inner, instance_meta),
        _ => None,
    }
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
    HashMap<Symbol, Arc<FunctionDef>>,
    HashMap<Symbol, Arc<FunctionDef>>,
    HashMap<Symbol, Vec<Arc<FunctionDef>>>,
    HashSet<String>,
    HashSet<String>,
    HashSet<Symbol>,
);

pub(crate) type ImportScopeSnapshot = (
    HashSet<Symbol>,
    HashSet<String>,
    NewlineMode,
    bool,
    bool,
    bool,
);

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

/// Immutable process-constant magic/dynamic variables hoisted into the shared
/// env base tier (see `Interpreter::new`). These hold the same value for the
/// whole process and are never reassigned/removed by normal programs, so they
/// need not live in every per-frame env overlay (docs/vm-dual-store.md 4c).
const IMMUTABLE_BASE_DYNAMICS: &[&str] = &[
    "*PID",
    "*TZ",
    "*INIT-INSTANT",
    "$*VM",
    "*VM",
    "?VM",
    "*PERL",
    "?PERL",
    "*RAKU",
    "?RAKU",
    "*KERNEL",
    "?KERNEL",
    "*DISTRO",
    "?DISTRO",
    "$*EXECUTABLE",
    "*EXECUTABLE",
    "$*EXECUTABLE-NAME",
    "*EXECUTABLE-NAME",
    "$*SPEC",
    "*SPEC",
];

#[cfg(test)]
mod tests {
    use super::Interpreter;
    use crate::ast::{Expr, Stmt};
    use crate::env::Env;
    use crate::opcode::{CompiledCode, OpCode};
    use crate::symbol::Symbol;
    use crate::value::{SubData, Value};
    use std::fs;
    use std::sync::Arc;
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
    fn last_value_from_expression() {
        use crate::value::Value;
        let mut interp = Interpreter::new();
        interp.run("3 + 4").unwrap();
        assert_eq!(interp.last_value, Some(Value::Int(7)));
    }

    #[test]
    fn last_value_none_for_say() {
        let mut interp = Interpreter::new();
        interp.run("say 42").unwrap();
        // say is a statement (Stmt::Say), not an expression, so no last_value
        // The REPL uses output detection instead for say/print
        assert!(interp.last_value.is_none());
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
        // Needs a larger stack: nested module loading runs each module body in a
        // fresh on-stack VM that owns a full `Interpreter` by value (see
        // `run_block_raw`), so the recursive A->B->A load chain is stack-heavy in
        // debug builds. Same precedent as `is_run_honors_compiler_include_paths`.
        std::thread::Builder::new()
            .stack_size(16 * 1024 * 1024)
            .spawn(|| {
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
            })
            .unwrap()
            .join()
            .unwrap();
    }

    #[test]
    fn is_run_honors_compiler_include_paths() {
        // Needs a larger stack: is_run loads Test::Util which has a deep call chain.
        let result = std::thread::Builder::new()
            .stack_size(16 * 1024 * 1024)
            .spawn(|| {
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
            })
            .unwrap()
            .join();
        result.unwrap();
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

    #[test]
    fn test_more_tests_arg_emits_plan() {
        let mut interp = Interpreter::new();
        let output = interp
            .run("use Test::More tests => 1; is 1, 1, 'one';")
            .unwrap();
        assert!(output.starts_with("1..1\n"));
        assert!(output.contains("ok 1 - one"));
    }

    #[test]
    fn forward_decl_uses_later_top_level_definition() {
        let mut interp = Interpreter::new();
        let output = interp
            .run("sub foo($a, $b); say foo(1, 2); sub foo($a, $b) { $a + $b }")
            .unwrap();
        assert_eq!(output, "3\n");
    }

    #[test]
    fn protect_block_cache_tracks_only_captured_lexicals() {
        let mut env = Env::new();
        env.insert("used".to_string(), Value::Int(1));
        env.insert("unused".to_string(), Value::Int(2));
        env.insert("$target".to_string(), Value::Int(0));
        env.insert("@noise".to_string(), Value::array(vec![Value::Int(3)]));

        let mut compiled = CompiledCode::new();
        compiled.constants = vec![
            Value::str("$target".to_string()),
            Value::str("@noise".to_string()),
            Value::str("$unused".to_string()),
        ];
        compiled.locals = vec![
            "used".to_string(),
            "@noise".to_string(),
            "$temp".to_string(),
        ];
        compiled.simple_locals = vec![true, false, true];
        compiled.ops = vec![
            OpCode::GetGlobal(0),
            OpCode::GetArrayVar(1),
            OpCode::SetGlobal(0),
            OpCode::SetLocal(2),
        ];

        let block = Arc::new(SubData {
            package: Symbol::intern("GLOBAL"),
            name: Symbol::intern("__protect_test__"),
            params: vec![],
            param_defs: vec![],
            body: vec![Stmt::Expr(Expr::Literal(Value::Int(0)))],
            is_rw: false,
            is_raw: false,
            env,
            assumed_positional: vec![],
            assumed_named: std::collections::HashMap::new(),
            id: 1,
            empty_sig: false,
            is_bare_block: false,
            compiled_code: Some(Arc::new(compiled)),
            deprecated_message: None,
            source_line: None,
            source_file: None,
            owned_captures: Vec::new(),
            upvalues: Vec::new(),
        });

        let mut interp = Interpreter::new();
        let (_, _, captured_bindings, _, captured_names) =
            interp.get_or_compile_protect_block_with_slots(&block);

        assert_eq!(
            captured_bindings.as_ref(),
            &vec![(0, "used".to_string()), (1, "@noise".to_string())]
        );
        assert_eq!(
            captured_names.as_ref(),
            &vec![
                "used".to_string(),
                "@noise".to_string(),
                "$target".to_string(),
            ]
        );
    }
}
