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
mod builtins_operators;
pub(crate) mod builtins_reduce;
mod builtins_string;
mod builtins_system;
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
mod ops;
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
    pub(crate) method_resolve_cache: HashMap<(Symbol, Symbol), crate::vm::MethodResolveEntry>,
    #[allow(clippy::type_complexity)]
    pub(crate) last_method_resolve: Option<(Symbol, Symbol, String, Arc<MethodDef>)>,
    pub(crate) fast_method_cache: HashMap<(Symbol, Symbol), crate::vm::FastMethodCacheEntry>,
    /// Sound multi-method resolution cache (§B): for a multi whose dispatch is
    /// purely type+arity based (no `where` / literal / subset / `:D`/`:U` smiley /
    /// coercion candidate), the resolved candidate is a function of the receiver
    /// class + method + the runtime types of the positional args, so it is cached
    /// here keyed on `(class, method, arg-type-keys)`. Cleared with the other
    /// method caches when the registry changes.
    #[allow(clippy::type_complexity)]
    pub(crate) multi_resolve_cache:
        HashMap<(Symbol, Symbol, Vec<Symbol>), Option<(String, Arc<MethodDef>)>>,
    /// Memoized `(class, method) -> is this multi's dispatch type+arity deterministic`
    /// (i.e. cacheable in `multi_resolve_cache`). Computed once by scanning the MRO
    /// candidates for value-dependent constraints.
    pub(crate) multi_type_cacheable: HashMap<(Symbol, Symbol), bool>,
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

impl Interpreter {
    /// Take any pending regex security error from the thread-local store.
    pub(crate) fn take_pending_regex_error() -> Option<RuntimeError> {
        // Delegate to the regex_parse module's thread-local error store
        regex_parse::PENDING_REGEX_ERROR.with(|e| e.borrow_mut().take())
    }

    pub fn new() -> Self {
        let mut env = HashMap::new();
        env.insert("*PID".to_string(), Value::Int(current_process_id()));
        env.insert("*TZ".to_string(), Value::Int(local_timezone_offset_secs()));
        env.insert("@*ARGS".to_string(), Value::real_array(Vec::new()));
        env.insert("*INIT-INSTANT".to_string(), Value::make_instant_now());
        // Populate %*ENV with all OS environment variables so that
        // %*ENV.keys, %*ENV.elems, and copying %*ENV work correctly.
        {
            let mut env_hash = HashMap::new();
            #[cfg(not(target_family = "wasm"))]
            for (key, value) in std::env::vars() {
                env_hash.insert(key, builtins_collection::builtin_val(&[Value::str(value)]));
            }
            env.insert("%*ENV".to_string(), Value::Hash(Value::hash_arc(env_hash)));
        }
        env.insert(
            "*SCHEDULER".to_string(),
            Value::make_instance(Symbol::intern("ThreadPoolScheduler"), HashMap::new()),
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
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
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
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "IterationBuffer".to_string(),
            ClassDef {
                parents: vec!["Any".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec![
                    "IterationBuffer".to_string(),
                    "Any".to_string(),
                    "Mu".to_string(),
                ],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
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
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Promise::Vow".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["keep", "break"].iter().map(|s| s.to_string()).collect(),
                mro: vec!["Promise::Vow".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
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
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Collation".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Collation".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Thread".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["finish", "id"].iter().map(|s| s.to_string()).collect(),
                mro: vec!["Thread".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
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
                    "encode",
                    "decode",
                    "repeated",
                    "do",
                    "reverse",
                    "split",
                    "interval",
                    "tail",
                    "delayed",
                    "min",
                    "minmax",
                    "collate",
                    "lines",
                    "merge",
                    "unique",
                    "on-close",
                    "classify",
                    "categorize",
                    "Channel",
                    "Supply",
                    "Promise",
                    "schedule-on",
                    "native-descriptor",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["Supply".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
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
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
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
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Blob".to_string(),
            ClassDef {
                parents: vec!["Any".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Blob".to_string(), "Any".to_string(), "Mu".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Buf".to_string(),
            ClassDef {
                parents: vec!["Blob".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec![
                    "Buf".to_string(),
                    "Blob".to_string(),
                    "Any".to_string(),
                    "Mu".to_string(),
                ],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Supplier".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "emit",
                    "done",
                    "quit",
                    "Supply",
                    "__mutsu_register_close_phaser",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["Supplier".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Supplier::Preserving".to_string(),
            ClassDef {
                parents: vec!["Supplier".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Supplier::Preserving".to_string(), "Supplier".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
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
                    "w",
                    "pid",
                    "stdout",
                    "stderr",
                    "Supply",
                    "bind-stdin",
                    "bind-stdout",
                    "bind-stderr",
                    "kill",
                    "write",
                    "close-stdin",
                    "ready",
                    "print",
                    "say",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["Proc::Async".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Proc".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "exitcode", "signal", "command", "pid", "err", "out", "in", "Numeric", "Int",
                    "Bool", "Str", "gist", "spawn", "shell", "run",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["Proc".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Tap".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["cancel", "close", "socket-port", "socket-host"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["Tap".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
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
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "ThreadPoolScheduler".to_string(),
            ClassDef {
                parents: vec!["Scheduler".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["cue", "uncaught_handler"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["ThreadPoolScheduler".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "CurrentThreadScheduler".to_string(),
            ClassDef {
                parents: vec!["Scheduler".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["cue", "uncaught_handler"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["CurrentThreadScheduler".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "FakeScheduler".to_string(),
            ClassDef {
                parents: vec!["Scheduler".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["cue", "progress-by", "time"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["FakeScheduler".to_string(), "Scheduler".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
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
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
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
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Lock::Async".to_string(),
            ClassDef {
                parents: vec!["Lock".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Lock::Async".to_string(), "Lock".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
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
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Semaphore".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["acquire", "try_acquire", "release"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["Semaphore".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
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
                    "Numeric",
                    "Real",
                    "Int",
                    "Rat",
                    "Num",
                    "FatRat",
                    "basename",
                    "dirname",
                    "cleanup",
                    "parts",
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
                    "rw",
                    "rwx",
                    "mode",
                    "s",
                    "z",
                    "created",
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
                    "symlink",
                    "starts-with",
                    "watch",
                    "succ",
                    "pred",
                    "CWD",
                    "SPEC",
                    "link",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["IO::Path".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "IO::Handle".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "path",
                    "IO",
                    "Str",
                    "gist",
                    "DESTROY",
                    "close",
                    "get",
                    "getc",
                    "readchars",
                    "lines",
                    "words",
                    "read",
                    "write",
                    "print",
                    "say",
                    "put",
                    "flush",
                    "seek",
                    "tell",
                    "eof",
                    "encoding",
                    "opened",
                    "slurp",
                    "out-buffer",
                    "Supply",
                    "open",
                    "nl-out",
                    "nl-in",
                    "print-nl",
                    "native-descriptor",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["IO::Handle".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
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
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "CompUnit::Repository::FileSystem".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["CompUnit::Repository::FileSystem".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "IO::Pipe".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["slurp", "Str", "gist", "print", "close"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["IO::Pipe".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "IO::Socket::INET".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "close",
                    "getpeername",
                    "accept",
                    "localport",
                    "print",
                    "say",
                    "put",
                    "write",
                    "recv",
                    "read",
                    "get",
                    "lines",
                    "nl-in",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["IO::Socket::INET".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "IO::Socket::Async".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "close",
                    "write",
                    "print",
                    "Supply",
                    "socket-port",
                    "peer-port",
                    "socket-host",
                    "peer-host",
                    "print-to",
                    "write-to",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["IO::Socket::Async".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "IO::Socket::Async::Listener".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["tap", "act"].iter().map(|s| s.to_string()).collect(),
                mro: vec!["IO::Socket::Async::Listener".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
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
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
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
                    "KERNELnames",
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
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Kernel".to_string(),
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
                    "release",
                    "hardware",
                    "arch",
                    "bits",
                    "hostname",
                    "signals",
                    "signal",
                    "cpu-cores",
                    "endian",
                    "gist",
                    "raku",
                    "Str",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["Kernel".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "VM".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "name",
                    "auth",
                    "version",
                    "precomp-ext",
                    "precomp-target",
                    "request-garbage-collection",
                    "gist",
                    "Str",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["VM".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
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
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
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
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
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
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Encoding::Decoder".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "decode-chars",
                    "add-bytes",
                    "consume-all-chars",
                    "consume-available-chars",
                    "bytes-available",
                    "is-empty",
                    "set-line-separators",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["Encoding::Decoder".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
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
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
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
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Pod::Block::Code".to_string(),
            ClassDef {
                parents: vec!["Pod::Block".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Pod::Block::Code".to_string(), "Pod::Block".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Pod::FormattingCode".to_string(),
            ClassDef {
                parents: vec!["Pod::Block".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Pod::FormattingCode".to_string(), "Pod::Block".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
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
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Pod::Block::Para".to_string(),
            ClassDef {
                parents: vec!["Pod::Block".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Pod::Block::Para".to_string(), "Pod::Block".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Pod::Block::Named".to_string(),
            ClassDef {
                parents: vec!["Pod::Block".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Pod::Block::Named".to_string(), "Pod::Block".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Pod::Heading".to_string(),
            ClassDef {
                parents: vec!["Pod::Block".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Pod::Heading".to_string(), "Pod::Block".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
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
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Pod::Config".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Pod::Config".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Pod::Item".to_string(),
            ClassDef {
                parents: vec!["Pod::Block".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Pod::Item".to_string(), "Pod::Block".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Exception".to_string(),
            ClassDef {
                parents: vec![],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Exception".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
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
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "X::TypeCheck".to_string(),
            ClassDef {
                parents: vec!["Exception".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["X::TypeCheck".to_string(), "Exception".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "X::TypeCheck::Binding".to_string(),
            ClassDef {
                parents: vec!["X::TypeCheck".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec![
                    "X::TypeCheck::Binding".to_string(),
                    "X::TypeCheck".to_string(),
                    "Exception".to_string(),
                ],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
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
                    "X::TypeCheck".to_string(),
                    "Exception".to_string(),
                ],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "X::Parameter".to_string(),
            ClassDef {
                parents: vec!["Exception".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["X::Parameter".to_string(), "Exception".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "X::Parameter::InvalidConcreteness".to_string(),
            ClassDef {
                parents: vec!["X::Parameter".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec![
                    "X::Parameter::InvalidConcreteness".to_string(),
                    "X::Parameter".to_string(),
                    "Exception".to_string(),
                ],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "X::Supply::Combinator".to_string(),
            ClassDef {
                parents: vec!["Exception".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["X::Supply::Combinator".to_string(), "Exception".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "X::TypeCheck::Argument".to_string(),
            ClassDef {
                parents: vec!["X::TypeCheck".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec![
                    "X::TypeCheck::Argument".to_string(),
                    "X::TypeCheck".to_string(),
                    "Exception".to_string(),
                ],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "X::TypeCheck::Assignment".to_string(),
            ClassDef {
                parents: vec!["X::TypeCheck".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec![
                    "X::TypeCheck::Assignment".to_string(),
                    "X::TypeCheck".to_string(),
                    "Exception".to_string(),
                ],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "X::Numeric::Real".to_string(),
            ClassDef {
                parents: vec!["Exception".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["X::Numeric::Real".to_string(), "Exception".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "X::TypeCheck::Return".to_string(),
            ClassDef {
                parents: vec!["Exception".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["X::TypeCheck::Return".to_string(), "Exception".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "X::Coerce::Impossible".to_string(),
            ClassDef {
                parents: vec!["Exception".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["X::Coerce::Impossible".to_string(), "Exception".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );

        // Register additional X:: exception classes using a helper closure
        // to reduce boilerplate.
        let mut register_x = |name: &str, parent: &str| {
            let mut mro = vec![name.to_string()];
            // Walk up through existing classes to build full MRO
            let mut cur = parent.to_string();
            loop {
                mro.push(cur.clone());
                if let Some(cls) = classes.get(&cur)
                    && let Some(p) = cls.parents.first()
                    && p != &cur
                {
                    cur = p.clone();
                    continue;
                }
                break;
            }
            if !mro.contains(&"Exception".to_string()) {
                mro.push("Exception".to_string());
            }
            classes.insert(
                name.to_string(),
                ClassDef {
                    parents: vec![parent.to_string()],
                    attributes: Vec::new(),
                    methods: HashMap::new(),
                    native_methods: HashSet::new(),
                    mro,
                    attribute_types: HashMap::new(),
                    attribute_smileys: HashMap::new(),
                    attribute_built: HashMap::new(),
                    wildcard_handles: Vec::new(),
                    alias_attributes: HashSet::new(),
                    class_level_attrs: HashMap::new(),
                },
            );
        };

        // X::Comp hierarchy (compile-time errors)
        register_x("X::Comp", "Exception");
        register_x("X::Comp::Group", "X::Comp");
        register_x("X::Comp::AdHoc", "X::Comp");
        register_x("X::Comp::NYI", "X::Comp");
        register_x("X::Composition::NotComposable", "Exception");
        register_x("X::Value", "Exception");
        register_x("X::Value::Dynamic", "X::Value");

        // X::Syntax hierarchy (syntax errors, subtypes of X::Comp)
        register_x("X::Syntax", "X::Comp");
        register_x("X::Syntax::Confused", "X::Syntax");
        register_x("X::Syntax::Extension::Null", "X::Syntax");
        register_x("X::Syntax::Missing", "X::Syntax");
        register_x("X::Syntax::VirtualCall", "X::Syntax");
        register_x("X::Syntax::NegatedPair", "X::Syntax");
        register_x("X::Syntax::Malformed", "X::Syntax");
        register_x("X::Syntax::Variable::Numeric", "X::Syntax");
        register_x("X::Syntax::Variable::Initializer", "X::Syntax");
        register_x("X::Syntax::Variable::IndirectDeclaration", "X::Syntax");
        register_x("X::Syntax::Variable::ConflictingTypes", "X::Syntax");
        register_x("X::Syntax::Number::LiteralType", "X::Syntax");
        register_x("X::Syntax::Regex::Adverb", "X::Syntax");
        register_x("X::Backslash::UnrecognizedSequence", "X::Backslash");
        register_x("X::Syntax::Regex::SolitaryQuantifier", "X::Syntax");
        register_x("X::Syntax::Regex::NullRegex", "X::Syntax");
        register_x("X::Syntax::Regex::NonQuantifiable", "X::Syntax");
        register_x("X::Syntax::Regex::QuantifierValue", "X::Syntax");
        register_x("X::Syntax::Term::MissingInitializer", "X::Syntax");
        register_x("X::Syntax::WithoutElse", "X::Syntax");
        register_x("X::Syntax::UnlessElse", "X::Syntax");
        register_x("X::Syntax::Reserved", "X::Syntax");
        register_x("X::Syntax::KeywordAsFunction", "X::Syntax");
        register_x("X::Syntax::Name::Null", "X::Syntax");
        register_x("X::Syntax::Signature", "X::Syntax");
        register_x(
            "X::Syntax::Signature::InvocantMarker",
            "X::Syntax::Signature",
        );

        // X::Obsolete (compile-time, subtype of X::Comp)
        register_x("X::Obsolete", "X::Comp");

        // X::Undeclared hierarchy
        register_x("X::Undeclared", "X::Comp");
        register_x("X::Undeclared::Symbols", "X::Comp");

        // X::Redeclaration
        register_x("X::Redeclaration", "X::Comp");

        // X::Assignment::RO
        register_x("X::Assignment::RO", "Exception");

        // X::Str::Numeric
        register_x("X::Str::Numeric", "Exception");

        // X::Str::Match::x — invalid :x argument to .subst / s///
        register_x("X::Str::Match::x", "Exception");

        // X::Multi::NoMatch / X::Multi::Ambiguous
        register_x("X::Multi::NoMatch", "Exception");
        register_x("X::Multi::Ambiguous", "Exception");

        // X::OutOfRange
        register_x("X::OutOfRange", "Exception");

        // X::Method::NotFound
        register_x("X::Method::NotFound", "Exception");

        // X::Immutable
        register_x("X::Immutable", "Exception");

        // X::Cannot::Lazy
        register_x("X::Cannot::Lazy", "Exception");
        register_x("X::Cannot::Capture", "Exception");

        // X::Match::Bool
        register_x("X::Match::Bool", "Exception");

        // X::Adverb
        register_x("X::Adverb", "Exception");

        // X::ControlFlow::Return
        register_x("X::ControlFlow::Return", "Exception");

        // X::Bind
        register_x("X::Bind", "Exception");
        register_x("X::Bind::NativeType", "X::Bind");

        // X::StubCode
        register_x("X::StubCode", "Exception");

        // X::Signature::Placeholder
        register_x("X::Signature::Placeholder", "Exception");

        // X::Signature::NameClash
        register_x("X::Signature::NameClash", "X::Comp");

        // X::SecurityPolicy
        register_x("X::SecurityPolicy", "Exception");

        // X::NotEnoughDimensions
        register_x("X::NotEnoughDimensions", "Exception");

        // X::IO::Closed
        register_x("X::IO::Closed", "Exception");

        // X::Role subtypes
        register_x("X::Role::Parametric::NoSuchCandidate", "Exception");
        register_x("X::Role::Unimplemented::Multi", "Exception");

        // X::NYI
        register_x("X::NYI", "Exception");

        // X::Method::Private::Permission
        register_x("X::Method::Private::Permission", "Exception");
        register_x("X::Method::Private::Unqualified", "Exception");
        register_x("X::Routine::Unwrap", "Exception");
        register_x("X::Str::Trans::InvalidArg", "Exception");
        register_x("X::Str::Trans::IllegalKey", "Exception");

        // X::ParametricConstant
        register_x("X::ParametricConstant", "Exception");

        // X::UnitScope::Invalid
        register_x("X::UnitScope::Invalid", "Exception");

        // X::Promise / X::Channel exceptions
        register_x("X::Promise::Vowed", "Exception");
        register_x("X::Promise::Resolved", "Exception");
        register_x("X::Promise::CauseOnlyValidOnBroken", "Exception");
        register_x("X::Channel::SendOnClosed", "Exception");
        register_x("X::Channel::ReceiveOnClosed", "Exception");

        // X::Comp::AdHoc does both X::Comp and X::AdHoc in rakudo (it is the
        // compile-time wrapper around an ad-hoc die), so `$e ~~ X::AdHoc` must
        // also be True. register_x only threads a single parent, so splice
        // X::AdHoc into the MRO here (after the closure's borrow of `classes`
        // has ended).
        if let Some(def) = classes.get_mut("X::Comp::AdHoc") {
            if !def.parents.iter().any(|p| p == "X::AdHoc") {
                def.parents.push("X::AdHoc".to_string());
            }
            if !def.mro.iter().any(|m| m == "X::AdHoc") {
                let insert_at = def.mro.len().saturating_sub(1);
                def.mro.insert(insert_at, "X::AdHoc".to_string());
            }
        }

        let mut interpreter = Self {
            env: Env::from(env),
            output_sink: Arc::new(RwLock::new(OutputSink::new())),
            warn_output: String::new(),
            warn_suppression_depth: 0,
            tap: TapState::default(),
            halted: false,
            exit_code: 0,
            nested_mode: false,
            native_call_specs: HashMap::new(),
            operator_assoc: HashMap::new(),
            imported_operator_names: HashSet::new(),
            lib_paths: Vec::new(),
            io_handles: Arc::new(RwLock::new(io_handles::IoHandleTable {
                map: HashMap::new(),
                next_id: 1,
            })),
            program_path: None,
            current_package: Arc::new(RwLock::new("GLOBAL".to_string())),
            routine_stack: Vec::new(),
            callframe_stack: Vec::new(),
            method_class_stack: Vec::new(),
            constructing_class: None,
            pending_call_arg_sources: None,
            test_pending_callsite_line: None,
            control_handler_depth: 0,
            test_assertion_line_stack: Vec::new(),
            block_stack: Vec::new(),
            doc_comments: HashMap::new(),
            doc_comment_list: Vec::new(),
            why_cache: HashMap::new(),
            type_metadata: HashMap::new(),
            when_matched: false,
            gather_items: Vec::new(),
            gather_take_limits: Vec::new(),
            block_scope_depth: 0,
            registry: {
                // Built-in class definitions (PR-A slice 3: `classes` now lives in the
                // shared Registry instead of an Interpreter field).
                let mut registry = Registry {
                    classes,
                    ..Registry::default()
                };
                // Built-in class -> composed-role seeds (PR-A slice 2: class metadata
                // now lives in the shared Registry instead of an Interpreter field).
                let ccr = &mut registry.class_composed_roles;
                ccr.insert(
                    "CompUnit::Repository::FileSystem".to_string(),
                    vec!["CompUnit::Repository".to_string()],
                );
                // Built-in type role composition
                ccr.insert(
                    "Int".to_string(),
                    vec!["Real".to_string(), "Numeric".to_string()],
                );
                ccr.insert(
                    "Num".to_string(),
                    vec!["Real".to_string(), "Numeric".to_string()],
                );
                ccr.insert(
                    "Rat".to_string(),
                    vec![
                        "Rational[Int,Int]".to_string(),
                        "Real".to_string(),
                        "Numeric".to_string(),
                    ],
                );
                ccr.insert(
                    "FatRat".to_string(),
                    vec![
                        "Rational[Int,Int]".to_string(),
                        "Real".to_string(),
                        "Numeric".to_string(),
                    ],
                );
                ccr.insert("Complex".to_string(), vec!["Numeric".to_string()]);
                ccr.insert("Str".to_string(), vec!["Stringy".to_string()]);
                // Built-in role definitions (PR-A slice 4: roles now live in the
                // shared Registry instead of an Interpreter field).
                registry.roles = {
                    let mut roles = HashMap::new();
                    roles.insert(
                        "Encoding".to_string(),
                        RoleDef {
                            attributes: Vec::new(),
                            methods: HashMap::new(),
                            is_stub_role: false,
                            is_hidden: false,
                            is_rw: false,
                            captured_env: None,
                            wildcard_handles: Vec::new(),
                            role_id: 0,
                            attribute_conflicts: Vec::new(),
                            own_attribute_names: std::collections::HashSet::new(),
                            deferred_body_stmts: Vec::new(),
                            deferred_custom_traits: Vec::new(),
                        },
                    );
                    roles.insert(
                        "Iterator".to_string(),
                        RoleDef {
                            attributes: Vec::new(),
                            methods: HashMap::new(),
                            is_stub_role: false,
                            is_hidden: false,
                            is_rw: false,
                            captured_env: None,
                            wildcard_handles: Vec::new(),
                            role_id: 0,
                            attribute_conflicts: Vec::new(),
                            own_attribute_names: std::collections::HashSet::new(),
                            deferred_body_stmts: Vec::new(),
                            deferred_custom_traits: Vec::new(),
                        },
                    );
                    roles.insert(
                        "PredictiveIterator".to_string(),
                        RoleDef {
                            attributes: Vec::new(),
                            methods: HashMap::new(),
                            is_stub_role: false,
                            is_hidden: false,
                            is_rw: false,
                            captured_env: None,
                            wildcard_handles: Vec::new(),
                            role_id: 0,
                            attribute_conflicts: Vec::new(),
                            own_attribute_names: std::collections::HashSet::new(),
                            deferred_body_stmts: Vec::new(),
                            deferred_custom_traits: Vec::new(),
                        },
                    );
                    roles.insert(
                        "Iterable".to_string(),
                        RoleDef {
                            attributes: Vec::new(),
                            methods: HashMap::new(),
                            is_stub_role: false,
                            is_hidden: false,
                            is_rw: false,
                            captured_env: None,
                            wildcard_handles: Vec::new(),
                            role_id: 0,
                            attribute_conflicts: Vec::new(),
                            own_attribute_names: std::collections::HashSet::new(),
                            deferred_body_stmts: Vec::new(),
                            deferred_custom_traits: Vec::new(),
                        },
                    );
                    roles.insert(
                        "X::Control".to_string(),
                        RoleDef {
                            attributes: Vec::new(),
                            methods: HashMap::new(),
                            is_stub_role: false,
                            is_hidden: false,
                            is_rw: false,
                            captured_env: None,
                            wildcard_handles: Vec::new(),
                            role_id: 0,
                            attribute_conflicts: Vec::new(),
                            own_attribute_names: std::collections::HashSet::new(),
                            deferred_body_stmts: Vec::new(),
                            deferred_custom_traits: Vec::new(),
                        },
                    );
                    // CompUnit::Repository role with required stub methods
                    {
                        let stub_body = vec![Stmt::Expr(Expr::Call {
                            name: Symbol::intern("__mutsu_stub_die"),
                            args: vec![],
                        })];
                        let stub_method = |body: Vec<Stmt>| MethodDef {
                            params: Vec::new(),
                            param_defs: Vec::new(),
                            body: std::sync::Arc::new(body),
                            is_rw: false,
                            is_private: false,
                            is_multi: false,
                            is_my: false,
                            role_origin: None,
                            original_role: None,
                            return_type: None,
                            compiled_code: None,
                            delegation: None,
                            is_default: false,
                            deprecated_message: None,
                            is_submethod: false,
                        };
                        let mut methods = HashMap::new();
                        for name in ["id", "need", "load", "loaded"] {
                            methods.insert(name.to_string(), vec![stub_method(stub_body.clone())]);
                        }
                        roles.insert(
                            "CompUnit::Repository".to_string(),
                            RoleDef {
                                attributes: Vec::new(),
                                methods,
                                is_stub_role: false,
                                is_hidden: false,
                                is_rw: false,
                                captured_env: None,
                                wildcard_handles: Vec::new(),
                                role_id: 0,
                                attribute_conflicts: Vec::new(),
                                own_attribute_names: std::collections::HashSet::new(),
                                deferred_body_stmts: Vec::new(),
                                deferred_custom_traits: Vec::new(),
                            },
                        );
                    }
                    roles
                };
                Arc::new(RwLock::new(registry))
            },
            proto_dispatch_stack: Vec::new(),
            proto_method_skip: None,
            pending_dispatch_error: None,
            end_phasers: Vec::new(),
            end_phaser_sites: HashSet::new(),
            chroot_root: None,
            loaded_modules: HashSet::new(),
            need_hidden_classes: HashSet::new(),
            cur_repo: Box::new(CurRepoState::default()),
            package_stash_hidden: HashSet::new(),
            chain_declared_packages: HashSet::new(),
            module_packages: HashMap::new(),
            closure_env_overrides: HashMap::new(),
            predictive_seq_iters: HashMap::new(),
            protect_block_cache: HashMap::new(),
            subset_predicate_cache: HashMap::new(),
            private_zeroarg_method_cache: HashMap::new(),
            module_load_stack: Vec::new(),
            current_distribution: None,
            package_distributions: HashMap::new(),
            exported_subs: HashMap::new(),
            exported_sub_values: HashMap::new(),
            exported_vars: HashMap::new(),
            unit_module_exported_subs: HashMap::new(),
            unit_module_loading_stack: Vec::new(),
            suppress_exports: false,
            in_lvalue_assignment: false,
            in_does_rhs: false,
            trait_mod_writeback_key: None,
            trait_mod_writeback_value: None,
            hash_autovivify: false,
            newline_mode: NewlineMode::Lf,
            import_scope_stack: Vec::new(),
            strict_mode: false,
            fatal_mode: false,
            our_vars: HashMap::new(),
            state_vars: HashMap::new(),
            closure_captured_state: HashMap::new(),
            once_values: HashMap::new(),
            once_scope_stack: Vec::new(),
            next_once_scope_id: 1,
            var_dynamic_flags: HashMap::new(),
            caller_env_stack: Vec::new(),
            var_bindings: HashMap::new(),
            variables_pragma: String::new(),
            attributes_pragma: String::new(),
            var_type_constraints: HashMap::new(),
            atomic_var_seen: false,
            var_defaults: HashMap::new(),
            var_hash_key_constraints: HashMap::new(),
            instance_type_metadata: Arc::new(RwLock::new(HashMap::new())),
            let_saves: Vec::new(),
            supply_emit_buffer: Vec::new(),
            supply_emit_timed_buffer: Vec::new(),
            supply_stream_consumers: Vec::new(),
            shared_vars: Arc::new(RwLock::new(HashMap::new())),
            shared_vars_active: false,
            sigilless_attrs_active: false,
            shared_vars_dirty: Arc::new(RwLock::new(HashSet::new())),
            encoding_registry: Self::builtin_encodings(),
            skip_pseudo_method_native: None,
            dispatch_ambiguous: false,
            pending_proxy_subclass_attr: None,
            multi_dispatch_stack: Vec::new(),
            method_dispatch_stack: Vec::new(),
            samewith_context_stack: Vec::new(),
            wrap_chains: HashMap::new(),
            wrap_sub_names: HashMap::new(),
            wrap_name_to_sub: HashMap::new(),
            wrap_callable_ids: HashMap::new(),
            wrap_handle_counter: 0,
            wrap_dispatch_stack: Vec::new(),
            method_wrap_chains: HashMap::new(),
            suppressed_names: HashSet::new(),
            poisoned_enum_aliases: HashMap::new(),
            enum_scope_names: vec![Vec::new()],
            my_scoped_package_items: HashSet::new(),
            lexical_class_scopes: Vec::new(),
            last_value: None,
            pending_local_updates: Vec::new(),
            readonly_vars: HashSet::new(),
            squish_iterator_meta: HashMap::new(),
            custom_type_data: HashMap::new(),
            rebless_map: HashMap::new(),
            action_made: None,
            current_grammar_actions: None,
            pending_regex_error: None,
            precomp_enabled: true,
            monkey_typing: false,

            // Merged VM execution registers (CP-3 collapse) — same defaults the
            // former `VM::new` installed.
            stack: Vec::new(),
            locals: Vec::new(),
            upvalues: Vec::new(),
            in_smartmatch_rhs: false,
            transliterate_in_smartmatch: false,
            substitution_in_smartmatch: false,
            last_topic_value: None,
            topic_save_stack: Vec::new(),
            container_ref_var: None,
            container_ref_reversed: false,
            topic_source_var: None,
            element_source: None,
            quanthash_bind_params: Vec::new(),
            for_param_restore_stack: Vec::new(),
            call_frames: Vec::new(),
            control_handlers: Vec::new(),
            current_code: 0,
            carrier_writes: None,
            method_dispatch_pure: false,
            resume_ip: None,
            bind_context: false,
            scalar_bind_context: false,
            bound_decont_active: false,
            rebind_context: false,
            constant_context: false,
            array_share_context: false,
            array_share_source: None,
            array_share_active: false,
            element_share_pending: false,
            explicit_initializer_context: false,
            vardecl_context: false,
            pending_rw_writeback_sources: Vec::new(),
            pending_caller_var_writeback: Vec::new(),
            local_bind_pairs: Vec::new(),
            otf_compile_cache: HashMap::new(),
            state_scope_id: None,
            fn_resolve_cache: HashMap::new(),
            fn_resolve_gen: 0,
            fn_resolve_cache_gen: 0,
            multi_candidates_cache: HashMap::new(),
            multi_candidates_cache_gen: 0,
            light_call_cache: HashMap::new(),
            light_call_cache_gen: 0,
            pos_light_call_cache: HashMap::new(),
            pos_light_call_cache_gen: 0,
            amp_param_shadowed_names: std::collections::HashSet::new(),
            registered_fn_fingerprints: HashMap::new(),
            prepared_fn_defs: HashMap::new(),
            method_resolve_cache: HashMap::new(),
            last_method_resolve: None,
            fast_method_cache: HashMap::new(),
            multi_resolve_cache: HashMap::new(),
            multi_type_cacheable: HashMap::new(),
            block_declared_vars: Vec::new(),
            loop_local_vars: Vec::new(),
            loop_local_saved_env: Vec::new(),
            loop_cond_active: false,
            outer_scope_locals: Vec::new(),
            enter_result_stack: Vec::new(),
            pending_alias_bind_names: Vec::new(),
            otf_call_cache: HashMap::new(),
            otf_call_cache_gen: 0,
            check_phaser_depth: 0,
            gather_for_loop_resume: None,
            rw_map_topic_capture: None,
        };
        interpreter.init_io_environment();
        // Built-in enum constants (Order/Endian/ProtocolFamily/Signal) are
        // process-wide immutables: collect them into the shared base tier
        // instead of every per-frame env overlay (docs/vm-dual-store.md 4b).
        let mut enum_base: HashMap<Symbol, Value> = HashMap::new();
        interpreter.init_order_enum(&mut enum_base);
        interpreter.init_endian_enum(&mut enum_base);
        interpreter.init_protocol_family_enum(&mut enum_base);
        interpreter.init_signal_enum(&mut enum_base);
        interpreter.init_seek_type_enum(&mut enum_base);
        // Hoist the immutable process-constant magic/dynamic vars out of every
        // per-frame env overlay into the shared base tier (docs/vm-dual-store.md
        // 4c "natural extension"). These are set once at interpreter start and
        // never reassigned/removed by normal programs; reads fall back to the
        // base tier, and a rare write is promoted into the overlay by
        // `Env::get_mut`, so semantics are preserved while the per-call deep
        // copy forks a smaller overlay. Mutable dynamics ($*OUT, $*CWD, %*ENV,
        // @*ARGS, $*SCHEDULER, $*REPO, handles, ...) intentionally stay in the
        // overlay.
        for key in IMMUTABLE_BASE_DYNAMICS {
            if let Some(v) = interpreter.env.remove(key) {
                enum_base.insert(Symbol::intern(key), v);
            }
        }
        crate::env::set_global_base(enum_base);
        interpreter.env.insert("Any".to_string(), Value::Nil);
        // Set up $*REPO as a default CompUnit::Repository::FileSystem instance
        {
            let mut attrs = HashMap::new();
            attrs.insert("prefix".to_string(), Value::str_from("."));
            attrs.insert("__mutsu_precomp_enabled".to_string(), Value::Bool(true));
            let repo =
                Value::make_instance(Symbol::intern("CompUnit::Repository::FileSystem"), attrs);
            interpreter.env.insert("*REPO".to_string(), repo);
        }
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
                name: "utf8-c8".to_string(),
                alternative_names: vec!["utf-8-c8".to_string()],
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

    pub(crate) fn unsuppress_name(&mut self, name: &str) {
        self.suppressed_names.remove(name);
    }

    /// Push a new lexical class scope frame.
    pub(crate) fn push_lexical_class_scope(&mut self) {
        self.lexical_class_scopes.push(Vec::new());
    }

    /// Pop a lexical class scope frame, suppressing all class names registered in it.
    pub(crate) fn pop_lexical_class_scope(&mut self) {
        if let Some(names) = self.lexical_class_scopes.pop() {
            for name in names {
                self.suppressed_names.insert(name);
            }
        }
    }

    /// Register a class name as lexically scoped in the current block.
    pub(crate) fn register_lexical_class(&mut self, name: String) {
        if let Some(scope) = self.lexical_class_scopes.last_mut() {
            scope.push(name);
        }
    }

    /// Mark a fully-qualified name as `my`-scoped within its parent package.
    /// Items in this set are excluded from the parent package's stash.
    pub(crate) fn mark_my_scoped_package_item(&mut self, fq_name: String) {
        self.my_scoped_package_items.insert(fq_name);
    }

    /// Check if a fully-qualified name is `my`-scoped within its parent package.
    pub(crate) fn is_my_scoped_package_item(&self, fq_name: &str) -> bool {
        self.my_scoped_package_items.contains(fq_name)
    }

    pub(crate) fn is_name_suppressed(&self, name: &str) -> bool {
        self.suppressed_names.contains(name)
    }

    /// Check if a bare enum variant name is poisoned (declared by multiple enums).
    pub(crate) fn is_poisoned_enum_alias(&self, name: &str) -> Option<&str> {
        self.poisoned_enum_aliases.get(name).map(|s| s.as_str())
    }

    /// Record a bare enum name in the current scope for poisoning detection.
    /// Only marks as poisoned if the name was already declared in the *same*
    /// scope level by a different enum.
    pub(crate) fn register_enum_bare_name(&mut self, name: &str, enum_type: &str) {
        // Check only the current scope for the same name from a different enum
        if let Some(current_scope) = self.enum_scope_names.last()
            && current_scope.iter().any(|n| n == name)
            && let Some(Value::Enum {
                enum_type: prev_type,
                ..
            }) = self.env.get(name)
            && prev_type.resolve() != enum_type
        {
            self.poisoned_enum_aliases
                .insert(name.to_string(), enum_type.to_string());
        }
        if let Some(scope) = self.enum_scope_names.last_mut() {
            scope.push(name.to_string());
        }
    }

    /// Push a new enum scope frame (called on block enter).
    pub(crate) fn push_enum_scope(&mut self) {
        self.enum_scope_names.push(Vec::new());
    }

    /// Pop an enum scope frame, removing poisoned aliases for names
    /// that were introduced in the exiting scope.
    pub(crate) fn pop_enum_scope(&mut self) {
        if let Some(names) = self.enum_scope_names.pop() {
            for name in names {
                self.poisoned_enum_aliases.remove(&name);
            }
        }
    }

    /// Resolve a suppressed nested class short name to its qualified form.
    /// For example, if `Frog` is suppressed and we are inside class `Forest`,
    /// this returns `Some("Forest::Frog")` if `Forest::Frog` is a known type.
    pub(crate) fn resolve_suppressed_type(&self, name: &str) -> Option<String> {
        if !self.suppressed_names.contains(name) {
            return None;
        }
        // Check current package
        let current_pkg = &self.current_package();
        if current_pkg != "GLOBAL" {
            let qualified = format!("{}::{}", current_pkg, name);
            if self.has_type(&qualified) {
                return Some(qualified);
            }
        }
        // Check method class stack
        for class_name in self.method_class_stack.iter().rev() {
            let qualified = format!("{}::{}", class_name, name);
            if self.has_type(&qualified) {
                return Some(qualified);
            }
        }
        // The class currently being constructed. A typed attribute's default
        // type object (`has Inner $.x` defaults to `BareWord("Inner")`) is
        // evaluated during construction with no method-class / package context,
        // yet the nested name must resolve within its owning class. This is set
        // only for the duration of attribute-default evaluation, so it does not
        // leak the suppressed name into outer lexical scopes (where raku keeps it
        // undeclared).
        if let Some(class_name) = &self.constructing_class {
            let qualified = format!("{}::{}", class_name, name);
            if self.has_type(&qualified) {
                return Some(qualified);
            }
        }
        None
    }

    pub fn set_pid(&mut self, pid: i64) {
        self.env.insert("*PID".to_string(), Value::Int(pid));
    }

    pub fn set_program_path(&mut self, path: &str) {
        self.program_path = Some(path.to_string());
        let io_path = self.make_io_path_instance(path);
        self.env.insert("*PROGRAM".to_string(), io_path);
        self.env
            .insert("*PROGRAM-NAME".to_string(), Value::str(path.to_string()));
    }

    pub fn set_args(&mut self, args: Vec<Value>) {
        self.env
            .insert("@*ARGS".to_string(), Value::real_array(args));
    }

    pub fn add_lib_path(&mut self, path: String) {
        if !path.is_empty() {
            self.lib_paths.push(path);
        }
    }

    /// Save current function/class keys for lexical import scoping.
    pub(crate) fn push_import_scope(&mut self) {
        let func_keys: HashSet<Symbol> = self.registry().functions.keys().copied().collect();
        let class_keys: HashSet<String> = self.registry().classes.keys().cloned().collect();
        self.import_scope_stack.push((
            func_keys,
            class_keys,
            self.newline_mode,
            self.strict_mode,
            self.fatal_mode,
            self.monkey_typing,
        ));
    }

    /// Restore function/class registries to the last saved snapshot,
    /// removing any entries added since the push.
    pub(crate) fn pop_import_scope(&mut self) {
        if let Some((
            func_snapshot,
            class_snapshot,
            newline_mode,
            strict_mode,
            fatal_mode,
            monkey_typing,
        )) = self.import_scope_stack.pop()
        {
            self.registry_mut()
                .functions
                .retain(|key, _| func_snapshot.contains(key));
            self.registry_mut()
                .classes
                .retain(|key, _| class_snapshot.contains(key));
            self.newline_mode = newline_mode;
            self.strict_mode = strict_mode;
            self.fatal_mode = fatal_mode;
            self.monkey_typing = monkey_typing;
            // Removing imported functions when a lexical import scope pops must
            // invalidate the name-keyed function-resolution caches: a sub that
            // was OTF-compiled and cached under its bare name while in scope
            // (otf_call_cache) would otherwise still be reachable after the
            // scope exits — e.g. `{ use Foo } EVAL('foo()')` must die, not hit
            // the stale cache (roast/S11-modules/lexical.t). Registration bumps
            // fn_resolve_gen; the matching un-registration here must too.
            self.fn_resolve_gen += 1;
        }
    }

    pub fn use_module(&mut self, module: &str) -> Result<(), RuntimeError> {
        self.use_module_with_tags(module, &[])
    }

    pub fn use_module_with_tags(
        &mut self,
        module: &str,
        tags: &[String],
    ) -> Result<(), RuntimeError> {
        if self.loaded_modules.contains(module) {
            if module == "strict" {
                self.strict_mode = true;
            } else if module == "fatal" {
                self.fatal_mode = true;
            }
            // Propagate package declarations from the already-loaded module
            // into the current chain so that chain_has_package_decl checks
            // correctly detect namespace contributions from transitive deps.
            if let Some(pkgs) = self.module_packages.get(module).cloned() {
                self.chain_declared_packages.extend(pkgs);
            }
            // When a module that declares a class/role matching its own name
            // is directly `use`d at the top level, un-hide it and its related
            // classes from the package stash. This handles the case where the
            // module was first loaded transitively by a non-contributing module.
            if self.module_load_stack.is_empty() && module.contains("::") {
                let is_contributor = {
                    let registry = self.registry();
                    registry.classes.contains_key(module) || registry.roles.contains_key(module)
                };
                if is_contributor {
                    self.package_stash_hidden.remove(module);
                }
            }
            return match self.import_module(module, tags) {
                Ok(()) => Ok(()),
                Err(err) if err.message.starts_with("No exports found for module:") => Ok(()),
                Err(err) => Err(err),
            };
        }
        if self.module_load_stack.iter().any(|m| m == module) {
            let mut chain = self.module_load_stack.clone();
            chain.push(module.to_string());
            return Err(RuntimeError::new(format!(
                "circular module dependency detected: {}",
                chain.join(" -> ")
            )));
        }
        // At the top level (no modules currently loading), save and clear
        // the chain-scoped package declarations so each top-level `use` gets
        // a fresh chain. Nested uses inherit the parent's chain.
        let is_top_level_use = self.module_load_stack.is_empty();
        let saved_chain_pkgs = if is_top_level_use {
            std::mem::take(&mut self.chain_declared_packages)
        } else {
            HashSet::new()
        };
        self.module_load_stack.push(module.to_string());
        let class_snapshot: HashSet<String> = self.registry().classes.keys().cloned().collect();
        let role_snapshot: HashSet<String> = self.registry().roles.keys().cloned().collect();
        let env_snapshot: HashSet<Symbol> = self.env.keys().copied().collect();
        let func_keys_before: HashSet<Symbol> = self.registry().functions.keys().copied().collect();

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
                    | "soft"
                    | "fatal"
                    | "oo"
                    | "class"
                    // NativeCall: the `is native(...)` trait machinery is built
                    // into the VM (see runtime/nativecall.rs); `use NativeCall`
                    // only needs to be a recognized no-op.
                    | "NativeCall"
                    // JSON::Fast / JSON::Tiny: the real distributions depend on
                    // ~50 nqp ops mutsu does not implement. Recognize them as
                    // built-in modules and provide native `to-json`/`from-json`
                    // (see runtime/json.rs, dispatched in vm_native_json.rs).
                    | "JSON::Fast"
                    | "JSON::Tiny"
            ) {
            // Track MONKEY-TYPING pragma
            if module == "MONKEY-TYPING" || module == "MONKEY" {
                self.monkey_typing = true;
            }
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
                Err(err) if err.is_unsatisfied_dependency() => Ok(()),
                Err(err) => Err(err),
            }
        } else {
            self.load_module(module)
        };

        self.module_load_stack.pop();
        if result.is_ok() {
            let module_short = if let Some((_, short)) = module.rsplit_once("::") {
                short
            } else {
                module
            };
            let class_names: Vec<String> = self.registry().classes.keys().cloned().collect();
            for class_name in &class_names {
                if class_snapshot.contains(class_name) {
                    continue;
                }
                let class_short = class_name
                    .rsplit_once("::")
                    .map(|(_, short)| short)
                    .unwrap_or(class_name.as_str());
                if class_short != module_short {
                    self.need_hidden_classes.insert(class_name.clone());
                    self.need_hidden_classes.insert(class_short.to_string());
                }
            }
            for key in self.env.keys() {
                if env_snapshot.contains(key) {
                    continue;
                }
                if key.starts_with("$")
                    || key.starts_with("@")
                    || key.starts_with("%")
                    || key.starts_with("&")
                {
                    continue;
                }
                let key_s = key.resolve();
                let key_short = key_s
                    .rsplit_once("::")
                    .map(|(_, short)| short)
                    .unwrap_or(key_s.as_str());
                if !key_short
                    .chars()
                    .next()
                    .is_some_and(|c| c.is_ascii_uppercase())
                {
                    continue;
                }
                if key_short != module_short {
                    self.need_hidden_classes.insert(key_s.clone());
                    self.need_hidden_classes.insert(key_short.to_string());
                }
            }
            // Determine if new classes/roles from this module should be
            // hidden from the parent namespace's package stash. This prevents
            // transitive dependencies from leaking into namespace stash lookups
            // (e.g. `Example2::.keys` should not show classes loaded only as
            // transitive deps of a module that doesn't contribute to the namespace).
            //
            // A module "contributes" to namespace X if either:
            //   (a) it registered a class/role matching its own FQ name (e.g.
            //       `use Example2::F` and `class Example2::F` was registered), or
            //   (b) its dependency chain includes a `package X {}` declaration
            //       (tracked in `chain_declared_packages` which is scoped to this load).
            // If neither condition holds, new classes/roles are hidden from X's stash.
            if let Some((namespace, _)) = module.rsplit_once("::") {
                let module_declares_own_class = {
                    let registry = self.registry();
                    registry.classes.contains_key(module) || registry.roles.contains_key(module)
                };
                let chain_has_package_decl = self.chain_declared_packages.contains(namespace);
                if !module_declares_own_class && !chain_has_package_decl {
                    // Hide newly registered classes/roles from the namespace stash
                    let class_names: Vec<String> =
                        self.registry().classes.keys().cloned().collect();
                    for class_name in &class_names {
                        if !class_snapshot.contains(class_name)
                            && class_name.starts_with(namespace)
                            && class_name.get(namespace.len()..namespace.len() + 2) == Some("::")
                        {
                            self.package_stash_hidden.insert(class_name.clone());
                        }
                    }
                    let role_names: Vec<String> = self.registry().roles.keys().cloned().collect();
                    for role_name in &role_names {
                        if !role_snapshot.contains(role_name)
                            && role_name.starts_with(namespace)
                            && role_name.get(namespace.len()..namespace.len() + 2) == Some("::")
                        {
                            self.package_stash_hidden.insert(role_name.clone());
                        }
                    }
                }
            }
            // Record which packages were declared during this module's chain
            // so they can be propagated when the module is re-used.
            if !self.chain_declared_packages.is_empty() {
                self.module_packages
                    .insert(module.to_string(), self.chain_declared_packages.clone());
            }
            // Restore the chain-scoped package declarations (top-level only)
            if is_top_level_use {
                self.chain_declared_packages = saved_chain_pkgs;
            }

            if module == "strict" {
                self.strict_mode = true;
            } else if module == "fatal" {
                self.fatal_mode = true;
            }
            // Remove GLOBAL:: function aliases for non-DEFAULT/non-MANDATORY
            // exports that were created by sub hoisting during module loading.
            // The hoisting registers ALL exported subs under GLOBAL:: before
            // export tag filtering can occur. We use the exported_subs table
            // to identify which functions should NOT be globally accessible.
            let requested_tags: HashSet<String> = if tags.is_empty() {
                ["DEFAULT".to_string()].into_iter().collect()
            } else {
                tags.iter().cloned().collect()
            };
            let want_all = requested_tags.contains("ALL");
            // Check exports registered under GLOBAL (for unit modules) and under module name
            let export_sources = ["GLOBAL", module];
            for source in &export_sources {
                if let Some(subs) = self.exported_subs.get(*source) {
                    for (name, symbol_tags) in subs {
                        let is_mandatory = symbol_tags.contains("MANDATORY");
                        if !want_all && !is_mandatory && symbol_tags.is_disjoint(&requested_tags) {
                            // This export should NOT be imported — remove from GLOBAL
                            let global_key = Symbol::intern(&format!("GLOBAL::{}", name));
                            if !func_keys_before.contains(&global_key) {
                                self.registry_mut().functions.remove(&global_key);
                            }
                            // Also remove multi-dispatch variants
                            let prefix = format!("GLOBAL::{}/", name);
                            let multi_keys: Vec<Symbol> = self
                                .registry()
                                .functions
                                .keys()
                                .filter(|k| {
                                    let ks = k.resolve();
                                    ks.starts_with(&prefix) && !func_keys_before.contains(k)
                                })
                                .copied()
                                .collect();
                            for mk in multi_keys {
                                self.registry_mut().functions.remove(&mk);
                            }
                        }
                    }
                }
            }

            // Remove GLOBAL:: operator sub entries (infix/prefix/postfix/circumfix)
            // that were added by sub hoisting during module loading but are NOT
            // exported. This prevents non-exported operators from leaking into
            // the caller's namespace while preserving regular function hoisting.
            let mut exported_op_names: HashSet<String> = HashSet::new();
            for source in ["GLOBAL", module] {
                if let Some(subs) = self.exported_subs.get(source) {
                    for name in subs.keys() {
                        if name.contains(":<") {
                            exported_op_names.insert(name.clone());
                        }
                    }
                }
            }
            if let Some(subs) = self.unit_module_exported_subs.get(module) {
                for name in subs.keys() {
                    if name.contains(":<") {
                        exported_op_names.insert(name.clone());
                    }
                }
            }
            let non_exported_op_globals: Vec<Symbol> = self
                .registry()
                .functions
                .keys()
                .filter(|k| {
                    if func_keys_before.contains(k) {
                        return false;
                    }
                    let ks = k.resolve();
                    if let Some(name) = ks.strip_prefix("GLOBAL::") {
                        let base = name.split('/').next().unwrap_or(name);
                        // Only remove operator subs (infix:<...>, prefix:<...>, etc.)
                        base.contains(":<") && !exported_op_names.contains(base)
                    } else {
                        false
                    }
                })
                .copied()
                .collect();
            for k in non_exported_op_globals {
                self.registry_mut().functions.remove(&k);
            }

            // Remove GLOBAL:: sub aliases that were leaked by sub hoisting during
            // this module load, are NOT exported, and shadow a core builtin.
            // A `unit module X` declares `our sub foo` under `X::foo`, but mutsu's
            // hoist pre-pass registers the body under `GLOBAL::foo` (the runtime
            // package is not switched by the compile-time `unit` declaration). For
            // a non-exported sub whose name matches a core builtin, that GLOBAL
            // alias wrongly masks the builtin in the caller's scope (e.g.
            // Test::Util's non-exported `our sub run` was hiding the core `run`
            // Proc spawner). Such a sub is reachable from the caller only as a bare
            // name — which Raku does not allow for a non-exported `our sub` — so we
            // drop the leaked alias. Gating on builtin collision keeps the change
            // safe: non-colliding helpers stay in GLOBAL so the module's own
            // (GLOBAL-package) bodies can still call them.
            {
                let mut exported_names: HashSet<String> = HashSet::new();
                for source in ["GLOBAL", module] {
                    if let Some(subs) = self.exported_subs.get(source) {
                        for name in subs.keys() {
                            exported_names.insert(name.clone());
                        }
                    }
                }
                if let Some(subs) = self.unit_module_exported_subs.get(module) {
                    for name in subs.keys() {
                        exported_names.insert(name.clone());
                    }
                }
                let leaked_globals: Vec<Symbol> = self
                    .registry()
                    .functions
                    .keys()
                    .filter(|k| {
                        if func_keys_before.contains(k) {
                            return false;
                        }
                        let ks = k.resolve();
                        let Some(name) = ks.strip_prefix("GLOBAL::") else {
                            return false;
                        };
                        let base = name.split('/').next().unwrap_or(name);
                        !exported_names.contains(base) && Self::is_builtin_function(base)
                    })
                    .copied()
                    .collect();
                for k in leaked_globals {
                    self.registry_mut().functions.remove(&k);
                }
            }

            self.loaded_modules.insert(module.to_string());
            if let Err(err) = self.import_module(module, tags)
                && !err.message.starts_with("No exports found for module:")
            {
                return Err(err);
            }
        }
        result
    }

    /// Record a trait-modified routine value for an exported sub, so that
    /// `import_module` can restore the `&name` env binding with the role mixed in.
    pub(crate) fn record_exported_sub_value(&mut self, package: String, name: String, val: Value) {
        self.exported_sub_values
            .entry(package)
            .or_default()
            .insert(name, val);
    }

    pub(crate) fn register_exported_sub(
        &mut self,
        package: String,
        name: String,
        mut tags: Vec<String>,
    ) {
        if tags.is_empty() {
            tags.push("DEFAULT".to_string());
        }
        // Register EXPORT namespace aliases so that EXPORT::TAG::name and
        // Package::EXPORT::TAG::name resolve via normal function lookup.
        let fq_key = format!("{}::{}", package, name);
        let fq_sym = crate::symbol::Symbol::intern(&fq_key);
        // Hoist the clone to a `let` so the read guard drops before the
        // registry_mut writes below (read->write on the same lock deadlocks).
        let def = self.registry().functions.get(&fq_sym).cloned();
        if let Some(def) = def {
            for tag in &tags {
                // Bare EXPORT::TAG::name (accessible from the same package)
                let bare_export = format!("EXPORT::{}::{}", tag, name);
                self.registry_mut()
                    .functions
                    .entry(crate::symbol::Symbol::intern(&bare_export))
                    .or_insert_with(|| def.clone());
                // Fully-qualified Package::EXPORT::TAG::name
                let pkg_export = format!("{}::EXPORT::{}::{}", package, tag, name);
                self.registry_mut()
                    .functions
                    .entry(crate::symbol::Symbol::intern(&pkg_export))
                    .or_insert_with(|| def.clone());
            }
            // Always register under EXPORT::ALL::name
            if !tags.contains(&"ALL".to_string()) {
                let bare_all = format!("EXPORT::ALL::{}", name);
                self.registry_mut()
                    .functions
                    .entry(crate::symbol::Symbol::intern(&bare_all))
                    .or_insert_with(|| def.clone());
                let pkg_all = format!("{}::EXPORT::ALL::{}", package, name);
                self.registry_mut()
                    .functions
                    .entry(crate::symbol::Symbol::intern(&pkg_all))
                    .or_insert_with(|| def);
            }
        }
        // Mirror this export into the unit-module export table so that
        // `import_module` can validate tags for `unit module X` files whose
        // runtime package registration used "GLOBAL".
        if let Some(unit_mod) = self.unit_module_loading_stack.last().cloned() {
            let mirror = self
                .unit_module_exported_subs
                .entry(unit_mod)
                .or_default()
                .entry(name.clone())
                .or_default();
            for tag in &tags {
                mirror.insert(tag.clone());
            }
        }
        let entry = self
            .exported_subs
            .entry(package)
            .or_default()
            .entry(name)
            .or_default();
        for tag in tags {
            entry.insert(tag);
        }
    }

    pub(crate) fn register_exported_var(
        &mut self,
        package: String,
        name: String,
        mut tags: Vec<String>,
    ) {
        if tags.is_empty() {
            tags.push("DEFAULT".to_string());
        }
        let entry = self
            .exported_vars
            .entry(package)
            .or_default()
            .entry(name)
            .or_default();
        for tag in tags {
            entry.insert(tag);
        }
    }

    pub(crate) fn import_module(
        &mut self,
        module: &str,
        tags: &[String],
    ) -> Result<(), RuntimeError> {
        let requested: HashSet<String> = if tags.is_empty() {
            ["DEFAULT".to_string()].into_iter().collect()
        } else {
            tags.iter().cloned().collect()
        };
        let import_all = requested.contains("ALL");

        let subs = self.exported_subs.get(module).cloned().unwrap_or_default();
        let vars = self.exported_vars.get(module).cloned().unwrap_or_default();
        // For `unit module Foo`, sub registration at runtime may have used
        // the default "GLOBAL" package (because the interpreter's runtime
        // `current_package` is not switched by the compile-time unit
        // declaration). When a module declared the `unit_module` marker,
        // its exports are tracked separately so we can still validate tags
        // and report X::Import::NoSuchTag correctly.
        let unit_global_subs: HashMap<String, HashSet<String>> = self
            .unit_module_exported_subs
            .get(module)
            .cloned()
            .unwrap_or_default();
        if subs.is_empty() && vars.is_empty() && unit_global_subs.is_empty() {
            return Err(RuntimeError::new(format!(
                "No exports found for module: {}",
                module
            )));
        }

        // Validate that all requested tags actually exist in the module's exports.
        if !tags.is_empty() && !import_all {
            // Collect all known tags from the module
            let mut known_tags: HashSet<String> = HashSet::new();
            known_tags.insert("DEFAULT".to_string());
            known_tags.insert("ALL".to_string());
            known_tags.insert("MANDATORY".to_string());
            for symbol_tags in subs.values() {
                for tag in symbol_tags {
                    known_tags.insert(tag.clone());
                }
            }
            for symbol_tags in vars.values() {
                for tag in symbol_tags {
                    known_tags.insert(tag.clone());
                }
            }
            for symbol_tags in unit_global_subs.values() {
                for tag in symbol_tags {
                    known_tags.insert(tag.clone());
                }
            }
            for tag in &requested {
                if !known_tags.contains(tag) {
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("source-package".to_string(), Value::str(module.to_string()));
                    attrs.insert("tag".to_string(), Value::str(tag.clone()));
                    attrs.insert(
                        "message".to_string(),
                        Value::str(format!(
                            "Error while importing from '{}': no such tag '{}' declared",
                            module, tag
                        )),
                    );
                    let ex = Value::make_instance(
                        crate::symbol::Symbol::intern("X::Import::NoSuchTag"),
                        attrs,
                    );
                    let mut err = RuntimeError::new(format!(
                        "Error while importing from '{}': no such tag '{}' declared",
                        module, tag
                    ));
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
            }
        }

        // Import into the current package scope so that `use Foo` inside
        // `module Bar { }` makes Foo's exports available as `Bar::name`
        // rather than polluting the GLOBAL namespace.
        let target_pkg = self.current_package();

        for (name, symbol_tags) in subs {
            // MANDATORY exports are always imported regardless of requested tags
            let is_mandatory = symbol_tags.contains("MANDATORY");
            if !import_all && !is_mandatory && symbol_tags.is_disjoint(&requested) {
                continue;
            }
            let source_single = format!("{module}::{name}");
            let source_prefix = format!("{module}::{name}/");
            let target_single = format!("{target_pkg}::{name}");
            let target_prefix = format!("{target_pkg}::{name}/");

            let function_entries: Vec<(Symbol, Arc<FunctionDef>)> = self
                .registry()
                .functions
                .iter()
                .filter_map(|(k, v)| {
                    let ks = k.resolve();
                    if ks == source_single {
                        Some((Symbol::intern(&target_single), v.clone()))
                    } else if ks.starts_with(&source_prefix) {
                        Some((
                            Symbol::intern(&ks.replacen(&source_prefix, &target_prefix, 1)),
                            v.clone(),
                        ))
                    } else {
                        None
                    }
                })
                .collect();
            for (k, v) in function_entries {
                self.registry_mut().functions.insert(k, v);
            }

            let proto_entries: Vec<(Symbol, Arc<FunctionDef>)> = self
                .registry()
                .proto_functions
                .iter()
                .filter_map(|(k, v)| {
                    if *k == *source_single {
                        Some((Symbol::intern(&target_single), v.clone()))
                    } else {
                        None
                    }
                })
                .collect();
            for (k, v) in proto_entries {
                self.registry_mut().proto_functions.insert(k, v);
            }

            // If this exported sub carried a trait-modified value (e.g. a role
            // mixed in via a custom `is` trait), restore it as the `&name` env
            // binding so `&name ~~ Role` works after import.
            if let Some(val) = self
                .exported_sub_values
                .get(module)
                .and_then(|m| m.get(&name))
                .cloned()
            {
                self.env.insert(format!("&{name}"), val.clone());
                self.env.insert(format!("&{target_pkg}::{name}"), val);
            }
        }

        for (name, symbol_tags) in vars {
            let is_mandatory = symbol_tags.contains("MANDATORY");
            if !import_all && !is_mandatory && symbol_tags.is_disjoint(&requested) {
                continue;
            }
            let (source, target) = if let Some(sigil) = name.chars().next()
                && matches!(sigil, '$' | '@' | '%' | '&')
            {
                let bare = &name[1..];
                (format!("{sigil}{module}::{bare}"), name.clone())
            } else {
                (format!("{module}::{name}"), name.clone())
            };
            if let Some(value) = self.env.get(&source).cloned() {
                if !target.contains("::") {
                    self.unsuppress_name(&target);
                }
                // Slice F (env<->locals coherence): `import` writes the symbol
                // into env by name, but a later bare reference (e.g. an imported
                // `constant c`) may read a stale caller local slot when the
                // reverse env->locals pull is disabled. Record the imported
                // name (sigil stripped to match the local-slot key) so the
                // ImportModule opcode writes it through to the caller slot.
                if !target.contains("::") {
                    let slot_name = match target.chars().next() {
                        Some('$' | '@' | '%') => target[1..].to_string(),
                        _ => target.clone(),
                    };
                    self.pending_rw_writeback_sources.push(slot_name);
                }
                self.env.insert(target, value);
            }
        }
        Ok(())
    }

    /// Load a module without importing its exports (Raku `need` keyword).
    pub(crate) fn need_module(&mut self, module: &str) -> Result<(), RuntimeError> {
        let is_nested_need = !self.module_load_stack.is_empty();
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
        let class_snapshot: HashSet<String> = self.registry().classes.keys().cloned().collect();
        let env_snapshot: HashSet<Symbol> = self.env.keys().copied().collect();
        let saved = self.suppress_exports;
        self.suppress_exports = true;
        let result = self.load_module(module);
        self.suppress_exports = saved;
        self.module_load_stack.pop();
        if result.is_ok() {
            let short_name = if let Some((_, short)) = module.rsplit_once("::") {
                short.to_string()
            } else {
                module.to_string()
            };
            let class_names: Vec<String> = self.registry().classes.keys().cloned().collect();
            for class_name in &class_names {
                if !class_snapshot.contains(class_name) {
                    self.need_hidden_classes.insert(class_name.clone());
                    if let Some((_, short)) = class_name.rsplit_once("::") {
                        self.need_hidden_classes.insert(short.to_string());
                    }
                }
            }
            for key in self.env.keys() {
                if env_snapshot.contains(key) {
                    continue;
                }
                if key.starts_with("$")
                    || key.starts_with("@")
                    || key.starts_with("%")
                    || key.starts_with("&")
                {
                    continue;
                }
                let key_s = key.resolve();
                let key_short = key_s
                    .rsplit_once("::")
                    .map(|(_, short)| short)
                    .unwrap_or(key_s.as_str());
                if !key_short
                    .chars()
                    .next()
                    .is_some_and(|c| c.is_ascii_uppercase())
                {
                    continue;
                }
                if is_nested_need || key_short != short_name {
                    self.need_hidden_classes.insert(key_s.clone());
                    self.need_hidden_classes.insert(key_short.to_string());
                }
            }
            if is_nested_need {
                self.need_hidden_classes.insert(short_name.clone());
            }
            self.loaded_modules.insert(module.to_string());
        }
        result
    }

    pub(crate) fn no_module(&mut self, module: &str) -> Result<(), RuntimeError> {
        if module == "strict" {
            self.strict_mode = false;
        } else if module == "fatal" {
            self.fatal_mode = false;
        }
        Ok(())
    }

    pub fn output(&self) -> String {
        self.output_sink().output.clone()
    }

    /// Clear the output buffer and reset the output-emitted flag.
    pub fn clear_output(&mut self) {
        let mut sink = self.output_sink_mut();
        sink.output.clear();
        sink.output_emitted = false;
    }

    /// Take the output buffer, leaving it empty.
    pub(crate) fn take_output(&mut self) -> String {
        std::mem::take(&mut self.output_sink_mut().output)
    }

    /// Take the stderr buffer, leaving it empty.
    pub(crate) fn take_stderr_output(&mut self) -> String {
        std::mem::take(&mut self.output_sink_mut().stderr_output)
    }

    /// Returns true if any output was emitted since the last `clear_output`.
    pub fn has_output_emitted(&self) -> bool {
        self.output_sink().output_emitted
    }

    /// Write to the output buffer and also flush to real stdout
    /// when not inside a subtest.
    pub(crate) fn emit_output(&mut self, text: &str) {
        let byte_count = text.len() as i64;
        if let Some(stdout_handle) = self
            .io_handles_mut()
            .map
            .values_mut()
            .find(|h| matches!(h.target, IoHandleTarget::Stdout))
        {
            stdout_handle.bytes_written += byte_count;
        }
        // The Stdout `bytes_written` accounting above touches `io_handles`; the
        // write decision + buffers live in `output_sink`.
        let subtest_active = self.tap.subtest_depth() != 0;
        self.output_sink_mut().emit(text, subtest_active);
    }

    /// Enable immediate flushing of output to stdout.
    pub fn set_immediate_stdout(&mut self, val: bool) {
        self.output_sink_mut().immediate_stdout = val;
    }

    pub fn flush_stderr_buffer(&mut self) {
        let stderr = std::mem::take(&mut self.output_sink_mut().stderr_output);
        if !stderr.is_empty() {
            eprint!("{}", stderr);
            let _ = std::io::stderr().flush();
        }
    }

    /// Enable or disable module precompilation cache.
    pub fn set_precomp_enabled(&mut self, val: bool) {
        self.precomp_enabled = val;
    }

    /// Check if MONKEY-TYPING pragma is active.
    pub(crate) fn monkey_typing_enabled(&self) -> bool {
        self.monkey_typing
    }

    pub fn exit_code(&self) -> i64 {
        self.exit_code
    }

    /// Return the value of `%*ENV<RAKU_EXCEPTIONS_HANDLER>`, if set.
    /// This selects the format used to print uncaught exceptions (e.g. "JSON").
    pub fn exceptions_handler(&self) -> Option<String> {
        let env_hash = self.env.get("%*ENV")?;
        if let Value::Hash(map) = env_hash
            && let Some(v) = map.get("RAKU_EXCEPTIONS_HANDLER")
        {
            let s = v.to_string_value();
            if !s.is_empty() {
                return Some(s);
            }
        }
        None
    }

    pub(crate) fn is_halted(&self) -> bool {
        self.halted
    }

    pub(crate) fn is_thread_clone(&self) -> bool {
        self.output_sink().is_thread_clone
    }

    /// Write a message to stderr, respecting nested mode.
    /// In nested mode the output is buffered for later inspection;
    /// otherwise it is emitted directly so `flush_stderr_buffer` does
    /// not duplicate it.
    pub(crate) fn emit_stderr(&mut self, text: &str) {
        if self.nested_mode {
            self.output_sink_mut().stderr_output.push_str(text);
        } else {
            eprint!("{}", text);
        }
    }

    pub(crate) fn write_warn_to_stderr(&mut self, message: &str) {
        let msg = format!("{}\n", message);
        // Read the thread-clone shared stderr Arc out under a scoped guard so it
        // is dropped before `self.warn_output` / `emit` re-borrow self.
        let thread_shared_stderr = {
            let sink = self.output_sink();
            if sink.is_thread_clone {
                sink.shared_thread_stderr.clone()
            } else {
                None
            }
        };
        if let Some(shared) = thread_shared_stderr {
            shared.lock().unwrap().push_str(&msg);
            self.warn_output.push_str(&msg);
            return;
        }
        self.warn_output.push_str(&msg);
        // In nested mode (e.g. in-process `is_run`), buffer to
        // `stderr_output` so the caller can inspect captured stderr.
        // Otherwise emit directly to the real stderr; if we also pushed
        // into `stderr_output`, the final flush would duplicate it.
        if self.nested_mode {
            self.output_sink_mut().stderr_output.push_str(&msg);
        } else {
            eprint!("{}", msg);
        }
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

    pub(crate) fn env(&self) -> &Env {
        &self.env
    }

    #[allow(dead_code)] // env-loan (CP-1 1e): VM callers migrated to the seam; kept for carriers.
    pub(crate) fn env_insert(&mut self, key: String, value: Value) {
        self.env.insert(key, value);
    }

    /// Clone the env for capture across a call/block/thread boundary. For a flat
    /// env this is the O(1) `Arc::clone`; for a *scoped* env (a converted call
    /// frame's transient overlay-over-parent) it flattens parent+overlay into a
    /// flat env so the captured copy exposes the full lexical view to consumers
    /// that iterate it overlay-only (nested call merges, `clone_for_thread`). See
    /// docs/vm-dual-store.md (Slice 6).
    #[allow(dead_code)] // env-loan (CP-1 1e): VM callers migrated to the seam; kept for carriers.
    pub(crate) fn clone_env(&self) -> Env {
        self.env.flattened()
    }

    /// Replace the entire env.
    #[allow(dead_code)]
    pub(crate) fn set_env(&mut self, env: Env) {
        self.env = env;
    }

    /// Take the env out, replacing it with an empty Env.
    #[allow(dead_code)]
    pub(crate) fn take_env(&mut self) -> Env {
        std::mem::take(&mut self.env)
    }

    fn normalize_var_meta_name(name: &str) -> &str {
        name.trim_start_matches(['$', '@', '%', '&'])
    }

    fn var_meta_value_key(name: &str) -> String {
        format!("__mutsu_var_meta::{}", name)
    }

    pub(crate) fn set_var_dynamic(&mut self, name: &str, dynamic: bool) {
        let key = Self::normalize_var_meta_name(name).to_string();
        self.var_dynamic_flags.insert(key, dynamic);
    }

    pub(crate) fn set_var_meta_value(&mut self, name: &str, value: Value) {
        self.env.insert(Self::var_meta_value_key(name), value);
    }

    pub(crate) fn var_meta_value(&self, name: &str) -> Option<Value> {
        self.env.get(&Self::var_meta_value_key(name)).cloned()
    }

    pub(crate) fn set_var_type_constraint(&mut self, name: &str, constraint: Option<String>) {
        let key = name.to_string();
        let meta_key = format!("__mutsu_type::{}", key);
        if let Some(constraint) = constraint {
            let info = Self::parse_container_constraint(name, &constraint);
            if info.value_type == "atomicint" || constraint.contains("atomicint") {
                self.atomic_var_seen = true;
            }
            self.var_type_constraints
                .insert(key.clone(), info.value_type.clone());
            self.env
                .insert(meta_key, Value::str(info.value_type.clone()));
            let hash_key_meta_key = format!("__mutsu_hash_key_type::{}", key);
            if let Some(key_type) = info.key_type.clone() {
                self.var_hash_key_constraints
                    .insert(key.clone(), key_type.clone());
                self.env.insert(hash_key_meta_key, Value::str(key_type));
            } else {
                self.var_hash_key_constraints.remove(&key);
                self.env.remove(&hash_key_meta_key);
            }
            // Only register container type metadata for container-sigil variables
            // (`@a`, `%h`). For scalar parameters (e.g. `Mu $a`) the bound value
            // may share an `Arc` with a caller's container, and tagging that Arc
            // would corrupt the caller's container type metadata via Arc pointer
            // keying (and Arc pointer reuse after drop).
            if name.starts_with('@') || name.starts_with('%') {
                self.register_var_container_type_metadata(&key, &info);
            }
        } else {
            self.var_type_constraints.remove(&key);
            self.var_hash_key_constraints.remove(&key);
            self.env.remove(&meta_key);
            self.env.remove(&format!("__mutsu_hash_key_type::{}", key));
        }
    }

    /// Register the type constraint of a *bound routine parameter*. For scalar
    /// parameters the constraint is written ONLY to the `env`-keyed
    /// `__mutsu_type::name` metadata (which is scoped — dropped when the callee's
    /// env is restored) and NOT to the global, name-keyed `var_type_constraints`
    /// map. This is what stops a typed parameter (`Str:D $x`) from leaking its
    /// constraint onto a same-named lexical in the *caller* (`my $x = f(...)`,
    /// where `f`'s parameter is also `$x`): the global map would otherwise retain
    /// the entry after the callee returns, and the env-first/`var_type_constraints`-
    /// fallback read would surface it. `my`-declared and `subset` constraints
    /// still go through `set_var_type_constraint` (both stores), so the global-map
    /// fallback remains available where the env entry isn't visible (e.g. an
    /// `EVAL`'d re-assignment to a `subset`-typed lexical). Container parameters
    /// (`@a`/`%h`) keep the full behaviour — their element/key-type metadata is
    /// consulted via the global map / container metadata for element checks.
    pub(crate) fn bind_param_type_constraint(&mut self, name: &str, constraint: Option<String>) {
        if name.starts_with('@') || name.starts_with('%') {
            self.set_var_type_constraint(name, constraint);
            return;
        }
        let meta_key = format!("__mutsu_type::{}", name);
        match constraint {
            Some(c) => {
                let info = Self::parse_container_constraint(name, &c);
                if info.value_type == "atomicint" || c.contains("atomicint") {
                    self.atomic_var_seen = true;
                }
                self.env.insert(meta_key, Value::str(info.value_type));
            }
            None => {
                // An untyped scalar parameter shadows any same-named lexical: it
                // has NO constraint in the callee's scope. Clear both the env
                // metadata AND a possibly-stale `var_type_constraints` entry left
                // by an earlier `my Type $x` declaration whose block has exited
                // (the global map is not block-scoped). Without this, reading the
                // (Nil-defaulted) parameter would surface the stale constraint via
                // the global-map fallback and return the type object instead of
                // Nil — roast S02-types/nil.t f4. An enclosing typed lexical that
                // is still in scope keeps its own env metadata, so its enforcement
                // (read env-first) survives the callee's return.
                self.env.remove(&meta_key);
                self.var_type_constraints.remove(name);
            }
        }
    }

    pub(crate) fn var_type_constraint(&self, name: &str) -> Option<String> {
        let key = name;
        let meta_key = format!("__mutsu_type::{}", key);
        if let Some(Value::Str(tc)) = self.env.get(&meta_key) {
            return Some(tc.to_string());
        }
        if let Some(tc) = self.var_type_constraints.get(key) {
            return Some(tc.clone());
        }
        None
    }

    /// Fast type constraint lookup — only checks the `var_type_constraints` HashMap,
    /// skipping the `format!("__mutsu_type::...")` + env lookup. Used by the SetLocal
    /// fast path for simple scalar variables where the env-based constraint is never set.
    pub(crate) fn var_type_constraint_fast(&self, name: &str) -> Option<&String> {
        self.var_type_constraints.get(name)
    }

    /// Whether any `atomicint`/atomic-storage variable has ever been registered
    /// (monotonic). When false, the hot variable-read path can skip the entire
    /// atomic-variable check (which otherwise costs `format!`s and constraint
    /// lookups on every `GetGlobal`/`GetLocal`).
    #[inline(always)]
    pub(crate) fn atomic_var_seen(&self) -> bool {
        self.atomic_var_seen
    }

    /// Mark that an atomic variable / atomic storage has been registered.
    pub(crate) fn mark_atomic_var_seen(&mut self) {
        self.atomic_var_seen = true;
    }

    /// Set the default value for a variable declared with `is default(...)`.
    pub(crate) fn set_var_default(&mut self, name: &str, value: Value) {
        self.var_defaults.insert(name.to_string(), value);
    }

    /// Get the default value for a variable, if one was set with `is default(...)`.
    pub(crate) fn var_default(&self, name: &str) -> Option<&Value> {
        self.var_defaults.get(name)
    }

    /// Remove a variable's cached `is default(...)` value. Called on
    /// variable redeclaration so a new `my @a` does not inherit the
    /// default from an earlier same-named variable.
    pub(crate) fn clear_var_default(&mut self, name: &str) {
        self.var_defaults.remove(name);
    }

    /// Get the evaluated `is default(...)` value for a class attribute.
    pub(crate) fn class_attribute_default(
        &self,
        class_name: &str,
        attr_name: &str,
    ) -> Option<Value> {
        self.registry()
            .class_attribute_defaults
            .get(&(class_name.to_string(), attr_name.to_string()))
            .cloned()
    }

    /// Get the `is DEPRECATED` message for a class attribute accessor.
    pub(crate) fn class_attribute_deprecated(
        &self,
        class_name: &str,
        attr_name: &str,
    ) -> Option<String> {
        self.registry()
            .class_attribute_deprecated
            .get(&(class_name.to_string(), attr_name.to_string()))
            .cloned()
    }

    /// Attach an `is default(...)` element default to a container, returning
    /// the (possibly rebuilt) value. For both arrays and hashes the default is
    /// embedded in the backing `ArrayData`/`HashData` so it travels with the
    /// container through copy-on-write; callers MUST store the returned value
    /// back into the slot it came from.
    pub(crate) fn tag_container_default(&mut self, value: Value, default: Value) -> Value {
        match value {
            Value::Array(mut arc, kind) => {
                if arc.default.as_deref() != Some(&default) {
                    Arc::make_mut(&mut arc).default = Some(Box::new(default));
                }
                Value::Array(arc, kind)
            }
            Value::Hash(mut map) => {
                if map.default.as_deref() != Some(&default) {
                    Arc::make_mut(&mut map).default = Some(Box::new(default));
                }
                Value::Hash(map)
            }
            other => other,
        }
    }

    /// Get the element default for a container (Array/Hash).
    pub(crate) fn container_default<'v>(&'v self, value: &'v Value) -> Option<&'v Value> {
        match value {
            Value::Array(items, ..) => items.default.as_deref(),
            Value::Hash(map) => map.default.as_deref(),
            _ => None,
        }
    }

    pub(crate) fn var_hash_key_constraint(&self, name: &str) -> Option<String> {
        let key = name;
        let meta_key = format!("__mutsu_hash_key_type::{}", key);
        if let Some(Value::Str(tc)) = self.env.get(&meta_key) {
            return Some(tc.to_string());
        }
        self.var_hash_key_constraints.get(key).cloned()
    }

    /// Fast check for hash key constraint — only checks the HashMap,
    /// skipping the `format!("__mutsu_hash_key_type::...")` + env lookup.
    pub(crate) fn var_hash_key_constraint_fast(&self, name: &str) -> bool {
        self.var_hash_key_constraints.contains_key(name)
    }

    /// Autovivify a typed array element when a mutating array method (push/append/
    /// unshift/prepend) is called on its type object, e.g. `my Array of Int @x;
    /// @x[0].push(3)`. `type_name` is the element type object name (e.g.
    /// `Array[Int]`). Returns `Some(new_array)` when `type_name` is a positional
    /// container type, `None` otherwise. The result carries type metadata so the
    /// elements stay constrained.
    pub(crate) fn autoviv_typed_array_push(
        &mut self,
        type_name: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        let base = type_name.split('[').next().unwrap_or(type_name);
        if !matches!(base, "Array" | "List") {
            return Ok(None);
        }
        let inner = type_name
            .strip_prefix(base)
            .and_then(|s| s.strip_prefix('['))
            .and_then(|s| s.strip_suffix(']'))
            .map(|s| s.trim().to_string());
        let mut items = Vec::new();
        for arg in args {
            match arg {
                Value::Slip(vals) => items.extend(vals.iter().cloned()),
                other => items.push(other.clone()),
            }
        }
        if let Some(ref constraint) = inner
            && constraint.starts_with(char::is_uppercase)
        {
            for item in &items {
                if !self.type_matches_value(constraint, item) {
                    return Err(crate::runtime::utils::type_check_element_typed_error(
                        "", constraint, item,
                    ));
                }
            }
        }
        let result = Value::real_array(items);
        if let Some(constraint) = inner {
            let info = ContainerTypeInfo {
                value_type: constraint,
                key_type: None,
                declared_type: Some(type_name.to_string()),
            };
            self.register_container_type_metadata(&result, info);
        }
        Ok(Some(result))
    }

    /// Build a `ContainerTypeInfo` from a hash's embedded type metadata, or
    /// `None` when the hash carries no type metadata. This is the authoritative
    /// source of hash type metadata, replacing the old `hash_type_metadata`
    /// Arc-pointer-keyed side table: the metadata travels WITH the `HashData`
    /// through copy-on-write and rebuilds, so a freshly-built hash literal can
    /// never inherit a stale typed-hash entry via pointer reuse.
    pub(crate) fn hashdata_type_info(hd: &crate::value::HashData) -> Option<ContainerTypeInfo> {
        if !hd.has_type_meta() {
            return None;
        }
        Some(ContainerTypeInfo {
            value_type: hd.value_type.clone().unwrap_or_default(),
            key_type: hd.key_type.clone(),
            declared_type: hd.declared_type.clone(),
        })
    }

    /// Attach container type metadata to `value`, returning the (possibly
    /// rebuilt) value. For hashes and Set/Bag/Mix the metadata is embedded in
    /// the backing data struct (via copy-on-write `Arc::make_mut`), so callers
    /// MUST use the returned value — store it back into the env/local slot it
    /// came from. For other container types this defers to the Arc-pointer
    /// side tables (unchanged) and returns the value untouched.
    pub(crate) fn tag_container_metadata(
        &mut self,
        value: Value,
        info: ContainerTypeInfo,
    ) -> Value {
        // Skip the copy-on-write clone when the metadata is already present.
        // `Arc::make_mut` on a shared container clones the backing data, which
        // changes the container's identity (`.WHICH` is pointer-based) — so a
        // no-op re-tag of e.g. `$map.Map` would otherwise break
        // `$map.Map === $map`.
        macro_rules! embed_type_info {
            ($arc:ident, $info:ident) => {{
                let new_vt = (!$info.value_type.is_empty()).then(|| $info.value_type.clone());
                if $arc.value_type != new_vt
                    || $arc.key_type != $info.key_type
                    || $arc.declared_type != $info.declared_type
                {
                    let data = Arc::make_mut(&mut $arc);
                    data.value_type = new_vt;
                    data.key_type = $info.key_type.clone();
                    data.declared_type = $info.declared_type.clone();
                }
            }};
        }
        match value {
            Value::Array(mut arc, kind) => {
                embed_type_info!(arc, info);
                Value::Array(arc, kind)
            }
            Value::Hash(mut arc) => {
                embed_type_info!(arc, info);
                Value::Hash(arc)
            }
            Value::Set(mut arc, m) => {
                embed_type_info!(arc, info);
                Value::Set(arc, m)
            }
            Value::Bag(mut arc, m) => {
                embed_type_info!(arc, info);
                Value::Bag(arc, m)
            }
            Value::Mix(mut arc, m) => {
                embed_type_info!(arc, info);
                Value::Mix(arc, m)
            }
            Value::Mixin(inner, m)
                if matches!(
                    inner.as_ref(),
                    Value::Array(..)
                        | Value::Hash(_)
                        | Value::Set(..)
                        | Value::Bag(..)
                        | Value::Mix(..)
                ) =>
            {
                let tagged = self.tag_container_metadata((*inner).clone(), info);
                Value::Mixin(Arc::new(tagged), m)
            }
            other => {
                self.register_container_type_metadata(&other, info);
                other
            }
        }
    }

    pub(crate) fn register_container_type_metadata(
        &mut self,
        value: &Value,
        info: ContainerTypeInfo,
    ) {
        match value {
            Value::Array(..)
            | Value::Hash(_)
            | Value::Set(..)
            | Value::Bag(..)
            | Value::Mix(..) => {
                // Array/Hash/Set/Bag/Mix type metadata is embedded in the backing
                // data struct — see `tag_container_metadata`. Reaching here
                // means such a value flowed through the by-reference register
                // path, which cannot embed (it has no owning slot to write
                // back). Such sites are migrated to `tag_container_metadata`;
                // this arm is intentionally a no-op.
                debug_assert!(
                    false,
                    "register_container_type_metadata called on a {}; use tag_container_metadata",
                    crate::value::what_type_name(value)
                );
            }
            Value::Instance { id, .. } => {
                self.instance_type_metadata
                    .write()
                    .unwrap()
                    .insert(*id, info);
            }
            Value::Mixin(inner, _) => self.register_container_type_metadata(inner, info),
            _ => {}
        }
    }

    /// Re-attach previously-captured container type metadata to whatever array
    /// is now bound to `key`. In-place array mutators (pop/shift/splice/append/
    /// prepend/unshift) call `Arc::make_mut`, which reallocates the backing
    /// buffer whenever the Arc is shared (e.g. the method receiver still holds a
    /// clone). The fresh heap pointer has no entry in the pointer-keyed
    /// `array_type_metadata` map, which silently demotes a typed `array[int]` /
    /// `Array[Int]` to a plain `Array`. Re-registering the saved metadata under
    /// the new pointer preserves the declared type across the mutation. No-op
    /// when the array was untyped (`saved` is `None`).
    pub(crate) fn reattach_array_type_metadata(
        &mut self,
        key: &str,
        saved: &Option<ContainerTypeInfo>,
    ) {
        if let Some(info) = saved
            && let Some(arr @ Value::Array(..)) = self.env.get(key).cloned()
        {
            let tagged = self.tag_container_metadata(arr, info.clone());
            self.env.insert(key.to_string(), tagged);
        }
    }

    /// Clear a hash's embedded type metadata, returning the rebuilt value.
    /// Callers must store the returned value back into its slot.
    pub(crate) fn clear_hash_type_metadata(value: Value) -> Value {
        if let Value::Hash(mut arc) = value {
            if arc.has_type_meta() {
                Arc::make_mut(&mut arc).clear_type_meta();
            }
            return Value::Hash(arc);
        }
        if let Value::Array(mut arc, kind) = value {
            if arc.has_type_meta() {
                let data = Arc::make_mut(&mut arc);
                data.value_type = None;
                data.key_type = None;
                data.declared_type = None;
            }
            return Value::Array(arc, kind);
        }
        value
    }

    pub(crate) fn container_type_metadata(&self, value: &Value) -> Option<ContainerTypeInfo> {
        container_type_metadata_with(value, &self.instance_type_metadata)
    }

    // Object-hash original keys are embedded in `HashData.original_keys`
    // (see `runtime::utils::set_hash_original_keys` / `hash_original_keys_snapshot`),
    // so the `hash_object_keys` side table and its pointer-migration helpers are
    // gone — the map now travels with the hash across copy-on-write.

    /// Check if a hash is an object hash (has a key_type constraint).
    pub(crate) fn is_object_hash(&self, hash: &Value) -> bool {
        self.hash_key_type(hash).is_some()
    }

    /// Get the key type constraint for an object hash, if any.
    pub(crate) fn hash_key_type(&self, hash: &Value) -> Option<String> {
        if let Value::Hash(arc) = hash {
            return arc.key_type.clone();
        }
        None
    }

    fn register_var_container_type_metadata(&mut self, name: &str, info: &ContainerTypeInfo) {
        if let Some(value) = self.env.get(name).cloned() {
            // Hashes embed metadata in `HashData`, so the tagged value must be
            // stored back; non-hash containers use the Arc-pointer side tables
            // and `tag_container_metadata` returns the same Arc (re-insert is a
            // no-op for them).
            let tagged = self.tag_container_metadata(value, info.clone());
            self.env.insert(name.to_string(), tagged);
        }
    }

    fn parse_container_constraint(name: &str, raw: &str) -> ContainerTypeInfo {
        let raw = raw.trim();
        // Note: For %-sigil variables, Hash[X] means "elements are Hash[X]",
        // NOT "elements are X". We do NOT unwrap Hash[...] here.
        // The only special case for % is the `TypeName{KeyType}` syntax
        // (e.g. `my Int %h{Str}`) which specifies both value and key types.
        if name.starts_with('%')
            && let Some((value_type, key_part)) = raw.split_once('{')
            && let Some(key_type) = key_part.strip_suffix('}')
        {
            return ContainerTypeInfo {
                value_type: value_type.trim().to_string(),
                key_type: Some(key_type.trim().to_string()),
                declared_type: Some(format!("Hash[{},{}]", value_type.trim(), key_type.trim())),
            };
        }
        // Note: For @-sigil variables, Array[X] means "elements are Array[X]",
        // NOT "elements are X". We do NOT unwrap Array[...] here.
        // `my Int @a` has raw="Int" and `my Array[Int] @a` has raw="Array[Int]".
        ContainerTypeInfo {
            value_type: raw.to_string(),
            key_type: None,
            declared_type: if name.starts_with('@')
                && crate::runtime::native_types::is_native_array_element_type(raw)
            {
                Some(format!("array[{raw}]"))
            } else {
                None
            },
        }
    }

    pub(crate) fn snapshot_var_type_constraints(&self) -> HashMap<String, String> {
        self.var_type_constraints.clone()
    }

    pub(crate) fn restore_var_type_constraints(&mut self, snapshot: HashMap<String, String>) {
        self.var_type_constraints = snapshot;
    }

    /// Check that all values satisfy the element type constraint for a
    /// container variable (e.g. `my Int @a`). Returns Ok(()) if no constraint
    /// exists or all values pass; returns a type-check error otherwise.
    pub(crate) fn check_container_element_types(
        &mut self,
        var_name: &str,
        values: &[Value],
    ) -> Result<(), RuntimeError> {
        if let Some(constraint) = self.var_type_constraint(var_name) {
            for val in values {
                if !matches!(val, Value::Nil) && !self.type_matches_value(&constraint, val) {
                    return Err(crate::runtime::utils::type_check_element_typed_error(
                        var_name,
                        &constraint,
                        val,
                    ));
                }
            }
        }
        Ok(())
    }

    /// Check element type constraints using container metadata (for typed arrays
    /// obtained from instance attributes or other non-variable sources).
    pub(crate) fn check_array_value_element_types(
        &mut self,
        target: &Value,
        values: &[Value],
    ) -> Result<(), RuntimeError> {
        if let Some(info) = self.container_type_metadata(target) {
            let constraint = &info.value_type;
            if constraint != "Mu" && constraint != "Any" {
                for val in values {
                    if !matches!(val, Value::Nil) && !self.type_matches_value(constraint, val) {
                        return Err(crate::runtime::utils::type_check_element_typed_error(
                            "@_", constraint, val,
                        ));
                    }
                }
            }
        }
        Ok(())
    }

    /// Finalize a typed `@`/`%` constructor attribute (`has Int @.nums` /
    /// `has Int %.map`): type-check each element against the declared element
    /// type (matching the variable path `my Int @a`, and raku's "Type check
    /// failed for an element of @!nums") and tag the container with element-type
    /// metadata so `.of` / `.WHAT` / `~~ Array[Int]` reflect the declared type.
    ///
    /// `value` must be the exact `Value` (Array/Hash) that will be stored in the
    /// instance: the metadata is keyed by the backing `Arc`'s pointer, so it has
    /// to be registered against the Arc that survives into `make_instance`.
    pub(crate) fn finalize_typed_container_attr(
        &mut self,
        attr_name: &str,
        sigil: char,
        elem_type: &str,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        if elem_type != "Mu" && elem_type != "Any" {
            let display = format!("{}!{}", sigil, attr_name);
            // Collect the values to type-check. A shaped array (`has Int @.g[2;2]`)
            // nests its leaves inside per-dimension sub-arrays, so descend to the
            // leaves; a plain array checks its direct elements (a `has Array @.x`
            // legitimately holds array elements, so do not descend into those).
            fn collect_leaves<'a>(v: &'a Value, descend: bool, out: &mut Vec<&'a Value>) {
                match v {
                    Value::Array(items, kind) if descend || matches!(kind, ArrayKind::Shaped) => {
                        for it in items.iter() {
                            collect_leaves(it, true, out);
                        }
                    }
                    _ => out.push(v),
                }
            }
            let mut elems: Vec<&Value> = Vec::new();
            match &value {
                Value::Array(items, ArrayKind::Shaped) => {
                    for it in items.iter() {
                        collect_leaves(it, true, &mut elems);
                    }
                }
                Value::Array(items, _) => elems.extend(items.iter()),
                Value::Hash(map) => elems.extend(map.values()),
                _ => {}
            }
            for it in elems {
                if !matches!(it, Value::Nil) && !self.type_matches_value(elem_type, it) {
                    return Err(crate::runtime::utils::type_check_element_typed_error(
                        &display, elem_type, it,
                    ));
                }
            }
        }
        let info = ContainerTypeInfo {
            value_type: elem_type.to_string(),
            key_type: None,
            declared_type: None,
        };
        Ok(self.tag_container_metadata(value, info))
    }

    pub(crate) fn is_var_dynamic(&self, name: &str) -> bool {
        let bare = Self::normalize_var_meta_name(name);
        // The implicit special variables `$_`, `$/`, `$!` are dynamic by nature
        // (no `*` twigil, but `.VAR.dynamic` is True and they are visible through
        // CALLER::), so report them as dynamic even without an explicit flag.
        if matches!(bare, "_" | "/" | "!") {
            return true;
        }
        self.var_dynamic_flags.get(bare).copied().unwrap_or(false)
    }

    pub(crate) fn push_caller_env(&mut self) {
        self.push_caller_env_with_code(None);
    }

    /// Push caller env with an explicit code (Sub) value for the current frame.
    pub(crate) fn push_caller_env_with_code(&mut self, code: Option<Value>) {
        self.caller_env_stack.push(self.env.clone());
        let file = self
            .env
            .get("?FILE")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let line = self
            .env
            .get("?LINE")
            .and_then(|v| match v {
                Value::Int(i) => Some(*i),
                _ => None,
            })
            .unwrap_or(0);
        let code = code.or_else(|| self.block_stack.last().cloned());
        self.callframe_stack.push(CallFrameEntry {
            file,
            line,
            code,
            env: self.env.clone(),
        });
    }

    pub(crate) fn pop_caller_env(&mut self) {
        self.caller_env_stack.pop();
        self.callframe_stack.pop();
    }

    /// Pop caller env and apply any dynamic variable writes back to the given env.
    /// Use this instead of `pop_caller_env()` at function return sites that restore `saved_env`.
    pub(crate) fn pop_caller_env_with_writeback(&mut self, restored_env: &mut Env) {
        if let Some(popped) = self.caller_env_stack.pop() {
            for (key, value) in &popped {
                key.with_str(|key_str| {
                    // `$_`, `$/`, `$!` report as dynamic for `.VAR.dynamic` /
                    // CALLER:: purposes, but they are *not* propagated back to the
                    // caller on return: each routine gets its own topic/match/error
                    // and the caller's value is restored from `saved_env`. Writing
                    // the callee frame's stale copy back here clobbers the caller's
                    // live `$_` (e.g. a `block.($_)` call inside a `for` loop would
                    // revert the loop topic to the previous iteration's value).
                    let bare = Self::normalize_var_meta_name(key_str);
                    if matches!(bare, "_" | "/" | "!") {
                        return;
                    }
                    if self.is_var_dynamic(key_str) && restored_env.get_sym(*key) != Some(value) {
                        restored_env.insert_sym(*key, value.clone());
                    }
                });
            }
        }
        self.callframe_stack.pop();
    }

    pub(crate) fn get_caller_line(&self, depth: usize) -> Option<Value> {
        let stack_len = self.caller_env_stack.len();
        if depth == 0 || depth > stack_len {
            return None;
        }
        self.caller_env_stack
            .get(stack_len - depth)
            .and_then(|env| env.get("?LINE").cloned())
    }

    /// Look up a variable through the $CALLER:: chain.
    /// `depth` is the number of CALLER:: levels (1 for $CALLER::x, 2 for $CALLER::CALLER::x).
    /// `name` is the bare variable name without sigil (e.g. "a" for $a).
    pub(crate) fn get_caller_var(&self, name: &str, depth: usize) -> Result<Value, RuntimeError> {
        let stack_len = self.caller_env_stack.len();
        if depth > stack_len {
            return Err(RuntimeError::new(format!(
                "Cannot access caller variable '${name}' — not enough caller frames"
            )));
        }
        let env = &self.caller_env_stack[stack_len - depth];
        if let Some(val) = env.get(name) {
            // Check that the variable is declared `is dynamic`
            if !self.is_var_dynamic(name) {
                return Err(crate::runtime::utils::caller_not_dynamic_error(name));
            }
            Ok(val.clone())
        } else {
            Err(RuntimeError::new(format!(
                "Cannot access '${name}' through CALLER"
            )))
        }
    }

    /// Set a variable through the $CALLER:: chain.
    pub(crate) fn set_caller_var(
        &mut self,
        name: &str,
        depth: usize,
        value: Value,
    ) -> Result<(), RuntimeError> {
        let stack_len = self.caller_env_stack.len();
        if depth > stack_len {
            return Err(RuntimeError::new(format!(
                "Cannot access caller variable '${name}' — not enough caller frames"
            )));
        }
        if !self.is_var_dynamic(name) {
            return Err(crate::runtime::utils::caller_not_dynamic_error(name));
        }
        let idx = stack_len - depth;
        self.caller_env_stack[idx].insert(name.to_string(), value.clone());
        // Also update current env if the variable exists there
        if self.env.contains_key(name) {
            self.env.insert(name.to_string(), value);
        }
        // Single-store coherence: this write targets a *caller frame's* lexical by
        // name (`callframe(d).my.<$x> = v` / `$CALLER::x = v`). `pop_caller_env_
        // with_writeback` propagates it into the restored caller env on return, but
        // the caller's local *slot* is not refreshed when blanket reconcile is off,
        // so the caller reads the stale slot. Record the bare name so the call site
        // drains `env[name]` into that slot. Uses the caller-var list (retain-on-
        // miss) rather than the rw list, so an intervening deeper call the writer
        // makes before returning does not consume it one frame too soon. No-op when
        // no such slot exists; the default build's blanket reconcile makes it
        // redundant = byte-identical.
        self.record_caller_var_writeback(name);
        Ok(())
    }

    /// Record a by-name env write to a caller-frame lexical (retain-on-miss list).
    /// The owning local slot may live several frames up — the writer can make an
    /// intervening deeper call before returning to the owner — so the source is
    /// retained until a frame whose `code` actually has the slot drains it (see
    /// `apply_pending_caller_var_writeback`). Use this (not
    /// `pending_rw_writeback_sources`, which is drop-on-miss / single-frame) when
    /// the write happens inside a nested callee/closure (e.g. a Proxy STORE whose
    /// referent lexical is owned by the caller of the `$proxy = v` assignment).
    pub(crate) fn record_caller_var_writeback(&mut self, name: &str) {
        if !self.pending_caller_var_writeback.iter().any(|n| n == name) {
            self.pending_caller_var_writeback.push(name.to_string());
        }
    }

    /// Look up a variable through $DYNAMIC:: — searches the entire caller stack.
    pub(crate) fn get_dynamic_var(&self, name: &str) -> Result<Value, RuntimeError> {
        // Search from the most recent caller to the oldest
        for env in self.caller_env_stack.iter().rev() {
            if let Some(val) = env.get(name)
                && self.is_var_dynamic(name)
            {
                return Ok(val.clone());
            }
        }
        // Also check current env
        if let Some(val) = self.env.get(name)
            && self.is_var_dynamic(name)
        {
            return Ok(val.clone());
        }
        Err(RuntimeError::new(format!(
            "Cannot find dynamic variable '${name}'"
        )))
    }

    /// Bind a caller variable to a local variable ($CALLER::target := $source).
    /// This creates an alias so that future reads/writes of target go through source.
    pub(crate) fn bind_caller_var(
        &mut self,
        target_name: &str,
        source_name: &str,
        depth: usize,
    ) -> Result<(), RuntimeError> {
        let stack_len = self.caller_env_stack.len();
        if depth > stack_len {
            return Err(RuntimeError::new(format!(
                "Cannot access caller variable '${target_name}' — not enough caller frames"
            )));
        }
        if !self.is_var_dynamic(target_name) {
            return Err(RuntimeError::new(format!(
                "Cannot access '${target_name}' through CALLER, because it is not declared as dynamic"
            )));
        }
        // Copy the current value to the caller env and set up the binding alias
        let source_val = self.env.get(source_name).cloned().unwrap_or(Value::Nil);
        let idx = stack_len - depth;
        self.caller_env_stack[idx].insert(target_name.to_string(), source_val.clone());
        // Set up binding alias so reads of target_name resolve to source_name
        self.var_bindings
            .insert(target_name.to_string(), source_name.to_string());
        // Also update current env
        self.env.insert(target_name.to_string(), source_val);
        Ok(())
    }

    /// Resolve a variable name through bindings (follow aliases).
    pub(crate) fn resolve_binding(&self, name: &str) -> Option<&str> {
        self.var_bindings.get(name).map(|s| s.as_str())
    }

    /// Save and clear var_bindings, returning the saved state.
    pub(crate) fn take_var_bindings(&mut self) -> HashMap<String, String> {
        std::mem::take(&mut self.var_bindings)
    }

    /// Restore previously saved var_bindings.
    pub(crate) fn restore_var_bindings(&mut self, bindings: HashMap<String, String>) {
        self.var_bindings = bindings;
    }

    pub(crate) fn has_class(&self, name: &str) -> bool {
        self.registry().classes.contains_key(name)
    }

    /// True when `name` was declared via the `package`/`module` declarator (and
    /// is therefore not type-like enough to constrain a variable/parameter).
    /// Used to raise X::Syntax::Variable::BadType / X::Parameter::BadType instead
    /// of a generic "not declared" error. A `class`/`role`/`enum`/`subset` is a
    /// real type and is NOT recorded here (it goes through the class registry).
    pub(crate) fn is_declared_package(&self, name: &str) -> bool {
        self.chain_declared_packages.contains(name)
            || matches!(self.env.get(name), Some(Value::Package(_)))
    }

    /// Check if a class name refers to a user-defined class that inherits from
    /// a container type (Hash, Array, etc.). Used to skip element-level type
    /// checking for container subclasses.
    /// True when `name` is a user-declared class / package / module that is NOT
    /// parametric — i.e. parameterizing it with `[T]` (or `of T`) is an error
    /// (X::NotParametric). Roles, built-in container types (Array/Hash/Buf/Blob/
    /// ...), and subclasses of containers ARE parametric and return false.
    pub(crate) fn is_non_parametric_type(&self, name: &str) -> bool {
        // Built-in parametric container types accept `[T]`. Some (Buf, Blob, ...)
        // are registered as classes, so they must be allow-listed here.
        let parametric_builtin = matches!(
            name,
            "Array"
                | "Hash"
                | "Map"
                | "List"
                | "Slip"
                | "Seq"
                | "Range"
                | "Set"
                | "Bag"
                | "Mix"
                | "SetHash"
                | "BagHash"
                | "MixHash"
                | "Buf"
                | "Blob"
                | "buf8"
                | "buf16"
                | "buf32"
                | "buf64"
                | "blob8"
                | "blob16"
                | "blob32"
                | "blob64"
                | "Positional"
                | "Associative"
                | "Iterable"
        );
        !parametric_builtin
            && !self.is_role(name)
            && !self.is_container_subclass(name)
            && (self.has_class(name) || self.is_declared_package(name))
    }

    pub(crate) fn is_container_subclass(&self, name: &str) -> bool {
        const CONTAINER_TYPES: &[&str] = &[
            "Hash", "Array", "Map", "List", "Bag", "Set", "Mix", "BagHash", "SetHash", "MixHash",
            "Seq",
        ];
        // Clone out the parents under a single guard, then recurse without holding it.
        let parents = {
            let reg = self.registry();
            reg.classes
                .get(name)
                .or_else(|| {
                    reg.classes
                        .iter()
                        .find(|(k, _)| k.rsplit_once("::").is_some_and(|(_, short)| short == name))
                        .map(|(_, v)| v)
                })
                .map(|cd| cd.parents.clone())
        };
        if let Some(parents) = parents {
            for parent in &parents {
                if CONTAINER_TYPES.contains(&parent.as_str()) {
                    return true;
                }
                if self.is_container_subclass(parent) {
                    return true;
                }
            }
        }
        false
    }

    /// Check if a class inherits from an immutable Setty type (Set, Bag, Mix).
    /// Whether a user-declared class `name` inherits (transitively) from the base
    /// `Exception` type (or a built-in `X::`/`CX::` exception). Walks the registry
    /// parent chain with a shared borrow.
    pub(crate) fn class_inherits_from_exception(&self, name: &str) -> bool {
        let parents = {
            let reg = self.registry();
            reg.classes
                .get(name)
                .or_else(|| {
                    reg.classes
                        .iter()
                        .find(|(k, _)| k.rsplit_once("::").is_some_and(|(_, short)| short == name))
                        .map(|(_, v)| v)
                })
                .map(|cd| cd.parents.clone())
        };
        if let Some(parents) = parents {
            for parent in &parents {
                if parent == "Exception"
                    || parent.starts_with("X::")
                    || parent.starts_with("CX::")
                    || self.class_inherits_from_exception(parent)
                {
                    return true;
                }
            }
        }
        false
    }

    pub(crate) fn class_inherits_from_immutable_setty(&self, name: &str) -> bool {
        const IMMUTABLE_SETTY: &[&str] = &["Set", "Bag", "Mix"];
        if IMMUTABLE_SETTY.contains(&name) {
            return true;
        }
        // Clone out the parents under a single guard, then recurse without holding it.
        let parents = {
            let reg = self.registry();
            reg.classes
                .get(name)
                .or_else(|| {
                    reg.classes
                        .iter()
                        .find(|(k, _)| k.rsplit_once("::").is_some_and(|(_, short)| short == name))
                        .map(|(_, v)| v)
                })
                .map(|cd| cd.parents.clone())
        };
        if let Some(parents) = parents {
            for parent in &parents {
                if IMMUTABLE_SETTY.contains(&parent.as_str()) {
                    return true;
                }
                if self.class_inherits_from_immutable_setty(parent) {
                    return true;
                }
            }
        }
        false
    }

    /// Check if a class has scoped subs declared in its body.
    pub(crate) fn has_class_scoped_subs(&self, class_name: &str) -> bool {
        // Single guard for both reads (no user-code re-entry here, so let-binding
        // is safe and avoids a same-thread recursive read lock).
        let registry = self.registry();
        registry
            .class_subs
            .get(class_name)
            .is_some_and(|subs| !subs.is_empty())
            || registry.class_subs.keys().any(|k| {
                k.ends_with(&format!("::{}", class_name))
                    || class_name.ends_with(&format!("::{}", k))
            })
    }

    /// Read access to the shared declaration [`Registry`]. Returns a temporary
    /// guard — NEVER `let`-bind it across a call that re-enters user-code
    /// execution (`eval_block_value`/`run_block_raw`/`call_function`): `RwLock`
    /// is not reentrant and would deadlock. Use as `self.registry().subsets...`.
    ///
    /// In debug builds the returned guard detects a re-entrant lock attempt on
    /// this thread and panics with a located message instead of deadlocking (see
    /// [`RegistryReadGuard`](crate::runtime::registry::RegistryReadGuard)).
    #[inline]
    pub(crate) fn registry(&self) -> crate::runtime::registry::RegistryReadGuard<'_> {
        crate::runtime::registry::RegistryReadGuard::new(&self.registry, "registry")
    }

    /// Write access to the shared declaration [`Registry`]. Same guard discipline
    /// as [`Self::registry`].
    #[inline]
    pub(crate) fn registry_mut(&self) -> crate::runtime::registry::RegistryWriteGuard<'_> {
        crate::runtime::registry::RegistryWriteGuard::new(&self.registry, "registry")
    }

    /// Read access to the shared [`IoHandleTable`](io_handles::IoHandleTable).
    /// Same guard discipline as [`Self::registry`]: never hold the returned guard
    /// across a call that re-enters another handle operation (`RwLock` is not
    /// reentrant). Use as `self.io_handles().map.get(&id)`.
    #[inline]
    pub(crate) fn io_handles(&self) -> io_handles::IoHandlesReadGuard<'_> {
        io_handles::IoHandlesReadGuard::new(&self.io_handles, "io_handles")
    }

    /// Read access to the shared [`OutputSink`]. Same guard discipline as
    /// [`Self::io_handles`]: never hold the returned guard across a call that
    /// re-enters another output operation.
    #[inline]
    pub(crate) fn output_sink(&self) -> output_sink::OutputSinkReadGuard<'_> {
        output_sink::OutputSinkReadGuard::new(&self.output_sink, "output_sink")
    }

    /// Write access to the shared [`OutputSink`]. Same guard discipline as
    /// [`Self::output_sink`].
    #[inline]
    pub(crate) fn output_sink_mut(&self) -> output_sink::OutputSinkWriteGuard<'_> {
        output_sink::OutputSinkWriteGuard::new(&self.output_sink, "output_sink")
    }

    /// Write access to the shared [`IoHandleTable`](io_handles::IoHandleTable).
    /// Same guard discipline as [`Self::io_handles`].
    #[inline]
    pub(crate) fn io_handles_mut(&self) -> io_handles::IoHandlesWriteGuard<'_> {
        io_handles::IoHandlesWriteGuard::new(&self.io_handles, "io_handles")
    }

    /// Whether a TAP subtest is currently in progress. The VM queries this (the
    /// TAP state machine stays interpreter-owned) to pass `subtest_active` into
    /// the output sink's emit decision for VM-native Stdout/Stderr output.
    pub(crate) fn subtest_active(&self) -> bool {
        self.tap.subtest_depth() != 0
    }

    /// Allocate a fresh handle id, store `state` under it, and return the id.
    /// Build `state` fully (including anything that needs `&self`, e.g.
    /// `default_line_separators()`) *before* calling this, since the short write
    /// guard taken here must not be held across other `self` operations.
    pub(super) fn insert_handle_state(&mut self, state: IoHandleState) -> usize {
        let mut table = self.io_handles_mut();
        let id = table.next_id;
        table.next_id += 1;
        table.map.insert(id, state);
        id
    }

    /// Normalize a scoped `state`-variable storage key into a form stable across
    /// mutsu's two compilations of the same routine (the registered body `&f` and
    /// the on-the-fly multi-candidate body `&f/0` reach `state $n` under different
    /// `current_package` suffixes and opcode positions). Used as the cross-thread
    /// shared-cell key so `start f()` and a direct `f()` agree (Track C). Strips a
    /// trailing `@<digits>` (opcode position) and any `/<digits>` candidate suffix.
    pub(crate) fn normalize_state_key(key: &str) -> String {
        let trimmed = match key.rfind('@') {
            Some(pos)
                if pos + 1 < key.len() && key[pos + 1..].bytes().all(|b| b.is_ascii_digit()) =>
            {
                &key[..pos]
            }
            _ => key,
        };
        let mut out = String::with_capacity(trimmed.len());
        let mut chars = trimmed.chars().peekable();
        while let Some(c) = chars.next() {
            if c == '/' && chars.peek().is_some_and(|n| n.is_ascii_digit()) {
                while chars.peek().is_some_and(|n| n.is_ascii_digit()) {
                    chars.next();
                }
            } else {
                out.push(c);
            }
        }
        out
    }

    /// Create a lightweight clone of this interpreter for use in a spawned thread.
    /// Shares function/class/role/enum definitions but starts with fresh output and test state.
    /// Array (`@`) and scalar (`$`) variables are shared between parent and child via `shared_vars`
    /// so that mutations are visible across threads.
    pub(crate) fn clone_for_thread(&mut self) -> Self {
        // Collapse a scoped (multi-tier overlay) env to a flat one first: the
        // shared-var seeding and the child's env clone below iterate the env
        // overlay-only, which would miss parent-chain lexicals on a scoped env.
        if self.env.is_scoped() {
            self.env = self.env.flattened();
        }
        // Copy user variables into shared_vars so both parent and child see mutations.
        // The compiler stores locals with bare names (no sigil), so we share everything
        // except internal/special variables that should remain thread-local.
        let shared = Arc::clone(&self.shared_vars);
        {
            let mut sv = shared.write().unwrap();
            for (key, val) in &self.env {
                // Skip internal variables and topic variables.
                // Also skip $*CWD/*CWD — in Raku, dynamic variables like $*CWD
                // are thread-local; mutations inside `start` blocks must not
                // propagate back to the parent thread.
                if key == "_"
                    || key == "@_"
                    || key == "/"
                    || key == "!"
                    || key == "$/"
                    || key == "$!"
                    || key == "$*CWD"
                    || key == "*CWD"
                    || key.starts_with("__mutsu_")
                    || key.starts_with("&")
                    || key == "?LINE"
                {
                    continue;
                }
                // Only insert if not already present — existing values may have
                // been updated by earlier threads that are already running.
                sv.entry(key.resolve()).or_insert_with(|| val.clone());
            }
            // Track C: migrate the parent's existing `state` variables into shared
            // cells (keyed by their normalized cross-compilation key) so a routine
            // whose `state` was already mutated before the first thread spawned
            // (`f(); f(); start { f() }`) carries that value into the threads
            // instead of re-initializing from the declaration. Only seeds cells
            // that don't exist yet; the value becomes the cell's initial content.
            for (skey, sval) in &self.state_vars {
                if matches!(sval, Value::ContainerRef(_)) {
                    continue;
                }
                let shared_key =
                    format!("__mutsu_shared_state::{}", Self::normalize_state_key(skey));
                sv.entry(shared_key)
                    .or_insert_with(|| sval.clone().into_container_ref());
            }
        }
        self.shared_vars_active = true;
        let mut referenced_handle_ids = std::collections::HashSet::new();
        for value in self.env.values() {
            if let Some(id) = Self::handle_id_from_value(value) {
                referenced_handle_ids.insert(id);
            }
        }
        let mut cloned_handles = HashMap::new();
        let handles_guard = self.io_handles();
        for (id, handle) in &handles_guard.map {
            if handle.closed || !referenced_handle_ids.contains(id) {
                continue;
            }
            let cloned = IoHandleState {
                target: handle.target,
                mode: handle.mode,
                path: handle.path.clone(),
                line_separators: handle.line_separators.clone(),
                line_chomp: handle.line_chomp,
                encoding: handle.encoding.clone(),
                file: handle.file.as_ref().and_then(|f| f.try_clone().ok()),
                socket: handle.socket.as_ref().and_then(|s| s.try_clone().ok()),
                listener: handle.listener.as_ref().and_then(|l| l.try_clone().ok()),
                closed: handle.closed,
                out_buffer_capacity: handle.out_buffer_capacity,
                out_buffer_pending: handle.out_buffer_pending.clone(),
                bin: handle.bin,
                nl_out: handle.nl_out.clone(),
                bytes_written: handle.bytes_written,
                read_attempted: handle.read_attempted,
                utf16_bom_written: handle.utf16_bom_written,
                utf16_detected_be: handle.utf16_detected_be,
                argfiles_index: handle.argfiles_index,
                argfiles_reader: None, // Cannot clone BufReader; will reopen if needed
                argfiles_paths: handle.argfiles_paths.clone(),
                pending_words: handle.pending_words.clone(),
                close_on_word_exhaust: handle.close_on_word_exhaust,
            };
            cloned_handles.insert(*id, cloned);
        }
        let cloned_next_handle_id = handles_guard.next_id;
        drop(handles_guard);
        // Thread clones write through the parent's shared stdout/stderr buffers
        // so concurrent output interleaves in real chronological order.
        let thread_output_sink = {
            let mut parent_sink = self.output_sink_mut();
            let shared_out = Arc::clone(
                parent_sink
                    .shared_thread_output
                    .get_or_insert_with(|| Arc::new(Mutex::new(String::new()))),
            );
            let shared_err = Arc::clone(
                parent_sink
                    .shared_thread_stderr
                    .get_or_insert_with(|| Arc::new(Mutex::new(String::new()))),
            );
            Arc::new(RwLock::new(OutputSink {
                output: String::new(),
                stderr_output: String::new(),
                output_emitted: false,
                immediate_stdout: false,
                is_thread_clone: true,
                shared_thread_output: Some(shared_out),
                shared_thread_stderr: Some(shared_err),
            }))
        };
        let mut cloned = Self {
            env: self.env.clone(),
            output_sink: thread_output_sink,
            warn_output: String::new(),
            warn_suppression_depth: 0,
            tap: self.tap.clone_for_thread(),
            halted: false,
            exit_code: 0,
            nested_mode: self.nested_mode,
            native_call_specs: self.native_call_specs.clone(),
            operator_assoc: self.operator_assoc.clone(),
            imported_operator_names: self.imported_operator_names.clone(),
            lib_paths: self.lib_paths.clone(),
            io_handles: Arc::new(RwLock::new(io_handles::IoHandleTable {
                map: cloned_handles,
                next_id: cloned_next_handle_id,
            })),
            program_path: self.program_path.clone(),
            // Snapshot (fresh lock), not a shared handle: thread-local registry
            // semantics — child sees a copy, writes don't leak to the parent.
            current_package: Arc::new(RwLock::new(self.current_package())),
            routine_stack: Vec::new(),
            callframe_stack: Vec::new(),
            method_class_stack: Vec::new(),
            constructing_class: None,
            pending_call_arg_sources: None,
            test_pending_callsite_line: None,
            control_handler_depth: 0,
            test_assertion_line_stack: Vec::new(),
            block_stack: Vec::new(),
            doc_comments: HashMap::new(),
            doc_comment_list: Vec::new(),
            why_cache: HashMap::new(),
            type_metadata: self.type_metadata.clone(),
            when_matched: false,
            gather_items: Vec::new(),
            gather_take_limits: Vec::new(),
            block_scope_depth: self.block_scope_depth,
            // Deep-clone the registry into a fresh Arc so the child thread gets an
            // independent snapshot (matches prior per-field clone semantics: the
            // child sees parent declarations but its own new ones don't leak back).
            registry: Arc::new(RwLock::new(self.registry.read().unwrap().clone())),
            proto_dispatch_stack: Vec::new(),
            proto_method_skip: None,
            pending_dispatch_error: None,
            end_phasers: Vec::new(),
            end_phaser_sites: HashSet::new(),
            chroot_root: self.chroot_root.clone(),
            loaded_modules: self.loaded_modules.clone(),
            need_hidden_classes: self.need_hidden_classes.clone(),
            cur_repo: self.cur_repo.clone(),
            package_stash_hidden: self.package_stash_hidden.clone(),
            chain_declared_packages: self.chain_declared_packages.clone(),
            module_packages: self.module_packages.clone(),
            closure_env_overrides: self.closure_env_overrides.clone(),
            predictive_seq_iters: self.predictive_seq_iters.clone(),
            protect_block_cache: HashMap::new(),
            subset_predicate_cache: HashMap::new(),
            private_zeroarg_method_cache: HashMap::new(),
            module_load_stack: Vec::new(),
            current_distribution: self.current_distribution.clone(),
            package_distributions: self.package_distributions.clone(),
            exported_subs: self.exported_subs.clone(),
            exported_vars: self.exported_vars.clone(),
            exported_sub_values: self.exported_sub_values.clone(),
            unit_module_exported_subs: self.unit_module_exported_subs.clone(),
            unit_module_loading_stack: Vec::new(),
            suppress_exports: false,
            in_lvalue_assignment: false,
            in_does_rhs: false,
            trait_mod_writeback_key: None,
            trait_mod_writeback_value: None,
            hash_autovivify: false,
            newline_mode: self.newline_mode,
            import_scope_stack: Vec::new(),
            strict_mode: self.strict_mode,
            fatal_mode: self.fatal_mode,
            our_vars: HashMap::new(),
            state_vars: HashMap::new(),
            // Mirror state_vars: a thread clone starts with no persisted
            // closure captured state (falls back to the captured-env initial
            // values), exactly as before this store existed.
            closure_captured_state: HashMap::new(),
            once_values: self.once_values.clone(),
            once_scope_stack: Vec::new(),
            next_once_scope_id: self.next_once_scope_id,
            var_dynamic_flags: self.var_dynamic_flags.clone(),
            caller_env_stack: Vec::new(),
            var_bindings: HashMap::new(),
            variables_pragma: self.variables_pragma.clone(),
            attributes_pragma: self.attributes_pragma.clone(),
            var_type_constraints: self.var_type_constraints.clone(),
            // Inherit monotonically: if the parent ever registered an atomic var,
            // the child (which shares the atomic storage via shared_vars) must keep
            // running the atomic-variable read check.
            atomic_var_seen: self.atomic_var_seen,
            var_defaults: self.var_defaults.clone(),
            var_hash_key_constraints: self.var_hash_key_constraints.clone(),
            // Per-thread snapshot (not a shared-handle clone): deep-copy the map
            // into a fresh `Arc` so the child thread's instance type metadata is
            // independent of the parent's, mirroring `io_handles`/`current_package`.
            instance_type_metadata: Arc::new(RwLock::new(
                self.instance_type_metadata.read().unwrap().clone(),
            )),
            let_saves: Vec::new(),
            supply_emit_buffer: Vec::new(),
            supply_emit_timed_buffer: Vec::new(),
            supply_stream_consumers: Vec::new(),
            shared_vars: Arc::clone(&self.shared_vars),
            shared_vars_active: true,
            sigilless_attrs_active: self.sigilless_attrs_active,
            shared_vars_dirty: Arc::clone(&self.shared_vars_dirty),
            encoding_registry: self.encoding_registry.clone(),
            skip_pseudo_method_native: None,
            dispatch_ambiguous: false,
            pending_proxy_subclass_attr: None,
            multi_dispatch_stack: Vec::new(),
            method_dispatch_stack: Vec::new(),
            samewith_context_stack: Vec::new(),
            wrap_chains: self.wrap_chains.clone(),
            wrap_sub_names: self.wrap_sub_names.clone(),
            wrap_name_to_sub: self.wrap_name_to_sub.clone(),
            wrap_callable_ids: self.wrap_callable_ids.clone(),
            wrap_handle_counter: self.wrap_handle_counter,
            wrap_dispatch_stack: Vec::new(),
            method_wrap_chains: self.method_wrap_chains.clone(),
            suppressed_names: self.suppressed_names.clone(),
            poisoned_enum_aliases: self.poisoned_enum_aliases.clone(),
            enum_scope_names: self.enum_scope_names.clone(),
            my_scoped_package_items: self.my_scoped_package_items.clone(),
            lexical_class_scopes: self.lexical_class_scopes.clone(),
            last_value: None,
            pending_local_updates: Vec::new(),
            readonly_vars: HashSet::new(),
            squish_iterator_meta: HashMap::new(),
            custom_type_data: self.custom_type_data.clone(),
            rebless_map: self.rebless_map.clone(),
            action_made: None,
            current_grammar_actions: None,
            pending_regex_error: None,
            precomp_enabled: self.precomp_enabled,
            monkey_typing: self.monkey_typing,

            // Merged VM execution registers (CP-3 collapse): a thread clone starts
            // with fresh per-execution registers, exactly as the former
            // `VM::new(thread_interp)` did for a spawned thread.
            stack: Vec::new(),
            locals: Vec::new(),
            upvalues: Vec::new(),
            in_smartmatch_rhs: false,
            transliterate_in_smartmatch: false,
            substitution_in_smartmatch: false,
            last_topic_value: None,
            topic_save_stack: Vec::new(),
            container_ref_var: None,
            container_ref_reversed: false,
            topic_source_var: None,
            element_source: None,
            quanthash_bind_params: Vec::new(),
            for_param_restore_stack: Vec::new(),
            call_frames: Vec::new(),
            control_handlers: Vec::new(),
            current_code: 0,
            carrier_writes: None,
            method_dispatch_pure: false,
            resume_ip: None,
            bind_context: false,
            scalar_bind_context: false,
            bound_decont_active: false,
            rebind_context: false,
            constant_context: false,
            array_share_context: false,
            array_share_source: None,
            array_share_active: false,
            element_share_pending: false,
            explicit_initializer_context: false,
            vardecl_context: false,
            pending_rw_writeback_sources: Vec::new(),
            pending_caller_var_writeback: Vec::new(),
            local_bind_pairs: Vec::new(),
            otf_compile_cache: HashMap::new(),
            state_scope_id: None,
            fn_resolve_cache: HashMap::new(),
            fn_resolve_gen: 0,
            fn_resolve_cache_gen: 0,
            multi_candidates_cache: HashMap::new(),
            multi_candidates_cache_gen: 0,
            light_call_cache: HashMap::new(),
            light_call_cache_gen: 0,
            pos_light_call_cache: HashMap::new(),
            pos_light_call_cache_gen: 0,
            amp_param_shadowed_names: std::collections::HashSet::new(),
            registered_fn_fingerprints: HashMap::new(),
            prepared_fn_defs: HashMap::new(),
            method_resolve_cache: HashMap::new(),
            last_method_resolve: None,
            fast_method_cache: HashMap::new(),
            multi_resolve_cache: HashMap::new(),
            multi_type_cacheable: HashMap::new(),
            block_declared_vars: Vec::new(),
            loop_local_vars: Vec::new(),
            loop_local_saved_env: Vec::new(),
            loop_cond_active: false,
            outer_scope_locals: Vec::new(),
            enter_result_stack: Vec::new(),
            pending_alias_bind_names: Vec::new(),
            otf_call_cache: HashMap::new(),
            otf_call_cache_gen: 0,
            check_phaser_depth: 0,
            gather_for_loop_resume: None,
            rw_map_topic_capture: None,
        };
        // Raku gives each start block fresh $/ and $! (they are lexically scoped).
        cloned.env.insert("/".to_string(), Value::Nil);
        cloned.env.insert("!".to_string(), Value::Nil);
        cloned.env.insert("$/".to_string(), Value::Nil);
        cloned.env.insert("$!".to_string(), Value::Nil);
        cloned.init_io_environment();
        cloned
    }

    /// Push values into a shared array variable in-place, avoiding full-array
    /// clones on every push.  When the variable lives in `shared_vars` the
    /// lock is held for the entire read-modify-write so concurrent pushes are
    /// safe and O(1) amortised instead of O(n).
    pub(crate) fn push_to_shared_var(
        &mut self,
        key: &str,
        values: Vec<Value>,
        target_fallback: &Value,
    ) -> Value {
        if key.starts_with('@') && self.shared_vars_active {
            // Drop env's copy of the Arc first so that shared_vars holds
            // the only strong reference (refcount=1). This keeps repeated
            // shared pushes in-place instead of degenerating into O(n²) COW.
            self.env.remove(key);
            let is_thread_clone = self.is_thread_clone();
            let mut sv = self.shared_vars.write().unwrap();
            // Try in-place mutation via get_mut first (avoids remove+insert overhead)
            if let Some(Value::Array(arc_items, kind)) = sv.get_mut(key) {
                let items = Arc::make_mut(arc_items);
                items.extend(values);
                if *kind == ArrayKind::List {
                    *kind = ArrayKind::Array;
                }
                let result = Value::Array(Arc::clone(arc_items), *kind);
                drop(sv);
                self.mark_shared_var_dirty(key);
                if !is_thread_clone {
                    self.env.insert(key.to_string(), result.clone());
                }
                return result;
            }
            // Fallback: the value might exist but not be an Array yet
            if let Some(shared_value) = sv.remove(key) {
                if let Value::Array(mut arc_items, kind) = shared_value {
                    let items = Arc::make_mut(&mut arc_items);
                    items.extend(values);
                    let normalized_kind = if kind == ArrayKind::List {
                        ArrayKind::Array
                    } else {
                        kind
                    };
                    let result = Value::Array(Arc::clone(&arc_items), normalized_kind);
                    sv.insert(key.to_string(), Value::Array(arc_items, normalized_kind));
                    drop(sv);
                    self.mark_shared_var_dirty(key);
                    if !is_thread_clone {
                        self.env.insert(key.to_string(), result.clone());
                    }
                    return result;
                }
                sv.insert(key.to_string(), shared_value);
            }
        }
        // Fallback for non-shared arrays: use Arc::make_mut for COW
        if let Some(Value::Array(arc_items, kind)) = self.env.get_mut(key) {
            let items = Arc::make_mut(arc_items);
            items.extend(values);
            // Normalize @-variables only from List to Array while preserving Shaped.
            if key.starts_with('@') && *kind == ArrayKind::List {
                *kind = ArrayKind::Array;
            }
            return Value::Array(Arc::clone(arc_items), *kind);
        }
        let mut items = match target_fallback {
            Value::Array(v, ..) => v.to_vec(),
            _ => Vec::new(),
        };
        items.extend(values);
        let result = Value::real_array(items);
        self.env.insert(key.to_string(), result.clone());
        result
    }

    pub(crate) fn push_to_existing_shared_array(
        &mut self,
        key: &str,
        values: Vec<Value>,
    ) -> Option<Value> {
        if !key.starts_with('@') || !self.shared_vars_active {
            return None;
        }
        let is_thread_clone = self.is_thread_clone();
        if is_thread_clone {
            self.env.remove(key);
        }
        let mut sv = self.shared_vars.write().unwrap();
        let Some(Value::Array(arc_items, kind)) = sv.get_mut(key) else {
            return None;
        };
        let items = Arc::make_mut(arc_items);
        items.extend(values);
        if *kind == ArrayKind::List {
            *kind = ArrayKind::Array;
        }
        let result = Value::Array(Arc::clone(arc_items), *kind);
        drop(sv);
        if is_thread_clone {
            let dirty_marker = format!("__mutsu_shared_dirty::{key}");
            if !self.env.contains_key(&dirty_marker) {
                self.mark_shared_var_dirty(key);
                self.env.insert(dirty_marker, Value::Bool(true));
            }
        } else {
            self.mark_shared_var_dirty(key);
            self.env.insert(key.to_string(), result.clone());
        }
        Some(result)
    }

    /// Track C: assign a single hash element (`%h{$k} = $v`) through the shared
    /// cell so concurrent `start { %h{...} = ... }` blocks all land instead of
    /// each mutating a private snapshot (last-writer-wins). Holds the
    /// `shared_vars` lock for the whole get-make_mut-insert so two threads
    /// writing different keys can't lose each other's update — the same
    /// per-element atomicity `push_to_existing_shared_var` gives `.push`.
    ///
    /// Returns `Some(value)` if it wrote through the shared hash, `None` if the
    /// variable is not a shared hash (caller falls back to the normal path).
    pub(crate) fn assign_hash_elem_to_shared_var(
        &mut self,
        key: &str,
        elem_key: String,
        value: Value,
    ) -> Option<Value> {
        if !key.starts_with('%') || !self.shared_vars_active {
            return None;
        }
        // A genuinely-shared plain lexical `%name` is routed through the
        // `__mutsu_atomic_hash::` shared store, the same way concurrent `.push`
        // is (see `shared_array_extend`). Writing the base key directly lets a
        // sibling thread's `set_shared_var` clobber it with a stale empty
        // snapshot during env sync (lost update). The atomic store is exempt
        // from that clobber. Attribute / dynamic / twigil'd hashes (`%!`, `%.`,
        // `%*`) share a name across instances, so they keep the base-key path;
        // a thread-local `my %h` (not present in shared_vars) also falls through
        // to the normal local assignment.
        if Self::is_plain_lexical_name(key) {
            let atomic_key = format!("__mutsu_atomic_hash::{key}");
            let is_shared = {
                let sv = self.shared_vars.read().unwrap();
                sv.contains_key(&atomic_key) || sv.contains_key(key)
            };
            if is_shared {
                return Some(self.shared_hash_elem_set(key, elem_key, value));
            }
        }
        // Only a genuinely-shared base-key hash takes the write-through path; a
        // thread-local `my %h` or an attribute hash (`%!data`, not stored under
        // its own name in shared_vars) must fall through to the normal local
        // assignment — and crucially must NOT have its env copy dropped, which
        // would discard accumulated element writes.
        {
            let sv = self.shared_vars.read().unwrap();
            if !matches!(sv.get(key), Some(Value::Hash(_))) {
                return None;
            }
        }
        let is_thread_clone = self.is_thread_clone();
        if is_thread_clone {
            // Drop env's private copy so the shared cell holds the only Arc and
            // make_mut mutates in place (the thread's env is discarded anyway).
            self.env.remove(key);
        }
        let mut sv = self.shared_vars.write().unwrap();
        let Some(Value::Hash(arc)) = sv.get_mut(key) else {
            return None;
        };
        Value::hash_insert_through(&mut Arc::make_mut(arc).map, elem_key, value.clone());
        let result = Value::Hash(Arc::clone(arc));
        drop(sv);
        if is_thread_clone {
            // Mark dirty once per key (a per-key env marker avoids re-locking the
            // dirty set on every element write).
            let dirty_marker = format!("__mutsu_shared_dirty::{key}");
            if !self.env.contains_key(&dirty_marker) {
                self.mark_shared_var_dirty(key);
                self.env.insert(dirty_marker, Value::Bool(true));
            }
        } else {
            self.mark_shared_var_dirty(key);
            self.env.insert(key.to_string(), result);
        }
        Some(value)
    }

    /// Track C: assign a single array element (`@a[$i] = $v`) through the shared
    /// cell so concurrent `start { @a[...] = ... }` blocks all land instead of
    /// each mutating a private snapshot (last-writer-wins). Mirrors
    /// `assign_hash_elem_to_shared_var`: holds the `shared_vars` lock across the
    /// whole get -> Arc::make_mut -> (grow with Nil) -> set so two threads
    /// writing different indices can't lose each other's update. `idx` must be a
    /// non-negative element index (the caller rejects negatives).
    ///
    /// Returns `Some(value)` if it wrote through the shared array, `None` if the
    /// variable is not a shared array (caller falls back to the normal path).
    pub(crate) fn assign_array_elem_to_shared_var(
        &mut self,
        key: &str,
        idx: usize,
        value: Value,
    ) -> Option<Value> {
        if !key.starts_with('@') || !self.shared_vars_active {
            return None;
        }
        // A genuinely-shared plain lexical `@name` routes through the
        // `__mutsu_atomic_arr::` shared store (same as concurrent `.push`, see
        // `shared_array_extend`) so a sibling thread's stale `set_shared_var`
        // cannot clobber the merged array. Attribute / dynamic / twigil'd arrays
        // keep the base-key path; a thread-local `my @a` (not present in
        // shared_vars) falls through to the normal local assignment.
        if Self::is_plain_lexical_name(key) {
            let atomic_key = format!("__mutsu_atomic_arr::{key}");
            let is_shared = {
                let sv = self.shared_vars.read().unwrap();
                sv.contains_key(&atomic_key) || sv.contains_key(key)
            };
            if is_shared {
                return Some(self.shared_array_elem_set(key, idx, value));
            }
        }
        // See `assign_hash_elem_to_shared_var`: only a genuinely-shared base-key
        // array takes the write-through path; otherwise fall through without
        // dropping the local env copy.
        {
            let sv = self.shared_vars.read().unwrap();
            if !matches!(sv.get(key), Some(Value::Array(..))) {
                return None;
            }
        }
        let is_thread_clone = self.is_thread_clone();
        if is_thread_clone {
            // Drop env's private copy so the shared cell holds the only Arc and
            // make_mut mutates in place (the thread's env is discarded anyway).
            self.env.remove(key);
        }
        let mut sv = self.shared_vars.write().unwrap();
        let Some(Value::Array(arc, kind)) = sv.get_mut(key) else {
            return None;
        };
        let data = Arc::make_mut(arc);
        if idx >= data.items.len() {
            data.items.resize(idx + 1, Value::Nil);
        }
        data.items[idx] = value.clone();
        if *kind == ArrayKind::List {
            *kind = ArrayKind::Array;
        }
        let result = Value::Array(Arc::clone(arc), *kind);
        drop(sv);
        if is_thread_clone {
            let dirty_marker = format!("__mutsu_shared_dirty::{key}");
            if !self.env.contains_key(&dirty_marker) {
                self.mark_shared_var_dirty(key);
                self.env.insert(dirty_marker, Value::Bool(true));
            }
        } else {
            self.mark_shared_var_dirty(key);
            self.env.insert(key.to_string(), result);
        }
        Some(value)
    }

    /// Read a shared variable. If the variable is in shared_vars, return
    /// the shared version (which may have been mutated by another thread).
    #[allow(dead_code)]
    pub(crate) fn get_shared_var(&self, key: &str) -> Option<Value> {
        let sv = self.shared_vars.read().unwrap();
        sv.get(key).cloned()
    }

    /// Returns true if the given key is in the shared_vars_dirty set
    /// (i.e., was modified by an atomic/CAS operation).
    pub(crate) fn is_shared_var_dirty(&self, key: &str) -> bool {
        self.shared_vars_dirty
            .read()
            .map(|d| d.contains(key))
            .unwrap_or(false)
    }

    fn mark_shared_var_dirty(&self, key: &str) {
        if self
            .shared_vars_dirty
            .read()
            .ok()
            .is_some_and(|dirty| dirty.contains(key))
        {
            return;
        }
        if let Ok(mut dirty) = self.shared_vars_dirty.write() {
            dirty.insert(key.to_string());
        }
    }

    /// Write a shared variable. Updates both the local env and shared_vars.
    pub(crate) fn set_shared_var(&mut self, key: &str, value: Value) {
        // Ensure @-variables always store Array(true) (real Arrays)
        let value = if key.starts_with('@') {
            match value {
                // Preserve Shaped arrays; only normalize List to Array.
                Value::Array(items, ArrayKind::List) => Value::Array(items, ArrayKind::Array),
                other => other,
            }
        } else {
            value
        };
        self.env.insert(key.to_string(), value.clone());
        if self.shared_vars_active {
            let mut sv = self.shared_vars.write().unwrap();
            // Skip overwriting @-variables that have an active CAS atomic
            // copy — the atomic copy is the authoritative source of truth
            // and must not be clobbered by stale local snapshots.
            if key.starts_with('@') {
                let atomic_key = format!("__mutsu_atomic_arr::{key}");
                if sv.contains_key(&atomic_key) {
                    return;
                }
            } else if key.starts_with('%') {
                // Symmetric to the array case: a hash with an active atomic
                // entry (concurrent element assignment) must not be clobbered
                // by a stale local snapshot during env sync.
                let atomic_key = format!("__mutsu_atomic_hash::{key}");
                if sv.contains_key(&atomic_key) {
                    return;
                }
            }
            if sv.contains_key(key) {
                sv.insert(key.to_string(), value);
                // Mark this key as explicitly updated so sync_shared_vars_to_env
                // knows to propagate it (vs keys only initialized by clone_for_thread).
                self.mark_shared_var_dirty(key);
            }
        }
    }

    /// Clear atomic array CAS state for a variable. Called when the variable
    /// is genuinely re-declared (e.g., new loop iteration with `my @arr`).
    pub(crate) fn clear_atomic_array_state(&self, key: &str) {
        if !key.starts_with('@') {
            return;
        }
        let atomic_key = format!("__mutsu_atomic_arr::{key}");
        if let Ok(mut sv) = self.shared_vars.write() {
            sv.remove(&atomic_key);
        }
    }

    /// Hash analogue of `clear_atomic_array_state`: drop the
    /// `__mutsu_atomic_hash::` shared-store entry when a `%`-variable is
    /// (re-)declared, so a fresh `my %h` does not seed from a previous lexical's
    /// concurrent element writes (e.g. inside a loop body).
    pub(crate) fn clear_atomic_hash_state(&self, key: &str) {
        if !key.starts_with('%') {
            return;
        }
        let atomic_key = format!("__mutsu_atomic_hash::{key}");
        if let Ok(mut sv) = self.shared_vars.write() {
            sv.remove(&atomic_key);
        }
    }

    /// Sync shared variables back from shared_vars into the local env.
    /// Only syncs keys that were explicitly updated via `set_shared_var`
    /// (tracked in `shared_vars_dirty`), so that function parameters
    /// initialized by `clone_for_thread` are not overwritten with stale values.
    pub(crate) fn sync_shared_vars_to_env(&mut self) {
        // Collect dirty keys first, then drop the lock before acquiring
        // shared_vars lock.  This avoids a lock-ordering deadlock:
        // set_shared_var acquires shared_vars then shared_vars_dirty,
        // so we must not hold shared_vars_dirty while acquiring shared_vars.
        let dirty_keys: Vec<String> = {
            let dirty = self.shared_vars_dirty.read().unwrap();
            dirty.iter().cloned().collect()
        };
        if dirty_keys.is_empty() {
            return;
        }
        let updates: Vec<(String, Value)> = {
            let sv = self.shared_vars.read().unwrap();
            let mut updates = Vec::new();
            for key in &dirty_keys {
                // Atomic ops store the value under an internal shared key, while
                // dirty tracking also marks the user-visible variable name.
                let name_key = format!("__mutsu_atomic_name::{key}");
                let value_key = match sv.get(&name_key).or_else(|| self.env.get(&name_key)) {
                    Some(Value::Str(vk)) => Some(vk.as_ref().clone()),
                    _ => None,
                };
                if let Some(value_key) = value_key
                    && let Some(val) = sv.get(value_key.as_str())
                {
                    updates.push((key.clone(), val.clone()));
                    continue;
                }

                // Check for atomic array CAS storage
                let atomic_arr_key = format!("__mutsu_atomic_arr::{key}");
                if let Some(val) = sv.get(&atomic_arr_key) {
                    updates.push((key.clone(), val.clone()));
                    continue;
                }

                // Check for atomic hash CAS storage
                let atomic_hash_key = format!("__mutsu_atomic_hash::{key}");
                if let Some(val) = sv.get(&atomic_hash_key) {
                    updates.push((key.clone(), val.clone()));
                    continue;
                }

                if let Some(val) = sv.get(key) {
                    updates.push((key.clone(), val.clone()));
                }
            }
            updates
        };
        // Instance attribute CAS updates need no propagation here: cell-CAS
        // mutates the receiver's shared attribute cell in place, so every
        // alias (including the parent thread's) observes the swap directly.
        //
        // Slice 1b (cross-thread cell sharing): a cross-thread update (e.g. a
        // worker `start { cas $seen, … }` whose result is `await`ed) lands in the
        // parent env here, but the parent's matching *local slot* is not refreshed
        // unless the blanket reconcile is on. Record each synced caller-visible
        // name so the await/`.result` call site drains it straight to the caller's
        // slot (`apply_pending_rw_writeback`), dropping the reverse-sync dependency.
        //
        // The synced var's owning slot may live an *unknown number of frames up*:
        // `await` runs `run_pending_instance_destroys()` (DESTROY dispatch, empty
        // `locals`) between the shared-var sync and returning to the top-level
        // frame that owns the slot. A drop-on-miss list (`pending_rw_writeback_sources`)
        // would be consumed and discarded by those intervening DESTROY frames
        // before reaching the owner. Use the retain-on-miss list
        // (`pending_caller_var_writeback`) instead, which carries the source up the
        // frame chain until the frame whose `code` actually has the slot drains it.
        for (key, val) in updates {
            if !self.pending_caller_var_writeback.contains(&key) {
                self.pending_caller_var_writeback.push(key.clone());
            }
            self.env.insert(key, val);
        }
    }

    /// Sync shared vars for a narrow set of captured lexical names.
    /// This is used by hot paths (e.g. Lock::Async.protect) to avoid scanning
    /// large closure environments on every invocation.
    pub(crate) fn sync_shared_vars_for_names<'a, I>(&mut self, names: I)
    where
        I: IntoIterator<Item = &'a str>,
    {
        if !self.shared_vars_active {
            return;
        }
        let sv = self.shared_vars.read().unwrap();
        for name in names {
            if let Some(val) = sv.get(name) {
                if matches!(val, Value::Array(..) | Value::Hash(..)) {
                    self.env.remove(name);
                } else {
                    self.env.insert(name.to_string(), val.clone());
                }
            }
        }
    }

    pub(crate) fn clear_private_zeroarg_method_cache(&mut self) {
        self.private_zeroarg_method_cache.clear();
    }

    pub(crate) fn reset_atomic_var_key(&mut self, name: &str) {
        let name_key = format!("__mutsu_atomic_name::{name}");
        let Some(Value::Str(value_key)) = self.env.remove(&name_key) else {
            return;
        };
        let mut shared = self.shared_vars.write().unwrap();
        shared.remove(value_key.as_str());
        shared.remove(&name_key);
    }

    pub(crate) fn reset_atomic_var_key_decl(&mut self, name: &str) {
        let name_key = format!("__mutsu_atomic_name::{name}");
        self.env.remove(&name_key);
        let mut shared = self.shared_vars.write().unwrap();
        if let Some(Value::Str(value_key)) = shared.remove(&name_key) {
            shared.remove(value_key.as_str());
        }
    }

    pub(crate) fn merge_sigilless_alias_writes(&self, saved_env: &mut Env, current_env: &Env) {
        for (key, alias) in current_env.iter() {
            if !key.starts_with("__mutsu_sigilless_alias::") {
                continue;
            }
            if !key.starts_with("__mutsu_sigilless_alias::!") {
                continue;
            }
            let Value::Str(alias_name) = alias else {
                continue;
            };
            if let Some(bare) = alias_name
                .strip_prefix('$')
                .or_else(|| alias_name.strip_prefix('@'))
                .or_else(|| alias_name.strip_prefix('%'))
                .or_else(|| alias_name.strip_prefix('&'))
                && let Some(value) = current_env.get(bare).cloned()
            {
                saved_env.insert(alias_name.to_string(), value.clone());
                saved_env.insert(bare.to_string(), value);
                continue;
            }
            saved_env.insert_sym(*key, alias.clone());
            if let Some(value) = current_env.get(alias_name.as_str()).cloned() {
                saved_env.insert(alias_name.to_string(), value);
                continue;
            }
            if let Some(bare_name) = key.strip_prefix_str("__mutsu_sigilless_alias::")
                && let Some(value) = current_env.get(&bare_name).cloned()
            {
                saved_env.insert(bare_name.to_string(), value.clone());
                saved_env.insert(alias_name.to_string(), value);
            }
        }
        for (key, value) in current_env.iter() {
            if key.starts_with("__mutsu_predictive_seq_iter::")
                || key.starts_with("__mutsu_sigilless_alias::!")
            {
                saved_env.insert_sym(*key, value.clone());
            }
        }
    }
}

impl Interpreter {
    /// Flush all open file handle buffers. Call before process exit.
    pub fn flush_all_handles(&mut self) {
        for state in self.io_handles_mut().map.values_mut() {
            if state.closed {
                continue;
            }
            if !state.out_buffer_pending.is_empty()
                && let Some(file) = state.file.as_mut()
            {
                let _ = file.write_all(&state.out_buffer_pending);
                let _ = file.flush();
                state.out_buffer_pending.clear();
            }
        }
    }
}

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
