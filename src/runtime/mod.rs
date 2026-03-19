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
use crate::opcode::{CompiledCode, CompiledFunction, OpCode};
use crate::parse_dispatch;
use crate::value::{
    ArrayKind, EnumValue, JunctionKind, LazyList, RuntimeError, SharedChannel, SharedPromise,
    Value, make_rat, take_pending_instance_destroys,
};
use num_traits::Signed;

/// Flatten arguments for `append` using Raku's "one-arg rule":
/// if exactly one non-itemized Array/List argument is passed, its elements
/// are flattened into the result. With multiple arguments, each is appended as-is.
fn flatten_append_args(args: Vec<Value>) -> Vec<Value> {
    if args.len() == 1 {
        match &args[0] {
            Value::Array(vals, kind) if !kind.is_itemized() => vals.to_vec(),
            Value::Hash(map) => map
                .iter()
                .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
                .collect(),
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

type ProtectBlockCacheEntry = (
    Arc<CompiledCode>,
    Arc<HashMap<String, CompiledFunction>>,
    Arc<Vec<(usize, String)>>,
    Arc<Vec<(usize, String)>>,
    Arc<Vec<String>>,
    Arc<Vec<String>>,
);
type ProtectBlockCache = HashMap<u64, ProtectBlockCacheEntry>;

mod accessors;
mod builtins;
mod builtins_atomic;
mod builtins_coerce;
mod builtins_collection;
mod builtins_io;
mod builtins_operators;
pub(crate) mod builtins_reduce;
mod builtins_string;
mod builtins_system;
mod call_helpers;
mod calls;
mod class;
mod dispatch;
mod handle;
mod io;
mod metamodel;
mod methods;
mod methods_classhow;
mod methods_collection;
mod methods_collection_ops;
mod methods_enum_dispatch;
mod methods_grammar;
mod methods_instance_ops;
mod methods_introspect;
mod methods_io_dispatch;
mod methods_match_dispatch;
mod methods_mut;
mod methods_object;
mod methods_promise;
mod methods_promise_class;
mod methods_seq_dispatch;
mod methods_signature;
mod methods_string;
mod methods_sub;
mod methods_supply_dispatch;
mod methods_temporal;
mod methods_trans;
mod methods_type_coerce;
mod native_io;
pub(crate) mod native_methods;
mod native_proc_async;
mod native_supply_methods;
pub(crate) mod native_types;
mod ops;
pub(crate) mod phasers;
mod regex;
mod regex_parse;
mod registration;
mod registration_class;
mod registration_sub;
mod resolution;
mod run;
mod seq_helpers;
mod sequence;
mod signal_watcher;
pub(super) mod sprintf;
mod subtest;
mod system;
mod test_functions;
pub(crate) mod types;
mod unicode;
pub(crate) mod utils;
pub(crate) use self::registration_class::ClassDeclModifiers;

pub(crate) use utils::*;

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

#[derive(Clone, Default)]
struct ClassDef {
    parents: Vec<String>,
    // (name, is_public, default, is_rw, is_required, sigil, where_constraint)
    attributes: Vec<ClassAttributeDef>,
    attribute_types: HashMap<String, String>, // attr_name -> type constraint
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
}

#[derive(Debug, Clone)]
struct RoleCandidateDef {
    type_params: Vec<String>,
    type_param_defs: Vec<ParamDef>,
    role_def: RoleDef,
}

#[derive(Debug, Clone)]
struct SubsetDef {
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
}

#[derive(Debug, Clone)]
struct MethodDispatchFrame {
    receiver_class: String,
    invocant: Value,
    args: Vec<Value>,
    remaining: Vec<(String, MethodDef)>,
}

/// Frame for navigating through wrapper chain during callsame/callwith.
#[derive(Debug, Clone)]
struct WrapDispatchFrame {
    /// The sub id being wrapped (to prevent re-entrant wrap dispatch).
    sub_id: u64,
    /// Remaining callables: inner wrappers then original sub. Next to call is first.
    remaining: Vec<Value>,
    /// Original call arguments.
    args: Vec<Value>,
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

#[derive(Debug)]
struct IoHandleState {
    target: IoHandleTarget,
    mode: IoHandleMode,
    path: Option<String>,
    line_separators: Vec<Vec<u8>>,
    line_chomp: bool,
    encoding: String,
    file: Option<fs::File>,
    socket: Option<std::net::TcpStream>,
    listener: Option<std::net::TcpListener>,
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

/// Entry in the callframe stack, tracking state for each call frame.
#[derive(Clone)]
pub(crate) struct CallFrameEntry {
    pub file: String,
    pub line: i64,
    pub code: Option<Value>,
    pub env: Env,
}

pub struct Interpreter {
    env: Env,
    output: String,
    stderr_output: String,
    warn_output: String,
    warn_suppression_depth: usize,
    test_state: Option<TestState>,
    subtest_depth: usize,
    halted: bool,
    exit_code: i64,
    /// When true, output is flushed to real stdout immediately (for Proc::Async children).
    immediate_stdout: bool,
    /// Tracks whether any output was emitted (useful when immediate_stdout
    /// skips the buffer).
    output_emitted: bool,
    bailed_out: bool,
    functions: HashMap<Symbol, FunctionDef>,
    operator_assoc: HashMap<String, String>,
    proto_functions: HashMap<Symbol, FunctionDef>,
    token_defs: HashMap<Symbol, Vec<FunctionDef>>,
    lib_paths: Vec<String>,
    handles: HashMap<usize, IoHandleState>,
    next_handle_id: usize,
    program_path: Option<String>,
    current_package: String,
    routine_stack: Vec<(String, String)>,
    callframe_stack: Vec<CallFrameEntry>,
    method_class_stack: Vec<String>,
    pending_call_arg_sources: Option<Vec<Option<String>>>,
    test_pending_callsite_line: Option<i64>,
    test_assertion_line_stack: Vec<i64>,
    block_stack: Vec<Value>,
    doc_comments: HashMap<String, String>,
    type_metadata: HashMap<String, HashMap<String, Value>>,
    when_matched: bool,
    gather_items: Vec<Vec<Value>>,
    gather_take_limits: Vec<Option<usize>>,
    block_scope_depth: usize,
    enum_types: HashMap<String, Vec<(String, EnumValue)>>,
    classes: HashMap<String, ClassDef>,
    cunion_classes: HashSet<String>,
    hidden_classes: HashSet<String>,
    /// Classes that were declared as stubs (`class Foo { ... }`).
    class_stubs: HashSet<String>,
    /// Packages/modules that were declared as stubs (`module Foo { ... }`).
    pub(crate) package_stubs: HashSet<String>,
    hidden_defer_parents: HashMap<String, HashSet<String>>,
    class_trusts: HashMap<String, HashSet<String>>,
    class_composed_roles: HashMap<String, Vec<String>>, // class -> roles composed via `does`
    roles: HashMap<String, RoleDef>,
    role_candidates: HashMap<String, Vec<RoleCandidateDef>>,
    role_parents: HashMap<String, Vec<String>>,
    role_hides: HashMap<String, Vec<String>>,
    role_type_params: HashMap<String, Vec<String>>,
    class_role_param_bindings: HashMap<String, HashMap<String, Value>>,
    attribute_build_overrides: HashMap<(String, String), Value>,
    subsets: HashMap<String, SubsetDef>,
    proto_subs: HashSet<String>,
    proto_tokens: HashSet<String>,
    proto_dispatch_stack: Vec<(String, Vec<Value>)>,
    pending_dispatch_error: Option<RuntimeError>,
    end_phasers: Vec<(Vec<Stmt>, Env)>,
    chroot_root: Option<PathBuf>,
    loaded_modules: HashSet<String>,
    need_hidden_classes: HashSet<String>,
    closure_env_overrides: HashMap<u64, Env>,
    protect_block_cache: ProtectBlockCache,
    private_zeroarg_method_cache: HashMap<(String, String), Option<(String, MethodDef)>>,
    module_load_stack: Vec<String>,
    /// Exported subroutine symbols by package and export tag.
    exported_subs: HashMap<String, HashMap<String, HashSet<String>>>,
    /// Exported variable/constant symbols by package and export tag.
    exported_vars: HashMap<String, HashMap<String, HashSet<String>>>,
    /// When true, `is export` trait is ignored (used by `need` to load without importing).
    pub(crate) suppress_exports: bool,
    /// When true, rw routine calls should not auto-FETCH Proxy return values.
    pub(crate) in_lvalue_assignment: bool,
    pub(crate) newline_mode: NewlineMode,
    /// Stack of snapshots for lexical import scoping.
    /// Each entry saves (function_keys, class_names, newline_mode, strict_mode, fatal_mode)
    /// before a block with `use`.
    import_scope_stack: Vec<ImportScopeSnapshot>,
    pub(crate) strict_mode: bool,
    pub(crate) fatal_mode: bool,
    state_vars: HashMap<String, Value>,
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
    /// Variable type constraints used to enforce typed re-assignment across closures.
    var_type_constraints: HashMap<String, String>,
    /// Variable default values set by `is default(...)` trait.
    var_defaults: HashMap<String, Value>,
    /// Container element defaults for arrays/hashes with `is default(...)`,
    /// keyed by Arc pointer identity.
    container_defaults: HashMap<usize, Value>,
    /// Optional hash key type constraints (e.g. `%h{Str}`).
    var_hash_key_constraints: HashMap<String, String>,
    /// Type metadata for Array values keyed by Arc pointer identity.
    array_type_metadata: HashMap<usize, ContainerTypeInfo>,
    /// Type metadata for Mix values keyed by Arc pointer identity.
    mix_type_metadata: HashMap<usize, ContainerTypeInfo>,
    /// Type metadata for Hash values keyed by Arc pointer identity.
    hash_type_metadata: HashMap<usize, ContainerTypeInfo>,
    /// Type metadata for instance values keyed by stable instance id.
    instance_type_metadata: HashMap<u64, ContainerTypeInfo>,
    let_saves: Vec<(String, Value, bool)>,
    pub(super) supply_emit_buffer: Vec<Vec<Value>>,
    pub(super) supply_emit_timed_buffer: Vec<Vec<(Value, std::time::Instant)>>,
    /// Shared variables between threads. When `start` spawns a thread,
    /// variables are stored here so both parent and child can see mutations.
    shared_vars: Arc<RwLock<HashMap<String, Value>>>,
    /// True when this interpreter participates in cross-thread variable sharing.
    /// Set by `clone_for_thread` on both parent and child.
    shared_vars_active: bool,
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
    /// Pending Proxy subclass attribute reference for writeback on mutating methods.
    /// Set when reading a Proxy subclass attribute; consumed by subsequent .push/.pop etc.
    pub(crate) pending_proxy_subclass_attr: Option<(crate::value::ProxySubclassAttrs, String)>,
    /// Stack of remaining multi dispatch candidates for callsame/nextsame/nextcallee.
    /// Each entry is (function_name, remaining_candidates, original_args).
    multi_dispatch_stack: Vec<(String, Vec<FunctionDef>, Vec<Value>)>,
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
    /// Counter for generating unique wrap handle IDs.
    wrap_handle_counter: u64,
    /// Stack of wrap dispatch frames for callsame/callwith inside wrappers.
    wrap_dispatch_stack: Vec<WrapDispatchFrame>,
    /// Names suppressed by `anon class`. These bare words should error as undeclared.
    suppressed_names: HashSet<String>,
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
    /// Pending error from regex security validation, to be propagated by the caller.
    #[allow(dead_code)]
    pending_regex_error: Option<RuntimeError>,
    /// When true, module precompilation cache is enabled.
    precomp_enabled: bool,
    /// When true, `augment class` is allowed (set by `use MONKEY-TYPING` or `use MONKEY`).
    monkey_typing: bool,
    /// Shared output buffer for concurrent threads (used by `start` blocks).
    /// When set on a thread-cloned interpreter, `emit_output` writes here instead
    /// of the per-interpreter buffer, preserving the real interleaving order of
    /// concurrent `say`/`print` calls.
    shared_thread_output: Option<Arc<Mutex<String>>>,
    /// Shared stderr buffer for concurrent threads.
    shared_thread_stderr: Option<Arc<Mutex<String>>>,
    /// True when this interpreter is a thread clone (should use shared output).
    is_thread_clone: bool,
    /// Deprecation tracker: maps a deprecation key (e.g. "Sub name (from GLOBAL)")
    /// to (message, Vec<(file, line)>) recording each call site.
    deprecation_tracker: HashMap<String, DeprecationRecord>,
    /// Maps function qualified name (e.g. "GLOBAL::a") to deprecation tracker key.
    deprecated_functions: HashMap<String, String>,
    /// Maps (class_name, method_name) to deprecation tracker key.
    deprecated_methods: HashMap<(String, String), String>,
}

/// Record of deprecation usage for a single deprecated callable.
#[derive(Debug, Clone)]
pub(crate) struct DeprecationRecord {
    /// "Sub" or "Method"
    pub(crate) kind: String,
    /// The callable name (e.g. "a", "foo")
    pub(crate) name: String,
    /// The package/class that defined it
    pub(crate) package: String,
    /// The deprecation message (None = use default "something else")
    pub(crate) message: Option<String>,
    /// Call sites: (file, line)
    pub(crate) call_sites: Vec<(String, i64)>,
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

#[derive(Debug, Clone)]
pub(crate) struct ContainerTypeInfo {
    pub(crate) value_type: String,
    pub(crate) key_type: Option<String>,
    pub(crate) declared_type: Option<String>,
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
    HashMap<Symbol, FunctionDef>,
    HashMap<Symbol, FunctionDef>,
    HashMap<Symbol, Vec<FunctionDef>>,
    HashSet<String>,
    HashSet<String>,
);

pub(crate) type ImportScopeSnapshot = (HashSet<Symbol>, HashSet<String>, NewlineMode, bool, bool);

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter {
    /// Take any pending regex security error from the thread-local store.
    pub(crate) fn take_pending_regex_error() -> Option<RuntimeError> {
        // Delegate to the regex_parse module's thread-local error store
        regex_parse::PENDING_REGEX_ERROR.with(|e| e.borrow_mut().take())
    }

    pub fn new() -> Self {
        let mut env = HashMap::new();
        env.insert("*PID".to_string(), Value::Int(current_process_id()));
        env.insert("@*ARGS".to_string(), Value::real_array(Vec::new()));
        env.insert("*INIT-INSTANT".to_string(), Value::make_instant_now());
        // Populate %*ENV with all OS environment variables so that
        // %*ENV.keys, %*ENV.elems, and copying %*ENV work correctly.
        {
            let mut env_hash = HashMap::new();
            #[cfg(not(target_family = "wasm"))]
            for (key, value) in std::env::vars() {
                env_hash.insert(key, Value::str(value));
            }
            env.insert(
                "%*ENV".to_string(),
                Value::Hash(std::sync::Arc::new(env_hash)),
            );
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
                    "collate",
                    "lines",
                    "merge",
                    "unique",
                    "on-close",
                    "classify",
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
                native_methods: ["emit", "done", "quit", "Supply"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["Supplier".to_string()],
                attribute_types: HashMap::new(),
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
                    "exitcode", "signal", "command", "pid", "err", "out", "Numeric", "Int", "Bool",
                    "Str", "gist",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["Proc".to_string()],
                attribute_types: HashMap::new(),
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
                native_methods: ["cue"].iter().map(|s| s.to_string()).collect(),
                mro: vec!["ThreadPoolScheduler".to_string()],
                attribute_types: HashMap::new(),
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
                native_methods: ["cue"].iter().map(|s| s.to_string()).collect(),
                mro: vec!["CurrentThreadScheduler".to_string()],
                attribute_types: HashMap::new(),
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
                    "watch",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["IO::Path".to_string()],
                attribute_types: HashMap::new(),
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
                native_methods: ["slurp", "Str", "gist"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["IO::Pipe".to_string()],
                attribute_types: HashMap::new(),
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
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["IO::Socket::Async".to_string()],
                attribute_types: HashMap::new(),
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
                    "gist",
                    "raku",
                    "Str",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["Kernel".to_string()],
                attribute_types: HashMap::new(),
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
                native_methods: ["decode-chars"].iter().map(|s| s.to_string()).collect(),
                mro: vec!["Encoding::Decoder".to_string()],
                attribute_types: HashMap::new(),
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

        // X::Syntax hierarchy (syntax errors, subtypes of X::Comp)
        register_x("X::Syntax", "X::Comp");
        register_x("X::Syntax::Confused", "X::Syntax");
        register_x("X::Syntax::Missing", "X::Syntax");
        register_x("X::Syntax::Malformed", "X::Syntax");
        register_x("X::Syntax::Variable::Numeric", "X::Syntax");
        register_x("X::Syntax::Variable::Initializer", "X::Syntax");
        register_x("X::Syntax::Variable::ConflictingTypes", "X::Syntax");
        register_x("X::Syntax::Number::LiteralType", "X::Syntax");
        register_x("X::Syntax::Regex::Adverb", "X::Syntax");
        register_x("X::Syntax::Type::Adverb", "X::Syntax");
        register_x("X::Backslash::UnrecognizedSequence", "X::Backslash");
        register_x("X::Syntax::Regex::SolitaryQuantifier", "X::Syntax");
        register_x("X::Syntax::Regex::NullRegex", "X::Syntax");
        register_x("X::Syntax::Term::MissingInitializer", "X::Syntax");
        register_x("X::Syntax::WithoutElse", "X::Syntax");
        register_x("X::Syntax::UnlessElse", "X::Syntax");

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
        register_x("X::Cannot::New", "Exception");
        register_x("X::Cannot::Capture", "Exception");

        // X::Match::Bool
        register_x("X::Match::Bool", "Exception");

        // X::Adverb
        register_x("X::Adverb", "Exception");

        // X::ControlFlow::Return
        register_x("X::ControlFlow::Return", "Exception");

        // X::Bind
        register_x("X::Bind", "Exception");

        // X::StubCode
        register_x("X::StubCode", "Exception");

        // X::Signature::Placeholder
        register_x("X::Signature::Placeholder", "Exception");

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

        // X::ParametricConstant
        register_x("X::ParametricConstant", "Exception");

        // X::UnitScope::Invalid
        register_x("X::UnitScope::Invalid", "Exception");

        let mut interpreter = Self {
            env: Env::from(env),
            output: String::new(),
            stderr_output: String::new(),
            warn_output: String::new(),
            warn_suppression_depth: 0,
            test_state: None,
            subtest_depth: 0,
            halted: false,
            exit_code: 0,
            immediate_stdout: false,
            output_emitted: false,
            bailed_out: false,
            functions: HashMap::new(),
            operator_assoc: HashMap::new(),
            proto_functions: HashMap::new(),
            token_defs: HashMap::new(),
            lib_paths: Vec::new(),
            handles: HashMap::new(),
            next_handle_id: 1,
            program_path: None,
            current_package: "GLOBAL".to_string(),
            routine_stack: Vec::new(),
            callframe_stack: Vec::new(),
            method_class_stack: Vec::new(),
            pending_call_arg_sources: None,
            test_pending_callsite_line: None,
            test_assertion_line_stack: Vec::new(),
            block_stack: Vec::new(),
            doc_comments: HashMap::new(),
            type_metadata: HashMap::new(),
            when_matched: false,
            gather_items: Vec::new(),
            gather_take_limits: Vec::new(),
            block_scope_depth: 0,
            enum_types: HashMap::new(),
            classes,
            cunion_classes: HashSet::new(),
            hidden_classes: HashSet::new(),
            class_stubs: HashSet::new(),
            package_stubs: HashSet::new(),
            hidden_defer_parents: HashMap::new(),
            class_trusts: HashMap::new(),
            class_composed_roles: {
                let mut ccr = HashMap::new();
                ccr.insert(
                    "CompUnit::Repository::FileSystem".to_string(),
                    vec!["CompUnit::Repository".to_string()],
                );
                ccr
            },
            roles: {
                let mut roles = HashMap::new();
                roles.insert(
                    "Encoding".to_string(),
                    RoleDef {
                        attributes: Vec::new(),
                        methods: HashMap::new(),
                        is_stub_role: false,
                        is_hidden: false,
                        captured_env: None,
                        wildcard_handles: Vec::new(),
                        role_id: 0,
                        attribute_conflicts: Vec::new(),
                    },
                );
                roles.insert(
                    "Iterator".to_string(),
                    RoleDef {
                        attributes: Vec::new(),
                        methods: HashMap::new(),
                        is_stub_role: false,
                        is_hidden: false,
                        captured_env: None,
                        wildcard_handles: Vec::new(),
                        role_id: 0,
                        attribute_conflicts: Vec::new(),
                    },
                );
                roles.insert(
                    "PredictiveIterator".to_string(),
                    RoleDef {
                        attributes: Vec::new(),
                        methods: HashMap::new(),
                        is_stub_role: false,
                        is_hidden: false,
                        captured_env: None,
                        wildcard_handles: Vec::new(),
                        role_id: 0,
                        attribute_conflicts: Vec::new(),
                    },
                );
                roles.insert(
                    "Iterable".to_string(),
                    RoleDef {
                        attributes: Vec::new(),
                        methods: HashMap::new(),
                        is_stub_role: false,
                        is_hidden: false,
                        captured_env: None,
                        wildcard_handles: Vec::new(),
                        role_id: 0,
                        attribute_conflicts: Vec::new(),
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
                            captured_env: None,
                            wildcard_handles: Vec::new(),
                            role_id: 0,
                            attribute_conflicts: Vec::new(),
                        },
                    );
                }
                roles
            },
            role_candidates: HashMap::new(),
            role_parents: HashMap::new(),
            role_hides: HashMap::new(),
            role_type_params: HashMap::new(),
            class_role_param_bindings: HashMap::new(),
            attribute_build_overrides: HashMap::new(),
            subsets: HashMap::new(),
            proto_subs: HashSet::new(),
            proto_tokens: HashSet::new(),
            proto_dispatch_stack: Vec::new(),
            pending_dispatch_error: None,
            end_phasers: Vec::new(),
            chroot_root: None,
            loaded_modules: HashSet::new(),
            need_hidden_classes: HashSet::new(),
            closure_env_overrides: HashMap::new(),
            protect_block_cache: HashMap::new(),
            private_zeroarg_method_cache: HashMap::new(),
            module_load_stack: Vec::new(),
            exported_subs: HashMap::new(),
            exported_vars: HashMap::new(),
            suppress_exports: false,
            in_lvalue_assignment: false,
            newline_mode: NewlineMode::Lf,
            import_scope_stack: Vec::new(),
            strict_mode: false,
            fatal_mode: false,
            state_vars: HashMap::new(),
            once_values: HashMap::new(),
            once_scope_stack: Vec::new(),
            next_once_scope_id: 1,
            var_dynamic_flags: HashMap::new(),
            caller_env_stack: Vec::new(),
            var_bindings: HashMap::new(),
            variables_pragma: String::new(),
            var_type_constraints: HashMap::new(),
            var_defaults: HashMap::new(),
            container_defaults: HashMap::new(),
            var_hash_key_constraints: HashMap::new(),
            array_type_metadata: HashMap::new(),
            mix_type_metadata: HashMap::new(),
            hash_type_metadata: HashMap::new(),
            instance_type_metadata: HashMap::new(),
            let_saves: Vec::new(),
            supply_emit_buffer: Vec::new(),
            supply_emit_timed_buffer: Vec::new(),
            shared_vars: Arc::new(RwLock::new(HashMap::new())),
            shared_vars_active: false,
            shared_vars_dirty: Arc::new(RwLock::new(HashSet::new())),
            encoding_registry: Self::builtin_encodings(),
            skip_pseudo_method_native: None,
            pending_proxy_subclass_attr: None,
            multi_dispatch_stack: Vec::new(),
            method_dispatch_stack: Vec::new(),
            samewith_context_stack: Vec::new(),
            wrap_chains: HashMap::new(),
            wrap_sub_names: HashMap::new(),
            wrap_name_to_sub: HashMap::new(),
            wrap_handle_counter: 0,
            wrap_dispatch_stack: Vec::new(),
            suppressed_names: HashSet::new(),
            lexical_class_scopes: Vec::new(),
            last_value: None,
            pending_local_updates: Vec::new(),
            readonly_vars: HashSet::new(),
            squish_iterator_meta: HashMap::new(),
            custom_type_data: HashMap::new(),
            rebless_map: HashMap::new(),
            action_made: None,
            pending_regex_error: None,
            precomp_enabled: true,
            monkey_typing: false,
            shared_thread_output: None,
            shared_thread_stderr: None,
            is_thread_clone: false,
            deprecation_tracker: HashMap::new(),
            deprecated_functions: HashMap::new(),
            deprecated_methods: HashMap::new(),
        };
        interpreter.init_io_environment();
        interpreter.init_order_enum();
        interpreter.init_endian_enum();
        interpreter.init_protocol_family_enum();
        interpreter.init_signal_enum();
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

    pub(crate) fn is_name_suppressed(&self, name: &str) -> bool {
        self.suppressed_names.contains(name)
    }

    /// Resolve a suppressed nested class short name to its qualified form.
    /// For example, if `Frog` is suppressed and we are inside class `Forest`,
    /// this returns `Some("Forest::Frog")` if `Forest::Frog` is a known type.
    pub(crate) fn resolve_suppressed_type(&self, name: &str) -> Option<String> {
        if !self.suppressed_names.contains(name) {
            return None;
        }
        // Check current package
        let current_pkg = &self.current_package;
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

    /// Register a deprecation for a callable (sub or method).
    pub(crate) fn register_deprecation(
        &mut self,
        kind: &str,
        name: &str,
        package: &str,
        message: Option<String>,
    ) {
        let key = format!("{} {} (from {})", kind, name, package);
        self.deprecation_tracker
            .entry(key.clone())
            .or_insert_with(|| DeprecationRecord {
                kind: kind.to_string(),
                name: name.to_string(),
                package: package.to_string(),
                message,
                call_sites: Vec::new(),
            });
        if kind == "Sub" {
            let fq = format!("{}::{}", package, name);
            self.deprecated_functions.insert(fq, key);
        } else if kind == "Method" {
            self.deprecated_methods
                .insert((package.to_string(), name.to_string()), key);
        }
    }

    /// Record a call to a deprecated callable by tracker key.
    pub(crate) fn record_deprecation_call_by_key(&mut self, key: &str, file: &str, line: i64) {
        if let Some(record) = self.deprecation_tracker.get_mut(key) {
            record.call_sites.push((file.to_string(), line));
        }
    }

    /// Check if a function call is deprecated and record the call site.
    pub(crate) fn check_function_deprecation(&mut self, name: &str, line: i64) {
        let pkg = self.current_package.clone();
        let fq = format!("{}::{}", pkg, name);
        let key = self.deprecated_functions.get(&fq).cloned();
        if key.is_none() {
            // Try GLOBAL
            let fq_global = format!("GLOBAL::{}", name);
            if let Some(k) = self.deprecated_functions.get(&fq_global).cloned() {
                let file = self
                    .program_path
                    .clone()
                    .unwrap_or_else(|| "-e".to_string());
                self.record_deprecation_call_by_key(&k, &file, line);
            }
            return;
        }
        if let Some(k) = key {
            let file = self
                .program_path
                .clone()
                .unwrap_or_else(|| "-e".to_string());
            self.record_deprecation_call_by_key(&k, &file, line);
        }
    }

    /// Check if a method call is deprecated and record the call site.
    pub(crate) fn check_method_deprecation(
        &mut self,
        class_name: &str,
        method_name: &str,
        line: i64,
    ) {
        let lookup_key = (class_name.to_string(), method_name.to_string());
        if let Some(k) = self.deprecated_methods.get(&lookup_key).cloned() {
            let file = self
                .program_path
                .clone()
                .unwrap_or_else(|| "-e".to_string());
            self.record_deprecation_call_by_key(&k, &file, line);
        }
    }

    /// Generate and consume the deprecation report.
    pub(crate) fn deprecation_report(&mut self) -> Value {
        if self.deprecation_tracker.is_empty() {
            return Value::Nil;
        }
        // Find entries with call_sites
        let mut reports: Vec<String> = Vec::new();
        let mut consumed_keys: Vec<String> = Vec::new();
        for (key, record) in &self.deprecation_tracker {
            if record.call_sites.is_empty() {
                continue;
            }
            consumed_keys.push(key.clone());
            let count = record.call_sites.len();
            let replacement = match &record.message {
                Some(msg) => msg.clone(),
                None => "something else".to_string(),
            };
            // Build location string
            let locations = if count == 1 {
                let (file, line) = &record.call_sites[0];
                format!("  {}, line {}", file, line)
            } else {
                // Group consecutive lines
                let lines: Vec<i64> = record.call_sites.iter().map(|(_, l)| *l).collect();
                let file = &record.call_sites[0].0;
                let line_strs: Vec<String> = lines.iter().map(|l| l.to_string()).collect();
                format!("  {}, lines {}", file, line_strs.join(","))
            };
            let report = format!(
                "Saw 1 occurrence of deprecated code.\n\
                 ================================================================================\n\
                 {} {} (from {}) seen at:\n\
                 {}\n\
                 Please use {} instead.\n\
                 --------------------------------------------------------------------------------",
                record.kind, record.name, record.package, locations, replacement
            );
            reports.push(report);
        }
        // Clear consumed entries
        for key in consumed_keys {
            if let Some(record) = self.deprecation_tracker.get_mut(&key) {
                record.call_sites.clear();
            }
        }
        if reports.is_empty() {
            Value::Nil
        } else {
            Value::str(reports.join("\n"))
        }
    }

    /// Save current function/class keys for lexical import scoping.
    pub(crate) fn push_import_scope(&mut self) {
        let func_keys: HashSet<Symbol> = self.functions.keys().copied().collect();
        let class_keys: HashSet<String> = self.classes.keys().cloned().collect();
        self.import_scope_stack.push((
            func_keys,
            class_keys,
            self.newline_mode,
            self.strict_mode,
            self.fatal_mode,
        ));
    }

    /// Restore function/class registries to the last saved snapshot,
    /// removing any entries added since the push.
    pub(crate) fn pop_import_scope(&mut self) {
        if let Some((func_snapshot, class_snapshot, newline_mode, strict_mode, fatal_mode)) =
            self.import_scope_stack.pop()
        {
            self.functions.retain(|key, _| func_snapshot.contains(key));
            self.classes.retain(|key, _| class_snapshot.contains(key));
            self.newline_mode = newline_mode;
            self.strict_mode = strict_mode;
            self.fatal_mode = fatal_mode;
        }
    }

    pub fn use_module(&mut self, module: &str) -> Result<(), RuntimeError> {
        if self.loaded_modules.contains(module) {
            if module == "strict" {
                self.strict_mode = true;
            } else if module == "fatal" {
                self.fatal_mode = true;
            }
            return match self.import_module(module, &[]) {
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
        self.module_load_stack.push(module.to_string());
        let class_snapshot: HashSet<String> = self.classes.keys().cloned().collect();
        let env_snapshot: HashSet<String> = self.env.keys().cloned().collect();

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
                Err(err) if err.message.starts_with("Module not found:") => Ok(()),
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
            for class_name in self.classes.keys() {
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
                if key.starts_with('$')
                    || key.starts_with('@')
                    || key.starts_with('%')
                    || key.starts_with('&')
                {
                    continue;
                }
                let key_short = key
                    .rsplit_once("::")
                    .map(|(_, short)| short)
                    .unwrap_or(key.as_str());
                if !key_short
                    .chars()
                    .next()
                    .is_some_and(|c| c.is_ascii_uppercase())
                {
                    continue;
                }
                if key_short != module_short {
                    self.need_hidden_classes.insert(key.clone());
                    self.need_hidden_classes.insert(key_short.to_string());
                }
            }
            if module == "strict" {
                self.strict_mode = true;
            } else if module == "fatal" {
                self.fatal_mode = true;
            }
            self.loaded_modules.insert(module.to_string());
            if let Err(err) = self.import_module(module, &[])
                && !err.message.starts_with("No exports found for module:")
            {
                return Err(err);
            }
        }
        result
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
        if let Some(def) = self.functions.get(&fq_sym).cloned() {
            for tag in &tags {
                // Bare EXPORT::TAG::name (accessible from the same package)
                let bare_export = format!("EXPORT::{}::{}", tag, name);
                self.functions
                    .entry(crate::symbol::Symbol::intern(&bare_export))
                    .or_insert_with(|| def.clone());
                // Fully-qualified Package::EXPORT::TAG::name
                let pkg_export = format!("{}::EXPORT::{}::{}", package, tag, name);
                self.functions
                    .entry(crate::symbol::Symbol::intern(&pkg_export))
                    .or_insert_with(|| def.clone());
            }
            // Always register under EXPORT::ALL::name
            if !tags.contains(&"ALL".to_string()) {
                let bare_all = format!("EXPORT::ALL::{}", name);
                self.functions
                    .entry(crate::symbol::Symbol::intern(&bare_all))
                    .or_insert_with(|| def.clone());
                let pkg_all = format!("{}::EXPORT::ALL::{}", package, name);
                self.functions
                    .entry(crate::symbol::Symbol::intern(&pkg_all))
                    .or_insert_with(|| def);
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
        if subs.is_empty() && vars.is_empty() {
            return Err(RuntimeError::new(format!(
                "No exports found for module: {}",
                module
            )));
        }

        for (name, symbol_tags) in subs {
            if !import_all && symbol_tags.is_disjoint(&requested) {
                continue;
            }
            let source_single = format!("{module}::{name}");
            let source_prefix = format!("{module}::{name}/");
            let target_single = format!("GLOBAL::{name}");
            let target_prefix = format!("GLOBAL::{name}/");

            let function_entries: Vec<(Symbol, FunctionDef)> = self
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
                self.functions.insert(k, v);
            }

            let proto_entries: Vec<(Symbol, FunctionDef)> = self
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
                self.proto_functions.insert(k, v);
            }
        }

        for (name, symbol_tags) in vars {
            if !import_all && symbol_tags.is_disjoint(&requested) {
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
        let class_snapshot: HashSet<String> = self.classes.keys().cloned().collect();
        let env_snapshot: HashSet<String> = self.env.keys().cloned().collect();
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
            for class_name in self.classes.keys() {
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
                if key.starts_with('$')
                    || key.starts_with('@')
                    || key.starts_with('%')
                    || key.starts_with('&')
                {
                    continue;
                }
                let key_short = key
                    .rsplit_once("::")
                    .map(|(_, short)| short)
                    .unwrap_or(key.as_str());
                if !key_short
                    .chars()
                    .next()
                    .is_some_and(|c| c.is_ascii_uppercase())
                {
                    continue;
                }
                if is_nested_need || key_short != short_name {
                    self.need_hidden_classes.insert(key.clone());
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

    pub fn output(&self) -> &str {
        &self.output
    }

    /// Clear the output buffer and reset the output-emitted flag.
    pub fn clear_output(&mut self) {
        self.output.clear();
        self.output_emitted = false;
    }

    /// Returns true if any output was emitted since the last `clear_output`.
    pub fn has_output_emitted(&self) -> bool {
        self.output_emitted
    }

    /// Write to the output buffer and also flush to real stdout
    /// when not inside a subtest.
    pub(crate) fn emit_output(&mut self, text: &str) {
        self.output_emitted = true;
        let byte_count = text.len() as i64;
        if let Some(stdout_handle) = self
            .handles
            .values_mut()
            .find(|h| matches!(h.target, IoHandleTarget::Stdout))
        {
            stdout_handle.bytes_written += byte_count;
        }
        if self.subtest_depth == 0 && self.immediate_stdout {
            use std::io::Write;
            let _ = std::io::stdout().write_all(text.as_bytes());
            let _ = std::io::stdout().flush();
        } else if self.is_thread_clone
            && let Some(ref shared) = self.shared_thread_output
        {
            // Thread clones write to the shared buffer so concurrent output is
            // interleaved in real chronological order (not grouped per-promise).
            shared.lock().unwrap().push_str(text);
        } else {
            self.output.push_str(text);
        }
    }

    /// Enable immediate flushing of output to stdout.
    pub fn set_immediate_stdout(&mut self, val: bool) {
        self.immediate_stdout = val;
    }

    pub fn flush_stderr_buffer(&mut self) {
        if !self.stderr_output.is_empty() {
            if !self.immediate_stdout {
                eprint!("{}", self.stderr_output);
                let _ = std::io::stderr().flush();
            }
            self.stderr_output.clear();
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

    pub(crate) fn is_halted(&self) -> bool {
        self.halted
    }

    pub(crate) fn write_warn_to_stderr(&mut self, message: &str) {
        let msg = format!("{}\n", message);
        if self.is_thread_clone
            && let Some(ref shared) = self.shared_thread_stderr
        {
            shared.lock().unwrap().push_str(&msg);
            self.warn_output.push_str(&msg);
            return;
        }
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

    pub(crate) fn env(&self) -> &Env {
        &self.env
    }

    pub(crate) fn env_insert(&mut self, key: String, value: Value) {
        self.env.insert(key, value);
    }

    /// O(1) clone of the env (just Arc::clone).
    pub(crate) fn clone_env(&self) -> Env {
        self.env.clone()
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
            self.register_var_container_type_metadata(&key, &info);
        } else {
            self.var_type_constraints.remove(&key);
            self.var_hash_key_constraints.remove(&key);
            self.env.remove(&meta_key);
            self.env.remove(&format!("__mutsu_hash_key_type::{}", key));
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

    /// Set the default value for a variable declared with `is default(...)`.
    pub(crate) fn set_var_default(&mut self, name: &str, value: Value) {
        self.var_defaults.insert(name.to_string(), value);
    }

    /// Get the default value for a variable, if one was set with `is default(...)`.
    pub(crate) fn var_default(&self, name: &str) -> Option<&Value> {
        self.var_defaults.get(name)
    }

    /// Set the element default for a container (Array/Hash) by Arc pointer identity.
    pub(crate) fn set_container_default(&mut self, value: &Value, default: Value) {
        let id = match value {
            Value::Array(items, ..) => Arc::as_ptr(items) as usize,
            Value::Hash(map) => Arc::as_ptr(map) as usize,
            _ => return,
        };
        self.container_defaults.insert(id, default);
    }

    /// Get the element default for a container (Array/Hash) by Arc pointer identity.
    pub(crate) fn container_default(&self, value: &Value) -> Option<&Value> {
        let id = match value {
            Value::Array(items, ..) => Arc::as_ptr(items) as usize,
            Value::Hash(map) => Arc::as_ptr(map) as usize,
            _ => return None,
        };
        self.container_defaults.get(&id)
    }

    pub(crate) fn var_hash_key_constraint(&self, name: &str) -> Option<String> {
        let key = name;
        let meta_key = format!("__mutsu_hash_key_type::{}", key);
        if let Some(Value::Str(tc)) = self.env.get(&meta_key) {
            return Some(tc.to_string());
        }
        self.var_hash_key_constraints.get(key).cloned()
    }

    pub(crate) fn register_container_type_metadata(
        &mut self,
        value: &Value,
        info: ContainerTypeInfo,
    ) {
        match value {
            Value::Array(items, ..) => {
                let id = Arc::as_ptr(items) as usize;
                self.array_type_metadata.insert(id, info);
            }
            Value::Mix(items) => {
                let id = Arc::as_ptr(items) as usize;
                self.mix_type_metadata.insert(id, info);
            }
            Value::Hash(items) => {
                let id = Arc::as_ptr(items) as usize;
                self.hash_type_metadata.insert(id, info);
            }
            Value::Instance { id, .. } => {
                self.instance_type_metadata.insert(*id, info);
            }
            Value::Mixin(inner, _) => self.register_container_type_metadata(inner, info),
            _ => {}
        }
    }

    pub(crate) fn container_type_metadata(&self, value: &Value) -> Option<ContainerTypeInfo> {
        match value {
            Value::Array(items, ..) => {
                let id = Arc::as_ptr(items) as usize;
                self.array_type_metadata.get(&id).cloned()
            }
            Value::Mix(items) => {
                let id = Arc::as_ptr(items) as usize;
                self.mix_type_metadata.get(&id).cloned()
            }
            Value::Hash(items) => {
                let id = Arc::as_ptr(items) as usize;
                self.hash_type_metadata.get(&id).cloned()
            }
            Value::Instance { id, .. } => self.instance_type_metadata.get(id).cloned(),
            Value::Mixin(inner, _) => self.container_type_metadata(inner),
            _ => None,
        }
    }

    fn register_var_container_type_metadata(&mut self, name: &str, info: &ContainerTypeInfo) {
        if let Some(value) = self.env.get(name).cloned() {
            self.register_container_type_metadata(&value, info.clone());
        }
    }

    fn parse_container_constraint(name: &str, raw: &str) -> ContainerTypeInfo {
        if name.starts_with('%') {
            if let Some(inner) = raw.strip_prefix("Hash[").and_then(|s| s.strip_suffix(']')) {
                let parts: Vec<String> = inner
                    .split(',')
                    .map(|p| p.trim())
                    .filter(|p| !p.is_empty())
                    .map(ToOwned::to_owned)
                    .collect();
                let value_type = parts.first().cloned().unwrap_or_else(|| "Any".to_string());
                let key_type = parts.get(1).cloned();
                return ContainerTypeInfo {
                    value_type,
                    key_type,
                    declared_type: Some(raw.to_string()),
                };
            }
            if let Some((value_type, key_part)) = raw.split_once('{')
                && let Some(key_type) = key_part.strip_suffix('}')
            {
                return ContainerTypeInfo {
                    value_type: value_type.trim().to_string(),
                    key_type: Some(key_type.trim().to_string()),
                    declared_type: Some(format!("Hash[{},{}]", value_type.trim(), key_type.trim())),
                };
            }
        }
        if name.starts_with('@')
            && let Some(inner) = raw
                .strip_prefix("Array[")
                .or_else(|| raw.strip_prefix("List["))
                .and_then(|s| s.strip_suffix(']'))
        {
            return ContainerTypeInfo {
                value_type: inner.trim().to_string(),
                key_type: None,
                declared_type: Some(format!("Array[{}]", inner.trim())),
            };
        }
        ContainerTypeInfo {
            value_type: raw.to_string(),
            key_type: None,
            declared_type: None,
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
                    return Err(RuntimeError::new(
                        crate::runtime::utils::type_check_element_error(var_name, &constraint, val),
                    ));
                }
            }
        }
        Ok(())
    }

    pub(crate) fn is_var_dynamic(&self, name: &str) -> bool {
        self.var_dynamic_flags
            .get(Self::normalize_var_meta_name(name))
            .copied()
            .unwrap_or(false)
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
                if self.is_var_dynamic(key) && restored_env.get(key) != Some(value) {
                    restored_env.insert(key.clone(), value.clone());
                }
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
                return Err(RuntimeError::new(format!(
                    "Cannot access '${name}' through CALLER, because it is not declared as dynamic"
                )));
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
            return Err(RuntimeError::new(format!(
                "Cannot access '${name}' through CALLER, because it is not declared as dynamic"
            )));
        }
        let idx = stack_len - depth;
        self.caller_env_stack[idx].insert(name.to_string(), value.clone());
        // Also update current env if the variable exists there
        if self.env.contains_key(name) {
            self.env.insert(name.to_string(), value);
        }
        Ok(())
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
        self.classes.contains_key(name)
    }

    /// Create a lightweight clone of this interpreter for use in a spawned thread.
    /// Shares function/class/role/enum definitions but starts with fresh output and test state.
    /// Array (`@`) and scalar (`$`) variables are shared between parent and child via `shared_vars`
    /// so that mutations are visible across threads.
    pub(crate) fn clone_for_thread(&mut self) -> Self {
        // Copy user variables into shared_vars so both parent and child see mutations.
        // The compiler stores locals with bare names (no sigil), so we share everything
        // except internal/special variables that should remain thread-local.
        let shared = Arc::clone(&self.shared_vars);
        {
            let mut sv = shared.write().unwrap();
            for (key, val) in &self.env {
                // Skip internal variables and topic variables
                if key == "_"
                    || key == "@_"
                    || key.starts_with("__mutsu_")
                    || key.starts_with("&")
                    || key == "?LINE"
                {
                    continue;
                }
                // Only insert if not already present — existing values may have
                // been updated by earlier threads that are already running.
                sv.entry(key.clone()).or_insert_with(|| val.clone());
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
        for (id, handle) in &self.handles {
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
            };
            cloned_handles.insert(*id, cloned);
        }
        let mut cloned = Self {
            env: self.env.clone(),
            output: String::new(),
            stderr_output: String::new(),
            warn_output: String::new(),
            warn_suppression_depth: 0,
            test_state: None,
            subtest_depth: 0,
            halted: false,
            exit_code: 0,
            immediate_stdout: false,
            output_emitted: false,
            bailed_out: false,
            functions: self.functions.clone(),
            operator_assoc: self.operator_assoc.clone(),
            proto_functions: self.proto_functions.clone(),
            token_defs: self.token_defs.clone(),
            lib_paths: self.lib_paths.clone(),
            handles: cloned_handles,
            next_handle_id: self.next_handle_id,
            program_path: self.program_path.clone(),
            current_package: self.current_package.clone(),
            routine_stack: Vec::new(),
            callframe_stack: Vec::new(),
            method_class_stack: Vec::new(),
            pending_call_arg_sources: None,
            test_pending_callsite_line: None,
            test_assertion_line_stack: Vec::new(),
            block_stack: Vec::new(),
            doc_comments: HashMap::new(),
            type_metadata: self.type_metadata.clone(),
            when_matched: false,
            gather_items: Vec::new(),
            gather_take_limits: Vec::new(),
            block_scope_depth: self.block_scope_depth,
            enum_types: self.enum_types.clone(),
            classes: self.classes.clone(),
            cunion_classes: self.cunion_classes.clone(),
            hidden_classes: self.hidden_classes.clone(),
            class_stubs: self.class_stubs.clone(),
            package_stubs: self.package_stubs.clone(),
            hidden_defer_parents: self.hidden_defer_parents.clone(),
            class_trusts: self.class_trusts.clone(),
            class_composed_roles: self.class_composed_roles.clone(),
            roles: self.roles.clone(),
            role_candidates: self.role_candidates.clone(),
            role_parents: self.role_parents.clone(),
            role_hides: self.role_hides.clone(),
            role_type_params: self.role_type_params.clone(),
            class_role_param_bindings: self.class_role_param_bindings.clone(),
            attribute_build_overrides: self.attribute_build_overrides.clone(),
            subsets: self.subsets.clone(),
            proto_subs: self.proto_subs.clone(),
            proto_tokens: self.proto_tokens.clone(),
            proto_dispatch_stack: Vec::new(),
            pending_dispatch_error: None,
            end_phasers: Vec::new(),
            chroot_root: self.chroot_root.clone(),
            loaded_modules: self.loaded_modules.clone(),
            need_hidden_classes: self.need_hidden_classes.clone(),
            closure_env_overrides: self.closure_env_overrides.clone(),
            protect_block_cache: HashMap::new(),
            private_zeroarg_method_cache: HashMap::new(),
            module_load_stack: Vec::new(),
            exported_subs: self.exported_subs.clone(),
            exported_vars: self.exported_vars.clone(),
            suppress_exports: false,
            in_lvalue_assignment: false,
            newline_mode: self.newline_mode,
            import_scope_stack: Vec::new(),
            strict_mode: self.strict_mode,
            fatal_mode: self.fatal_mode,
            state_vars: HashMap::new(),
            once_values: self.once_values.clone(),
            once_scope_stack: Vec::new(),
            next_once_scope_id: self.next_once_scope_id,
            var_dynamic_flags: self.var_dynamic_flags.clone(),
            caller_env_stack: Vec::new(),
            var_bindings: HashMap::new(),
            variables_pragma: self.variables_pragma.clone(),
            var_type_constraints: self.var_type_constraints.clone(),
            var_defaults: self.var_defaults.clone(),
            container_defaults: self.container_defaults.clone(),
            var_hash_key_constraints: self.var_hash_key_constraints.clone(),
            array_type_metadata: self.array_type_metadata.clone(),
            mix_type_metadata: self.mix_type_metadata.clone(),
            hash_type_metadata: self.hash_type_metadata.clone(),
            instance_type_metadata: self.instance_type_metadata.clone(),
            let_saves: Vec::new(),
            supply_emit_buffer: Vec::new(),
            supply_emit_timed_buffer: Vec::new(),
            shared_vars: Arc::clone(&self.shared_vars),
            shared_vars_active: true,
            shared_vars_dirty: Arc::clone(&self.shared_vars_dirty),
            encoding_registry: self.encoding_registry.clone(),
            skip_pseudo_method_native: None,
            pending_proxy_subclass_attr: None,
            multi_dispatch_stack: Vec::new(),
            method_dispatch_stack: Vec::new(),
            samewith_context_stack: Vec::new(),
            wrap_chains: self.wrap_chains.clone(),
            wrap_sub_names: self.wrap_sub_names.clone(),
            wrap_name_to_sub: self.wrap_name_to_sub.clone(),
            wrap_handle_counter: self.wrap_handle_counter,
            wrap_dispatch_stack: Vec::new(),
            suppressed_names: self.suppressed_names.clone(),
            lexical_class_scopes: self.lexical_class_scopes.clone(),
            last_value: None,
            pending_local_updates: Vec::new(),
            readonly_vars: HashSet::new(),
            squish_iterator_meta: HashMap::new(),
            custom_type_data: self.custom_type_data.clone(),
            rebless_map: self.rebless_map.clone(),
            action_made: None,
            pending_regex_error: None,
            precomp_enabled: self.precomp_enabled,
            monkey_typing: self.monkey_typing,
            shared_thread_output: {
                let buf = self
                    .shared_thread_output
                    .get_or_insert_with(|| Arc::new(Mutex::new(String::new())));
                Some(Arc::clone(buf))
            },
            shared_thread_stderr: {
                let buf = self
                    .shared_thread_stderr
                    .get_or_insert_with(|| Arc::new(Mutex::new(String::new())));
                Some(Arc::clone(buf))
            },
            is_thread_clone: true,
            deprecation_tracker: self.deprecation_tracker.clone(),
            deprecated_functions: self.deprecated_functions.clone(),
            deprecated_methods: self.deprecated_methods.clone(),
        };
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
            let mut sv = self.shared_vars.write().unwrap();
            if let Some(shared_value) = sv.remove(key) {
                if let Value::Array(mut arc_items, kind) = shared_value {
                    let items = Arc::make_mut(&mut arc_items);
                    items.extend(values.clone());
                    let normalized_kind = if kind == ArrayKind::List {
                        ArrayKind::Array
                    } else {
                        kind
                    };
                    let result = Value::Array(Arc::clone(&arc_items), normalized_kind);
                    sv.insert(key.to_string(), Value::Array(arc_items, normalized_kind));
                    if let Ok(mut dirty) = self.shared_vars_dirty.write() {
                        dirty.insert(key.to_string());
                    }
                    drop(sv);
                    self.env.insert(key.to_string(), result.clone());
                    return result;
                }
                sv.insert(key.to_string(), shared_value);
            }
            if let Some(Value::Array(arc_items, kind)) = sv.get_mut(key) {
                let items = Arc::make_mut(arc_items);
                items.extend(values);
                if *kind == ArrayKind::List {
                    *kind = ArrayKind::Array;
                }
                let result = Value::Array(Arc::clone(arc_items), *kind);
                if let Ok(mut dirty) = self.shared_vars_dirty.write() {
                    dirty.insert(key.to_string());
                }
                drop(sv);
                self.env.insert(key.to_string(), result.clone());
                return result;
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

    /// Read a shared variable. If the variable is in shared_vars, return
    /// the shared version (which may have been mutated by another thread).
    #[allow(dead_code)]
    pub(crate) fn get_shared_var(&self, key: &str) -> Option<Value> {
        let sv = self.shared_vars.read().unwrap();
        sv.get(key).cloned()
    }

    pub(crate) fn has_shared_var(&self, key: &str) -> bool {
        let sv = self.shared_vars.read().unwrap();
        sv.contains_key(key)
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
            if sv.contains_key(key) {
                sv.insert(key.to_string(), value);
                // Mark this key as explicitly updated so sync_shared_vars_to_env
                // knows to propagate it (vs keys only initialized by clone_for_thread).
                if let Ok(mut dirty) = self.shared_vars_dirty.write() {
                    dirty.insert(key.to_string());
                }
            }
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

                if let Some(val) = sv.get(key) {
                    updates.push((key.clone(), val.clone()));
                }
            }
            updates
        };
        for (key, val) in updates {
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

    pub(crate) fn merge_sigilless_alias_writes(
        &self,
        saved_env: &mut HashMap<String, Value>,
        current_env: &HashMap<String, Value>,
    ) {
        for (key, alias) in current_env {
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
            saved_env.insert(key.clone(), alias.clone());
            if let Some(value) = current_env.get(alias_name.as_str()).cloned() {
                saved_env.insert(alias_name.to_string(), value);
                continue;
            }
            if let Some(bare_name) = key.strip_prefix("__mutsu_sigilless_alias::")
                && let Some(value) = current_env.get(bare_name).cloned()
            {
                saved_env.insert(bare_name.to_string(), value.clone());
                saved_env.insert(alias_name.to_string(), value);
            }
        }
        for (key, value) in current_env {
            if key.starts_with("__mutsu_predictive_seq_iter::")
                || key.starts_with("__mutsu_sigilless_alias::!")
            {
                saved_env.insert(key.clone(), value.clone());
            }
        }
    }
}

#[derive(Debug, Default)]
struct TestState {
    planned: Option<usize>,
    ran: usize,
    failed: usize,
    force_todo: Vec<TodoRange>,
}

#[derive(Debug, Clone)]
struct TodoRange {
    start: usize,
    end: usize,
    reason: String,
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
            compiled_code: Some(Arc::new(compiled)),
        });

        let mut interp = Interpreter::new();
        let (_, _, captured_bindings, _, captured_names, sync_names) =
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
        assert_eq!(
            sync_names.as_ref(),
            &vec!["used".to_string(), "$target".to_string()]
        );
    }
}

impl Interpreter {
    /// Flush all open file handle buffers. Call before process exit.
    pub fn flush_all_handles(&mut self) {
        for state in self.handles.values_mut() {
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
