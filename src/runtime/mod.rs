use crate::symbol::Symbol;
use std::cell::Cell;
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
use std::sync::atomic::{AtomicU32, AtomicU64, Ordering};
use std::sync::{Arc, Mutex, RwLock};

static ROLE_ID_COUNTER: AtomicU64 = AtomicU64::new(1);

pub(crate) fn next_role_id() -> u64 {
    ROLE_ID_COUNTER.fetch_add(1, Ordering::Relaxed)
}
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use crate::ast::{Expr, FunctionDef, ParamDef, PhaserKind, ReadonlyKind, Stmt};
use crate::env::Env;
use crate::opcode::{CompiledCode, CompiledFns, CompiledFunction};
use crate::parse_dispatch;
use crate::value::ValueView;
use crate::value::{
    ArrayKind, AttrMap, EnumValue, JunctionKind, LazyList, RuntimeError, SharedChannel,
    SharedPromise, Value, make_rat, take_pending_instance_destroys,
};

/// The `X::Phaser::PrePost` a falsy `PRE`/`POST` phaser throws.
///
/// The message is derived from the phaser and its condition source text, and it
/// has to live on the exception instance as well as on the `RuntimeError` —
/// `.message` reads the instance attribute, so leaving it off made every
/// `throws-like ..., message => /.../` assertion see an empty string.
pub(crate) fn phaser_prepost_error(is_pre: bool, condition: &str) -> RuntimeError {
    let phaser = if is_pre { "PRE" } else { "POST" };
    // raku: "Precondition '<cond>' failed" / "Postcondition '<cond>' failed".
    let kind = if is_pre {
        "Precondition"
    } else {
        "Postcondition"
    };
    // The MESSAGE quotes the condition trimmed, while `.condition` keeps the
    // raw source slice: raku reports `Precondition '0' failed` for a
    // `PRE 0` whose `.condition` is `"0 "` (the parser's slice runs to the
    // enclosing `}`). A block-form condition is unaffected — `{ ... }` has no
    // surrounding whitespace to trim.
    let message = format!("{} '{}' failed", kind, condition.trim());
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("phaser".to_string(), Value::str(phaser.to_string()));
    attrs.insert("condition".to_string(), Value::str(condition.to_string()));
    attrs.insert("message".to_string(), Value::str(message.clone()));
    let exception =
        Value::make_instance(crate::symbol::Symbol::intern("X::Phaser::PrePost"), attrs);
    let mut err = RuntimeError::new(message);
    err.exception = Some(Box::new(exception));
    err
}

/// Flatten arguments for `append` using Raku's "one-arg rule":
/// if exactly one non-itemized Array/List argument is passed, its elements
/// are flattened into the result. With multiple arguments, each is appended as-is.
///
/// ADR-0040 slice 1: the returned `Vec<Value>` is the final, post-flattening
/// per-element list for every call site (~13 of them) that extends a real
/// Array with it, so each element is itemized here — after the one-arg-rule
/// decision above, never before it (an itemized single Array argument must
/// stay itemized and NOT flatten: `!kind.is_itemized()` already guards that;
/// a flattened element that is itself an aggregate, e.g.
/// `@x.append(([1,2],[3,4]))`, itemizes too, matching raku).
pub(crate) fn flatten_append_args(args: Vec<Value>) -> Vec<Value> {
    let flattened = if args.len() == 1 {
        match args[0].view() {
            ValueView::Array(vals, kind) if !kind.is_itemized() => vals.to_vec(),
            ValueView::Seq(vals) => vals.to_vec(),
            ValueView::Hash(map) => {
                // Flatten hash into key-value pairs
                let mut result = Vec::new();
                for (k, v) in map.iter() {
                    result.push(Value::pair(k.clone(), v.clone()));
                }
                result
            }
            // A single Range flattens to its elements (`@x.append: 1..3` /
            // `"a".."c"`), same one-arg rule as an Array/List argument.
            ValueView::Range(..)
            | ValueView::RangeExcl(..)
            | ValueView::RangeExclStart(..)
            | ValueView::RangeExclBoth(..)
            | ValueView::GenericRange { .. } => crate::runtime::utils::value_to_list(&args[0]),
            _ => args,
        }
    } else {
        args
    };
    // Appended elements are COPIES, so a first-class element cell reaching
    // here as a plain value (`@a.append($p.value)`, ADR-0036) must be read
    // through rather than stored -- otherwise a later write to the source
    // rewrites the appended element. Only a bind aliases.
    flattened
        .into_iter()
        .map(|v| v.into_deref().itemize_for_element_store())
        .collect()
}

/// Flatten the *replacement* arguments of `.splice($offset, $size, ...)` --
/// i.e. `args[2..]` -- into the final list of elements to insert.
///
/// `splice` has its own one-arg rule, distinct from `append`'s
/// ([`flatten_append_args`]). Rakudo spells it as three families of
/// candidates (`Array.^lookup('splice').candidates>>.signature`):
///
/// - `(..., **@new)` -- the *non*-flattening slurpy: each argument becomes
///   exactly one element.
/// - `(..., @new)` -- a single argument that does `Positional`: its elements
///   are used.
/// - `(..., @new is item)` -- ditto for an *itemized* `Positional` (`$[7,8]`).
///
/// So the discriminator is `Positional`, and the `is item` candidate is why
/// splice differs from push/append in both directions:
///
/// - an itemized single Array still flattens here
///   (`@a.splice(1,1,$[7,8])` inserts `7, 8`), while `@a.append($[7,8])`
///   keeps it whole;
/// - a single `Hash`/`Set`/`Bag` is `Associative`, not `Positional`, so it
///   stays ONE element here, while `@a.append(%h)` flattens it to pairs.
///
/// A `Slip` flattens at *any* arity -- that is what a Slip is, and it is
/// independent of which candidate binds.
///
/// ADR-0040 slice 1: every value returned is a final stored element, so it is
/// itemized here -- after the one-arg-rule decision, never before it.
/// ADR-0049 slice 4: a `Nil` replacement decays to plain `Any`, NOT to the
/// target container's `is default(...)` value (confirmed against real `raku`;
/// splice differs from push/append/unshift/prepend here).
pub(crate) fn flatten_splice_replacement_args(args: &[Value]) -> Vec<Value> {
    let single = args.len() == 1;
    let mut out: Vec<Value> = Vec::new();
    for arg in args {
        match arg.view() {
            // A Slip flattens regardless of how many arguments there are.
            ValueView::Slip(vals) => out.extend(vals.iter().cloned()),
            // The one-arg rule proper: a lone `Positional` argument
            // contributes its elements, itemized or not.
            ValueView::Array(vals, _) if single => out.extend(vals.iter().cloned()),
            ValueView::Seq(vals) | ValueView::HyperSeq(vals) | ValueView::RaceSeq(vals)
                if single =>
            {
                out.extend(vals.iter().cloned())
            }
            ValueView::Range(..)
            | ValueView::RangeExcl(..)
            | ValueView::RangeExclStart(..)
            | ValueView::RangeExclBoth(..)
            | ValueView::GenericRange { .. }
                if single =>
            {
                out.extend(crate::runtime::utils::value_to_list(arg))
            }
            // A `Blob`/`Buf` does `Positional` too, so a lone one binds the
            // same `(..., @new)` candidate an `Array`/`List`/`Range` does and
            // contributes its *elements*. It reaches here as an `Instance`
            // rather than as a list-shaped view, which is why it needs its own
            // arm: `value_to_list` deliberately keeps a buffer whole (list
            // *assignment*, `my @a = $buf`, is one element), so the decode goes
            // through the buffer's own element accessor instead.
            ValueView::Instance { .. } if single => match Interpreter::buf_as_byte_items(arg) {
                Some(items) => out.extend(items),
                None => out.push(arg.clone()),
            },
            _ => out.push(arg.clone()),
        }
    }
    out.into_iter()
        .map(|v| {
            if v.is_nil() {
                Value::package(crate::symbol::Symbol::intern("Any"))
            } else {
                v.itemize_for_element_store()
            }
        })
        .collect()
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
    // Miri cannot call a foreign function, so it takes the documented
    // "offset could not be determined" arm below rather than aborting the
    // interpreter it is trying to check.
    #[cfg(all(not(target_arch = "wasm32"), not(miri), feature = "native"))]
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
    #[cfg(not(all(not(target_arch = "wasm32"), not(miri), feature = "native")))]
    {
        0
    }
}

type ProtectBlockCacheEntry = (
    Arc<CompiledCode>,
    Arc<CompiledFns>,
    Arc<Vec<(usize, String)>>,
    Arc<Vec<(usize, String)>>,
    Arc<Vec<String>>,
);
type ProtectBlockCache = HashMap<u64, ProtectBlockCacheEntry>;

/// ADR-0037 §2.3: how `EVAL ..., context => $ctx`'s `return` classifies,
/// derived once at EVAL entry from the routine identity `CALLER::` stamped
/// on `$ctx` (`Interpreter::eval_context_routine`). Liveness is decided at
/// entry rather than at each `return`, because the snippet runs synchronously
/// inside the `EVAL` call so no frame below it can disappear in between (see
/// the ADR's §2.3 rationale).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum EvalContextRoutineState {
    /// `$ctx` named a mainline (no routine dynamically encloses the captured
    /// frame): the snippet's `return` throws `X::ControlFlow::Return` right
    /// at the `return` site, matching raku's §1.1(a) probe.
    Mainline,
    /// `$ctx` named a routine that is still live on the dynamic call stack:
    /// an enclosing routine exists, same as the ambient (no-`context`)
    /// classification. The payload is that routine's registration clone id
    /// (`Interpreter::registration_clone_id`, the same identity space
    /// `RuntimeError::return_target_callable_id` compares against) when one
    /// resolves — `None` for a nameless routine frame (e.g. an anonymous sub
    /// with no `__mutsu_callable_id` registration), which cannot be targeted
    /// this way and falls back to the pre-Slice-4 first-boundary-catches
    /// behavior. ADR-0037 Slice 4: when `Some`, `compile_block_value_opts`
    /// bakes the id onto the compiled EVAL unit's `CompiledCode` so its
    /// `Return` targets that specific frame past any intervening routines
    /// (raku's §1.1(b) probe) instead of the first one encountered.
    Live(Option<u64>),
    /// `$ctx` named a routine that has already exited the dynamic call
    /// stack: throws `X::ControlFlow::Return` right at the `return` site,
    /// with `out-of-dynamic-scope` set and rakudo's fuller wording, matching
    /// raku's §1.1(c) probe.
    Dead,
}

/// The ambient interpreter state `compile_block_value_opts` folds into a
/// fresh `Compiler` before compiling a carrier block's body (`is_routine`/
/// `lexically_in_routine`, the enclosing package scope, sigilless/placeholder
/// seeding, `$?DISTRIBUTION`). A block invoked repeatedly from the SAME call
/// site (the overwhelmingly common shape — a `lives-ok { ... }` in a loop, a
/// comparator called many times) has an identical context on every call, so
/// this is the key `carrier_compile_cache` matches on to decide whether a
/// cached compile from a previous call is reusable. `PartialEq`, not `Eq`/
/// `Hash`: `distribution` is a `Value` (no `Hash` impl, and its `PartialEq`
/// is Raku's semantic equality) — see the doc comment on `CarrierCompileCache`
/// for why this rules out a plain `HashMap<Key, _>`.
#[derive(Clone, PartialEq)]
struct CarrierCompileCtxKey {
    is_eval_unit: bool,
    in_routine: bool,
    /// The fully-resolved package scope string `compile_block_value_opts`
    /// passes to `compiler.set_current_package` — already encodes whether an
    /// enclosing routine frame was present (`"{pkg}::&{name}"`) or not (bare
    /// `self.current_package()`), so no separate `enclosing_package` field is
    /// needed.
    scope: String,
    sigilless: Vec<String>,
    placeholder_params: Vec<String>,
    /// ADR-0059 Slice 2: whether the body's bare tail compiles in container
    /// mode (an `is rw`/`is raw` routine body). Part of the key because it
    /// changes the tail's bytecode.
    rw_tail: bool,
    distribution: Option<Value>,
    /// ADR-0037 §2.3's classification (only ever set when `is_eval_unit`),
    /// which affects the compiled bytecode beyond what `in_routine` alone
    /// captures: a `Mainline` and a `Dead` classification both compile
    /// `in_routine == false`, but a `Dead` unit's `return` additionally
    /// carries the out-of-dynamic-scope wording (`Compiler::
    /// eval_context_dead_routine`). Must stay in the key or the cache could
    /// serve a unit compiled under the wrong classification.
    eval_context_routine: Option<EvalContextRoutineState>,
}

/// Per-`SubData.id` cache of `(context, compiled)` pairs for
/// `eval_block_value_inner`'s carrier-block compile (see
/// `todo/deep/eval-block-value-recompiles-every-call.md`). A `Vec` rather
/// than a nested `HashMap` because `CarrierCompileCtxKey` cannot implement
/// `Hash`/`Eq` (it embeds a `Value`, compared by Raku's semantic `PartialEq`,
/// not a total order) — and because the realistic size is 1 entry per id
/// (the same block invoked from the same call site every time), so a linear
/// scan against `CarrierCompileCtxKey::eq` costs nothing. Capped at
/// `CARRIER_COMPILE_CACHE_MAX_CONTEXTS_PER_ID` entries per id to bound memory
/// for the rare block invoked from many distinct contexts.
type CarrierCompileCache =
    HashMap<u64, Vec<(CarrierCompileCtxKey, Arc<CompiledCode>, Arc<CompiledFns>)>>;

const CARRIER_COMPILE_CACHE_MAX_CONTEXTS_PER_ID: usize = 4;

/// Key for `map_grep_compile_cache`: pointer identity of a closure literal's
/// pre-existing `compiled_code`, plus whether the call site is lexically
/// inside a routine. Holds a clone of the `Arc` (not just its address) so the
/// key stays alive for as long as its cache entry does — see the field's doc
/// comment for why a bare pointer would be unsound.
#[derive(Clone)]
struct MapGrepCacheKey {
    origin: Arc<CompiledCode>,
    lexically_in_routine: bool,
}

impl PartialEq for MapGrepCacheKey {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.origin, &other.origin)
            && self.lexically_in_routine == other.lexically_in_routine
    }
}

impl Eq for MapGrepCacheKey {}

impl std::hash::Hash for MapGrepCacheKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (Arc::as_ptr(&self.origin) as usize).hash(state);
        self.lexically_in_routine.hash(state);
    }
}

mod accessors;
mod accessors_misc;
mod accessors_resolve;
mod accessors_stack;
mod accessors_stash;
mod accessors_state;
mod attr_build_defaults;
mod builtins;
mod builtins_atomic;
mod builtins_atomic_cas;
mod builtins_atomic_cas_code;
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
mod builtins_multidim_exists_adverb;
mod builtins_multidim_ops;
mod builtins_multidim_subscript;
mod builtins_multidim_subscript_adverb;
mod builtins_postcircumfix;
mod proxy_store;
pub(crate) use builtins_multidim_subscript::PositionalMissing;
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
mod ctor_phase_plan;
mod nqp_ops;
mod nqp_ops_process;
pub(crate) use class_introspection::UserMethodOrAccessor;
pub(crate) mod cstruct_layout;
mod decl_types;
pub(crate) mod nativecall_fnptr;
pub(crate) use self::decl_types::*;
pub(crate) mod deprecation;
pub(crate) mod did_you_mean;
mod dispatch;
mod dispatch_candidates;
mod dispatch_proto;
mod dispatch_proto_call;
mod dispatch_proto_rewrite;
mod dispatch_resolve;
mod eval_check;
mod eval_routine_magicals;
mod exception_message;
mod gc_roots;
mod handle;
mod handle_io;
mod handle_open;
mod handle_read;
mod handle_read_chars;
pub(crate) mod hoist_visibility;
mod incdec_rw_sub;
mod io;
mod io_doc;
mod io_env;
mod io_handles;
mod io_pod;
mod io_pod_blocks;
mod io_pod_config;
mod io_pod_entries;
mod io_pod_format;
mod io_pod_heredoc;
mod io_pod_table;
mod io_sysinfo;
mod io_sysinfo_host;
mod io_sysinfo_kernel;
mod io_sysinfo_user;
mod io_sysinfo_vm_config;
mod iterator_protocol;
pub(crate) mod json;
mod list_element_stringify;
mod listop_functions;
mod lock_async_recursion;
mod lock_reentry;
pub(crate) mod loop_handler_depth;
mod lvalue_container_return;
mod main_args;
mod match_target;
mod metamodel;
mod methods;
mod methods_adhoc_slurpy;
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
mod methods_distribution_cur_files;
mod methods_distribution_cur_inst;
mod methods_distribution_cur_resolve;
mod methods_distribution_helpers;
mod methods_enum_dispatch;
mod methods_enum_roles;
mod methods_enumhow;
mod methods_format;
mod methods_grammar;
mod methods_instance_ops;
mod methods_introspect;
mod methods_io_dispatch;
mod methods_match_dispatch;
mod methods_mixin_dispatch;
mod methods_mixin_what_cache;
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
mod methods_raku_dispatch;
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
mod methods_subscript_protocol;
mod methods_supply_dispatch;
mod methods_temporal;
mod methods_trans;
mod methods_type_coerce;
mod methods_walk;
mod native_io;
mod uncaught_render;
pub(crate) use native_io::{path_is_executable, path_is_readable, path_is_writable};
mod native_io_special;
pub(crate) mod native_methods;
mod native_proc_async;
mod native_supplier_methods;
mod native_supply_dispatch;
mod native_supply_methods;
mod native_supply_mut_methods;
pub(crate) mod native_types;
pub(crate) mod nativecall;
#[cfg(feature = "libffi")]
pub(crate) mod nativecall_callback;
pub(crate) mod nativecall_cast;
pub(crate) mod nativecall_global;
pub(crate) mod nativecall_manage;
pub(crate) mod once_store;
mod ops_bits;
mod ops_compare;
mod ops_reduction;
mod ops_set;
mod output_sink;
pub(crate) mod phasers;
mod promise_broken_gist;
mod promise_errors;
mod react_died;
pub(crate) mod react_done_handler_depth;
mod receiver_class;
pub(crate) mod regex;
pub(crate) mod regex_parse;
mod regex_parse_charclass;
mod regex_parse_core;
mod regex_parse_ltm;
mod regex_parse_modifier;
mod regex_types;
mod registration;
mod registration_class;
mod registration_class_attr;
mod registration_class_augment;
mod registration_class_body;
mod registration_class_body_attr;
mod registration_class_body_does;
mod registration_class_body_exit;
mod registration_class_body_method;
mod registration_class_body_method_forms;
mod registration_class_compose;
mod registration_class_compose_body;
mod registration_class_compose_record;
mod registration_class_decl;
mod registration_class_validate;
mod registration_role;
mod registration_role_body;
mod registration_role_decl;
mod registration_role_method;
pub(crate) mod registration_sub;
mod registry;
mod registry_method_table;
pub(crate) mod resolution;
mod resolution_call_sub;
mod resolution_deferral;
mod resolution_eval;
mod resolution_lazy;
pub(crate) mod resolution_map_grep;
mod resolution_map_grep_rw;
mod resolution_method;
mod resolution_private_method;
mod resolution_sequence;
mod run;
mod run_dist;
mod run_main;
mod run_modules;
mod run_prelude;
mod run_roast_preprocess;
mod runtime_caller_env;
mod runtime_class_query;
mod runtime_container;
mod runtime_encoding;
mod runtime_init;
mod runtime_module;
mod runtime_module_export_sub;
mod runtime_module_exports;
mod runtime_output;
pub(crate) mod runtime_shared_vars;
mod runtime_thread;
pub(crate) mod runtime_var_meta;
mod seq_helpers;
mod sequence;
pub(crate) mod shared_store;
mod signal_watcher;
pub(crate) mod slang_activation;
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
pub(crate) use test_functions::TEST_MODULE_EXPORTS;
pub(crate) mod thread_compat;
pub(crate) mod types;
// `pub(crate)`: the analysis frontend (`crate::analysis`, ADR-0065) calls the
// interpreter-free entry point directly.
pub(crate) mod undeclared_routines;
mod unicode;
pub(crate) mod utf8_c8;
pub(crate) mod utils;
pub(crate) mod value_iterator;
/// Cooperative scheduler standing in for OS threads in the browser.
#[cfg(target_arch = "wasm32")]
pub(crate) mod wasm_sched;
/// Elastic worker pool for short-lived user tasks (ADR-0020).
pub(crate) mod worker_pool;
pub(crate) use self::match_target::MatchTarget;
pub(crate) use self::methods_subscript_protocol::refuse_map_removal;
pub(crate) use self::output_sink::OutputSink;
#[allow(unused_imports)]
pub(crate) use self::output_sink::{OutputSinkReadGuard, OutputSinkWriteGuard};
pub(crate) use self::regex_types::*;
pub(crate) use self::registration_class::ClassDeclModifiers;
pub(crate) use self::registry::Registry;
pub(crate) use self::tap_state::{TapState, TestState, TodoRange};

pub(crate) use utils::*;

// Re-export thread utility functions for VM access
pub(crate) use methods_collection_ops::{current_mutsu_thread_id, is_initial_thread};
pub(crate) use methods_raku_dispatch::container_needs_raku_dispatch;

use self::unicode::{check_unicode_property, check_unicode_property_with_args};

/// One class/role attribute declaration.
///
/// Field order matches the historical tuple layout:
/// `(attr_name, is_public, default, is_rw, is_required, sigil, where_constraint)`.
///
/// `default`/`where_constraint` are `DeclTraitArg` rather than a raw `Expr`
/// (ADR-0019 D2c-2): every reader now runs them through
/// `Interpreter::eval_decl_trait_arg`/`.literal()` instead of its own
/// `Expr::Literal` pattern match, unifying the eval mechanism across the
/// ~15 sites that fill/check attribute defaults and `where` constraints.
/// Both fields may now be a `Compiled` chunk (ADR-0019 D2c-4), so
/// `.as_expr()` is no longer panic-free on them — `declared_shape` exists
/// precisely so the one caller that used to read `default` as an `Expr`
/// (the shaped-`@`-attribute pattern match) does not need to call it.
#[derive(Debug, Clone)]
pub(crate) struct ClassAttributeDef {
    pub(crate) name: String,
    pub(crate) is_public: bool,
    pub(crate) default: Option<crate::opcode::DeclTraitArg>,
    pub(crate) is_rw: bool,
    pub(crate) is_required: Option<Option<String>>,
    pub(crate) sigil: char,
    pub(crate) where_constraint: Option<crate::opcode::DeclTraitArg>,
    /// Declared shape dimensions for an `@`-sigil attribute (`has @.a[2]`),
    /// copied from `CompiledAttrDecl::declared_shape` at registration time
    /// (ADR-0019 D2c-4). `None` for a non-plan-backed construction site
    /// (`.^add_attribute`, builtin `Proc` attributes) — none of those are
    /// ever compiler-generated shaped-array defaults.
    pub(crate) declared_shape: Option<Vec<usize>>,
}

/// The set of read-only variable names (`readonly_vars`), and the type of a
/// snapshot taken by `save_readonly_vars`.
///
/// `Symbol`-keyed and copy-on-write: every user function call snapshots this set
/// on entry and restores it on return, so a `HashSet<String>` cost one table
/// allocation plus one heap `String` per entry *per call*. Behind an `Arc` the
/// snapshot is a refcount bump, and a mutation (`mark_readonly` /
/// `unmark_readonly`) pays a `memcpy` of `u32`s only when it actually changes
/// the set while a snapshot is alive. `Symbol` keys also replace the default
/// hasher's SipHash-over-the-name with a `u32` hash.
///
/// The value records *why* the name is readonly ([`ReadonlyKind`]), which is
/// what decides the exception an assignment through it throws.
pub(crate) struct ReadonlySet {
    map: rustc_hash::FxHashMap<Symbol, ReadonlyKind>,
    /// Whether the topic `_` is currently in `map`.
    ///
    /// Every routine call clears the caller's readonly mark on `$_` before
    /// binding its parameters (see `call_compiled_function_positional_light_at`),
    /// and the guard for that was "is the set non-empty" -- true for any program
    /// with a single readonly parameter anywhere on the stack, so the call paid a
    /// full hash `remove` that missed, on every call. This answers the question
    /// exactly, in one branch.
    ///
    /// Kept on the set itself rather than on the `Interpreter` so that every
    /// mutation path maintains it by construction -- including
    /// [`replay_readonly_undo`], which reaches the set through a raw pointer from
    /// a `Drop` impl and never sees the `Interpreter` at all. `topic_marked`
    /// re-derives the slow answer under `debug_assert`, and CI runs the whole
    /// `t/` suite on a debug binary (ADR-0014), so the invariant is checked by
    /// 3600+ files on every push.
    topic: bool,
    /// Direct-mapped *positive* cache over [`Self::map`], indexed by
    /// `sym.raw() & (READONLY_CACHE_SLOTS - 1)`.
    ///
    /// Every routine call marks each of its parameters readonly and unmarks
    /// them on return, and in the recursive/monomorphic steady state the mark
    /// is a pure no-op -- the same name is already in the set with the same
    /// kind, put there by an outer frame. Answering "already marked with this
    /// kind?" through the hash map cost a full SwissTable probe (plus, before
    /// this cache, a hash *insert*: probe, write, length bookkeeping) on the
    /// hottest call path; `bench-fib` spent ~6% of its cycles there.
    ///
    /// Invariant: an occupied slot `(s, k)` implies `map[s] == k`. A slot never
    /// implies *absence*, so a miss (empty slot, or a slot holding a different
    /// symbol that evicted this one) falls through to the map. That is what
    /// makes the cache sound under collision: an insert always overwrites its
    /// slot, and a remove only clears a slot that still names the symbol being
    /// removed -- an evicted entry simply stops being cached, it is never
    /// wrongly reported.
    ///
    /// Kept on the set itself, like [`Self::topic`], so every mutation path
    /// maintains it by construction (including the whole-set `mem::take` /
    /// assignment in `take_readonly_state` / `restore_readonly_state`, which
    /// move the cache with the map it describes). Each read re-derives the slow
    /// answer under `debug_assert`, and CI runs the whole `t/` suite on a debug
    /// binary (ADR-0014), so the invariant is checked by 3600+ files per push.
    cache: [Option<(Symbol, ReadonlyKind)>; READONLY_CACHE_SLOTS],
}

/// Slot count of [`ReadonlySet::cache`]. A power of two so the index is a mask.
/// Sized to hold every readonly name a realistic call stack has live at once
/// (parameters and loop aliases) without the table itself costing a cache line
/// per probe.
const READONLY_CACHE_SLOTS: usize = 64;

impl Default for ReadonlySet {
    fn default() -> Self {
        Self {
            map: rustc_hash::FxHashMap::default(),
            topic: false,
            cache: [None; READONLY_CACHE_SLOTS],
        }
    }
}

impl ReadonlySet {
    #[inline(always)]
    fn slot(sym: Symbol) -> usize {
        (sym.raw() as usize) & (READONLY_CACHE_SLOTS - 1)
    }

    #[inline]
    pub(crate) fn insert(&mut self, sym: Symbol, kind: ReadonlyKind) -> Option<ReadonlyKind> {
        if sym == crate::symbol::wk::topic() {
            self.topic = true;
        }
        // Overwrite unconditionally: whatever this evicts stays correct in the
        // map, it merely stops being cached.
        self.cache[Self::slot(sym)] = Some((sym, kind));
        self.map.insert(sym, kind)
    }

    #[inline]
    pub(crate) fn remove(&mut self, sym: &Symbol) -> Option<ReadonlyKind> {
        if *sym == crate::symbol::wk::topic() {
            self.topic = false;
        }
        let slot = Self::slot(*sym);
        // Only clear a slot that still names this symbol -- a slot holding the
        // symbol that evicted it still describes a live map entry.
        if let Some((cached, _)) = self.cache[slot]
            && cached == *sym
        {
            self.cache[slot] = None;
        }
        self.map.remove(sym)
    }

    /// Is `sym` marked with exactly `kind`? The question every parameter mark
    /// asks before doing anything, answered from the cache when it can be.
    #[inline]
    pub(crate) fn marked_with(&self, sym: Symbol, kind: ReadonlyKind) -> bool {
        if let Some((cached, cached_kind)) = self.cache[Self::slot(sym)]
            && cached == sym
        {
            debug_assert_eq!(
                self.map.get(&sym),
                Some(&cached_kind),
                "ReadonlySet::cache drifted from the map"
            );
            return cached_kind == kind;
        }
        self.map.get(&sym) == Some(&kind)
    }

    #[inline]
    pub(crate) fn contains_key(&self, sym: &Symbol) -> bool {
        if let Some((cached, cached_kind)) = self.cache[Self::slot(*sym)]
            && cached == *sym
        {
            debug_assert_eq!(
                self.map.get(sym),
                Some(&cached_kind),
                "ReadonlySet::cache drifted from the map"
            );
            return true;
        }
        self.map.contains_key(sym)
    }

    #[inline]
    pub(crate) fn get(&self, sym: &Symbol) -> Option<&ReadonlyKind> {
        self.map.get(sym)
    }

    #[inline]
    pub(crate) fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    /// Is the topic `_` marked readonly? O(1), no hashing.
    #[inline]
    pub(crate) fn topic_marked(&self) -> bool {
        debug_assert_eq!(
            self.topic,
            self.map.contains_key(&crate::symbol::wk::topic()),
            "ReadonlySet::topic drifted from the map"
        );
        self.topic
    }
}

#[cfg(test)]
mod readonly_set_cache_tests {
    use super::{READONLY_CACHE_SLOTS, ReadonlySet};
    use crate::ast::ReadonlyKind;
    use crate::symbol::Symbol;

    /// Two distinct symbols that land in the same `ReadonlySet::cache` slot.
    /// Interned ids are assigned sequentially, so probing a few hundred names
    /// always finds a colliding pair.
    fn colliding_pair() -> (Symbol, Symbol) {
        let syms: Vec<Symbol> = (0..READONLY_CACHE_SLOTS * 4)
            .map(|i| Symbol::intern(&format!("__ro_cache_probe_{i}")))
            .collect();
        for (i, &a) in syms.iter().enumerate() {
            for &b in &syms[i + 1..] {
                if ReadonlySet::slot(a) == ReadonlySet::slot(b) {
                    return (a, b);
                }
            }
        }
        unreachable!("no colliding symbol pair among {} names", syms.len());
    }

    /// The cache is a *positive* cache: an occupied slot proves membership, an
    /// empty or evicted one proves nothing. Eviction and removal must never
    /// turn that into a wrong answer.
    #[test]
    fn an_evicted_entry_is_still_reported_from_the_map() {
        let (a, b) = colliding_pair();
        let mut set = ReadonlySet::default();
        set.insert(a, ReadonlyKind::Alias);
        set.insert(b, ReadonlyKind::Alias); // evicts `a` from the shared slot
        assert!(set.contains_key(&a), "an evicted entry is still a member");
        assert!(set.contains_key(&b));
        assert!(set.marked_with(a, ReadonlyKind::Alias));
        assert!(set.marked_with(b, ReadonlyKind::Alias));

        // Removing the evicted symbol must not clear the slot the *other*
        // symbol now owns.
        set.remove(&a);
        assert!(!set.contains_key(&a));
        assert!(set.contains_key(&b), "the evicting entry survives");
        assert!(set.marked_with(b, ReadonlyKind::Alias));

        set.remove(&b);
        assert!(!set.contains_key(&b));
        assert!(set.is_empty());
    }

    /// Re-marking with a different kind must be visible through the cache.
    #[test]
    fn a_rekinded_entry_reports_the_new_kind() {
        let sym = Symbol::intern("__ro_cache_rekind");
        let mut set = ReadonlySet::default();
        set.insert(sym, ReadonlyKind::Alias);
        assert!(set.marked_with(sym, ReadonlyKind::Alias));
        set.insert(sym, ReadonlyKind::Immutable);
        assert!(set.marked_with(sym, ReadonlyKind::Immutable));
        assert!(!set.marked_with(sym, ReadonlyKind::Alias));
        assert!(set.contains_key(&sym));
    }

    /// A symbol that was never inserted is not reported by a stale slot.
    #[test]
    fn a_never_inserted_symbol_is_not_a_member() {
        let (a, b) = colliding_pair();
        let mut set = ReadonlySet::default();
        set.insert(a, ReadonlyKind::Alias);
        assert!(!set.contains_key(&b), "a colliding non-member stays absent");
        assert!(!set.marked_with(b, ReadonlyKind::Alias));
    }
}

/// One journaled readonly-set mutation (see `Interpreter::enter_readonly_frame`):
/// the inverse to replay on scope exit, or a `Scope` sentinel marking a frame
/// boundary (bounds the unmark/mark cancellation peephole).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum ReadonlyUndo {
    Marked(Symbol),
    Unmarked(Symbol, ReadonlyKind),
    /// The name was already readonly but with a different kind; restore it.
    Rekinded(Symbol, ReadonlyKind),
    Scope,
}

/// The full readonly state swapped out around a lazily-forced body run — see
/// `Interpreter::take_readonly_state` / `restore_readonly_state`.
pub(crate) struct SavedReadonlyState {
    pub(crate) vars: ReadonlySet,
    pub(crate) undo: Vec<ReadonlyUndo>,
    pub(crate) frames: u32,
}

/// Close a readonly scope: undo every journaled mutation made since the
/// matching `enter_readonly_frame`, newest first, then pop the scope
/// sentinel. This is the shared implementation behind both
/// `Interpreter::exit_readonly_frame` (called with `&mut Interpreter`, e.g.
/// from `pop_call_frame`) and
/// [`crate::vm::vm_call_state_guard::ReadonlyFrameGuard`]'s `Drop` impl
/// (called through raw pointers into `readonly_vars`/`readonly_undo`/
/// `readonly_frames`'s own boxed allocations, since `Drop::drop` cannot
/// obtain `&mut Interpreter` — see that guard's doc comment). Taking `&Cell`/
/// `&RefCell` here rather than `&mut Interpreter` is what lets both callers
/// share one implementation without either duplicating this logic or
/// reintroducing the unsound whole-`Interpreter` raw-pointer patterns
/// documented in `vm_call_state_guard.rs`'s module doc.
pub(crate) fn replay_readonly_undo(
    vars: &std::cell::RefCell<ReadonlySet>,
    undo: &std::cell::RefCell<Vec<ReadonlyUndo>>,
    frames: &Cell<u32>,
    mark: usize,
) {
    frames.set(frames.get().saturating_sub(1));
    let mut undo_ref = undo.borrow_mut();
    let mut vars_ref = vars.borrow_mut();
    while undo_ref.len() > mark {
        match undo_ref.pop().unwrap() {
            ReadonlyUndo::Marked(sym) => {
                vars_ref.remove(&sym);
            }
            ReadonlyUndo::Unmarked(sym, kind) => {
                vars_ref.insert(sym, kind);
            }
            ReadonlyUndo::Rekinded(sym, kind) => {
                vars_ref.insert(sym, kind);
            }
            // An abandoned inner scope's sentinel: its exit was skipped by an
            // error unwind (or, prior to this guard's introduction, a Rust
            // panic), so re-balance the open-scope counter here.
            ReadonlyUndo::Scope => {
                frames.set(frames.get().saturating_sub(1));
            }
        }
    }
    // Pop this scope's own sentinel (at `mark - 1`).
    debug_assert!(matches!(undo_ref.last(), Some(ReadonlyUndo::Scope)));
    undo_ref.pop();
}

/// A set of variable names (`block_declared_vars` / `loop_local_vars`), keyed by
/// the interned `Symbol` rather than an owned `String`.
///
/// Every `my` declaration probes both sets, and every consumer already holds the
/// name's `Symbol` (env keys, `CompiledCode::locals_sym`, closure free vars) —
/// the String keying made each probe hash the name's bytes and `memcmp` them on
/// a hit, and each insert allocate. A `Symbol` is a `Copy` u32: the hash is
/// free, the compare is an integer compare, and the (cold) consumers that need
/// text call `resolve()`.
pub(crate) type NameSet = rustc_hash::FxHashSet<Symbol>;

/// Per-class plan for the native default constructor
/// (`try_native_default_construct`): everything about the class shape that the
/// constructor consulted on EVERY construction but that only changes when the
/// registry's class shape changes — eligibility, the MRO-collected attribute
/// defs, attribute type constraints, and the BUILD/TWEAK/smiley MRO probes.
/// Cached in `Interpreter::native_ctor_plan_cache`; invalidated together with
/// the method-dispatch caches at every registry/type mutation site, plus the
/// MOP mutators that alter class shape without passing those sites
/// (`Attribute.set_build`, `^add_attribute`, `^add_method`, `^compose`).
pub(crate) struct NativeCtorPlan {
    pub(crate) is_cunion: bool,
    pub(crate) eligible: bool,
    pub(crate) class_attrs: Arc<Vec<ClassAttributeDef>>,
    /// Interned attribute names, same order as `class_attrs`. Construction
    /// inserts attributes by Symbol so the per-bless per-attribute
    /// `String` clone + re-intern is paid once per class, not per instance.
    pub(crate) attr_syms: Arc<Vec<crate::symbol::Symbol>>,
    pub(crate) type_constraints: Arc<HashMap<String, String>>,
    pub(crate) has_build: bool,
    pub(crate) has_tweak: bool,
    pub(crate) has_smiley: bool,
    /// True when this class's attribute set is FULLY known to the registry:
    /// the class is user-declared and every type in its MRO other than the
    /// universal roots (`Any`/`Mu`/`Cool`) is user-declared too.
    ///
    /// Raku's default `BUILDALL` only initialises DECLARED attributes and
    /// silently ignores a named argument that names none — upstream
    /// `Cro::HTTP2::FrameParser` relies on that, splatting a `conn => …` header
    /// into every frame class. mutsu used to stash such a stray key in the
    /// instance's attribute map, where `.^attributes` never showed it but
    /// `eqv`/`===` compared it, so a parsed frame never matched an otherwise
    /// identical one built by hand. Construction drops the stray key when this
    /// is true. It has to be false for a class with a BUILTIN base (`is
    /// Exception`, `is Supplier`, …): those keep attributes of their own
    /// outside the registry (`message`, `payload`, …) that construction must
    /// still accept.
    pub(crate) attrs_fully_known: bool,
    /// A user-defined (or role-composed) public `bless` method anywhere in the
    /// MRO — such a class must take the interpreter's generic dispatch instead
    /// of the native `bless` fork.
    pub(crate) has_custom_bless: bool,
    /// True if this class declares an `is default(...)` element default on any
    /// attribute (keyed by the receiver class name in `class_attribute_defaults`
    /// / `class_attribute_default_exprs`). When false, `apply_container_attribute_defaults`
    /// is a guaranteed no-op — every per-attribute registry probe returns `None` —
    /// so the whole scan (its keys `Vec` plus the `(String, String)` registry-key
    /// allocs) is skipped. The overwhelmingly common case.
    pub(crate) has_container_defaults: bool,
    /// MRO-resolved `is Type` container attribute traits (`has %.h is X`):
    /// attr name -> type name. Replaces the per-construction
    /// `attribute_is_type_in_mro` MRO walk (a `(String, String)` tuple-key
    /// alloc per MRO level per unfilled `@`/`%` attribute).
    pub(crate) attr_is_types: Arc<HashMap<String, String>>,
    /// Pre-derived BUILD/TWEAK phase step lists (`runtime/ctor_phase_plan.rs`):
    /// the base-first MRO walk, per-level registry probes, role-submethod
    /// ordering, and 6.c/6.e skip decisions that the construction phases
    /// re-derived on every single construction. Empty when `has_build` /
    /// `has_tweak` is false.
    pub(crate) build_steps: Arc<Vec<ConstructionPhaseStep>>,
    pub(crate) tweak_steps: Arc<Vec<ConstructionPhaseStep>>,
    /// Attribute-name skeleton (declared attr names -> Nil) usable as the
    /// phase-dispatch probe map when the live cell carries no sigilless-alias
    /// metadata: every consumer on that path reads only the key set (see
    /// `run_construction_phase_steps`), so the per-construction whole-cell
    /// `to_map()` value clone is skipped.
    pub(crate) probe_skeleton: Arc<crate::value::AttrMap>,
}

/// One pre-derived step of a construction phase (BUILD or TWEAK) — see
/// `NativeCtorPlan::{build_steps, tweak_steps}`.
pub(crate) enum ConstructionPhaseStep {
    /// A role-composed submethod at this MRO level, with its owning role and
    /// already-collected def (what `ordered_role_submethods_for_class`
    /// re-derived per construction).
    Role { role_name: String, def: MethodDef },
    /// The class's own candidate at this MRO level, dispatched with
    /// `mro_class` as the receiver. `pinned` carries the single simple
    /// candidate when dispatch may bypass method resolution (the common
    /// `submethod TWEAK` shape); `None` keeps the full
    /// `run_instance_method_celled` path.
    Class {
        mro_class: String,
        pinned: Option<MethodDef>,
    },
}

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

/// Intern a static name list into the `Arc<[Symbol]>` shape used by
/// [`ClassDef::mro`]. Registration-time helper (not a dispatch hot path).
pub(crate) fn sym_mro(names: &[&str]) -> std::sync::Arc<[crate::symbol::Symbol]> {
    names
        .iter()
        .map(|s| crate::symbol::Symbol::intern(s))
        .collect()
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

    pub(crate) fn set_nonblocking(&self, nonblocking: bool) -> std::io::Result<()> {
        match self {
            SocketStream::Tcp(s) => s.set_nonblocking(nonblocking),
            #[cfg(unix)]
            SocketStream::Unix(s) => s.set_nonblocking(nonblocking),
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
    /// Whether a read from a *non-seekable* stream (stdin, or the stdin
    /// fallback of `$*ARGFILES`) has already hit end-of-stream. Such handles
    /// cannot answer `.eof` by comparing a position against a length, and
    /// Rakudo does not peek ahead either: `$*IN.eof` stays `False` until a
    /// read actually came back empty, and only then flips to `True`.
    stream_hit_eof: bool,
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

/// Entry in the callframe stack, tracking state for each call frame.
#[derive(Clone)]
pub(crate) struct CallFrameEntry {
    pub file: String,
    pub line: i64,
    pub code: Option<Value>,
    pub env: Env,
}

/// Entry in the routine stack, tracking the call chain for backtraces.
///
/// `package`/`lexical_package`/`name`/`file`/`def_file` are interned
/// `Symbol`s (not `String`s): they used to be several heap allocations per
/// push, which made the fast repeat-call path (`call_compiled_function_fast`,
/// see `vm/vm_call_fast.rs`) skip pushing a frame entirely to avoid the cost —
/// the bug this fixes (`todo/tickets/repeat-call-loses-backtrace-frame.md`).
/// `Symbol` is `Copy` and the strings are almost always already interned
/// (constant-pool names, `SubData::package`/`name`), so a push is now a plain
/// `Vec::push` with no allocation, and every call path can afford to push one
/// unconditionally. Readers resolve back to `&str`/`String` via
/// `Symbol::as_str()` / `Symbol::resolve()` at render time.
#[derive(Clone, Copy, Debug)]
pub(crate) struct RoutineFrame {
    pub package: Symbol,
    /// Package whose compunit lexical routines are visible to this frame.
    pub lexical_package: Option<Symbol>,
    pub name: Symbol,
    pub line: Option<u32>,
    pub file: Option<Symbol>,
    pub is_method: bool,
    /// Whether this method frame belongs to a `submethod` declaration.
    pub is_submethod: bool,
    /// Whether this frame is a block/closure (not a named routine).
    pub is_block: bool,
    /// The file this routine's BODY lives in (None = same as the caller /
    /// main script). `line`/`file` above record the call-site; a backtrace
    /// displays each frame at its defining file (module subs report the
    /// module path, integration/error-reporting.t test 15).
    pub def_file: Option<Symbol>,
    /// Monotonic id of THIS invocation. Distinguishes one call of a routine
    /// from the next, which is what a per-call anonymous state (`$++` inside a
    /// block inside a routine) keys on — see `Interpreter::anon_state_key`.
    pub invocation_id: u64,
}

/// Hands out *blocks* of routine-invocation ids, not individual ones.
///
/// The id is an opaque per-call discriminator, so all it has to be is unique
/// among concurrently live frames and never 0 (0 means "the mainline is the
/// innermost scope"). It used to be an `AtomicU64::fetch_add` per id, which put
/// a `lock xadd` on a process-global line on the entry path of *every* routine
/// call — about 8% of `benchmarks/fib.raku`, spent entirely on being ready to
/// interleave with threads that are usually not there. Each interpreter claims
/// a block instead and counts inside it with a plain increment, so the atomic
/// fires once per `INVOCATION_ID_BLOCK` calls per thread and ids stay globally
/// unique. Blocks are never returned; at 2^64 ids that is not a budget.
static NEXT_INVOCATION_ID_BLOCK: std::sync::atomic::AtomicU64 =
    std::sync::atomic::AtomicU64::new(1);

/// Ids claimed per block. Large enough that the atomic is noise on any call
/// path; a thread that exits having used one id wastes the rest, which costs
/// nothing.
const INVOCATION_ID_BLOCK: u64 = 4096;

/// Claim a fresh block of invocation ids, returning its first id.
fn claim_invocation_id_block() -> u64 {
    NEXT_INVOCATION_ID_BLOCK.fetch_add(INVOCATION_ID_BLOCK, std::sync::atomic::Ordering::Relaxed)
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

/// One instance's worth of "attributes BUILD assigned", pushed for the duration
/// of that instance's BUILD phase (see `Interpreter::build_attr_writes`).
pub(crate) struct BuildWriteFrame {
    /// Address of the instance's shared attribute cell, used to attribute a
    /// write to the right frame when BUILD constructs further objects.
    pub(crate) cell_addr: usize,
    /// Attribute cell keys written while this frame was live.
    pub(crate) written: HashSet<crate::symbol::Symbol>,
}

/// A registered `END` phaser, held until program exit.
///
/// Raku's `END` is a closure over its enclosing lexical scope, so it must see
/// the *final* value of every lexical it mentions. mutsu's `Env` is value-keyed
/// rather than cell-keyed, so the body carries a captured copy instead; keeping
/// that copy faithful is what `dead_keys` is for (see its doc comment).
#[derive(Clone)]
pub(crate) struct EndPhaser {
    pub(crate) body: Vec<crate::ast::Stmt>,
    /// The lexical env as of the moment the declaring scope died (or as of
    /// registration, for a scope that is still alive at program exit).
    pub(crate) env: Env,
    /// The declaring package. END bodies run at program exit, long after
    /// `current_package` has returned to GLOBAL — a phaser declared in a
    /// `unit module Foo` must still see `Foo`'s routines by their bare names.
    pub(crate) package: String,
    /// Keys whose declaring scope has since died. At exit the captured value is
    /// the only surviving one, so it must win over a live same-named variable
    /// in an enclosing scope — `{ my $a = 42; END { say $a } }` prints 42 even
    /// when an outer `my $a = 1` is what the exit-time env holds. Every other
    /// captured key names a variable that is *still alive*, so the live value
    /// wins and a later mutation is visible, as it is in Raku.
    pub(crate) dead_keys: NameSet,
    /// Install order, which is what decides the exit-time run order (END
    /// phasers run in reverse of it). It is NOT the registration order: mutsu
    /// registers the main compunit's top-level ENDs *eagerly*, before the body
    /// runs, so a `use` on line 1 registers the module's END after them even
    /// though rakudo installs it first. See [`end_order`].
    pub(crate) order: u64,
}

/// Install-order bases for [`EndPhaser::order`]; lowest = installed earliest =
/// run last.
///
/// rakudo installs an END phaser when the compunit that declares it is
/// *compiled*, so `use M` on line 1 installs `M`'s ENDs before any of the
/// script's own, and the LIFO run order then puts the script's first. mutsu
/// loads modules at run time and hoists the main compunit's top-level ENDs to
/// before the body (so they still run when the body dies), which reverses the
/// two. Sorting by these bases at exit restores rakudo's order without giving
/// up the hoist.
pub(crate) mod end_order {
    /// A module's ENDs, in load order — a nested `use` installs the inner
    /// module's first, exactly as rakudo does.
    pub(crate) const MODULE: u64 = 0;
    /// The main compunit's ENDs, keyed by SOURCE POSITION — a top-level one
    /// and one inside a block or a sub share this class, because rakudo
    /// installs both as its compiler walks past them. Ordering them by
    /// registration instead put every top-level END (mutsu hoists those) ahead
    /// of every block-scoped one, so `{ END {…} } END {…}` ran the block's
    /// first where rakudo runs the mainline's first.
    pub(crate) const MAIN: u64 = 1 << 40;
    /// ENDs registered from inside an `EVAL`. rakudo compiles an EVAL'd snippet
    /// at RUN time, so its ENDs install after everything the main compunit
    /// declared and run before them — the opposite of a plain `use`
    /// (`File::Temp`'s `03-tempfile.rakutest` turns on exactly this).
    pub(crate) const RUNTIME: u64 = 2 << 40;

    /// Position of one END within its class. A main-compunit END is keyed by
    /// its source LINE, with the monotonic registration sequence only breaking
    /// ties (several ENDs on one line, or one line reached repeatedly); a
    /// module's or an EVAL's END has no meaningful line in the main compunit's
    /// numbering and is keyed by the sequence alone.
    pub(crate) fn slot(line: Option<u32>, seq: u64) -> u64 {
        match line {
            Some(line) => ((line as u64) << 20) | (seq & 0xF_FFFF),
            None => seq,
        }
    }
}

/// What a `(name, callsite package)` pair in `pos_light_call_cache` resolves to.
///
/// Both variants denote a body that `is_positional_light_call_eligible` has
/// already accepted for this name, so the hot `CallFunc` path can dispatch to
/// `call_compiled_function_positional_light` without re-running the eligibility
/// and argument-shape analysis. They differ only in who owns the body.
#[derive(Debug, Clone)]
pub(crate) enum PosLightTarget {
    /// A body the compiler emitted ahead of time; it lives in `compiled_fns`
    /// and is re-validated against its fingerprint on each hit.
    Compiled { key: Symbol, fingerprint: u64 },
    /// A body compiled on the fly from its `FunctionDef` AST — the shape every
    /// routine declared inside a block takes, because its `compiled_fns` key is
    /// namespaced by the enclosing closure and bare-name resolution cannot
    /// reach it. Before this variant existed, such a call could never reach the
    /// ultra-fast path: it hit `otf_call_cache` further down `exec_call_func_op`
    /// and re-derived the callsite analysis on every single call, which made a
    /// block-local sub 1.7x more expensive to call than an identical file-scope
    /// one. The package half of the map key mirrors `otf_call_cache`'s package
    /// keying (the same bare name means different routines in different
    /// packages).
    Otf { cf: Arc<CompiledFunction> },
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
    /// Parse warnings (e.g. "Duplicate 'is export' trait") already surfaced
    /// during the current top-level `run()` invocation, keyed by (origin
    /// file, message text). A module's source can be parsed more than once
    /// within a single run — once during the importer's export scan, once
    /// more when the `use` actually loads it — and each parse's warnings are
    /// drained and printed independently, so without this the same warning
    /// prints once per parse. Reset at the top of `run()` (not left to
    /// accumulate for the process lifetime), so a *separate* top-level
    /// program sharing this Interpreter instance (a later REPL line, e.g.)
    /// still sees its own warnings rather than having them silently
    /// swallowed by a stale entry. See
    /// `todo/tickets/module-parse-warning-reported-twice.md`.
    surfaced_parse_warnings: std::collections::HashSet<(Option<String>, String)>,
    /// All TAP / `Test` module runtime state (counter, subtest stack, bail-out).
    /// See [`TapState`] — extracted out of this struct so its ownership can later
    /// move (lever B). Access only through `self.tap`'s methods.
    tap: TapState,
    halted: bool,
    exit_code: i64,
    /// Set while the END phasers run for a program that is already exiting, and
    /// once any END phaser has itself called `exit`. A further `exit` still
    /// unwinds but leaves [`Self::exit_code`] alone — rakudo latches the process
    /// status at the first `exit` (`the-end-is-nigh`), so `exit 42; END { exit 7 }`
    /// exits 42. See `Interpreter::finish` and `builtin_exit`.
    exit_status_locked: bool,
    /// Body fingerprints (see [`crate::ast::function_body_fingerprint`]) of MAIN
    /// candidates declared `is hidden-from-USAGE`. Such a candidate is skipped
    /// when generating the usage message (but still participates in dispatch).
    main_hidden_from_usage: std::collections::HashSet<u64>,
    /// Set once the program explicitly calls `RUN-MAIN`. When set, the implicit
    /// end-of-program `MAIN` dispatch is suppressed: a program that drives MAIN
    /// itself via `RUN-MAIN` (as the `S06-other/main-refactored` spec does) must
    /// not have mutsu re-run MAIN a second time — Rakudo has no separate implicit
    /// dispatch, `RUN-MAIN` *is* the mechanism.
    explicit_run_main: bool,
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
    /// Short-form infix operator sub names (`infix:<+>`, ...) that have ever
    /// been user-declared, regardless of package/associativity. Consulted as a
    /// cheap guard by the VM's native-arithmetic fast paths (`exec_add_op` and
    /// friends) so they only pay for a full multi-dispatch resolution lookup
    /// when a user override could plausibly exist, keeping the common
    /// no-override hot path (e.g. tight `Int + Int` loops) free of registry
    /// lookups.
    /// Each entry maps the operator name to the compilation units that declared
    /// it (`?FILE` at declaration time). An EMPTY file set means "provenance
    /// unknown, visible everywhere" and is what module *exports* record, since
    /// an exported operator is lexically visible in whatever unit imported it.
    ///
    /// The file set is what makes operator scoping lexical rather than dynamic:
    /// a `sub infix:<+>` declared in the main script must not override
    /// arithmetic inside an imported module (Test.rakumod's own counter
    /// arithmetic is the motivating case), and must still apply inside a
    /// main-script block even when a module routine is what invokes that block.
    /// See `Interpreter::user_infix_override`.
    pub(crate) user_declared_infix_ops: HashMap<String, HashSet<Symbol>>,
    /// The compilation unit whose code is executing right now. Saved and
    /// restored around every compiled-routine call, and around every `EVAL`, so
    /// it names the unit the running code was COMPILED in rather than anything
    /// about the call stack. Read by `Interpreter::user_infix_override`.
    pub(crate) current_unit: Symbol,
    /// Monotonically increasing count of closures created by the
    /// block/lambda/anon-sub-literal exec ops (`MakeAnonSub`,
    /// `MakeAnonSubParams`, `MakeLambda`, `MakeBlockClosure` —
    /// `exec_make_anon_sub_op` and siblings). A routine body that declares an
    /// inner routine snapshots the routine registry and restores it on
    /// return so the lexical routine stops being callable by name — unless it
    /// escaped via the return value (`return_value_escapes_routine`). That
    /// check misses every *side-channel* escape: a closure literal created
    /// during the call and handed to `.tap`/stored in an attribute/pushed
    /// onto an array can reference the inner routine by name and outlive the
    /// call. Comparing this counter before/after the call is a runtime
    /// over-approximation that also skips the restore whenever *any* closure
    /// literal was created during the call — see
    /// `todo/tickets/lexical-sub-lost-after-routine-return.md`. This can
    /// leave an unrelated inner routine registered a little longer than
    /// strictly necessary, but never wrongly unregisters one that is still
    /// reachable, and a routine that declares no inner routines never pays
    /// for the snapshot at all (`declares_inner_routines` gates it).
    pub(crate) closures_created: u64,
    lib_paths: Vec<String>,
    /// Bundled-battery module search paths (`modules/<Dist>/lib` shipped
    /// alongside the binary). Searched *after* every `lib_paths` entry so the
    /// bundle is the lowest-priority source — an explicit `-I`/`MUTSULIB` path,
    /// a project-local module, or an `mzef`-installed (site-repo) version all
    /// shadow it. This is the batteries "floor + independent-update" mechanism
    /// (BATTERIES.md §3/§6). Resolved once at startup (exe-relative, or via
    /// `MUTSU_BUNDLE_DIR`).
    bundled_lib_paths: Vec<String>,
    /// Open IO handles (files/sockets/listeners) shared between the VM and the
    /// Interpreter behind transitional `Arc<RwLock>` scaffolding. Snapshot-cloned
    /// per thread (see [`io_handles`] module docs and `clone_for_thread`).
    io_handles: Arc<RwLock<io_handles::IoHandleTable>>,
    pub(crate) program_path: Option<String>,
    /// Name of the package currently in scope (e.g. `GLOBAL`, `Foo::Bar`),
    /// used to build fully-qualified names during function/method dispatch and
    /// declaration. Held behind transitional `Arc<RwLock>` scaffolding so the VM
    /// can read/write it through its own handle (mirroring `io_handles` /
    /// `registry`) rather than bouncing through `self.interpreter`. Snapshot-cloned
    /// per thread (see `clone_for_thread`). Accessed only via
    /// `current_package()` / `set_current_package()`, which read-clone / write the
    /// lock and never hold the guard across user-code re-entry.
    current_package: Arc<RwLock<String>>,
    /// Interned-symbol mirror of `current_package`, kept in lockstep by the two
    /// setters. Reading the `RwLock<String>` clones a `String` (one malloc), which
    /// is far too expensive for per-call use; the name-keyed call caches need the
    /// package identity on every hit to stay package-scoped, so they read this
    /// relaxed atomic instead.
    current_package_sym: Arc<AtomicU32>,
    routine_stack: Vec<RoutineFrame>,
    callframe_stack: Vec<CallFrameEntry>,
    method_class_stack: Vec<String>,
    /// The class whose instance is currently being constructed, set only while
    /// evaluating typed-attribute default type objects so a suppressed nested
    /// class name resolves within its owning class (see `resolve_suppressed_type`).
    constructing_class: Option<String>,
    /// The registry storage key (the fully-qualified/mangled name actually
    /// used as the registry key, NOT the source-level bare name) of the
    /// class most recently registered by `exec_register_class_op`. Set right
    /// before that function returns `Ok`, and consumed immediately by the
    /// very next opcode when it is `PushLastRegisteredClass` — the compiler
    /// only ever emits that opcode directly after `RegisterClass` for a
    /// NAMED `class` declaration used in expression position (`(class A
    /// { ... })`), so nothing else can run between the write and the read.
    /// Exists so that expression evaluates to the type object the
    /// declaration just created, rather than a bareword lookup of `A` that
    /// can resolve to an unrelated, same-named class from a different scope
    /// (e.g. one declared inside `EVAL`'d code running in a different
    /// package than the caller). See
    /// `news/2026-08/class-decl-expr-is-not-a-name-lookup.md`.
    pub(crate) last_registered_class_key: Option<String>,
    /// The qualified registry key most recently installed by
    /// `exec_register_role_op`. Consumed immediately by
    /// `PushLastRegisteredRole` for a named role declaration expression.
    pub(crate) last_registered_role_key: Option<String>,
    /// Attribute writes observed through an instance's shared cell while its
    /// BUILD phase runs, one frame per instance under construction (BUILD may
    /// itself construct objects, so the frames nest). A frame is keyed by the
    /// cell's address; `write_attr_cell_by_key` records into the matching frame.
    /// Raku applies a `has $.x = <default>` initializer *after* BUILD and only
    /// for attributes BUILD did not set, so this is what "BUILD set it" means
    /// (an explicit `$!x = Any` counts, exactly like rakudo's null check).
    /// Interior mutability: the write path takes `&self`.
    pub(crate) build_attr_writes: std::cell::RefCell<Vec<BuildWriteFrame>>,
    /// The class whose body is currently being registered, set only while
    /// executing `BEGIN`/`EVAL` code inside a class body (see
    /// `register_class_decl`). Lets a `has`-attribute declaration that reaches
    /// the VM at runtime (`class Foo { BEGIN EVAL q[has $.x] }`) attach the
    /// attribute to the class still under construction rather than throwing
    /// `X::Attribute::NoPackage`.
    pub(crate) defining_class: Option<String>,
    pending_call_arg_sources: Option<Vec<Option<String>>>,
    /// Every positional argument of the value-call currently being dispatched
    /// (`$b(7)` / `&b(7)` — `OpCode::CallOnValue`/`CallOnCodeVar`'s `bare_args`)
    /// is a syntactically container-less expression, so a bare block's implicit
    /// `$_` aliases a value with no container and raku refuses `$_ = ...`
    /// inside it. Read (and cleared) by `call_compiled_closure_with_topic`
    /// BEFORE it pushes its call frame; `push_call_frame` clears it too, so the
    /// flag can never leak past one call boundary into an unrelated block.
    pub(crate) pending_call_topic_bare: bool,
    /// Set while `require` resolves a module: a missing `Test::`-namespace
    /// module must surface as a catchable X::CompUnit::UnsatisfiedDependency
    /// instead of the silent no-op `use Test::Util` relies on.
    pub(crate) require_propagates_missing_module: bool,
    /// Companion to `pending_call_arg_sources` (§1.4/§1.5): the compiler-baked
    /// `arg-source name -> caller local slot` for the current call, decoded from the
    /// `Pair(name, Int(slot))` arg-source entries. Set alongside the names by
    /// `decode_arg_sources`, taken with them by `bind_function_args_values`.
    pub(crate) pending_call_arg_source_slots: std::collections::HashMap<String, u32>,
    /// `rw-arg writeback source name -> caller local slot`, captured at arg-binding
    /// time (clobber-safe: before the callee body runs) from
    /// `pending_call_arg_source_slots`. The rw writeback drain
    /// (`apply_pending_rw_writeback`) prefers this slot over the by-name `position`
    /// resolution, so the write lands on the LIVE (inner shadow) caller slot.
    pub(crate) pending_rw_writeback_slots: std::collections::HashMap<String, u32>,
    test_pending_callsite_line: Option<i64>,
    /// Current source line of the executing statement (`$?LINE` for internal
    /// consumers: backtraces, warn/die locations, callframe records). Lives as
    /// a plain field — NOT an env entry — so refreshing it is a scalar store
    /// instead of an env insert (which forked the CoW overlay map on every
    /// statement and kept callee overlays non-empty, defeating the empty-tier
    /// reuse in `Env::scoped_child`). The value is derived from the executing
    /// chunk's static ip -> line table (`CompiledCode::op_lines`, refreshed by
    /// `sync_source_line`), so it costs no instruction of its own. Call paths
    /// that push a `CallFrameEntry` restore it on pop from the entry's `line`;
    /// the frame-less VM fast paths save/restore it manually.
    pub(crate) cur_source_line: i64,
    /// Source location at which this worker interpreter was spawned. A worker
    /// starts with an empty routine stack, so an anonymous callback can have no
    /// rendered frame of its own; this origin supplies its enclosing location
    /// without inventing a mainline frame for every worker backtrace.
    pub(crate) thread_spawn_origin: Option<(Symbol, u32)>,
    /// Recycled `locals` backing vectors. The frame-less VM fast paths take the
    /// caller's `locals` aside and need a fresh Vec per call; popping one here
    /// instead of allocating removes a malloc/free pair per call (recursion
    /// otherwise allocates one per frame down the whole chain). Entries are
    /// cleared before being returned to the pool; bounded by `LOCALS_POOL_MAX`.
    pub(crate) locals_pool: Vec<Vec<Value>>,
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
    /// Pod declarators keyed by the concrete WHEREFORE object's stable id.
    /// DOC INIT uses AST-built declarants before runtime registration, so a
    /// name key would collide for multis and same-named parameters.
    why_object_cache: HashMap<u64, Value>,
    type_metadata: HashMap<String, HashMap<String, Value>>,
    /// `Box<Cell<bool>>`-backed (not a plain `bool`, and not a bare `Cell`):
    /// read/written through the `when_matched()`/`set_when_matched()`
    /// accessors below AND directly by `vm_call_state_guard::WhenMatchedGuard`,
    /// whose `Drop` impl restores it via a raw pointer into this separate heap
    /// allocation -- immune to Stacked Borrows retags of `Interpreter`'s own
    /// memory from `&mut self` calls made after the guard was constructed
    /// (see that module's doc comment for why a bare `Cell` field is not
    /// enough).
    pub(crate) when_matched: Box<Cell<bool>>,
    gather_items: Vec<Vec<Value>>,
    gather_take_limits: Vec<Option<usize>>,
    block_scope_depth: usize,
    /// Declaration registry (enums/subsets/... — migrated group-by-group, PLAN.md ②),
    /// shared with the VM behind `Arc<RwLock>`. See [`Registry`] and `src/runtime/registry.rs`.
    /// Lock discipline: never hold a guard across user-code re-entry (deadlock).
    ///
    /// The inner `Arc<Registry>` makes a per-thread spawn an O(1) share instead
    /// of a deep clone of ~40 maps: `clone_for_thread` clones the `Arc`, and the
    /// first *write* on either side after the share pays the one deep clone via
    /// `Arc::make_mut` (see `RegistryWriteGuard::deref_mut`). Each thread still
    /// gets its own outer `Arc<RwLock<...>>`, so declarations never leak between
    /// threads — only the initial snapshot is lazily shared.
    registry: Arc<RwLock<Arc<Registry>>>,
    /// Monotonic counter bumped on every `registry_mut()` acquisition, i.e. every
    /// time the declaration registry may have been mutated. Several resolution
    /// caches consult it to detect "did anything write the registry since I last
    /// checked". `AtomicU64` (not `Cell`) so `Interpreter` stays `Send`/`Sync` —
    /// `registry_mut()` takes `&self`.
    registry_write_gen: std::sync::atomic::AtomicU64,
    /// Active `{*}` proto dispatch frames: (proto_name, args, method_ctx).
    /// `method_ctx` is `Some` when the active proto is a `proto method` body, so
    /// `{*}` redispatches to a multi *method* candidate on the invocant rather
    /// than a proto sub candidate.
    proto_dispatch_stack: Vec<(String, Vec<Value>, Option<ProtoMethodCtx>)>,
    pending_dispatch_error: Option<RuntimeError>,
    /// One-shot suppression of the user `postcircumfix:<[ ]>`/`<{ }>`
    /// multi-candidate probe in `exec_index_op_with_positional`. Set only
    /// while the *core* subscript routine (`builtin_postcircumfix_subscript`,
    /// what `&postcircumfix:<[ ]>` resolves to) drives that op: real Raku's
    /// CORE candidate performs native indexing and never re-enters the
    /// user's override, so a delegating candidate (`old-same SELF, $index`,
    /// the `Array::Rounded` idiom) must not recurse into itself. Consumed by
    /// the probe with `mem::take`, so it only ever masks the one immediately
    /// following dispatch, never a nested subscript evaluated underneath it.
    pub(crate) skip_postcircumfix_overload: bool,
    /// Distribution selectors (`:ver`/`:auth`/`:api`) of the `use` currently
    /// being resolved, split off the module name by `use_module_with_tags` and
    /// consulted by `resolve_module_path` to pick among installed dists that
    /// provide the same short name. Saved/restored around each load so a
    /// transitive `use` resolves with its own (usually absent) selectors.
    pending_dist_selectors: Vec<(String, String)>,
    /// Arguments passed to the `use` currently being loaded (`use Foo "a", "b"`
    /// / `use Foo <a b c>`), evaluated by the caller and pushed for the
    /// `UseModule` op. Consumed once by `load_module`, which snapshots them into
    /// a local before running the module body (so a transitive `use` inside the
    /// body cannot see them) and hands them to the module's `sub EXPORT`.
    pub(crate) pending_use_export_args: Option<Vec<Value>>,
    /// An `&EXPORT` sub a module *imported* from another module's EXPORT map
    /// (the Slangify pattern: `sub EXPORT($grammar, ...) { ...; Map.new:
    /// '&EXPORT' => &inner-EXPORT }`), keyed by the name of the module that was
    /// loading when the import happened. `apply_module_export` consumes the
    /// entry: the imported sub becomes that module's own EXPORT for *its*
    /// importers, called with their `use` arguments.
    pending_inner_export_subs: HashMap<String, Value>,
    /// Each loaded module's EXPORT (own `sub EXPORT` or a Slangify-style
    /// imported one), remembered so a re-`use` of the already-loaded module
    /// can run it again with the new import's arguments.
    module_export_defs: HashMap<String, crate::runtime::runtime_module_export_sub::ModuleExportDef>,
    /// Grammar-rule names recorded by `$*LANG.define_slang` during a slang
    /// activation run (ADR-0026). Only ever populated in the dedicated
    /// activation sub-interpreter; read once by its thread runner.
    pub(crate) defined_slang_rules: Vec<String>,
    /// Registered END phasers, in registration order (they run in reverse).
    end_phasers: Vec<EndPhaser>,
    /// Monotonic tie-breaker for [`EndPhaser::order`], so phasers within one
    /// [`end_order`] class keep the order they were registered in.
    end_phaser_seq: u64,
    /// One entry per module body currently executing, holding the [`end_order`]
    /// class the END phasers it registers belong to. Empty while the main
    /// compunit runs. See `load_module` for why a `use` reached from an `EVAL`
    /// is not `end_order::MODULE`.
    module_load_order: Vec<u64>,
    /// Tracks END phaser site_ids to ensure each is registered only once.
    end_phaser_sites: HashSet<u64>,
    chroot_root: Option<PathBuf>,
    loaded_modules: HashSet<String>,
    /// Package-qualified routine keys a module load introduced (`M::helper`,
    /// `M::EXPORT::ALL::foo`) — never the bare `GLOBAL::` import aliases, which
    /// stay lexical to the importing scope.
    ///
    /// `loaded_modules` is never rolled back, so these must not be either: a
    /// scope that restores the routine registry wholesale (a bare block, an
    /// `EVAL`) would otherwise leave the module marked as loaded while its own
    /// routines are gone, and a re-`use` — being a no-op — could not bring them
    /// back. See `reinstate_module_functions`.
    module_registered_functions: HashSet<Symbol>,
    /// The package-qualified globals (`Base::flag`, `$NativeLibs::config`) each
    /// loaded module declared with `our`, keyed by module name.
    ///
    /// The routine-registry counterpart above cannot cover these: `our`
    /// variables live in `env`, and every scope that restores `env` wholesale —
    /// a sub call, a block, an `EVAL` — drops the ones a module load nested
    /// inside it created. `loaded_modules` is never rolled back, so a later
    /// `use` of that module is a no-op and could not bring them back. The
    /// already-loaded path of `use_module_with_tags_inner` reinstates whatever
    /// is missing from here instead.
    module_package_globals: HashMap<String, Vec<(Symbol, Value)>>,
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
    /// Sigilless parameter names (`\attr`, `my \x`) of the routine whose body is
    /// about to be compiled by the interpret path (`compile_block_value_opts`).
    /// The multi/user-sub fallback runs a body via a *fresh* `Compiler`, which
    /// otherwise would not know these bare names are lexical variables — so a
    /// nested closure would compile them as barewords and lose the capture
    /// (e.g. Attribute::Predicate's `is predicate` builds `method {
    /// attr.get_value(self) }`). Seeded right before the eval and consumed by the
    /// fresh compiler's `enclosing_sigilless`. Empty except across that call.
    pending_eval_sigilless: Vec<String>,
    /// Placeholder params (e.g. `^p`) the current interpret-path sub call has
    /// bound in env, seeded into `compile_block_value_opts`'s fresh compiler
    /// so its stray-placeholder checks know they are attached.
    pending_eval_placeholder_params: Vec<String>,
    /// ADR-0059 Slice 2: the interpret-path sub call about to recompile a
    /// `SubData` body through `eval_block_value_cached` is an `is rw`/`is raw`
    /// routine, so the fresh compiler must compile the body's bare tail as the
    /// container it denotes (`Compiler::rw_tail`). Consumed (taken) by
    /// `eval_block_value_inner` at entry, so it never leaks into a block
    /// compiled from *inside* that body.
    pending_eval_rw_tail: bool,
    /// ADR-0037 §2.3: how `EVAL ..., context => $ctx` should classify the
    /// snippet's `return`, computed once by `builtin_eval` from `$ctx`'s
    /// stamped routine identity (`Interpreter::eval_context_routine`) and
    /// consumed by `compile_block_value_opts`/`carrier_compile_ctx_key` while
    /// compiling the EVAL unit's mainline. `None` means either no `context`
    /// argument was passed at all, or it carried no routine identity — both
    /// leave the ambient `enclosing_routine_exists()`-driven classification
    /// unchanged. Saved/restored around the `EVAL` call (mirrors
    /// `pending_eval_sigilless`), since a nested EVAL sets and restores its
    /// own.
    pending_eval_context_routine: Option<EvalContextRoutineState>,
    /// Set right before the interpret path evaluates the body of a `supply { … }`
    /// block, and consumed by the very next `eval_block_value_inner` so the
    /// freshly compiled chunk carries `CompiledCode::is_supply_block_body`.
    ///
    /// The compiler already marks the supply lambda's own `CompiledCode`, but
    /// `call_sub_value` does not run that chunk — it re-compiles `data.body` from
    /// the AST, and that copy would otherwise lose the mark. Consumed (taken) on
    /// entry, so a nested block compiled from inside the body does not inherit it.
    pending_supply_block_body: bool,
    /// The emitter parameter name that goes with `pending_supply_block_body`, so
    /// the re-compiled chunk also carries `CompiledCode::supply_emitter_sym`.
    pending_supply_emitter_sym: Option<Symbol>,
    /// The compiler-vouched never-written captures of the supply lambda whose
    /// body `eval_block_value` is about to re-compile. Only the *original*
    /// compile saw the creating frame, so the carrier chunk cannot derive
    /// `authoritative_free_vars` itself — it has no enclosing frame to vouch.
    /// Travels with `pending_supply_block_body`.
    pending_supply_authoritative_free_vars: Vec<Symbol>,
    /// The `authoritative_captures` of the closure whose body `eval_block_value`
    /// is about to re-compile, so the carrier chunk can hand them to any
    /// `whenever` it registers (`CompiledCode::inherited_owned_lexicals`).
    ///
    /// This is what lets a `whenever` nested inside another `whenever`'s body
    /// keep the outer callback's owned lexicals — above all the supply block's
    /// shared-per-parse-site emitter name. Travels the same way as
    /// `pending_supply_authoritative_free_vars`, but is NOT restricted to supply
    /// bodies: a `whenever` callback has no `CompiledCode` of its own at all.
    pending_whenever_inherited_owned: Vec<Symbol>,
    /// Names the block `eval_block_value_inner` most recently FINISHED running
    /// declared with its own `my` (excluding those it also uses as free
    /// variables). Written just before that function returns, so after a call
    /// the value belongs to the outermost block that just completed — nested
    /// blocks run and publish their own set first, and are overwritten.
    ///
    /// `call_sub_value`'s exit merge reads it to keep such names out of the
    /// caller: a body compiled on the fly from AST (a `whenever` body, a
    /// `supply` block body) has no `CompiledCode` on its `SubData`, so the
    /// compile-time `my_declared_sym` is otherwise unreachable from there.
    last_block_my_declared: Vec<Symbol>,
    /// Append-only log of the free variables that carrier bodies run with
    /// `record_free_var_writes` (an `EVAL`'d compilation unit, a `where` clause)
    /// WROTE. `parse_and_eval_with_operators` reads back the slice its own snippet
    /// appended: those names are assignments to *outer* lexicals, so they must
    /// survive the "drop the EVAL's own `my` lexicals" cleanup even though the
    /// caller's env had no entry for them before (a caller's `my $a;` with no
    /// initializer materializes no env key, so `EVAL '$a = 32'` looks exactly like
    /// a snippet-local declaration to a key-set diff). Names the snippet really
    /// DECLARED are locals of its code, never free variables, so they never land
    /// here.
    pub(crate) recorded_free_var_writes: Vec<String>,
    /// The subset of [`Self::pending_caller_var_writeback`] that came from a write
    /// whose TARGET NAME was resolved at RUN TIME — `$::($n) = v`, `::('$x') = v`,
    /// an assignment inside an `EVAL`'d snippet. Only these names are carried
    /// across a frame boundary by `propagate_pending_caller_writes`.
    ///
    /// Kept separate on purpose: the main list is fed by many long-standing
    /// mechanisms (an `is rw` writeback whose slot is not in this frame, a Proxy
    /// STORE, a `$CALLER::x` write, the shared-var lane), and replaying *those*
    /// into every intervening caller env is far too blunt — it broke a
    /// `given $in { when IO::Handle {...} }` dispatch in the bundled Text::CSV by
    /// carrying an unrelated frame's `in` upward. A runtime-name write is exactly
    /// the case the compile-time filters cannot see, so it is the only one that
    /// needs the extra hop.
    ///
    /// Entries are dropped by `apply_pending_caller_var_writeback` at the same
    /// moment the main list drops them: when a frame that actually owns the slot
    /// has absorbed the value.
    pub(crate) pending_runtime_name_writes: Vec<String>,
    /// PredictiveIterator backing a `Seq.new(iterator)`, keyed by the Seq's
    /// Arc pointer (`seq_id`). Kept off the scoped `env` so the association
    /// survives sub/block returns between Seq creation and `.tail`/`.Numeric`
    /// (an env-keyed side table was lost on scope exit).
    /// TODO: entries are never reclaimed; acceptable as predictive Seqs are rare.
    predictive_seq_iters: HashMap<usize, Value>,
    protect_block_cache: ProtectBlockCache,
    /// Lock ids this caller chain has entered through
    /// `Lock::Async.protect-or-queue-on-recursion` (see
    /// `runtime::lock_async_recursion`). A spawned thread starts with an empty
    /// stack, which is precisely the "the lock is held by something outside the
    /// caller chain" case that method distinguishes.
    lock_async_recursion: Vec<u64>,
    /// Blocks queued by a *recursive* `protect-or-queue-on-recursion` call,
    /// drained by the outermost such frame once it has released the lock.
    /// Held here (rather than in a thread-local) so the queued `Value`s are
    /// enumerated by `visit_roots` while they wait.
    lock_async_deferred: Vec<(u64, Value, crate::value::SharedPromise)>,
    /// See `CarrierCompileCache`: reuses `eval_block_value_inner`'s carrier
    /// compile across repeated calls to the same `SubData` id instead of
    /// recompiling its AST every time. Opt-in per call site via
    /// `eval_block_value_cached`/`eval_test_block_value`'s `cache_id`
    /// parameter — starts empty per thread (pure recomputable optimization).
    carrier_compile_cache: CarrierCompileCache,
    /// Parsed `s///` / `S///` replacement plans, keyed by the replacement's
    /// source text (see `vm::vm_subst_repl`). The replacement is a `qq` quote,
    /// so it is parsed with the real interpolation grammar; caching keeps a
    /// `:g` substitution from re-parsing it per match and gives the dynamic
    /// plan a stable carrier-compile-cache id.
    pub(crate) subst_repl_plans: HashMap<String, crate::vm::vm_subst_repl::SubstReplPlan>,
    /// The map/grep/`.first` inline-loop fast paths (`resolution_map_grep.rs`)
    /// compile the callback block once per `.map()`/`.grep()`/`.first()` CALL
    /// and then run every item through the same compiled bytecode via
    /// `run_reuse` -- cheap when one call processes many items, but a block
    /// literal declared *inside* a loop (`for @blocks { @xs.map({ ... }) }`,
    /// the shape `Digest::RIPEMD` hits once per compression round) is a fresh
    /// `SubData` on every outer iteration, so the naive path recompiles its
    /// AST from scratch every time even though the block's own source never
    /// changes. `data.compiled_code` is already an `Arc<CompiledCode>` shared
    /// across every instantiation of the same source closure literal (see
    /// `vm_register_ops::resolve_closure_code` -- it is pulled from the
    /// enclosing scope's `closure_compiled_codes`, baked once at that
    /// enclosing scope's own compile time), so its pointer identity is a
    /// free cache key for this fast path's own (differently-shaped,
    /// tail-normalized) compile. `MapGrepCacheKey` HOLDS a clone of that
    /// `Arc` (not just its address) so the key stays alive for as long as the
    /// cache entry does — a bare `usize` pointer would go unsound the moment
    /// the *original* Arc (e.g. one built fresh per call by a dynamic
    /// `EVAL`/RakuAST closure, never otherwise retained) is dropped and its
    /// address reused by an unrelated later `CompiledCode` allocation, which
    /// would then collide with a stale cache entry
    /// (`t/rakuast-eval-block-arg.t`'s chained `.map().grep()` on one line
    /// caught this during development). Keyed additionally on
    /// `lexically_in_routine` since that is the only other compiler input
    /// drawn from ambient state here. Starts empty per thread (pure
    /// recomputable optimization).
    map_grep_compile_cache:
        HashMap<MapGrepCacheKey, (std::sync::Arc<CompiledCode>, std::sync::Arc<CompiledFns>)>,
    /// Compiled bytecode for `gather` block bodies, keyed the same way (pointer
    /// identity of the body's analysis `CompiledCode`). `exec_make_gather_op`
    /// used to run the whole compiler on the body every time the `gather`
    /// EXPRESSION was evaluated, so a `gather` inside a loop re-compiled per
    /// iteration: 3 constant-pool additions per creation, and ~1.75us per body
    /// statement per creation. Kept separate from `map_grep_compile_cache`
    /// because the compile target differs (a body that declares routines is
    /// wrapped in a `Stmt::Block` first), so the two must never share a slot for
    /// the same origin chunk. Starts empty per thread (a pure recomputable
    /// optimization).
    gather_compile_cache:
        HashMap<MapGrepCacheKey, (std::sync::Arc<CompiledCode>, std::sync::Arc<CompiledFns>)>,
    /// Compiled bytecode for subset `where` predicates, keyed by subset name.
    /// A subset's predicate is a fixed `Expr`, so it is compiled once and reused
    /// across all type checks instead of recompiling + cloning the entire
    /// function/proto registry on every check (the old `eval_block_value` path).
    /// Cleared per-name on subset redeclaration; starts empty per thread (the
    /// cache is a pure recomputable optimization). See `type_matches_value`.
    subset_predicate_cache: HashMap<String, SubsetPredicateCompiled>,
    /// Side-channel: the exception raised by the most recent subset `where`
    /// predicate that failed by *throwing* (a `fail "msg"` inside the `where`,
    /// e.g. `subset Even of Int where { $_ %% 2 or fail "..." }`). `type_matches_value`
    /// records it here (returning `false` as usual), so the ASSIGNMENT/binding
    /// type-check can surface the custom message instead of the generic
    /// "expected X, got Y". Smartmatch / dispatch callers ignore it (a `where`
    /// that fails is just "no match" there). Set to `None` before each subset
    /// predicate runs; consumed (and cleared) by the type-check op.
    pub(crate) subset_where_fail: Option<Box<RuntimeError>>,
    private_zeroarg_method_cache: HashMap<(String, String), Option<(String, MethodDef)>>,
    module_load_stack: Vec<String>,
    /// The current distribution context ($?DISTRIBUTION).
    pub(crate) current_distribution: Option<Value>,
    /// `routine_stack` height when the in-progress module load established
    /// `current_distribution`. Frames at or above it were pushed by code the
    /// loading module called, so they are the ones whose own distribution owns a
    /// `%?RESOURCES` they read; frames below belong to whoever triggered the
    /// load and must not shadow the module being loaded. See
    /// `build_resources_for_package`.
    pub(crate) current_distribution_frame_floor: usize,
    /// Maps package names to their distribution context.
    /// Populated during module loading so OTF compilation can resolve $?DISTRIBUTION.
    pub(crate) package_distributions: HashMap<String, Value>,
    /// Short type names a module imported for its OWN lexical scope, keyed by the
    /// module name and by every class/role that module declares:
    /// `{"Drv2" | "Drv2::Native" => {"THING2" => "Drv2::Native::THING2"}}`.
    ///
    /// A module body runs in the *caller's* env (`load_module` → `run_block`), so
    /// the `Package` aliases its own `use` statements install land in whatever
    /// frame triggered the load and die with it. That is invisible for a
    /// compile-time `use` at file scope (the alias outlives every later call),
    /// but a `require` executed inside a method frame loses them the moment the
    /// method returns — and the module's own methods then cannot resolve their
    /// own imported type names. Recording the aliases against the module makes
    /// the resolution lexical to the module instead of dynamic to the frame.
    /// Consulted by `package_type_alias` from `has_type` / `GetBareWord`.
    pub(crate) package_type_aliases: HashMap<String, HashMap<String, String>>,
    /// The module's other file-scope bare names — `constant`s and sigilless
    /// declarations its own routines close over — keyed the same way as
    /// `package_type_aliases`, and lost for the same reason. Consulted by
    /// `module_scope_lexical` as the LAST resort in bareword resolution, just
    /// before the undeclared-bareword-as-`Str` fallback, so a live `env` binding
    /// always wins. Distinct from `package_lexicals`, which is the *mutable*
    /// package-block `my` store with its own writeback path; these are a module's
    /// immutable file-scope terms. `NativeHelpers::Blob`'s `MoarVM::Guts::REPRs`
    /// is the motivating case: `constant Offset` is read by the exported
    /// `OBJECT_BODY` sub of the same module, and resolved to the string
    /// `"Offset"` once the frame that loaded the module was gone.
    pub(crate) module_scope_lexicals: HashMap<String, HashMap<String, Value>>,
    /// Names the module currently being loaded imported from another module,
    /// accumulated by `import_module` and folded into `module_scope_lexicals`
    /// when the load finishes. The env diff `load_module` takes cannot see these:
    /// re-importing a name a *previously* loaded module already installed adds
    /// nothing to `env`, so `DBDish::mysql::StatementHandle`'s `use
    /// DBDish::mysql::Native` looked like a no-op even though `intptr` is part of
    /// its lexical scope. Saved/restored around each nested load.
    pub(crate) module_imported_names: Vec<(String, Value)>,
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
    /// Exports each module registered while it was the module currently being
    /// loaded (attributed via `module_load_stack`), mapping module -> name ->
    /// tags. Unlike `exported_subs["GLOBAL"]`, which pools every unit-module
    /// export ever hoisted, this correctly attributes an export to the module
    /// that declared it. The `use MOD` tag-filter consults this so it only
    /// hides MOD's *own* exports and never a symbol MOD imported from a
    /// transitively-`use`d module (which MOD's methods must still resolve).
    module_owned_exports: HashMap<String, HashMap<String, HashSet<String>>>,
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
    /// True only on the throwaway nested `Interpreter` `eval-lives-ok`/
    /// `eval-dies-ok` construct to run their code string (`test_fn_eval_lives_ok`/
    /// `test_fn_eval_dies_ok` in `runtime/test_functions/eval_exception.rs`).
    /// Real raku's own `Test.rakumod` implements both via a helper (`sub
    /// eval_exception($code) { try { EVAL($code) }; $! }`) that calls `EVAL`
    /// with NO explicit `context =>` argument -- unlike `throws-like`, which
    /// explicitly passes `context => $caller-context` (the actual calling
    /// program's lexical scope). An `EVAL` with no context defaults to the
    /// lexical scope where the `EVAL` keyword is textually written, which for
    /// `eval_exception` is Test.rakumod's own module scope, not the calling
    /// program's -- so a `class Foo {}` inside `eval-lives-ok`'s string
    /// installs under a package distinct from the caller's, and does NOT
    /// conflict with a same-named class the calling program already declared
    /// (verified against real raku: `class A {}; eval-lives-ok 'class A {}'`
    /// lives). `throws-like`'s explicit caller context is why its EVAL'd
    /// string DOES conflict with an outer same-named class (also verified).
    /// mutsu provides `Test` natively (no real Test.rakumod compunit to
    /// inherit a distinct package from), so this flag stands in for that:
    /// it gates OFF `check_eval_class_redeclarations`'s cross-boundary
    /// `has_class` check (the same-EVAL-string duplicate-declaration check
    /// is unaffected) for the nested interpreter these two functions create.
    pub(crate) suppress_cross_eval_class_redeclaration_check: bool,
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
    /// Names in `package_lexicals` that are class-body `my` statics
    /// (`class C { my $x = ...; method m { $x } }`), keyed by class. These are
    /// stored in `package_lexicals` so a method's BARE `$x` read/write and the
    /// `writeback_package_scope_var` mutation path reuse the existing machinery,
    /// but — unlike a `package Foo { my $x }` block lexical — they must NOT be
    /// reachable through a QUALIFIED `$C::x`, which is a distinct package variable
    /// (see t/package-lookup.t). `package_scope_lexical`'s qualified branch skips
    /// any (class, name) recorded here.
    pub(crate) class_body_static_names: HashMap<String, std::collections::HashSet<String>>,
    /// File-scope `my` lexicals of a loaded `unit` compunit, keyed by the unit
    /// package name then env var name, each holding a shared `ContainerRef` cell.
    ///
    /// A module body runs in the env of whatever frame loaded it, so a file-scope
    /// `my $output` lands in that flat env under the plain key `output` — the SAME
    /// storage a script's own `my $output` uses. The two then alias one another and
    /// writes go both ways (`todo/deep/module-file-scope-my-shares-the-callers-env.md`).
    /// After the module body has run, `load_module` moves those names out of `env`
    /// into this store and restores whatever the loading scope had under them; the
    /// module's own routines — which run with `current_package` set to the unit
    /// package — resolve them here instead, read through `unit_scope_lexical` and
    /// written through `unit_scope_lexical_write`. Cells, not snapshots, so a write
    /// from one routine is seen by every other (`_init_io` sets `$output`, `proclaim`
    /// reads it).
    ///
    /// Distinct from `module_scope_lexicals`, which is a *last-resort* read-only
    /// snapshot keeping a module's bare names reachable once the loading frame is
    /// gone; this store is authoritative and consulted BEFORE `env`.
    pub(crate) unit_lexicals: HashMap<String, HashMap<String, Value>>,
    /// Names of mainline-declared named subs that captured at least one
    /// mainline `my` scalar free variable into
    /// `unit_lexicals[MAINLINE_UNIT_KEY]` at registration time (ADR-0024).
    ///
    /// A free-variable read/write resolves through the mainline unit-lexical
    /// cells ONLY while the last (non-block) routine frame's name is in this
    /// set AND its package is `GLOBAL` — see
    /// `Interpreter::mainline_lexical_frame_active`. Empty for a program with
    /// no such capture: zero cost beyond the map-presence check already paid
    /// by `unit_lexical_slot`.
    pub(crate) mainline_lexical_subs: std::collections::HashSet<String>,
    /// Shared cells for block lexicals captured by an `our`-scoped named sub
    /// declared inside a *bare* block (not a package block). Unlike a `my sub`, an
    /// `our sub` is installed into the package registry and stays callable after
    /// the block exits, but a registry routine carries no per-sub closure env. When
    /// the captured local (`my $a`) is declared, the VM boxes it into a shared
    /// `ContainerRef` cell and records it here keyed by the variable name; a
    /// free-var read inside the escaped sub resolves through this cell
    /// (`escaping_our_read`), so `our sub f { $a }` called after the block sees the
    /// live value (Raku semantics). Populated by the sub's source-order
    /// `RegisterSub` (`exec_register_sub_op`) once the captured local has been boxed
    /// — keyed to the sub's declaration, not the box site, so a same-named sibling-
    /// block `my` cannot pollute it. A read BEFORE the block runs misses (the cell is
    /// not yet recorded), correctly yielding the undefined value.
    pub(crate) escaped_our_lexical_cells: HashMap<String, Value>,
    /// Names of block lexicals that are captured by an `our`-scoped named sub
    /// (the union of every code's `needs_cell_escaping_our_sub`). Seeded once at the
    /// start of `run()` from the top-level code, so it is known BEFORE the declaring
    /// block executes. A free-variable read of such a name from inside a routine
    /// resolves through `escaped_our_lexical_cells` ONLY — never the shared env —
    /// so an unrelated leaked `env` value from a sibling block cannot shadow it, and
    /// a read before the block correctly yields the undefined value (the cell is not
    /// recorded yet). Empty for ordinary programs: zero cost.
    pub(crate) escaping_our_lexical_names: std::collections::HashSet<String>,
    /// Names of the `our`-scoped subs declared in bare blocks (the subs whose
    /// free-variable reads/writes may resolve through `escaped_our_lexical_cells`).
    /// The cell resolution fires ONLY while the innermost named routine frame is
    /// one of these subs — a plain `my sub` that merely shares a captured
    /// variable's name must keep resolving through its own live env capture.
    pub(crate) escaped_our_sub_names: std::collections::HashSet<String>,
    /// Bare (sigil-less) names of the plain `our` SCALARS whose canonical home
    /// is a shared `ContainerRef` cell published under a package-qualified key
    /// (`OpCode::DeclareOurScalar` — see `vm_our_package_vars`). Recorded only
    /// for a declaration inside a real package (a file-scope `our $x` collapses
    /// its qualified name to the bare name and is therefore never redirected).
    ///
    /// This is a cheap pre-gate, not the resolution itself: a bare-name read or
    /// write consults `our_package_scalar_*` only when the name is in this set,
    /// so the ordinary program — which never declares a package `our` scalar —
    /// pays a single empty-set check on the variable hot path.
    pub(crate) our_scalar_cell_names: std::collections::HashSet<String>,
    /// Keyed by `(base key symbol, closure scope id)` instead of a formatted
    /// `String` — see `scoped_state_key`/`state_key_display`. The `Option<u64>`
    /// distinguishes an un-scoped (named-sub/module-level) `state` var from one
    /// scoped to a specific closure clone.
    state_vars: HashMap<(Symbol, Option<u64>), Value>,
    /// Names re-declared (`my $x` / `if ... -> $x`) in THIS thread while the
    /// cross-thread shared store is active. A re-declaration is a fresh
    /// binding shadowing the captured outer lexical, so subsequent writes to
    /// the name must stay thread-local: `set_shared_var_sym` skips the shared
    /// write and `sync_shared_vars_to_env` skips the pull for these names.
    /// Reset to empty in `clone_for_thread` (a child thread captures the
    /// parent's *current* bindings). Only populated while
    /// `shared_vars_active`; empty (zero-cost) for single-threaded programs.
    /// Boxed and wrapped in `RefCell` (not a plain `HashSet<String>` field) so
    /// `ThreadParamMaskGuard` (`vm::vm_call_state_guard`) can hold a raw
    /// pointer into this field's OWN heap allocation -- disjoint from
    /// `Interpreter`'s own allocation -- and mutate it on `Drop` (including
    /// during a Rust panic unwind) without ever needing a reference to
    /// `Interpreter` itself. See that module's doc comment ("v3") for why a
    /// pointer taken directly into a field embedded in `Interpreter`'s own
    /// struct is unsound. `RefCell` (not `Cell`, unlike `state_scope_id`/
    /// `when_matched`) because `HashSet` isn't `Copy`, so `Cell`'s get/set API
    /// is awkward for it; `RefCell` gives the same disjoint-allocation
    /// property while keeping ordinary `insert`/`remove`/`contains` methods
    /// available through `borrow`/`borrow_mut`.
    pub(crate) thread_redeclared_vars: Box<std::cell::RefCell<std::collections::HashSet<String>>>,
    /// Subset of [`Self::thread_redeclared_vars`] whose declaration is still
    /// *in flight*: the `my` has run but its initializer has not stored a value
    /// yet, so neither the slot nor `env` holds the new binding — both still
    /// carry the shadowed OUTER value.
    ///
    /// `clone_for_thread` normally drops a re-declaration mask because it
    /// force-seeds the name's *current* value into the child lineage first. That
    /// premise fails for a name in this set: a spawn that happens **inside the
    /// initializer** (`my $tap = Supply.tap(...)`, whose `.tap` starts a worker)
    /// would seed the outer binding's value and then unmask the name, so the
    /// next `sync_shared_vars_to_env` pulls that stale value back over the
    /// binding the initializer is about to create. Keeping the mask for the
    /// in-flight window closes that hole; the store is republished normally once
    /// the initializer's value lands. Empty for single-threaded programs.
    pub(crate) thread_decl_in_flight: std::collections::HashSet<String>,
    /// Plain-lexical `@`/`%` names this frame's spawns put on the bare-name
    /// cross-thread lane **only because every spawn publishes every live
    /// container**, not because any spawned block actually names them
    /// (ADR-0039 §8.6).
    ///
    /// Such an entry is needed only for as long as a worker might reach the
    /// container *indirectly* — through a routine the block calls rather than
    /// names. Once the next cross-thread drain (`sync_shared_vars_to_env`) has
    /// merged whatever the workers did back into `env`, it has served its whole
    /// purpose, and keeping it is what let a callee's own `my @items` outlive
    /// its frame in a process-visible, bare-name-keyed store and hijack an
    /// unrelated caller's same-named binding. So the drain withdraws them.
    ///
    /// A name a later spawn's block DOES reference is removed from this set at
    /// that spawn: it is then a genuinely shared container and keeps the lane.
    /// Empty for single-threaded programs.
    pub(crate) transient_lane_containers: std::collections::HashSet<String>,
    /// Bare scalar names currently masked in [`Self::thread_redeclared_vars`]
    /// because of a **parameter binding** (`mask_thread_redeclared_params`),
    /// not a `my` declaration. `clone_for_thread_excluding` must treat the two
    /// differently: a `my` re-declaration's mask means "this spawn should see
    /// MY new value as authoritative for the rest of the block", so it force-
    /// `declare`s the value into the shared lineage. A parameter's shadow is
    /// scoped to exactly this call and must never overwrite an unrelated
    /// caller's live entry for the same bare name — it should always take the
    /// `seed_if_absent` (no-op-if-already-visible) branch instead, even for a
    /// nested spawn *inside this call's own body*.
    ///
    /// `thread_decl_in_flight` looked like the same "always seed_if_absent"
    /// signal, but it is unsuitable here: `exec_set_local_op` clears an entry
    /// from it as soon as ANY `SetLocal` targets a same-named slot — which the
    /// call body's own bytecode does routinely (e.g. a coercion or a
    /// re-assignment of the parameter), silently un-suppressing the force-
    /// `declare` behavior partway through the call before any nested spawn.
    /// A dedicated set, touched only by
    /// [`mask_thread_redeclared_params`](Self::mask_thread_redeclared_params) /
    /// [`unmask_thread_redeclared_params`](Self::unmask_thread_redeclared_params),
    /// has no such interference. Empty for single-threaded programs.
    /// Same `Box<RefCell<...>>` wrapping and same reason as
    /// [`Self::thread_redeclared_vars`] -- `ThreadParamMaskGuard` needs a
    /// stable, `Interpreter`-disjoint pointer into this field too.
    pub(crate) thread_param_shadow_vars: Box<std::cell::RefCell<std::collections::HashSet<String>>>,
    /// `@`/`%` names bound as **parameters through the env-level (runtime)
    /// binding path** — a destructuring sub-signature (`-> [$a, @K] { ... }`)
    /// or a runtime-invoked callback's plain parameter (`reduce -> $h, @words
    /// { ... }`) — with their sigils, each mapped to the container that binding
    /// stored in `env`.
    ///
    /// Such a name is a fresh per-invocation binding, never the one shared
    /// object the name-keyed `shared_vars` lane exists to represent. Left on that
    /// lane it is seeded once (`seed_if_absent`) and then frozen at the first
    /// spawn's value, so two `start` blocks created by two iterations of the same
    /// block both read the same `@K`/`@words`. `clone_for_thread_for_block`
    /// consults this map — intersected with the spawned block's free variables
    /// AND checked for container identity against the current env value, so an
    /// unrelated outer aggregate that merely shares a name (or a later `my`
    /// re-binding of it) is unaffected — to keep those names off the lane and
    /// mask them in the child instead.
    ///
    /// Populated unconditionally (not gated on `shared_vars_active`): the
    /// *first* spawn in a process consults it before any thread exists, and a
    /// gate would leave exactly that spawn's binding to be seeded — and frozen —
    /// on the lane.
    pub(crate) param_bound_aggregates: std::collections::HashMap<String, Value>,
    /// Set while an *incidental* locals -> env mirror is running: the I/O
    /// pre-sync (`sync_env_from_locals_declared`, run before Say/Put/Print/Note
    /// so a `$*OUT` override or a `.gist` sees fresh values) and the regex
    /// interpolation pre-sync. Both exist purely so a name-based reader in THIS
    /// interpreter can observe the frame's live slots through `env`.
    ///
    /// `set_env_with_main_alias` does double duty: it writes `env` AND publishes
    /// to the cross-thread shared store. Publishing from these two is wrong,
    /// because the store is keyed by BARE NAME while the mirror walks *whichever
    /// frame happens to be printing*: a callee's parameter `$url` overwrote the
    /// lane belonging to the caller's own `my $url`, and the caller's next
    /// `sync_shared_vars_to_env` pulled it back — `Cro::HTTP::Client.get("$url/")`
    /// grew a `/` on the caller's URL on every request, so the third server on a
    /// port answered 404.
    ///
    /// Frame *teardown* (`sync_env_from_locals`) is deliberately NOT suppressed;
    /// see the comment there.
    pub(crate) suppress_shared_publish: bool,
    /// Union of every executed `CompiledCode::type_body_written_lexicals`:
    /// lexicals written by a registered class/role method body. These keep the
    /// name-keyed `shared_vars` lane even when a spawned block also captures
    /// them — the capture analysis cannot see such a write (PLAN.md §6).
    /// Populated at `RegisterClass` / `RegisterRole`, which always run before
    /// the type can be instantiated.
    pub(crate) type_body_written_lexicals: std::collections::HashSet<String>,
    /// Per-closure-instance captured-variable state, keyed by
    /// (closure instance id, captured variable Symbol). This is the hot
    /// closure-call persistence store (loaded/saved on every closure call for
    /// its free variables); a typed key avoids the per-call
    /// `format!("__mutsu_closure_cap::{id}::{name}")` String allocation and the
    /// String hashing that dominated the closure dispatch profile.
    closure_captured_state: HashMap<(u64, Symbol), Value>,
    /// Fired `once { ... }` results, keyed by `(routine-clone-id, op-position)`.
    /// Shared by `Arc` handle into every spawned thread's clone so a `once` in a
    /// sub run from multiple `start` blocks fires exactly once across threads
    /// (see [`once_store::OnceStore`]).
    once_values: Arc<once_store::OnceStore>,
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
    /// Legacy name-keyed bridge for EVAL and specialized compatibility paths.
    /// Ordinary scalar enforcement lives on ContainerCell.
    var_type_constraints: HashMap<String, String>,
    /// Monotonic flag: set once any `atomicint` variable / atomic storage has been
    /// registered in this interpreter (or inherited from a parent thread). The
    /// per-`GetGlobal`/`GetLocal` atomic-variable check is expensive (a `format!`
    /// plus two `var_type_constraint` lookups, each itself a `format!`), yet
    /// atomics are exotic; when this flag is clear the entire check is skipped,
    /// which removes that cost from the hot variable-read path. Never cleared, so
    /// a program that stops using an atomic still resolves correctly. See also the
    /// process-global `atomic_var_seen_anywhere`, which the reset path needs
    /// because a worker thread's `cas` marks only the WORKER's copy of this field.
    /// pub(crate) so `vm_jit_layout` can `offset_of!` it: the Tier B inline
    /// GetLocal fast path reads this flag from native code.
    pub(crate) atomic_var_seen: bool,
    /// Monotonic flag: set once any *env-scoped* variable type constraint has been
    /// written (via `set_var_type_constraint`'s `env.insert` branch or
    /// `bind_param_type_constraint`). The hot `var_type_constraint` read does a
    /// `format!("__mutsu_type::{}")` + `env.get` on every variable write-back to
    /// support env-first, block-scoped constraints. When this flag is clear the
    /// name-keyed bridge is authoritative and the env lookup can be skipped.
    env_type_constraint_seen: bool,
    /// Monotonic flag: set once any sigilless-parameter alias
    /// (`__mutsu_sigilless_alias::name` env key, created when binding a `\target`
    /// raw/sigilless parameter or a `:=`-style alias) has been registered. The hot
    /// write-back path calls `propagate_sigilless_alias_chain` on every inc-dec /
    /// compound-assign, which builds `format!("__mutsu_sigilless_alias::{name}")`
    /// plus an env lookup to walk the alias chain. Sigilless aliases are rare; when
    /// this flag is clear no alias key exists, the chain is empty, and the whole
    /// walk (and its `format!`) is skipped. Set at every alias-insert site (see
    /// `sigilless_alias_key`). Never cleared, so removing an alias still resolves.
    sigilless_alias_seen: bool,
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
    /// live-shared: `clone_for_thread` shares the inner `Arc` (O(1); see the
    /// `registry` field's copy-on-write doc, docs/per-task-clone-slimming.md
    /// slice 4) into a fresh outer `Arc<RwLock<...>>`, so the lock never
    /// contends across threads and the first write on either side after a
    /// share pays the one deep clone via `Arc::make_mut`. Collapses to a plain
    /// VM field once the Interpreter execution path is removed (PLAN.md ④/⑤).
    instance_type_metadata: Arc<RwLock<Arc<HashMap<u64, ContainerTypeInfo>>>>,
    /// `let`/`temp` save stack: (name, saved value, is_temp, compiler-baked slot).
    /// The baked slot (§1.4/§1.5) lets the scope-exit restore write `locals[slot]`
    /// directly instead of resolving the name to the OUTER slot via
    /// `find_local_slot`. `None` for a non-local target (by-name fallback).
    let_saves: Vec<(String, Value, bool, Option<u32>)>,
    /// `rule name -> its own `:my $*/%*/@*x = …;` declarations`, for the grammar
    /// currently being parsed. `establish_grammar_dynamic_vars` also evaluates
    /// them once into `env` (a parse-wide slot, which is what a non-declaring
    /// rule's action reads); this map is what lets the reduce walk give each
    /// *match* of a declaring rule its own binding on top of that, so a
    /// per-match `:my $*FINAL` is not read as the last match's value.
    pub(crate) grammar_rule_dynvar_decls: HashMap<String, Vec<String>>,
    pub(super) supply_emit_buffer: Vec<Vec<Value>>,
    /// `whenever` subscription markers registered while a react drive loop is
    /// already running (a `whenever` nested inside another `whenever`'s body).
    /// The loop adopts them on its next round; see
    /// `Interpreter::adopt_newly_registered_subscriptions`.
    pub(crate) pending_react_subscriptions: Vec<Value>,
    /// Sub ids of the callbacks of such nested `whenever`s. A sibling
    /// `whenever` of the react body shares that body's lexicals, so its
    /// callback must re-read them from the live caller env on every value --
    /// hence `call_react_callback` drops the callback's per-instance closure
    /// state. A NESTED `whenever` closes over the *enclosing whenever body's*
    /// frame, which has already exited by the time values arrive, so for it
    /// that per-instance state is the only copy of those lexicals (an
    /// accumulator like `my Buf $in-buf` in HTTP::UserAgent's TestServer) and
    /// dropping it resets them on every value.
    pub(crate) nested_react_callbacks: std::collections::HashSet<u64>,
    /// Emitter `Supplier`s of the `supply` blocks whose code is currently on the
    /// stack, innermost last. `emit` is caught by the innermost *dynamically*
    /// enclosing supply, so a `sub` that is not lexically inside the block still
    /// emits into it when called from within — the parser's `supply` rewrite
    /// (`emit x` -> `$__mutsu_supply_emitter_N.emit(x)`) only reaches `emit`
    /// written directly in the body, and a nested sub's closure never captured
    /// the emitter. Pushed around a `whenever` body invoked as a live-supplier
    /// tap, where the emitter is recovered from the callback's captured env.
    pub(super) active_supply_emitters: Vec<Value>,
    /// `whenever <Promise>` sources inside a `supply` block, rewritten to a
    /// stand-in supplier and waiting to be armed. A supplier keeps no backlog,
    /// so the promise must not be armed until the consumer has registered the
    /// taps for the rewritten subscription — see
    /// `Interpreter::normalize_promise_whenever_markers`.
    pub(crate) pending_promise_whenever_arms: Vec<(crate::value::SharedPromise, Value)>,
    pub(super) supply_emit_timed_buffer: Vec<Vec<(Value, crate::runtime::thread_compat::Instant)>>,
    /// Active streaming consumers for on-demand `supply { ... }` bodies driven by
    /// `react`. When a stream consumer is registered for an emitter's
    /// `supplier_id`, `emit` delivers the value to the consumer callback
    /// synchronously (instead of buffering into `supply_emit_buffer`), so an
    /// infinite synchronous body (`supply { loop { emit(...) } }`) can be
    /// terminated by the consumer's `done` on emit-to-dead-consumer.
    pub(super) supply_stream_consumers: Vec<crate::runtime::subtest::StreamConsumer>,
    /// Nesting depth of the running `react` drive loop. `> 0` while the event
    /// loop is polling subscriptions and dispatching `whenever`/`LAST`/`QUIT`
    /// callbacks. Used so a `whenever` that taps an on-demand supply from inside
    /// a running react (`whenever $outer { whenever $sod { } }`) routes the
    /// supply's `closing => { ... }` callbacks to the main react thread instead
    /// of firing them on an async body's worker thread (where a write to a
    /// captured react-block lexical would be lost).
    pub(super) react_active: usize,
    /// Async on-demand supplies tapped by a nested `whenever` while a react drive
    /// loop is running: `(done_signal_promise, closing_callbacks)`. The drive
    /// loop fires each entry's `closing` callbacks on the main thread once the
    /// promise resolves (the emitter signalled `done`), so per-tap
    /// `closing => { ... }` runs on the react thread rather than a worker thread.
    pub(super) pending_tap_closes: Vec<(crate::value::SharedPromise, Vec<Value>)>,
    /// The waker of the innermost running react/await drive loop on this
    /// thread, so sources wired up mid-loop (a nested `whenever` tapping an
    /// async on-demand supply -> `pending_tap_closes`) can wake the loop when
    /// they become ready instead of waiting out its idle cap.
    pub(super) current_react_waker: Option<crate::value::waker::ReactWaker>,
    /// Cross-thread lexical store for THIS spawn lineage (ADR-0010). `start`
    /// and friends give the child a store chained to this one, so a child sees
    /// and can write the parent's lexicals while its own declarations stay
    /// private to it — sibling threads (e.g. hyper workers each declaring
    /// `my $uri`) cannot clobber each other, which one process-global bare-name
    /// map allowed.
    shared_vars: Arc<crate::runtime::shared_store::SharedStore>,
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
    /// Keys in shared_vars that were written by some thread *while it held a
    /// critical section* (Semaphore/Lock). Entering a critical section syncs
    /// exactly these scalars back into the local env, so a bare
    /// read-modify-write of a shared accumulator (`$s.acquire; $r += $i;
    /// $s.release`) reads the value the previous holder committed — while a
    /// per-iteration loop lexical (`my $i = $_`, written outside any critical
    /// section) keeps this thread's own captured snapshot.
    shared_critical_dirty: Arc<RwLock<HashSet<String>>>,
    /// Depth of nested critical sections (Semaphore/Lock) this interpreter
    /// currently holds. Writes performed while > 0 mark `shared_critical_dirty`.
    critical_section_depth: usize,
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
    /// Roles whose `.new` is currently constructing through their pun. `.new` on
    /// a role composes it into a class of the same name and re-enters
    /// `dispatch_new` to run *that class's* constructor; the role name is pushed
    /// here for the duration so the re-entry takes the class path instead of
    /// recognising the name as a role again and looping.
    pub(crate) role_pun_construction: Vec<String>,
    /// Ids currently being rendered by `Mu.rakuseen($id, &code)` — the
    /// cyclic-structure guard for `.raku`/`.gist`. A repeated id means a cycle:
    /// `rakuseen` returns a backreference name instead of re-running `&code`
    /// (which would recurse forever), and the first (outer) occurrence wraps its
    /// result in `(my \NAME = ...)`.
    pub(crate) rakuseen_active: Vec<String>,
    /// Ids for which a cycle backreference was emitted during the current render;
    /// the outer `rakuseen` for that id consumes the flag to add the `(my \NAME =
    /// ...)` binding wrapper.
    pub(crate) rakuseen_cycle_hit: std::collections::HashSet<String>,
    /// Instance ids whose `.raku` is currently being rendered by the nested-leaf
    /// walker (`methods_raku_dispatch`). A self-referencing object
    /// (`$obj.myself[0] = $obj`) would otherwise recurse forever: instance →
    /// attribute container → the same instance. A repeated id renders as a
    /// Rakudo-style backreference name (`Bug_48`) instead of dispatching again.
    pub(crate) raku_leaf_active: Vec<u64>,
    /// Instance ids for which a cycle backreference was emitted during the
    /// current native `.raku` render; the frame that pushed the id onto
    /// [`raku_leaf_active`] consumes the flag to wrap its rendering in the
    /// `(my \NAME = ...)` binding (mirroring the user-facing `rakuseen`).
    pub(crate) raku_leaf_cycle_hit: std::collections::HashSet<u64>,
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
    /// Stack of samewith dispatch contexts, pushed whenever a multi sub,
    /// multi method, or proto is entered, popped on exit. ADR-0019 E9c-1:
    /// a single `Vec<SamewithContext>` — every push site funnels through
    /// `push_samewith_context`/`push_method_samewith_context`, so `args` can
    /// never desync from `name`/`invocant` the way the former separate
    /// `samewith_call_args_stack` could (see `SamewithContext`'s doc comment).
    samewith_context_stack: Vec<SamewithContext>,
    /// Metamodel-method dispatch contexts:
    /// (samewith_depth, receiver_class, method_name, args).
    /// Pushed alongside the samewith context when the receiver's MRO includes
    /// a builtin metamodel class (Metamodel::ClassHOW / Metamodel::GrammarHOW),
    /// so a `callsame` in a user HOW method that exhausts the user MRO can
    /// fall through to the NATIVE metamodel implementation (e.g. the default
    /// `find_method`), which is not represented as a `MethodDef` candidate.
    /// `samewith_depth` ties each entry to its samewith frame so the shared
    /// pop helper knows whether the top entry belongs to the frame being popped.
    metamodel_dispatch_stack: Vec<(usize, String, String, Vec<Value>)>,
    /// The type object of the DECLARE'd class whose registration is currently
    /// driving the user HOW protocol (`new_type` → `add_method`* → `compose`).
    /// A `callsame` from a user `new_type` override returns it as the base
    /// candidate — the native part of `new_type` (creating and registering the
    /// type) has already run by the time the user hook is called.
    pending_declare_new_type: Option<Value>,
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
    /// Monotonic counter stamped onto `wrap_dispatch_stack`/`method_dispatch_stack`/
    /// `multi_dispatch_stack` frames at push time (ADR-0019 E9b-0). callsame/nextsame/
    /// lastcall/nextcallee select the live frame with the HIGHEST token — the innermost
    /// dynamic dispatch context — instead of a fixed wrap-then-method-then-multi search
    /// order, so a method deferral nested inside a sub wrapper (or vice versa) resolves
    /// to its own chain instead of shadowing/being shadowed by the other stack.
    dispatch_token_counter: u64,
    /// One-shot chain-skip for callsame/callwith invoking the *original* sub
    /// (or an inner wrapper) of an active wrap dispatch: the very next
    /// `call_sub_value` on this sub id must run the sub directly instead of
    /// re-entering its wrap chain. Everything else — notably a *recursive*
    /// named call from inside the original body — re-enters the chain, the
    /// way Raku re-dispatches every fresh call of a wrapped sub.
    wrap_skip_once: Option<u64>,
    /// When set, a binding failure inside `call_compiled_closure` is returned
    /// raw instead of going through `enhance_binding_error`. The interpreter
    /// value-call carrier (`call_sub_value`) sets this around its
    /// compiled-routine fork (ADR-0019 C6d-4): a value call is never a
    /// compile-time-diagnosable call, so reclassifying its binding failure as
    /// a compile-flavored `X::TypeCheck::Argument` loses the runtime
    /// `X::TypeCheck::Binding` identity a sequence endpoint check relies on
    /// (roast S03-sequence/misc.t).
    pub(crate) suppress_binding_error_enhance: bool,
    /// Metamodel method fallbacks registered via `.^add_fallback(cond, calc)`:
    /// class_name -> list of (condition, calculator) code pairs. When a method
    /// is not found on a value of that class, each condition is called with
    /// `(invocant, method_name)`; the first that returns True has its calculator
    /// called with `(invocant, method_name)` to produce the method body, which is
    /// then invoked with the invocant.
    method_fallbacks: HashMap<String, Vec<(Value, Value)>>,
    /// Names suppressed by `anon class`. These bare words should error as undeclared.
    suppressed_names: HashSet<String>,
    /// Short names of types declared *inside a class body* (`class Outer { grammar
    /// Inner {...} }` records `Inner`). Unlike `suppressed_names` this set is never
    /// cleared: it records the fact that the short name belongs to some owner
    /// package, which stays true for the rest of the program even after another
    /// module registers an unrelated type of the same short name. It gates the
    /// owner-package-chain probe in `resolve_suppressed_type`, so a method body
    /// keeps seeing its own class's nested type (see `resolve_suppressed_type`).
    class_scoped_short_names: HashSet<String>,
    /// Bare enum variant names poisoned by redeclaration from different enums.
    /// Maps bare name -> latest enum package name.
    poisoned_enum_aliases: HashMap<String, String>,
    /// Per-scope stack of bare enum names introduced, for cleanup on scope exit.
    enum_scope_names: Vec<Vec<String>>,
    /// Fully-qualified names of `my`-scoped classes/subs inside packages.
    /// These should NOT appear in the parent package's stash.
    my_scoped_package_items: HashSet<String>,
    /// Names published by an explicit `our` declaration; wins over
    /// `my_scoped_package_items` (see `mark_our_scoped_package_item`).
    our_scoped_package_items: HashSet<String>,
    /// Stack of lexically-scoped class names per block scope depth.
    /// When a block scope exits, classes registered in that scope get suppressed.
    lexical_class_scopes: Vec<Vec<String>>,
    /// Maps a lexical class's qualified name to the storage name a
    /// currently-open scope most recently registered it under, for stub ->
    /// full-definition continuation across two separate `decl_id`s (ADR-0047
    /// P1; see `lexical_class_pending_stub`'s doc comment).
    lexical_class_pending: std::collections::HashMap<String, String>,
    /// Per block-scope stack of `(qualified_name, storage_name)` records added
    /// to `lexical_class_pending` while that scope was open. Released (not
    /// just popped) at `pop_lexical_class_scope` so the map can never answer a
    /// query with an entry from an already-exited scope.
    lexical_class_pending_scopes: Vec<Vec<(String, String)>>,
    /// Last expression value from VM execution, used by REPL for auto-display.
    pub(crate) last_value: Option<Value>,
    /// Pending env updates from regex code blocks, to be synced to VM locals.
    pub(crate) pending_local_updates: Vec<(String, Value)>,
    /// Set of variable names that are readonly (default parameter binding).
    /// Copy-on-write and `Symbol`-keyed — see [`ReadonlySet`]. Boxed (its own
    /// heap allocation, separate from `Interpreter`'s) so
    /// [`crate::vm::vm_call_state_guard::ReadonlyFrameGuard`] can hold a raw
    /// pointer into it that survives intervening `&mut self` calls — see that
    /// guard's doc comment and the module doc in `vm_call_state_guard.rs`
    /// ("v3": each guarded field is its own separate heap allocation).
    pub(crate) readonly_vars: Box<std::cell::RefCell<ReadonlySet>>,
    /// Journal of readonly-set mutations made while at least one readonly
    /// scope is open (newest last); `exit_readonly_frame` replays the
    /// inverses back to its scope's mark. `Scope` sentinels bound each open
    /// frame's entries (see `enter_readonly_frame`). Journaling is off at top
    /// level (`readonly_frames == 0`), so the journal cannot grow across a
    /// program's lifetime. Boxed for the same reason as [`Self::readonly_vars`].
    pub(crate) readonly_undo: Box<std::cell::RefCell<Vec<ReadonlyUndo>>>,
    /// Number of currently-open readonly scopes (see `enter_readonly_frame`).
    /// Boxed for the same reason as [`Self::readonly_vars`].
    pub(crate) readonly_frames: Box<Cell<u32>>,
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
    pub(crate) monkey_typing: bool,
    /// Defaults selected by the import list of the latest
    /// `use JSON::Fast <...>` / `use JSON::Tiny` (see `runtime/json.rs`).
    pub(crate) json_import_defaults: crate::runtime::json::JsonImportDefaults,

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
    /// Free-var names the currently-running frame vouches for (its own
    /// `authoritative_free_vars` plus any inherited via `owned_captures`). A
    /// closure created in this frame inherits authoritative (overwrite) capture
    /// for any of its free vars listed here — the runtime counterpart of the
    /// compile-time `propagate_authoritative_down`, which does not reach a closure
    /// created inside a `.map`/`.grep`-invoked block (its runtime CompiledCode is
    /// a different copy than the one the compile-time propagation mutates). Set on
    /// closure entry, saved/restored across call frames like `upvalues`.
    pub(crate) frame_authoritative: Vec<crate::symbol::Symbol>,
    /// Free-var names the currently-running closure frame vouches for as
    /// loop-frozen (ADR-0027) — its own `owned_captures`, installed
    /// force-overwrite at entry because they held a distinct value for this
    /// closure's creating iteration. A closure created in this frame
    /// inherits owned (force-overwrite) capture for any of its free vars
    /// listed here WHOSE CURRENTLY CAPTURED VALUE IS PLAIN — a
    /// `ContainerRef`-valued name is a live shared cell (already handled by
    /// the unconditional cell-overwrite merge) and must NOT be cascaded as
    /// frozen, which would reintroduce the `roast/S17-lowlevel/lock.t`
    /// stale-snapshot hazard `frame_authoritative` deliberately excludes
    /// `owned_captures` from. Set on closure entry, saved/restored across
    /// call frames like `frame_authoritative`, emptied on every other frame
    /// push.
    pub(crate) frame_owned: Vec<crate::symbol::Symbol>,
    pub(crate) in_smartmatch_rhs: bool,
    pub(crate) transliterate_in_smartmatch: bool,
    pub(crate) substitution_in_smartmatch: bool,
    pub(crate) last_topic_value: Option<Value>,
    pub(crate) topic_save_stack: Vec<Value>,
    /// Saved `$_` + `topic_source_var` for a pointy-topic scope (`if COND -> $_`,
    /// `with COND -> $_`). The pointy binding introduces a FRESH lexical `$_`
    /// that shadows an enclosing `given`'s topic, so its writes must NOT flow
    /// back to the given's source variable — `EnterPointyTopic` saves + clears
    /// `topic_source_var` for the block, `ExitPointyTopic` restores it.
    pub(crate) topic_source_save_stack: Vec<(Value, Option<String>)>,
    /// The named container the current topic/loop source came from
    /// (`TagContainerRef`), paired with its compile-time-baked local slot
    /// (§1.5; `None` = non-local or runtime-derived) and the fingerprint of
    /// the `CompiledCode` that set it (`resume_code_fp`). The slot lets the
    /// for/given container writeback target the exact `locals` slot when
    /// shadow slots are active, instead of the by-name `position` search.
    /// The fingerprint scopes the signal to its own frame: the tag is always
    /// emitted immediately before the for/given op that consumes it, in the
    /// SAME code object, so consumers (`take_container_ref_for`) discard a
    /// tag whose fingerprint does not match — a leftover from a callee frame
    /// (e.g. a module method's own `for @x` loop) would otherwise be mistaken
    /// for the caller's loop source and its slot would index the WRONG frame's
    /// locals (Text::CSV t/90_csv.t 507-508: `method CSV`'s `@in` tag, slot 28
    /// in the method frame, made the caller's untagged `for in () -> $in` loop
    /// write its items over the mainline's slot 28).
    pub(crate) container_ref_var: Option<(String, Option<u32>, usize)>,
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
    /// Deferred restore of a single named for-loop param's prior binding, applied
    /// by `RestoreForParam` after the loop's LAST/post phasers. Tuple is
    /// `(name, saved_env_value, colliding_local_slot)`: the slot is `Some` when
    /// the loop param shares a compile-time local slot with an enclosing binding
    /// of the same bare name (`my \x = 10; for ... -> \x { }`), so the restore
    /// must write the saved value back through that slot too — otherwise a later
    /// `GetLocal` read of the outer name sees the loop's last iteration value.
    pub(crate) for_param_restore_stack: Vec<(String, Option<Value>, Option<u32>)>,
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
    /// True while evaluating an embedded regex `{ ... }` code block from a grammar
    /// rule (`execute_regex_code_blocks`). Such a block closes over the lexical
    /// scope where the grammar was defined, so a bare free variable the compiler
    /// auto-qualified to the grammar package (`$x` -> `SetGlobal("G::x")`) must
    /// fall back to an existing outer lexical of the same bare name. Scopes that
    /// outer-lexical-write fallback to exactly this context so ordinary `our`/
    /// package-qualified writes elsewhere are unaffected.
    pub(crate) in_regex_code_block: bool,
    /// Resume point for a `.resume`d control signal: `(code_fp, ip)` where
    /// `code_fp` identifies the CompiledCode the ip belongs to (see
    /// `Interpreter::resume_code_fp`). Consumers must verify the fp matches the
    /// code they are about to resume in — an ip from a different (callee) frame
    /// must never be reused as an ip in the handler's frame.
    pub(crate) resume_ip: Option<(usize, usize)>,
    /// Error slot for JIT-compiled bodies (ADR-0004 J1): an `extern "C"` opcode
    /// helper cannot return a `RuntimeError` by value across the native-code
    /// boundary, so it parks the error here and returns a nonzero status; the
    /// JIT entry wrapper takes it back out. Always `None` outside a JIT call.
    pub(crate) jit_error: Option<RuntimeError>,
    /// The following ten fields (through `vardecl_context`) back
    /// `vm_call_state_guard::MarkContextGuard`. They are `Box<Cell<_>>`-backed
    /// (a HEAP allocation separate from `Interpreter`'s own, not a plain
    /// `Cell`/`bool`/`Option<String>` embedded directly in this struct) so the
    /// guard's `Drop` impl can restore them via a raw pointer taken straight
    /// into that separate allocation. A plain `Cell<T>` field is NOT enough:
    /// Miri's Stacked-Borrows retagging does not carve out an embedded Cell's
    /// own byte range as exempt from a later `&mut Interpreter` call's Unique
    /// retag over the WHOLE struct, so a raw pointer into `Interpreter`
    /// itself -- even one that only ever touches a Cell field -- still goes
    /// stale. A `Box`'s heap allocation is a separate Stacked-Borrows
    /// allocation entirely, immune to retags of `Interpreter`'s own memory
    /// (the same reason `runtime::accessors_stack::CurrentPackageGuard`'s
    /// `Arc<RwLock<String>>`/`Arc<AtomicU32>` backing works) — see that
    /// module's doc comment for the full history.
    pub(crate) bind_context: Box<Cell<bool>>,
    pub(crate) scalar_bind_context: Box<Cell<bool>>,
    /// Set by `MarkParamRawBindContext` just before the SetLocal/SetGlobal of
    /// an assignment whose target is a sigilless binding (`-> \v` loop-param
    /// bind statements, writes through a sigilless alias). Its ONLY effect is
    /// to skip scalar-store itemization — a sigilless name is a non-container
    /// alias, so the stored value must stay bare. No other bind semantics.
    pub(crate) param_raw_bind_context: Box<Cell<bool>>,
    pub(crate) bound_decont_active: Box<Cell<bool>>,
    pub(crate) rebind_context: Box<Cell<bool>>,
    /// Set by `MarkAccessorRefContext` immediately before a CallMethod(Mut)
    /// whose result is wanted as a container (`:=` bind RHS / `.VAR` chain).
    /// Consumed and unconditionally cleared at CallMethod entry.
    pub(crate) accessor_ref_pending: bool,
    /// `(sigilless name, the bind source denotes a container)`, recorded by
    /// `OpCode::MarkSigillessBindSource` with the source still on the stack and
    /// consumed by `OpCode::MarkSigillessBind` just after the declaration's
    /// store. The two ops bracket the store because neither side alone can
    /// answer the question: the marker has to be written AFTER the store (a
    /// declaration clears the name's inherited readonly flag), but the store
    /// destroys the evidence — a slot can hold a `ContainerRef` for reasons
    /// unrelated to this bind (see `OpCode::MarkSigillessBind`). Carries the
    /// name so a store that re-enters user code (a tied container's `STORE`)
    /// cannot make one declaration consume another's verdict.
    pub(crate) sigilless_bind_source: Option<(Symbol, bool)>,
    pub(crate) constant_context: Box<Cell<bool>>,
    /// Slice 2a (`docs/scalar-array-sharing.md`): set by `MarkArrayShareContext`
    /// just before a `SetLocal` for `$scalar = @arr` / `$scalar = %hash`. Tells
    /// the assignment to promote the source container to a shared `ContainerRef`
    /// cell (raku reference semantics) rather than snapshotting it.
    pub(crate) array_share_context: Box<Cell<bool>>,
    /// Slice 2a/2b: the source variable name whose container the upcoming
    /// `SetLocal`/`AssignExpr` should share (set by `MarkArrayShareSource`).
    /// `@z`/`%h` for a whole-container RHS (`$n = @z`), or a scalar name for a
    /// chained share (`$r = $q`); the runtime only shares when that source holds
    /// a container/`ContainerRef` (so a plain `$x = $y` stays a copy).
    pub(crate) array_share_source: Box<Cell<Option<String>>>,
    /// Slice 2a: cheap gate — `true` once any `__mutsu_array_share::` marker has
    /// been set, so the `SetLocal` write-through fast path only pays the marker
    /// lookup when at least one `=`-array-shared scalar exists.
    pub(crate) array_share_active: bool,
    /// Slice 2b: set by `MarkElementShare` to flag the upcoming
    /// `IndexAssignExprNamed` as a `=`-reference share of an array/hash element
    /// (vs a true `:=` bind). Consumed by `exec_index_assign_expr_named_op`,
    /// which marks the written element `__mutsu_elem_share::` after the store.
    pub(crate) element_share_pending: bool,
    pub(crate) explicit_initializer_context: Box<Cell<bool>>,
    pub(crate) vardecl_context: Box<Cell<bool>>,
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
    /// Appended every time a resume-safe `CONTROL` handler is run INLINE at a
    /// warn raise site (`try_resume_safe_control_inline`) and writes one of the
    /// installing frame's lexicals into `env`; each entry is the `Symbol` of
    /// the lexical written.
    ///
    /// That write is an outward mutation made *without* a call opcode, which is
    /// exactly the invariant a leaf closure's return path relies on when it
    /// skips the caller-writeback env scan (`needs_caller_writeback` in
    /// `call_compiled_closure_with_topic`: "no calls were made, so nothing the
    /// caller cares about can have changed"). A closure like
    /// `warns-like`'s `{ 'x' x Int }` makes no calls at all — the warning comes
    /// straight out of an arithmetic opcode — so without this log the
    /// handler's `$did-warn = True` is discarded with the frame's env
    /// (`roast/S03-operators/repeat.t` test 56). Frames snapshot its length on
    /// entry and force the scan when it grew.
    ///
    /// Recording the *names*, not just a counter, also lets the writeback scan
    /// exempt them from the "unchanged capture, skip" optimization
    /// (`call_compiled_closure_with_topic`'s `captured_names`/`values_identical`
    /// check): a name this log names was written by an ANCESTOR frame's
    /// CONTROL handler during the call, so even when its value happens to
    /// equal the closure's own capture-time snapshot (coincidence, not a
    /// no-op), the write must still propagate — seen when a caller variable
    /// already held the CONTROL handler's target value from an earlier call
    /// (`t/control-warn-resume-list-assign-first-target.t`).
    pub(crate) inline_control_env_writes: Vec<Symbol>,
    pub(crate) local_bind_pairs: Vec<(usize, usize)>,
    pub(crate) otf_compile_cache: HashMap<u64, Arc<CompiledFunction>>,
    /// Compiled bodies of subs defined in `use`d modules, captured at module-load
    /// time and keyed by the sub's body/signature fingerprint. Unlike the per-call
    /// `otf_compile_cache` (which a worker thread starts *empty* — every thread
    /// re-OTF-compiles a module sub into a *distinct* body, giving distinct `state`
    /// cells), this map is a snapshot shared by value into every spawned thread's
    /// clone. A module sub routed through the same captured body across threads
    /// reaches its `state` variable under a stable cross-thread key, so
    /// `await (^N).map: { start f() }` accumulates into one shared cell — the piece
    /// the per-thread OTF path could not provide. Populated by `load_module`; read
    /// via `imported_state_body_for_def`. Empty for programs that `use` nothing.
    pub(crate) imported_compiled_fns: HashMap<u64, std::sync::Arc<CompiledFunction>>,
    /// `Box<Cell<_>>`-backed for the same reason as `bind_context` et al.
    /// above: `vm_call_state_guard::StateScopeGuard::Drop` restores it via a
    /// raw pointer into this separate heap allocation, immune to Stacked
    /// Borrows retags of `Interpreter`'s own memory.
    pub(crate) state_scope_id: Box<Cell<Option<u64>>>,
    /// One-shot handoff of a `state` scope into the next nested run: the
    /// interpreter-fallback call path runs a routine body via `run_nested`,
    /// whose register reset clears `state_scope_id` — this field survives the
    /// reset and is consumed by `with_nested_registers` as the nested run's
    /// initial scope, so a fallback-dispatched named sub still keys its state
    /// by its registration clone id (per-clone `state` in nested named subs).
    pub(crate) pending_nested_state_scope: Option<u64>,
    #[allow(clippy::type_complexity)]
    /// Keyed by `(callee name, callsite package, arity, argument type names)`.
    /// The package is part of the key because `resolve_function_with_types` is
    /// package-sensitive: `PkgA::which` and `PkgB::which` are different
    /// routines reached by the same bare name, and a package-blind key let
    /// whichever package called first answer for both.
    pub(crate) fn_resolve_cache:
        rustc_hash::FxHashMap<(Symbol, Symbol, usize, Vec<String>), (Symbol, u64, String)>,
    pub(crate) fn_resolve_gen: u64,
    pub(crate) fn_resolve_cache_gen: u64,
    pub(crate) multi_candidates_cache: rustc_hash::FxHashMap<Symbol, bool>,
    pub(crate) multi_candidates_cache_gen: u64,
    /// Memo for [`Self::has_proto`], keyed by the full bare-name lookup
    /// context `(current_package, innermost lexical_package, name)` — the
    /// exact inputs `bare_name_packages()` derives the search list from — so
    /// a hit can never answer for the wrong package scope. Invalidated by
    /// `Registry::proto_generation()` (bumped on every `proto_subs`
    /// mutation; the field is private so a bump can't be missed). This probe
    /// used to run 3+ times per `CallFunc` dispatch, each walk paying a
    /// `Vec<String>` + two `format!`s per candidate package.
    pub(crate) has_proto_cache: rustc_hash::FxHashMap<(Symbol, Option<Symbol>, Symbol), bool>,
    pub(crate) has_proto_cache_gen: u64,
    /// Memo for [`Self::has_declared_function`], same key shape as
    /// `has_proto_cache`; guarded by `fn_resolve_gen` like
    /// `multi_candidates_cache` (the `functions` map is what it reads).
    pub(crate) declared_fn_cache: rustc_hash::FxHashMap<(Symbol, Option<Symbol>, Symbol), bool>,
    pub(crate) declared_fn_cache_gen: u64,
    /// Memo for [`Self::has_multi_function`], same key shape as
    /// `has_proto_cache`; guarded by `fn_resolve_gen`. The uncached probe
    /// scans EVERY registry function key with a `String` resolve per key,
    /// per call.
    pub(crate) multi_fn_cache: rustc_hash::FxHashMap<(Symbol, Option<Symbol>, Symbol), bool>,
    pub(crate) multi_fn_cache_gen: u64,
    /// Per-name memo of "does ANY registry function key carry this base name?"
    /// (`fn_base_name_registered`). `false` lets `resolve_function_with_types`
    /// return `None` without scanning the whole functions map — the common case
    /// for interpreter-native builtins like `make` / `prefix:<~>`, which
    /// otherwise pay a full failed candidate walk on every call. Guarded by
    /// `fn_resolve_gen` like `multi_candidates_cache`.
    pub(crate) fn_base_name_cache: rustc_hash::FxHashMap<Symbol, bool>,
    pub(crate) fn_base_name_cache_gen: u64,
    /// Keyed by `(callee name, callsite package)` for the same reason as
    /// [`Self::pos_light_call_cache`] below.
    pub(crate) light_call_cache: rustc_hash::FxHashMap<(Symbol, Symbol), (Symbol, u64)>,
    pub(crate) light_call_cache_gen: u64,
    /// Keyed by `(callee name, callsite package)`: the same bare name means
    /// different routines in two packages (`PkgA::which` vs `PkgB::which`), and
    /// a name-only key made whichever package called first answer for both.
    pub(crate) pos_light_call_cache: rustc_hash::FxHashMap<(Symbol, Symbol), PosLightTarget>,
    pub(crate) pos_light_call_cache_gen: u64,
    /// Bare names that appear as a `&`-sigil parameter in some registered sub
    /// (e.g. `foo` from `sub callit(&foo) {...}`). A call to such a name may be
    /// shadowed by a lexical `&name` binding in the current frame, so the
    /// name-keyed light-call caches must be bypassed for it (the slow path's
    /// `lexical_override` check resolves the correct callable). Populated at sub
    /// registration; checked cheaply (guarded by `is_empty()`) on each call.
    pub(crate) amp_param_shadowed_names: std::collections::HashSet<Symbol>,
    /// Names declared with an empty-signature proto (`proto bar {*}`). Such a
    /// proto's signature gates the whole multi dispatch: any call with
    /// positional arguments "will never work with signature of the proto ()"
    /// (rakudo rejects it at compile time). Populated at proto registration;
    /// checked cheaply (guarded by `is_empty()`) on each call.
    pub(crate) empty_sig_proto_names: std::collections::HashSet<Symbol>,
    /// Fingerprint of the sub declaration currently installed under each
    /// `package::name` (single, non-multi) routine key. A re-executed
    /// `RegisterSub` whose compile-time fingerprint matches the installed one is
    /// an idempotent no-op (see [`crate::ast::sub_registration_fingerprint`]),
    /// so the registrar can return early without re-deriving the FunctionDef and
    /// without invalidating the resolution caches. Entries are best-effort: a
    /// miss simply takes the full registration path.
    pub(crate) registered_fn_fingerprints: rustc_hash::FxHashMap<Symbol, u64>,
    /// Declaration sites (fully-qualified name, compile-time site fingerprint)
    /// that registered a yada-stub routine. A `RegisterSub` executes both
    /// hoisted at block top and in place, so a stub's in-place re-arrival can
    /// find its name already overwritten by the real definition (which the
    /// stub forward-declared); membership here identifies that re-arrival as
    /// an idempotent no-op, while a textually NEW stub after a definition (a
    /// different site, different fingerprint) still raises X::Redeclaration.
    pub(crate) registered_stub_decl_sites: rustc_hash::FxHashSet<(Symbol, u64)>,
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
    /// ADR-0019 E3: the generation-keyed resolved-sequence cache (design
    /// decision 5, `todo/deep/adr0019-e2-e4-resolver-core.md`). Caches the
    /// ordered candidate universe for `(receiver TypeId, method, call shape)`
    /// — not a resolved winner, so unlike `multi_resolve_cache` an ambiguous
    /// per-call ranking never disqualifies an entry from being cached; ranking
    /// against fresh call args happens every time from the cached candidates.
    /// Cleared with the other method caches on any registry generation change
    /// ([`crate::vm::vm_call_method_compiled_cache::Interpreter::refresh_method_caches_for_generation`]).
    pub(crate) resolved_seq_cache: rustc_hash::FxHashMap<
        (
            crate::type_id::TypeId,
            Symbol,
            resolution_sequence::CallShape,
        ),
        Arc<resolution_sequence::ResolvedSequence>,
    >,
    /// Registry method generation observed when the method caches were last valid.
    pub(crate) method_cache_generation: u64,
    #[allow(clippy::type_complexity)]
    pub(crate) last_method_resolve: Option<(Symbol, Symbol, Symbol, Arc<MethodDef>)>,
    pub(crate) fast_method_cache:
        rustc_hash::FxHashMap<(Symbol, Symbol), crate::vm::FastMethodCacheEntry>,
    /// Memoized `class -> NativeCtorPlan` for the native default constructor.
    /// Cleared wherever `fast_method_cache` is cleared, plus the MOP class-shape
    /// mutators (`Attribute.set_build`, `^add_attribute`, `^add_method`,
    /// `^compose`). A class not yet registered is never cached (a role punned
    /// to a class on first use must not freeze a negative plan).
    pub(crate) native_ctor_plan_cache: rustc_hash::FxHashMap<Symbol, Arc<NativeCtorPlan>>,
    /// Sound multi-method resolution cache (§B): for a multi whose dispatch is
    /// purely type+arity based (no `where` / literal / subset / `:D`/`:U` smiley /
    /// coercion candidate), the resolved candidate is a function of the receiver
    /// class + method + the runtime types of the positional args, so it is cached
    /// here keyed on `(class, method, arg-type-keys)`. Cleared with the other
    /// method caches when the registry changes.
    #[allow(clippy::type_complexity)]
    pub(crate) multi_resolve_cache:
        rustc_hash::FxHashMap<(Symbol, Symbol, Vec<Symbol>), Option<(Symbol, Arc<MethodDef>)>>,
    /// Memoized `(class, method) -> is this multi's dispatch type+arity deterministic`
    /// (i.e. cacheable in `multi_resolve_cache`). Computed once by scanning the MRO
    /// candidates for value-dependent constraints.
    pub(crate) multi_type_cacheable: rustc_hash::FxHashMap<(Symbol, Symbol), bool>,
    /// Memoized `(native type name, method) -> does a user `augment` declare this
    /// method on that type or an MRO ancestor` — the `native_lever_a_user_override`
    /// gate every native method call passes through. The answer is a pure function
    /// of the registry shape, so it is sound to key on the pair and clear it with
    /// the other method caches on a registry-generation bump. Without the memo the
    /// gate re-walked the receiver's whole MRO (`Int` -> `Cool` -> `Any` -> `Mu`),
    /// asking `user_method_overloads` at each level, on EVERY `$x.foo` — which
    /// showed up as ~7% of a native-method-dispatch loop's profile
    /// (`has_user_method` + `class_mro` + `user_method_overloads`) purely to
    /// re-derive "no, nobody augmented Int".
    pub(crate) native_lever_a_override_cache: rustc_hash::FxHashMap<(Symbol, Symbol), bool>,
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
    /// The `fn_resolve_gen` value `func_multi_resolve_cache`/`func_multi_type_cacheable`
    /// were last cleared for (see `refresh_func_multi_caches_for_generation`, ADR-0019
    /// Phase F box F5). Mirrors `method_cache_generation`'s role for the method-side
    /// caches: a mismatch means a sub/multi registration happened since these caches
    /// were built, so they are cleared lazily on next read instead of at every one of
    /// `fn_resolve_gen`'s many bump sites.
    pub(crate) func_multi_cache_generation: u64,
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
    pub(crate) block_declared_vars: Vec<NameSet>,
    /// Local-frame slot indices of `given`/`with` pointy-topic parameters
    /// (`given EXPR -> $v {...}`) currently mid-writeback: the enclosing
    /// `Given`/`With` op still needs the slot's final value after its body
    /// finishes. The pointy param's own `VarDecl` makes
    /// `exec_block_local_scope_op` treat it as an ordinary vanishing
    /// block-local `my`, Nil-ing its slot on block exit (and, when the name
    /// shadows an outer variable, `pop_loop_local_scope` may instead
    /// overwrite the slot with the outer binding's restored value) — both of
    /// which run BEFORE the enclosing op's writeback can read it, and a
    /// scalar pointy param's live value has NO other home by then (a plain
    /// scalar lexical skips its env mirror under the `(B)` per-store
    /// env-write gate, see `docs/lexical-scope-slot-campaign.md`). So
    /// `exec_block_local_scope_op` captures each protected slot's live value
    /// into `given_pointy_captured` unconditionally, right after body
    /// execution finishes and before either of those two exit paths can
    /// touch it.
    ///
    /// Keyed by exact SLOT index, not by name/symbol: two nested `given`s can
    /// bind the SAME name (`given $a -> $v { given $b -> $v {...} }`), each
    /// getting its own distinct compiled slot under shadow slots, and a
    /// pointy param can also shadow an outer variable of the same name
    /// (`given 5 -> $x {...}` inside `my $x = 1`) — slot identity is the only
    /// thing that disambiguates either case; name-based matching captured
    /// from (or reset) the wrong declaration's slot in both. `exec_given_op`
    /// determines its own pointy param's slot by peeking the compiled body
    /// for the first `SetLocalDecl`, which is always that param's own
    /// synthetic declaration (`pointy_topic_bind` always inserts it as the
    /// body's first statement) — found before any nested construct's own
    /// declarations, so it is unambiguous even under same-name nesting.
    pub(crate) given_pointy_capture_slots: Vec<usize>,
    /// Parallel stack to `given_pointy_capture_slots`: the captured final value for
    /// each active `given`/`with` pointy param's slot, filled in by
    /// `exec_block_local_scope_op` (`None` until then) and consumed by
    /// `exec_given_op`'s writeback.
    pub(crate) given_pointy_captured: Vec<Option<Value>>,
    pub(crate) loop_local_vars: Vec<NameSet>,
    /// Names currently bound as for-loop parameters in this frame chain, one
    /// set per active loop (ADR-0023). Bare names (no `$` sigil), matching
    /// env keys. Consulted by `block_captured_scalars` only; never persisted.
    pub(crate) active_loop_param_names: Vec<rustc_hash::FxHashSet<String>>,
    /// Parallel to [`Self::active_loop_param_names`], for the parameters that
    /// **alias** rather than copy: the bare names the enclosing `for` loops
    /// currently bind as genuinely rw parameters (`is rw`, a `<->` block, a
    /// sigilless `\v`, a `.kv` value slot).
    ///
    /// An rw parameter is the source element's own container, so a closure over
    /// it reads *through* it and a later write to the element is visible
    /// (`for @a -> $x is rw, $y is rw { $c = -> { $x } }; @a[0] = 99; $c()` is
    /// `99`). `freeze_readonly_owned_captures` consults this to leave such a
    /// name alone: a MULTI-parameter loop binds through
    /// `build_for_bind_stmts`' declaration prefix, which registers the name as
    /// loop-local, and the freeze would otherwise deep-deref the element cell
    /// into a per-iteration snapshot. A single-parameter rw loop binds natively
    /// and never registers, so it was always right -- this is what makes the two
    /// forms agree.
    ///
    /// Runtime-scoped, not a per-`CompiledCode` name set: names are reused
    /// across the loops of one compilation unit, so a compile-time set would let
    /// one loop's `is rw` exempt an unrelated later loop's same-named *non-rw*
    /// parameter (measured: `t/for-loop-element-alias.t`'s per-iteration
    /// identity rows).
    pub(crate) active_loop_rw_param_names: Vec<rustc_hash::FxHashSet<String>>,
    /// Names of every `constant $name = ...` scalar ever declared in this run
    /// (ADR-0022 Slice 5's `__mutsu_constant_var::` marker). Lets
    /// `exec_set_local_op_inner` skip the marker-removal `format!` + env
    /// lookup on an ordinary (non-constant) scalar `my`/`state` whose name was
    /// never used by a `constant` — the overwhelming common case, and NOT the
    /// same as "no constant has been declared anywhere": a single
    /// program-wide bool here previously made every subsequent `my`/`state`
    /// in the whole program (any name) pay the removal cost the instant just
    /// one `constant` existed anywhere (`benchmarks/debug-guard.raku`'s
    /// `constant DEBUG = False` followed by a hot-loop `my $y`, e.g.). Per-name
    /// membership is the actual precondition for the marker existing at all.
    /// Entries are never removed; a name is either "never a constant" (never
    /// pays the removal) or "was once a constant" (pays it — still correct,
    /// just no longer free to skip for THAT name).
    pub(crate) constant_var_names_seen: rustc_hash::FxHashSet<String>,
    /// Per loop-body scope: what each body-local `my` name must be restored to
    /// when the loop exits. `Some(v)` is a genuine shadow (re-expose the outer
    /// binding's value); `None` means the name did not exist before the loop, so
    /// the entry must be REMOVED — otherwise a body-local `my` outlives its block
    /// as an env key, which is how `HTTP::HPACK`'s Huffman-table `my int $i`
    /// stayed visible process-wide and was later merged over an unrelated frame's
    /// loop variable.
    pub(crate) loop_local_saved_env: Vec<HashMap<String, Option<Value>>>,
    pub(crate) loop_cond_active: bool,
    pub(crate) outer_scope_locals: Vec<Vec<Value>>,
    /// Stack of captured ENTER-phaser values for blocks whose textually-last
    /// statement is an ENTER phaser (its entry-time value becomes the block
    /// result). Pushed by `PushEnterResult` in the ENTER section and popped by
    /// `LoadEnterResult` at the end of the block body.
    pub(crate) enter_result_stack: Vec<Value>,
    pub(crate) pending_alias_bind_names: Vec<(String, String)>,
    /// Name-keyed cache of OTF-compiled routine bodies. The cached entry records
    /// the package the resolution was made *under* (the callsite's
    /// `current_package`), because the same bare name resolves to different
    /// routines in different packages: a `unit module Foo`'s non-exported sub is
    /// visible as `foo` only while `current_package == Foo`, and reusing that
    /// entry at a GLOBAL callsite would leak it into the consumer's scope
    /// (PLAN 8.22). A package mismatch falls through to a fresh resolve.
    /// Entries are `(callsite package, defining package, body)`. The defining
    /// package is what the body must run under (it scopes `$?PACKAGE`, qualified
    /// name resolution and the `__mutsu_callable_id::PKG::NAME` lookup that keys
    /// `once`); reading `current_package()` at the callsite instead would give
    /// the caller's package, which only happened to agree while every module sub
    /// registered into GLOBAL.
    /// The body is `Arc`-shared, not owned by value: the hot dispatch path in
    /// `exec_call_func_op` used to `remove()` the entry, run the call, and
    /// `insert()` it back purely to avoid holding a borrow on `self`. A
    /// `CompiledFunction` embeds a whole `CompiledCode` (~1 kB of `Vec`/`HashMap`
    /// headers), so that round trip memcpy'd the struct out of and back into the
    /// table on EVERY call — it profiled as the single largest cost of calling a
    /// block-local sub (`memmove` alone was 15% of the run). Cloning the `Arc`
    /// is one refcount bump and leaves the table untouched.
    pub(crate) otf_call_cache:
        rustc_hash::FxHashMap<Symbol, (Symbol, Symbol, Arc<CompiledFunction>)>,
    pub(crate) otf_call_cache_gen: u64,
    pub(crate) check_phaser_depth: u32,
    /// ADR-0041 §9: hoist-pass sub registrations whose own in-sequence
    /// `RegisterDecl` has not executed yet, keyed by `Pkg::name`. A BEGIN-time
    /// region (`constant` initializer, `BEGIN`/`CHECK` body) rolls these back
    /// so a name reference evaluated there sees only what the program has
    /// textually reached, as rakudo's compile-time pad install does.
    pub(crate) hoisted_unreached_decls:
        rustc_hash::FxHashMap<Symbol, crate::runtime::hoist_visibility::HoistedDeclRecord>,
    /// One frame per open BEGIN-time region: the registry entries that region
    /// hid, and the defs to put back when it closes. Depth-aligned with
    /// `check_phaser_depth`.
    pub(crate) begin_time_hidden: Vec<Vec<(Symbol, Option<Arc<FunctionDef>>)>>,
    /// Depth of `with_nested_registers` re-entry (nested VM runs: closure
    /// bodies dispatched from native code, EVAL, dies-ok blocks, ...). The
    /// uncaught-CX::Return -> X::ControlFlow::Return conversion in `run_inner`
    /// only fires at the TRUE top level (depth 0): inside a nested run the
    /// signal's target routine may well be an outer VM frame, so it must keep
    /// propagating (a tap/quit callback's `return` targeting the sub that
    /// called `.emit`, for example).
    pub(crate) nested_run_depth: u32,
    pub(crate) gather_for_loop_resume: Option<crate::value::ForLoopResumeState>,
    /// Transient hand-off from a consumed `ForLoopResumeState` to its loop
    /// executor: the mid-body ip the resumed iteration's first body run
    /// starts at (see `ForLoopResumeState::resume_body_ip`).
    pub(crate) gather_resume_body_ip: Option<usize>,
    /// Set by `take_value` when a lazy pull's take limit is reached inside a
    /// condition-driven loop (`while`/`until`/C-style `loop`): the suspension
    /// is DEFERRED to that loop's next iteration boundary, where re-entering
    /// from the condition on resume is exact. Suspending at the `take` itself
    /// replayed the statements between the take and the iteration end
    /// (`while $n > 1 { take $n; $n div= 2 }` yielded 6,6,6... —
    /// 99problems-31-to-40.t P37).
    pub(crate) gather_suspend_pending: bool,
    /// True while the innermost enclosing loop op is condition-driven
    /// (`while`/`until`/C-style/`repeat`), i.e. a take-limit hit should defer
    /// to its iteration boundary (`gather_suspend_pending`). `for` loops keep
    /// the immediate at-take signal: their positional resume state
    /// (`next_index`) makes the at-take suspension exact for element values,
    /// and roast pins its side-effect timing (S04-statements/gather.t
    /// "gather is lazy"). Saved/restored on loop-op entry/exit.
    pub(crate) lazy_take_boundary_defer: bool,
    /// Call-frame depth (`call_frames.len()`) at entry to the innermost active
    /// lazy-gather pull (`force_lazy_list_vm_n_inner`), `None` outside one.
    /// The pull driver can only snapshot/resume ITS OWN frame (ip, stack,
    /// locals of the gather body's compiled code), so a take-limit hit inside
    /// a NESTED routine call cannot suspend soundly: the signal would unwind
    /// the callee frames and leave the saved ip pointing at the caller's
    /// call op with its arguments already drained (resume then skips the call
    /// or underflows the stack — `gather trip(5)` with `take` inside `trip`'s
    /// `for` loop). `take_value` compares the live depth against this and
    /// keeps collecting eagerly instead of suspending when the take is
    /// deeper: the pull over-produces but stays correct. Saved/restored
    /// around each pull, so nested pulls compare against their own entry.
    pub(crate) lazy_pull_entry_call_depth: Option<usize>,
    pub(crate) rw_map_topic_capture: Option<Value>,
    /// Next routine-invocation id this interpreter will hand out, and one past
    /// the end of the block it was claimed from (see `NEXT_INVOCATION_ID_BLOCK`).
    /// Equal when the block is exhausted, which is the refill condition.
    pub(crate) next_invocation_id: u64,
    pub(crate) invocation_id_block_end: u64,
    /// Direct-mapped call-dispatch cache (ADR-0066): what each callee name last
    /// resolved to, so a repeat call skips both hash probes the name-keyed path
    /// pays (`pos_light_call_cache`, then `compiled_fns`) — together about 60%
    /// of `exec_call_func_op`'s self time on a call-dominated program. Inline
    /// in the interpreter and indexed by a mask, so a lookup is one dependent
    /// load; per-interpreter (hence per-thread), so the entries need no
    /// synchronisation.
    pub(crate) call_ic: [crate::opcode::CallIcSlot; crate::opcode::CALL_IC_WAYS],
    /// Version stamp every filled [`Self::call_ic`] slot carries. Bumped on any
    /// change to `pos_light_call_cache` (insert or the generation clear), which
    /// makes a slot's validity exactly "the name-keyed cache has not moved
    /// since I read it" — the property that lets the slot stand in for it.
    pub(crate) pos_light_ic_epoch: u64,
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
type SubsetPredicateCompiled = Arc<(crate::opcode::CompiledCode, crate::opcode::CompiledFns)>;

/// Read a value's container type metadata. Array/Hash/Set/Bag/Mix carry it
/// embedded in their backing data struct (travels across copy-on-write);
/// `Instance` values look it up in the shared `instance_type_metadata` side
/// table by id. This free function is the single implementation shared by
/// `Interpreter::container_type_metadata` and the VM's peer-handle native read
/// (CP-3 Track 1: removes the interpreter bounce for `Instance` type-meta
/// reads). It touches no `env`, so neither caller needs an env loan.
pub(crate) fn container_type_metadata_with(
    value: &Value,
    instance_meta: &Arc<RwLock<Arc<HashMap<u64, ContainerTypeInfo>>>>,
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
    match value.view() {
        ValueView::Array(items, ..) => embedded_type_info!(items),
        ValueView::Mix(items, _) => embedded_type_info!(items),
        ValueView::Set(items, _) => embedded_type_info!(items),
        ValueView::Bag(items, _) => embedded_type_info!(items),
        ValueView::Hash(items) => Interpreter::hashdata_type_info(&items),
        ValueView::Instance { id, .. } => instance_meta.read().unwrap().get(&id).cloned(),
        ValueView::Mixin(inner, _) => container_type_metadata_with(inner, instance_meta),
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

/// Which compilation unit each `EVAL` unit was compiled inside, keyed by the
/// unit's `?FILE` name (`EVAL_0`, ...). Process-global, like the counter that
/// mints those names: the names are unique for the life of the process, and an
/// EVAL unit's parent never changes once recorded.
static EVAL_UNIT_PARENTS: std::sync::LazyLock<std::sync::RwLock<HashMap<Symbol, Symbol>>> =
    std::sync::LazyLock::new(|| std::sync::RwLock::new(HashMap::new()));

/// The unit key for the main script. A routine body carries `source_file =
/// None` when it was AOT-compiled and `Some(program_path)` when it was
/// compiled on the fly from the same script, so both normalise to this.
pub(crate) fn main_unit() -> Symbol {
    static MAIN: std::sync::LazyLock<Symbol> =
        std::sync::LazyLock::new(|| Symbol::intern("<main>"));
    *MAIN
}

pub(crate) fn note_eval_unit_parent(unit: Symbol, parent: Symbol) {
    if let Ok(mut map) = EVAL_UNIT_PARENTS.write() {
        map.insert(unit, parent);
    }
}

/// The compilation unit an `EVAL` unit was compiled inside, if `unit` is one.
pub(crate) fn eval_unit_parent(unit: Symbol) -> Option<Symbol> {
    EVAL_UNIT_PARENTS.read().ok()?.get(&unit).copied()
}

pub(crate) type RoutineRegistrySnapshot = (
    rustc_hash::FxHashMap<Symbol, Arc<FunctionDef>>,
    rustc_hash::FxHashMap<Symbol, Arc<FunctionDef>>,
    rustc_hash::FxHashMap<Symbol, Vec<Arc<FunctionDef>>>,
    rustc_hash::FxHashSet<String>,
    rustc_hash::FxHashSet<String>,
    rustc_hash::FxHashSet<Symbol>,
    std::collections::HashMap<String, HashSet<Symbol>>, // user_declared_infix_ops snapshot
);

/// What a lexical import scope (`{ use Foo; ... }`) restores when it pops: the
/// registry key sets that existed before the `use`, plus the pragma flags `use`
/// can flip. Every symbol table an import writes into has to be listed here —
/// `proto_subs`/`proto_functions` were missing, so an imported `proto sub skip`
/// stayed visible to `has_proto` after the block and kept `skip(5, @a)` on the
/// user-routine argument path (VarRef-wrapped) instead of the list builtin's.
pub(crate) struct ImportScopeSnapshot {
    pub(crate) functions: HashSet<Symbol>,
    pub(crate) classes: HashSet<String>,
    pub(crate) proto_subs: HashSet<String>,
    pub(crate) proto_functions: HashSet<Symbol>,
    /// Exact `env` keys `import_module` wrote as an imported alias while
    /// this scope was on top of `import_scope_stack` (e.g. `&ok`, `$CONST`,
    /// or the importing-package-qualified `&GLOBAL::ok` the trait-value
    /// path also writes). Recorded explicitly at the write site
    /// (`record_import_env_key`) rather than diffed from a before/after
    /// snapshot: `env` also carries ordinary statement-level state that has
    /// nothing to do with imports (`$!`, `$_`, a plain `my` local, ...), and
    /// diffing would drop those too just because they happened to be
    /// written for the first time inside a `use`-containing block — see
    /// `pop_import_scope`'s doc comment for the regression that caused.
    pub(crate) imported_env_keys: HashSet<Symbol>,
    pub(crate) newline_mode: NewlineMode,
    pub(crate) strict_mode: bool,
    pub(crate) fatal_mode: bool,
    pub(crate) monkey_typing: bool,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

/// Internal trait marking a routine that came from a prelude spliced into the
/// host compunit rather than from its source. Such a routine registers under
/// `GLOBAL` (so a method body under any package reaches it by bare name) and
/// enters no module's export map. Marker traits are `__`-prefixed by
/// convention, which is how registration tells them from a user trait — see
/// `has_user_custom_traits` in `registration_sub`.
pub(crate) const PRELUDE_SUB_TRAIT: &str = "__mutsu_prelude";

/// Reserved pseudo-unit key mainline's own captured `my` lexicals are stored
/// under in `Interpreter::unit_lexicals` (ADR-0024). Contains `<`/`>`, which
/// cannot appear in a real Raku package name, so no user `package`/`module`/
/// `class` can collide with it.
pub(crate) const MAINLINE_UNIT_KEY: &str = "UNIT<mainline>";

/// Immutable process-constant magic/dynamic variables hoisted into the shared
/// env base tier (see `Interpreter::new`). These hold the same value for the
/// whole process and are never reassigned/removed by normal programs, so they
/// need not live in every per-frame env overlay (docs/vm-dual-store.md 4c).
///
/// `$*VM`/`$*PERL`/`$*RAKU`/`$*KERNEL`/`$*DISTRO` are deliberately NOT listed
/// here (todo/tickets/magic-vars-should-be-built-lazily.md Slice 2): building
/// their `Instance` values (Version parses, a 32-element signal array, the
/// `vm_config` hash) is real CPU work a program that never reads them
/// shouldn't pay at every `Interpreter::new()`/thread-clone. They instead
/// materialize on first read via `Interpreter::lazy_magic_dynamic_var`
/// (`src/runtime/io_env.rs`), cached process-wide the same way as everything
/// else here (a `OnceLock` per var).
const IMMUTABLE_BASE_DYNAMICS: &[&str] = &[
    "*PID",
    "*TZ",
    "*INIT-INSTANT",
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
    fn sub_declaration_installs_its_compiled_candidate() {
        let mut interp = Interpreter::new();
        let output = interp
            .run("sub compiled-adapter() { 42 }; say compiled-adapter();")
            .unwrap();
        assert_eq!(output, "42\n");
        let key = Symbol::intern("GLOBAL::compiled-adapter");
        let registry = interp.registry();
        let def = registry
            .functions
            .get(&key)
            .expect("registered function candidate");
        assert!(def.compiled.is_some());
    }

    #[test]
    fn compiled_sub_candidate_carries_normalized_signature_metadata() {
        let mut interp = Interpreter::new();
        let output = interp
            .run("sub normalized(:$value = 42) { $value }; say normalized();")
            .unwrap();
        assert_eq!(output, "42\n");
        let key = Symbol::intern("GLOBAL::normalized");
        let registry = interp.registry();
        let def = registry
            .functions
            .get(&key)
            .expect("registered function candidate");
        let compiled = def
            .compiled
            .as_ref()
            .expect("normalized candidate uses its compiled body");
        assert_eq!(
            format!("{:?}", compiled.param_defs),
            format!("{:?}", def.param_defs)
        );
        assert_eq!(compiled.empty_sig, def.empty_sig);
    }

    /// ADR-0019 C6c: a code object built from a registry routine must carry that
    /// routine's compiled body, so dispatching it never compiles the AST body the
    /// declaration copied into the `Sub`.
    #[test]
    fn code_object_from_a_routine_carries_the_routines_compiled_body() {
        let mut interp = Interpreter::new();
        let output = interp
            .run("sub code-object-twice($n) { $n * 2 }; say &code-object-twice(21);")
            .unwrap();
        assert_eq!(output, "42\n");
        let def = {
            let registry = interp.registry();
            let def = registry
                .functions
                .get(&Symbol::intern("GLOBAL::code-object-twice"))
                .expect("registered function candidate");
            (**def).clone()
        };
        let routine = def
            .compiled
            .as_ref()
            .expect("the declaration plan attached a compiled body")
            .clone();
        let sub_val = interp.sub_value_from_function_def(def);
        let crate::value::ValueView::Sub(data) = sub_val.view() else {
            panic!("&code-object-twice resolves to a Sub");
        };
        let carried = data
            .compiled_routine
            .as_ref()
            .expect("the code object carries the routine's compiled body");
        assert!(
            Arc::ptr_eq(carried, &routine),
            "the code object shares the routine's CompiledFunction rather than a re-compile"
        );
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
        assert_eq!(interp.last_value, Some(Value::int(7)));
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
        env.insert("used".to_string(), Value::int(1));
        env.insert("unused".to_string(), Value::int(2));
        env.insert("$target".to_string(), Value::int(0));
        env.insert("@noise".to_string(), Value::array(vec![Value::int(3)]));

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
        compiled.plain_locals = vec![true, false, false];
        compiled.ops = vec![
            OpCode::GetGlobal(0),
            OpCode::GetArrayVar(1),
            OpCode::SetGlobal(0),
            OpCode::SetLocal(2),
        ];

        let block = crate::gc::Gc::new(SubData {
            package: Symbol::intern("GLOBAL"),
            name: Symbol::intern("__protect_test__"),
            params: vec![],
            param_defs: vec![],
            body: std::sync::Arc::new(vec![Stmt::Expr(Expr::Literal(Value::int(0)))]),
            is_rw: false,
            is_raw: false,
            env,
            assumed_positional: vec![],
            assumed_named: std::collections::HashMap::new(),
            id: 1,
            empty_sig: false,
            is_bare_block: false,
            compiled_code: Some(Arc::new(compiled)),
            compiled_fns: None,
            compiled_routine: None,
            is_decl_expr_thunk: false,
            deprecated_message: None,
            source_line: None,
            source_file: None,
            owned_captures: Vec::new(),
            authoritative_captures: Vec::new(),
            upvalues: Vec::new(),
            captured_fatal_mode: false,
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
