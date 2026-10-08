use crate::symbol::Symbol;
use std::cell::Cell;
use std::collections::{HashMap, HashSet};

mod complex_real;
mod package_keyed;
mod rw_writeback_slots;
mod tolerance;
/// A per-package table of per-name entries: `package -> name -> V`, carrying
/// the name filter that lets a failed package-chain walk cost one hash probe
/// instead of one per tier per candidate. See the module.
pub(crate) use package_keyed::PackageKeyed;
pub(crate) use tolerance::approx_eq_f64;
/// The compunit / package-block lexical stores (`unit_lexicals`,
/// `package_lexicals`): see [`PackageKeyed`].
pub(crate) type PackageLexicals = PackageKeyed<Value>;
/// The full, specificity-sorted candidate list a multi dispatch frame carries
/// (`push_multi_dispatch_frame`), shared between the frames that reuse it.
pub(crate) type MultiCandidateList = std::sync::Arc<Vec<std::sync::Arc<FunctionDef>>>;

/// Key of the sound multi-*sub* resolution caches (`func_multi_resolve_cache`
/// and the per-argument-type `func_multi_argkey_cacheable` refinement):
/// `(package, name, argument type keys)`.
pub(crate) type FuncMultiResolveKey = (Symbol, Symbol, Vec<Symbol>);

/// Key of the `CallMethodMut` plain-method lane (`plain_method_lane`):
/// `(receiver class, method name, argument type keys)`, the last empty for a
/// call without arguments. See `vm_call_method_plain_lane`.
pub(crate) type PlainMethodLaneKey = (Symbol, Symbol, Vec<Symbol>);

/// `is export`-ed regex declarator bodies, keyed by the module that declared
/// them and then by the declarator's name (see `Interpreter::exported_token_defs`).
type ExportedTokenDefs = HashMap<String, HashMap<String, Vec<std::sync::Arc<FunctionDef>>>>;
/// `(name, current package, frame lexical package) -> candidate list`: see
/// `Interpreter::multi_dispatch_candidates_memo`.
pub(crate) type MultiDispatchCandidatesMemo =
    rustc_hash::FxHashMap<(Symbol, Symbol, Option<Symbol>), MultiCandidateList>;
use std::env;
use std::fs;
use std::io::{Read, Seek, SeekFrom, Write};
use std::net::ToSocketAddrs;
#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;
#[cfg(windows)]
use std::os::windows::fs as windows_fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::AtomicU32;
use std::sync::{Arc, Mutex, RwLock};

/// A role's `role_id` is drawn from the SAME process-global counter as the
/// parse-time declaration-site ids (`crate::ast::next_global_decl_id`). Both
/// end up after a `\u{0}` in a type name -- `R\u{0}<role_id>` is one parametric
/// candidate of group `R`, `R\u{0}<decl_id>` a `my role R`'s storage name
/// (ADR-0047 P1) -- so they must never coincide, or a lexical role's storage
/// name would read as a candidate of an unrelated package-scoped `R`.
pub(crate) fn next_role_id() -> u64 {
    // Minted at compile time into a role's declaration plan. Inside a cacheable
    // module compile it comes from the compile's content-addressed session, so
    // a cached plan's id cannot collide with one minted in the loading process
    // (ADR-11756 §2.3); its high bit keeps it above every counter value.
    if crate::compiler::compile_session::in_content_session() {
        return crate::compiler::compile_session::mint();
    }
    crate::ast::next_global_decl_id()
}
use std::time::Duration;

use crate::ast::{Expr, FunctionDef, ParamDef, PhaserKind, ReadonlyKind, Stmt};
use crate::env::Env;
use crate::opcode::{CompiledCode, CompiledFns, CompiledFunction};
use crate::parse_dispatch;
use crate::runtime::gen_cache::GenCache;
use crate::value::ValueView;
use crate::value::{
    ArrayKind, AttrMap, EnumValue, JunctionKind, LazyList, RuntimeError, SharedChannel,
    SharedPromise, Value, make_rat, take_pending_instance_destroys,
};

/// Callback printing an uncaught exception; see [`Interpreter::set_uncaught_reporter`].
pub type UncaughtReporter = Box<dyn FnMut(&mut Interpreter, &RuntimeError) + Send>;

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

/// Seed the representation payload carried by a subclass of a native scalar.
///
/// `Mu.new` and `Mu.bless` both construct an ordinary instance, but the native
/// `Int`/`Num`/`Str` implementations need their scalar value in a reserved attribute
/// so value-level coercion and rendering can see it without consulting the
/// class registry. Keep the convention in one place so the two constructor
/// paths cannot drift again.
pub(crate) fn seed_native_subclass_payloads(
    attrs: &mut AttrMap,
    class_mro: &[Symbol],
    args: &[Value],
    positional_args: &[Value],
) {
    use crate::builtins::numeric_subclass::{INT_PAYLOAD, NUM_PAYLOAD, RAT_PAYLOAD};
    if class_mro.iter().any(|name| name == "Int") && !attrs.contains_key(INT_PAYLOAD) {
        let payload = positional_args.first().map_or(0, crate::runtime::to_int);
        attrs.insert(INT_PAYLOAD, Value::int(payload));
    } else if class_mro.iter().any(|name| name == "Num") && !attrs.contains_key(NUM_PAYLOAD) {
        // raku: `Num.new(\value)` boxes `value.Num` into the subclass; no
        // argument is `0e0`.
        let payload = positional_args.first().map_or(0.0, |v| {
            crate::runtime::coerce_to_numeric(v.clone()).to_f64()
        });
        attrs.insert(NUM_PAYLOAD, Value::num(payload));
    } else if class_mro.iter().any(|name| name == "Rat") && !attrs.contains_key(RAT_PAYLOAD) {
        // raku: `Rat.new(\nu, \de)` boxes the reduced fraction into the
        // subclass; no argument is `0/1`. The native constructor builds it.
        let payload = Interpreter::build_native_rat_value(positional_args);
        attrs.insert(RAT_PAYLOAD, payload);
    }
    if class_mro.iter().any(|name| name == "Str") && !attrs.contains_key("__mutsu_str_value") {
        let payload = args
            .iter()
            .find_map(|arg| match arg.view() {
                ValueView::Pair(key, value) if key == "value" => Some(value.to_str_context()),
                _ => None,
            })
            .unwrap_or_default();
        attrs.insert("__mutsu_str_value", Value::str(payload));
    }
    // An `is IterationBuffer` subclass keeps its elements in a reserved array
    // attribute, which nqp code (`nqp::push(self, ...)`) writes into from the
    // first statement of a method. `nqp::create` seeds it (`create_instance`),
    // so `.new` / `.bless` have to as well.
    let items = nqp_ops_list::iteration_buffer_items_key();
    if class_mro.iter().any(|name| name == "IterationBuffer") && !attrs.contains_key(items) {
        attrs.insert(items, Value::real_array(Vec::new()));
    }
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
            ValueView::Slip(vals) => vals.to_vec(),
            // Same itemization guard as the Array arm above: an itemized
            // Hash (`my $h = {a=>1}; @a.append($h)`) is a single `$`-held
            // element and must NOT flatten into its pairs -- confirmed
            // directly against `raku` (`@a.append($h)` there stays a
            // one-element array holding the Hash itself).
            ValueView::Hash(map) if !args[0].hash_is_itemized() => {
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
                Value::package(crate::symbol::wk::any())
            } else {
                v.itemize_for_element_store()
            }
        })
        .collect()
}

/// Split a string by commas while respecting bracket/paren depth.
/// Returns the trimmed, non-empty parts.
pub(crate) fn split_balanced_comma_list(input: &str) -> Vec<String> {
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
pub(crate) fn current_process_id() -> i64 {
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
    sys_resources::local_offset_at(crate::builtins::epoch_nanos() / 1_000_000_000).0
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
    /// The ambient `is_routine` / `lexically_in_routine` the body compiles
    /// under (both set from it, outside an EVAL unit's ADR-0037 answer).
    in_routine: bool,
    /// The definition-site `return` classification an owned body carries
    /// (ADR-0050 §2.3): a chunk compiled for one must not be served for a body
    /// classified differently.
    return_routineness: Option<resolution_eval::BlockRoutineness>,
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
    /// The four post-compile mutations `eval_block_value_inner` applies to the
    /// chunk it just compiled: the supply-body mark, the emitter name, the
    /// vouched capture set and the inherited owned-lexical set. They come from
    /// the `SubData` being run, so they are the same on every call for one
    /// `cache_id` -- but they are *not* the same across two different code
    /// objects that happen to share one, so they belong in the key.
    ///
    /// They used to bypass the cache instead ("compile fresh, don't store"),
    /// which meant a `whenever` callback -- whose `authoritative_captures` is
    /// never empty -- was re-compiled from AST on every single emitted value.
    /// On Cro's HTTP/2 parser that was one full `Compiler::compile` per DATA
    /// frame, 38% of the frame's instructions (#7667).
    supply_block_body: bool,
    supply_emitter_sym: Option<Symbol>,
    supply_authoritative_free_vars: Vec<Symbol>,
    whenever_inherited_owned: Vec<Symbol>,
}

/// Split-and-shared `whenever` body, memoized per parse site.
///
/// `split_whenever_body_phasers` partitions a `whenever` body into its main
/// statements and its `LAST`/`QUIT` phaser bodies. That is a pure function of
/// the body AST, but it ran on every *registration*, deep-cloning every
/// statement into fresh `Vec`s -- so a `whenever` registered in a loop paid an
/// O(body) AST copy each time AND handed the callback `Sub` a brand-new
/// `Arc<Vec<Stmt>>`, which meant the carrier compile cache (keyed by parse
/// site, see [`CarrierCacheKey`]) could never hit for it. Memoizing it hands
/// every registration the same `Arc`s. Keyed by the pool-owned body `Arc`,
/// held rather than addressed for the same soundness reason as
/// [`MapGrepCacheKey`].
type WheneverBodySplit = (
    Arc<Vec<crate::ast::Stmt>>,
    Vec<Arc<Vec<crate::ast::Stmt>>>,
    Vec<Arc<Vec<crate::ast::Stmt>>>,
);

/// Key for `whenever_body_splits`: pointer identity of the pool-owned body
/// `Arc`, held so the allocation cannot be freed and reused underneath a
/// cached split.
#[derive(Clone)]
pub(crate) struct WheneverBodyKey(Arc<Vec<crate::ast::Stmt>>);

impl PartialEq for WheneverBodyKey {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for WheneverBodyKey {}

impl std::hash::Hash for WheneverBodyKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (Arc::as_ptr(&self.0) as *const u8 as usize).hash(state);
    }
}

/// What a carrier-block compile is cached *under*.
///
/// The compiled chunk is a pure function of the body AST plus the ambient
/// context in [`CarrierCompileCtxKey`], so the right identity is the **parse
/// site**, not the code object that happens to be running it.
///
/// [`Site`](Self::Site) is that identity: `CompiledCode::closure_body_arc`
/// builds one `Arc<Vec<Stmt>>` per `stmt_pool` slot and hands every later
/// instantiation of the same closure literal an `Arc` bump of it, so pointer
/// identity of that `Arc` *is* "same literal". The `Arc` is held, not just its
/// address, so the allocation cannot be freed and a later one reused at the
/// same address under a stale entry — the same soundness argument
/// [`MapGrepCacheKey`] makes.
///
/// This used to be a bare `SubData.id`, which is `next_instance_id()` — a
/// fresh number for every `Sub` *value*. A block instantiated more than once
/// from one literal therefore never hit: `body-blob`'s
/// `Promise(supply { whenever … })` builds a new supply block per call, so it
/// re-compiled three chunks on every call and left three permanently
/// unreachable cache entries behind (#7667).
///
/// [`Id`](Self::Id) keeps that behaviour for the call sites that legitimately
/// have their own stable identity to key on — the regex code-block and `s///`
/// replacement paths, whose ids name a parsed node rather than a value.
#[derive(Clone)]
pub(crate) enum CarrierCacheKey {
    Site(Arc<Vec<crate::ast::Stmt>>),
    Id(u64),
}

impl PartialEq for CarrierCacheKey {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Site(a), Self::Site(b)) => Arc::ptr_eq(a, b),
            (Self::Id(a), Self::Id(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for CarrierCacheKey {}

impl std::hash::Hash for CarrierCacheKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Site(a) => {
                0u8.hash(state);
                (Arc::as_ptr(a) as *const u8 as usize).hash(state);
            }
            Self::Id(a) => {
                1u8.hash(state);
                a.hash(state);
            }
        }
    }
}

/// Per-parse-site cache of `(context, compiled)` pairs for
/// `eval_block_value_inner`'s carrier-block compile (see
/// `todo/deep/eval-block-value-recompiles-every-call.md`). A `Vec` rather
/// than a nested `HashMap` because `CarrierCompileCtxKey` cannot implement
/// `Hash`/`Eq` (it embeds a `Value`, compared by Raku's semantic `PartialEq`,
/// not a total order) — and because the realistic size is 1 entry per site
/// (the same block invoked from the same call site every time), so a linear
/// scan against `CarrierCompileCtxKey::eq` costs nothing. Capped at
/// `CARRIER_COMPILE_CACHE_MAX_CONTEXTS_PER_ID` entries per key to bound memory
/// for the rare block invoked from many distinct contexts.
type CarrierCompileCache =
    HashMap<CarrierCacheKey, Vec<(CarrierCompileCtxKey, Arc<CompiledCode>, Arc<CompiledFns>)>>;

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

// Keep declarations in the runtime root namespace while splitting the module
// index by family. Add new modules to the matching file below.
include!("modules_accessors_builtins.rs");
include!("modules_calls_nqp.rs");
include!("modules_dispatch_io.rs");
include!("modules_methods_class.rs");
include!("modules_methods_instance.rs");
include!("modules_methods_object.rs");
include!("modules_native.rs");
include!("modules_async_regex.rs");
include!("modules_registration.rs");
include!("modules_state_run.rs");
include!("modules_supply_system.rs");
include!("modules_utilities.rs");
pub(crate) use self::any_cool_method_gate::cool_method_not_found as cool_method_not_found_on_any;
pub(crate) use self::locals::Locals;
pub(crate) use self::methods_subscript_protocol::refuse_map_removal;
pub(crate) use self::output_sink::OutputSink;
pub(crate) use self::regex_types::*;
pub(crate) use self::registration_class::{ClassDeclModifiers, HoistedShell};
pub(crate) use self::registry::Registry;
pub(crate) use self::scope_stack::ScopeStack;
pub(crate) use self::tap_state::TapState;
pub(crate) use crate::value::regex_caps::MatchTarget;
pub(crate) use crate::value::regex_caps::{NamedCaptureMap, NamedSlot};

pub(crate) use utils::*;

// Re-export thread utility functions for VM access
pub(crate) use methods_collection_ops::{current_mutsu_thread_id, current_thread_value};
pub(crate) use methods_raku_dispatch::container_needs_raku_dispatch;

use self::unicode::check_unicode_property;
use crate::value::ValueMap;

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
    /// Lexicals visible where a role attribute default was declared. A role's
    /// default is copied onto each consuming class, but its compiled chunk must
    /// still resolve imported names in the role's own scope.
    pub(crate) captured_env: Option<ValueMap>,
    /// Compunit whose imports were visible where a role attribute default was
    /// declared. Composed defaults run during construction, after the role's
    /// compunit has finished loading, so they must restore that visibility.
    pub(crate) captured_unit: Option<crate::symbol::Symbol>,
    /// The package this declaration was WRITTEN in -- the class or role body it
    /// appears in, which is NOT necessarily the class being constructed. A
    /// subclass inherits the declaration together with the scope its initializer
    /// has to resolve names in: `has NcplaneHandle $!plane` in a base class
    /// carries the synthesized default `BareWord("NcplaneHandle")`, and that
    /// bareword only resolves through the *declaring* package's import aliases
    /// (`package_type_aliases`) -- anchoring it on the constructed subclass
    /// instead let it degrade to the plain string `"NcplaneHandle"` (#8842).
    pub(crate) declaring_package: Option<crate::symbol::Symbol>,
    pub(crate) is_rw: bool,
    pub(crate) is_required: Option<Option<String>>,
    pub(crate) sigil: char,
    /// The declaration's own type constraint, kept with the sigil so a
    /// scalar and a container sharing a bare name do not borrow each other's
    /// constraint from the legacy name-keyed class metadata map.
    pub(crate) type_constraint: Option<String>,
    pub(crate) where_constraint: Option<crate::opcode::DeclTraitArg>,
    /// Declared shape dimensions for an `@`-sigil attribute (`has @.a[2]`),
    /// copied from `CompiledAttrDecl::declared_shape` at registration time
    /// (ADR-0019 D2c-4). `None` for a non-plan-backed construction site
    /// (`.^add_attribute`, builtin `Proc` attributes) — none of those are
    /// ever compiler-generated shaped-array defaults.
    pub(crate) declared_shape: Option<Vec<usize>>,
    /// `Code.line`/`Code.file` for the auto-generated accessor method this
    /// attribute produces (`instance_accessor_method_object`), mirroring
    /// `MethodDef::source_file`/`compiled_code.source_line` for a
    /// user-declared method. `None` when the declaration line was not
    /// tracked at registration time (a non-plan-backed construction site, or
    /// a mainline/EVAL `has`) -- the accessor then reports `Nil`, matching a
    /// synthetic method with no declaration site to point at.
    ///
    /// Interned rather than an owned `String`: the attribute list is cloned
    /// per construction on the bless path, and a `String` here cost one heap
    /// allocation per attribute per clone (#10090).
    pub(crate) source_line: Option<i64>,
    pub(crate) source_file: Option<crate::symbol::Symbol>,
    /// `default` is the seed the parser synthesizes for a typed scalar with
    /// no initializer (`has Int $.x`), not one the source wrote: construction
    /// stores it as the slot's seed, which `nqp::attrinited` reports as not
    /// initialized (ADR-0121 D4).
    pub(crate) default_is_seed: bool,
}

/// Attribute declarations with the same bare name but different sigils are
/// distinct Raku attributes. Most instance maps can continue to use the bare
/// name, but a colliding declaration needs a sigil-qualified key so its value
/// does not overwrite the other declaration's slot.
pub(crate) fn attribute_storage_key(
    class_attrs: &[ClassAttributeDef],
    name: &str,
    sigil: char,
) -> crate::symbol::Symbol {
    let collides = class_attrs
        .iter()
        .any(|attr| attr.name == name && attr.sigil != sigil);
    if collides {
        crate::symbol::Symbol::intern(&format!("{sigil}{name}"))
    } else {
        crate::symbol::Symbol::intern(name)
    }
}

/// Return whether adding this declaration would create a second public
/// accessor with the same bare name. Attribute identity includes the sigil,
/// but autogenerated accessor methods do not, so private `$!x` and `%!x` may
/// coexist while public `@.x` and `&.x` must still be rejected.
pub(crate) fn attribute_accessor_conflicts(
    class_attrs: &[ClassAttributeDef],
    name: &str,
    sigil: char,
    is_public: bool,
) -> bool {
    is_public
        && class_attrs
            .iter()
            .any(|attr| attr.name == name && attr.sigil != sigil && attr.is_public)
}

/// Return the type constraint belonging to one declaration. The historical
/// `attribute_types` map is keyed only by bare name, so it cannot distinguish
/// `$!value` from `%!value` when both are declared. In that case an untyped
/// declaration must not inherit the sibling declaration's constraint.
pub(crate) fn attribute_type_constraint(
    class_attrs: &[ClassAttributeDef],
    attr: &ClassAttributeDef,
    type_constraints: &std::collections::HashMap<String, String>,
) -> Option<String> {
    let collides = class_attrs
        .iter()
        .any(|other| other.name == attr.name && other.sigil != attr.sigil);
    if collides {
        return attr.type_constraint.clone();
    }
    type_constraints
        .get(&attr.name)
        .cloned()
        .or_else(|| attr.type_constraint.clone())
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

    /// Every marked name with its kind, in no particular order.
    // Cost: O(r), r = marked names.
    pub(crate) fn iter(&self) -> impl Iterator<Item = (Symbol, ReadonlyKind)> + '_ {
        self.map.iter().map(|(sym, kind)| (*sym, *kind))
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

/// One entry of `Interpreter::method_class_stack`: the class (or role) that
/// owns the running method body.
///
/// Held as a [`Symbol`] rather than a `String`: every method call pushed an
/// owned copy of the owner's name (one malloc and free per call), and every
/// `$!x` read re-derived what it needed from that string -- the qualified
/// private key was interned per access and "is the owner a role?" was a
/// registry lookup per access (ADR-0121 D1).
pub(crate) struct MethodClassFrame {
    pub(crate) name: Symbol,
    /// Memoized `is_role(name)`: 0 = not asked yet, 1 = no, 2 = yes. A name's
    /// role-ness cannot change while a method it owns is running, so the first
    /// attribute access of the frame answers it for the rest of the frame.
    is_role: std::cell::Cell<u8>,
}

impl MethodClassFrame {
    pub(crate) fn new(name: Symbol, is_role: Option<bool>) -> Self {
        let memo = match is_role {
            None => 0,
            Some(false) => 1,
            Some(true) => 2,
        };
        Self {
            name,
            is_role: std::cell::Cell::new(memo),
        }
    }
}

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
    pub(crate) eligible: bool,
    /// Not `eligible` only because the class (or an ancestor) declares a
    /// user `new`: when no such candidate accepts a call's arguments, the
    /// call falls back to the default constructor, which the native builder
    /// then serves exactly as for an `eligible` class.
    pub(crate) eligible_when_user_new_declines: bool,
    /// Memo of `user_new_declines` for a call with NO arguments, whose answer
    /// is a function of the class shape alone -- exactly what this plan is
    /// dropped on (`native_ctor_plan_cache` is cleared at every class-shape
    /// mutation and generation bump), so the memo cannot outlive it.
    pub(crate) noarg_user_new_declines: std::sync::OnceLock<bool>,
    /// Per attribute (same order as `class_attrs`): the value its seed
    /// default (`default_is_seed`, the declared type's type object for a
    /// `has Str $.x` with no initializer) evaluated to, once it has evaluated
    /// to a type object that passed the attribute's type check. A type name
    /// resolves the same way in the declaring scope every time and a type
    /// object is immutable, so the memo is exact for as long as this plan
    /// lives (it is dropped at every class-shape mutation).
    pub(crate) seed_defaults: Box<[std::sync::OnceLock<Value>]>,
    pub(crate) class_attrs: Arc<Vec<ClassAttributeDef>>,
    /// Interned attribute names, same order as `class_attrs`. Construction
    /// inserts attributes by Symbol so the per-bless per-attribute
    /// `String` clone + re-intern is paid once per class, not per instance.
    pub(crate) attr_syms: Arc<Vec<crate::symbol::Symbol>>,
    pub(crate) type_constraints: Arc<HashMap<String, String>>,
    pub(crate) has_build: bool,
    pub(crate) has_tweak: bool,
    pub(crate) has_smiley: bool,
    /// True when some attribute is typed with a user `subset`, whose predicate
    /// is checked at construction (defaults included) like a `where` clause.
    pub(crate) has_subset_attr: bool,
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
    /// Attribute name -> index into `class_attrs` / `attr_syms` / `attr_seeds`.
    ///
    /// `dispatch_bless` used to answer "does this named argument name a
    /// declared attribute?" with a linear `class_attrs.iter().position(...)`
    /// string scan PER ARGUMENT — on the `Zef::Distribution` shape that is 7
    /// args x 21 attributes = ~147 `memcmp`s per construction (`bench-ctor`
    /// profile: `__memcmp_avx2_movbe` at 1.7%). The map answers it with one
    /// hash, and the answer is pure class shape.
    pub(crate) attr_index: Arc<rustc_hash::FxHashMap<Box<str>, u32>>,
    /// The value a no-initializer attribute seeds with, one per `class_attrs`
    /// entry. Re-deriving it per construction meant a `type_constraints` hash
    /// lookup plus — for the overwhelmingly common untyped/class-typed `$`
    /// attribute — a `nominal_type_object_name_for_constraint` walk and a
    /// `Symbol::intern` of the resulting type name, on EVERY attribute of
    /// EVERY construction (16 interns per `bench-ctor` construction, each a
    /// thread-local + string-hash round trip). It is pure class shape.
    pub(crate) attr_seeds: Arc<Vec<AttrSeed>>,
    /// Each attribute's effective type constraint, one per `class_attrs`
    /// entry: [`attribute_type_constraint`] resolved once per class. Asking it
    /// per construction scanned every attribute for a sigil collision and
    /// cloned the constraint `String`, for every attribute, three times per
    /// `.new` -- O(n^2) in the attribute count, for an answer that is pure
    /// class shape.
    pub(crate) attr_constraints: Arc<[Option<String>]>,
    /// Whether the default constructor binds a named argument to each
    /// attribute (`is_attribute_buildable`), one per `class_attrs` entry.
    /// Pure class shape; re-derived per named argument it was a registry
    /// probe plus an attribute scan per MRO level.
    pub(crate) attr_buildable: Arc<[bool]>,
    /// Which user-defined whole-object build hook this class's MRO declares —
    /// `Some("BUILDALL")`, `Some("POPULATE")`, or `None` (the overwhelmingly
    /// common case). `run_user_buildall_hook` probed this per construction with
    /// an MRO walk x 2 method names of `user_method_overloads` lookups, each
    /// interning both names; the answer is pure class shape, so it belongs in
    /// the plan next to `has_build`/`has_tweak`/`has_custom_bless`.
    pub(crate) user_buildall: Option<&'static str>,
    /// The attribute map a `CREATE` / `nqp::create` of this class starts
    /// with: every declared attribute under its bare name, holding its
    /// type-default empty value (see `create_default_attr_slots`). It is pure
    /// class shape, and `nqp::create` rebuilt it on every call -- an MRO walk
    /// collecting the attribute defs plus a type-constraint resolution per
    /// attribute (ADR-0121 D1, #9134).
    pub(crate) create_slots: Arc<crate::value::AttrMap>,
    /// The slot layout of this class's instances (ADR-0121 D2): one slot per
    /// `attr_syms` key, in the same order. Every construction path that works
    /// from this plan lays its instance out by it.
    pub(crate) layout: Arc<crate::value::ClassLayout>,
    /// The `has $x` (no twigil) attribute names across the MRO, whose alias
    /// metadata a construction adds (`add_alias_attribute_metadata`). Walking
    /// the MRO for them on every construction cost ~400 instructions of each
    /// `.new` (#9291), almost always to find none.
    pub(crate) alias_attributes: Arc<[String]>,
}

impl NativeCtorPlan {
    /// Whether constructing the class evaluates no declaration expression
    /// any more: every initializer is absent, a literal, or a seed default
    /// already memoized in `seed_defaults`, and no attribute has a `where`.
    /// Such a construction neither reads nor temporarily rebinds the caller's
    /// env (see `try_ctor_lane`).
    // Cost: O(a), a = attributes.
    pub(crate) fn evaluates_no_decl_expr(&self) -> bool {
        self.class_attrs.iter().enumerate().all(|(i, a)| {
            a.where_constraint.is_none()
                && match &a.default {
                    None | Some(crate::opcode::DeclTraitArg::Literal(_)) => true,
                    Some(_) => {
                        a.default_is_seed
                            && self.seed_defaults.get(i).is_some_and(|c| c.get().is_some())
                    }
                }
        })
    }
}

/// The no-initializer seed of one `$`-sigil attribute, precomputed per class
/// (see `NativeCtorPlan::attr_seeds`). `@`/`%` attributes seed an empty
/// container (or an `is Type` one) instead and carry [`AttrSeed::Container`].
#[derive(Clone, Copy)]
pub(crate) enum AttrSeed {
    /// A native integer attribute (`int`, `uint8`, `byte`, `atomicint`, ...).
    NativeInt,
    /// A native float attribute (`num`, `num32`, `num64`).
    NativeNum,
    /// A native string attribute (`str`).
    NativeStr,
    /// A non-native `$` attribute: its nominal type object (`Any` when
    /// untyped, `Int` for `has Int $.x`, the subset's base for a subset, ...).
    TypeObject(crate::symbol::Symbol),
    /// An `@`/`%` attribute — seeded by the container logic, not from here.
    Container,
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

pub(crate) use crate::decl_doc::{DocComment, DocDeclKind};

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
    /// When set, the handle is closed automatically the moment a line or word
    /// read reaches EOF (Raku's `words($fh, :close)` close-on-exhaust
    /// semantics, and the handle `IO::Path.lines` / `.words` open).
    close_on_exhaust: bool,
    /// The buffered reader of a handle only a Seq can reach (the one
    /// `IO::Path.lines` / `.words` open); `file` is `None` then. File reads go
    /// through it instead of one syscall per byte (see `handle_seq_reader`).
    seq_reader: Option<handle_seq_reader::SeqFileReader>,
}

/// Entry in the callframe stack, tracking state for each call frame.
#[derive(Clone)]
pub(crate) struct CallFrameEntry {
    pub file: String,
    pub line: i64,
    pub code: Option<CodeFrame>,
    pub env: Env,
    /// Package the frame's code was running in, for `callframe(N).my<::?PACKAGE>`.
    pub package: Symbol,
    /// The routine-stack top when the entry was pushed, i.e. the frame the call
    /// was made from. Only a *named-routine* frame carries a lazily built
    /// `code`; a method or the mainline does not, and this is what lets
    /// `callframe(N).code.name` report `bar` / `<unit>` for them.
    pub routine: Option<RoutineFrame>,
    /// True for the synthetic EVAL frames, which have no code object at all.
    pub synthetic: bool,
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
    /// Whether this block frame is an *inlined* bare block (a statement-level
    /// `{ ... }` run in place), not a code object that was called. Rakudo
    /// inlines such a block, so it is no frame of its own for a `{*}`'s
    /// `X::NoDispatcher` name (#10786); a backtrace still shows it.
    pub is_inlined_block: bool,
    /// Whether this routine carries `is hidden-from-backtrace`.
    pub is_hidden_from_backtrace: bool,
    /// The file this routine's BODY lives in (None = same as the caller /
    /// main script). `line`/`file` above record the call-site; a backtrace
    /// displays each frame at its defining file (module subs report the
    /// module path, integration/error-reporting.t test 15).
    pub def_file: Option<Symbol>,
    /// Monotonic id of THIS invocation. Distinguishes one call of a routine
    /// from the next, which is what a per-call anonymous state (`$++` inside a
    /// block inside a routine) keys on — see `Interpreter::anon_state_key`.
    pub invocation_id: u64,
    /// The `__mutsu_callable_id` a non-local `return` stamps when it targets
    /// this frame, for a frame whose id is not its routine's registration id:
    /// a method invocation binds a fresh id per call. `0` = none recorded (the
    /// registration id identifies the frame). Read by
    /// `Interpreter::return_target_is_live`.
    pub callable_id: u64,
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
    /// The declaring *compunit*, the compunit-scoping counterpart of
    /// [`package`](Self::package) (#7837). END bodies run at program exit,
    /// when `current_unit` is back to the main script's — so a phaser
    /// declared in a module whose qualified self-reference the #7797
    /// visibility gate checks (`Log::Async.instance` inside
    /// `Log/Async.rakumod`'s own `END`) would otherwise be judged against a
    /// compunit that never `use`d it. That is not hypothetical for a module
    /// pulled in by an `EVAL "use ..."` (`Test.rakumod`'s `use-ok`), where
    /// no compunit on the exit-time chain ever named it.
    pub(crate) unit: Symbol,
    /// Keys whose declaring scope has since died. At exit the captured value is
    /// the only surviving one, so it must win over a live same-named variable
    /// in an enclosing scope — `{ my $a = 42; END { say $a } }` prints 42 even
    /// when an outer `my $a = 1` is what the exit-time env holds. Every other
    /// captured key names a variable that is *still alive*, so the live value
    /// wins and a later mutation is visible, as it is in Raku.
    pub(crate) dead_keys: NameSet,
    /// Install order, which is what decides the exit-time run order (END
    /// phasers run in reverse of it). It is NOT the registration order: mutsu
    /// installs every one of the main compunit's ENDs *eagerly*, before the
    /// body runs, so a `use` on line 1 registers the module's END after them
    /// even though rakudo installs it first. See [`end_order`].
    pub(crate) order: u64,
    /// Mark of the moment [`env`](Self::env) was captured, drawn from
    /// `Interpreter::end_phaser_capture_seq`, or `None` for a phaser whose
    /// declaration execution never reached.
    ///
    /// Every main-compunit END is *installed* before the body runs (rakudo
    /// installs at compile time, so an END in a never-entered block still runs
    /// at exit), which means "was this phaser registered inside the scope that
    /// is now dying" can no longer be answered by comparing positions in
    /// the `end_phasers` vector. This mark answers it instead: a scope records the
    /// capture counter on entry and `update_end_phaser_envs` freezes exactly
    /// the phasers that captured at or after it.
    pub(crate) capture_seq: Option<u64>,
}

/// Install-order bases for [`EndPhaser::order`]; lowest = installed earliest =
/// run last.
///
/// rakudo installs an END phaser when the compunit that declares it is
/// *compiled*, so `use M` on line 1 installs `M`'s ENDs before any of the
/// script's own, and the LIFO run order then puts the script's first. mutsu
/// loads modules at run time but installs all of the main compunit's ENDs
/// before its body (`runtime::end_phasers`, so one still runs when the body
/// dies or never reaches it), which reverses the two. Sorting by these bases
/// at exit restores rakudo's order without giving up the eager installation.
pub(crate) mod end_order {
    /// A module's ENDs, in load order — a nested `use` installs the inner
    /// module's first, exactly as rakudo does.
    pub(crate) const MODULE: u64 = 0;
    /// The main compunit's ENDs, keyed by SOURCE POSITION — a top-level one
    /// and one inside a block or a sub share this class, because rakudo
    /// installs both as its compiler walks past them. Ordering them by
    /// registration instead put every top-level END (mutsu installs those
    /// first) ahead of every block-scoped one, so `{ END {…} } END {…}` ran
    /// the block's first where rakudo runs the mainline's first.
    pub(crate) const MAIN: u64 = 1 << 40;
    /// ENDs registered from inside an `EVAL`. rakudo compiles an EVAL'd snippet
    /// at RUN time, so its ENDs install after everything the main compunit
    /// declared and run before them — the opposite of a plain `use`
    /// (`File::Temp`'s `03-tempfile.rakutest` turns on exactly this).
    pub(crate) const RUNTIME: u64 = 2 << 40;

    /// Position of one END within its class. A main-compunit END is keyed by
    /// the source-order index the parser handed its declaration
    /// (`ast::Stmt::Phaser::end_index`), which is exactly the order rakudo's
    /// compiler installs them in — including several ENDs on one physical
    /// line, which a source-LINE key could only tie. A module's or an EVAL's
    /// END has no position in the main compunit's numbering and is keyed by
    /// the monotonic registration sequence instead, which for those two
    /// classes *is* the install order (load order, and EVAL-execution order).
    pub(crate) fn slot(end_index: Option<u32>, seq: u64) -> u64 {
        match end_index {
            Some(index) => index as u64,
            None => seq,
        }
    }
}

/// The package chain a bare name is looked up in, innermost first — the value
/// [`Interpreter::bare_name_packages_syms`] hands out and
/// [`ResolutionCaches::bare_name_packages_memo`](crate::runtime::resolution_caches::ResolutionCaches::bare_name_packages_memo) stores.
///
/// Shared behind an `Arc` because the callers that need it most are `&self`
/// probes that then call back into `&mut self` resolution, so lending a
/// borrow out of the memo is not an option; an `Arc` clone is a refcount bump
/// where the old `Vec<String>` was one allocation per enclosing package.
pub(crate) type BareNamePackages = std::sync::Arc<[Symbol]>;

/// [`ResolutionCaches::bare_name_packages_memo`](crate::runtime::resolution_caches::ResolutionCaches::bare_name_packages_memo)'s table: the `(current package,
/// innermost lexical package)` pair a search list is derived from, to that
/// list.
pub(crate) type BareNamePackagesMemo =
    Box<std::cell::RefCell<rustc_hash::FxHashMap<(Symbol, Option<Symbol>), BareNamePackages>>>;

/// Key of [`ResolutionCaches::multi_compiled_key_cache`](crate::runtime::resolution_caches::ResolutionCaches::multi_compiled_key_cache): everything
/// `find_compiled_function_inner`'s probe chain reads for a bare `multi` name.
///
/// `pkg` and `lexical_pkg` are the two inputs `bare_name_packages()` derives its
/// search list from, so a hit can never answer for the wrong package scope
/// (the same pair `has_proto`'s memo keys on). `arity`/`pos_arity` are both
/// present because the probe chain builds keys from each, and two calls sharing
/// a type signature can still differ in how many of their arguments are
/// string-keyed `Pair`s. `fingerprint` is the resolved winner's body
/// fingerprint, which every probe filters on. Names containing `::` are never
/// cached here — their probe chain additionally consults `env` for prefix
/// visibility, which this key does not capture.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct MultiCompiledKey {
    pub(crate) name: Symbol,
    pub(crate) pkg: Symbol,
    pub(crate) lexical_pkg: Option<Symbol>,
    pub(crate) arity: usize,
    pub(crate) pos_arity: usize,
    pub(crate) fingerprint: u64,
    pub(crate) type_sig: Vec<&'static str>,
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

/// Copy-on-write write access to one of [`Interpreter`]'s `Arc<...>`
/// program-table fields.
///
/// A thin wrapper over [`std::sync::Arc::make_mut`] that counts the copies it
/// causes, mirroring what `RegistryWriteGuard::deref_mut` does for the
/// declaration registry. The copy happens only while a thread clone still
/// shares the table, so the counter (`program-table-cow: clones=` under
/// `MUTSU_VM_STATS`) answers "is a spawn-heavy loop re-copying a table every
/// iteration?" -- see the note on [`Interpreter`] for why these tables are
/// shared at all.
#[inline]
pub(crate) fn cow_table_mut<T: Clone>(table: &mut std::sync::Arc<T>) -> &mut T {
    if std::sync::Arc::strong_count(table) > 1 {
        crate::vm::vm_stats::record_program_table_cow_clone();
    }
    std::sync::Arc::make_mut(table)
}

impl Interpreter {
    /// The single mutation funnel for `unit_lexicals`, bumping
    /// [`LexicalState::unit_lexical_gen`](crate::runtime::lexical_state::LexicalState::unit_lexical_gen) so caches keyed on it invalidate.
    /// Every `cow_table_mut(&mut self.lexicals.unit_lexicals)` goes through here.
    #[inline]
    pub(crate) fn unit_lexicals_cow_mut(&mut self) -> &mut PackageLexicals {
        self.lexicals.unit_lexical_gen = self.lexicals.unit_lexical_gen.wrapping_add(1);
        cow_table_mut(&mut self.lexicals.unit_lexicals)
    }

    /// The mutation funnel for `package_lexicals`. It bumps the same
    /// [`LexicalState::unit_lexical_gen`](crate::runtime::lexical_state::LexicalState::unit_lexical_gen), because TRIR's free-variable cache
    /// also holds bindings read out of this table (a `package P { my $x }`
    /// lexical, which is often a plain value rather than a cell). Writing
    /// through a cell already in the table needs no bump, as for
    /// `unit_lexicals`.
    #[inline]
    pub(crate) fn package_lexicals_cow_mut(&mut self) -> &mut PackageLexicals {
        self.lexicals.unit_lexical_gen = self.lexicals.unit_lexical_gen.wrapping_add(1);
        cow_table_mut(&mut self.lexicals.package_lexicals)
    }
}

/// The interpreter.
///
/// **On the `Arc<...>` collection fields.** A large group of this struct's
/// symbol tables -- loaded modules, exported names, per-package lexicals, class
/// and distribution bookkeeping -- are held as `std::sync::Arc<HashMap<...>>`
/// rather than owning the map directly. That is a copy-on-write share, not an
/// ownership subtlety: reads go through `Deref` and are unchanged, while every
/// write goes through `std::sync::Arc::make_mut`, which clones the map only
/// when someone else still holds it.
///
/// The someone else is `clone_for_thread_excluding`. Every thread clone -- a
/// `start` block, a `.then`, a `Promise` chained onto a supply, and above all a
/// `whenever` registration -- used to deep-copy each of those tables, so the cost
/// of spawning grew with the size of the *program* rather than the work. Sharing
/// them makes a spawn a handful of refcount bumps, and a table that neither side
/// writes afterwards is never copied at all.
///
/// The semantics are identical either way: a thread clone that writes one of
/// these tables still gets its own copy of it (that is what `make_mut` does), so
/// its declarations do not leak back to the parent. Only the *timing* of the
/// copy moved -- from every spawn, to the first write after a spawn.
///
/// The same share has a second holder, for the same reason: a **scope snapshot**
/// taken to make a declaration lexical. `snapshot_routine_registry` (every
/// routine declaring inner `my sub`s) and `eval_block_value_inner` (every
/// carrier block) save the registry's routine tables on entry and restore them
/// on exit, and those three tables -- `Registry::functions`,
/// `Registry::proto_functions`, `Registry::proto_subs` -- are in this group
/// too. The reasoning carries over unchanged: the snapshot is refcount bumps,
/// the copy happens on the scope's first declaration, and the overwhelmingly
/// common scope that declares nothing never copies at all (#7887).
///
/// When adding a field here, put it in this group if it is a program-global
/// table that is written during declaration/module loading and read everywhere
/// else. Do NOT if it is per-call or per-frame state that a hot path mutates:
/// `make_mut` under an active share would then copy the table on every write.
pub struct Interpreter {
    env: Env,
    /// Body fingerprints (see [`crate::ast::function_body_fingerprint`]) of MAIN
    /// candidates declared `is hidden-from-USAGE`. Such a candidate is skipped
    /// when generating the usage message (but still participates in dispatch).
    main_hidden_from_usage: std::sync::Arc<std::collections::HashSet<u64>>,
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
    /// Name of the package currently in scope (e.g. `GLOBAL`, `Foo::Bar`),
    /// used to build fully-qualified names during function/method dispatch and
    /// declaration, held as its interned `Symbol` id. A relaxed atomic (not a
    /// `Cell`) so the `&self` regex matcher can switch it
    /// (`set_current_package_shared_sym`); snapshot-copied per thread (see
    /// `clone_for_thread`). It used to be an `Arc<RwLock<String>>` with this
    /// atomic as a mirror, which made every package switch and read allocate.
    current_package_sym: Arc<AtomicU32>,
    routine_stack: routine_stack::RoutineStack,
    callframe_stack: Vec<CallFrameEntry>,
    pending_call_arg_sources: Option<Vec<Option<String>>>,
    /// An exception thrown while *evaluating a `where` constraint* during
    /// candidate matching. Raku propagates such an exception out of the whole
    /// dispatch (the `where` block is ordinary code); mutsu's matchers are
    /// `bool`-returning predicates, so they cannot return it directly. They
    /// stash it here instead and the dispatch funnels
    /// (`resolve_function_with_types`, `choose_best_matching_candidate`) stop
    /// scanning; `Interpreter::exec_one` is the backstop that turns a stash no
    /// funnel drained into the error it always was, so it can never be silently
    /// dropped. Only genuine exceptions are recorded -- a control-flow signal
    /// (`return`/`next`/...) is not an exception and still reads as "no match".
    pub(crate) pending_where_exception: Option<Box<RuntimeError>>,
    /// Set right before binding the winning candidate of a value-dependent
    /// `multi` (one carrying a `where` clause or a user-subset-typed
    /// parameter) whose resolution already ran fresh, against these exact
    /// arguments: by `dispatch_func_call_inner` for a sub
    /// ([#8697](https://github.com/tokuhirom/mutsu/issues/8697)), and by
    /// `compiled_mut_resolved_dispatch` → `call_compiled_method` for a method
    /// ([#10986](https://github.com/tokuhirom/mutsu/issues/10986)).
    /// `bind_function_args_values_inner` takes (clears) it at entry into a
    /// local, so a positional parameter's `where` post-constraint and subset
    /// predicate checks can skip re-evaluating a predicate resolution already
    /// proved true for this call, instead of running the user's code a second
    /// time. Any nested call the bind or the routine body itself makes reads
    /// it as `false` again, since the flag is consumed before either runs.
    pub(crate) pending_skip_constraint_recheck: bool,
    /// ADR-0067 slice 3b: the caller's container for the invocant of the method
    /// call currently being dispatched, staged by
    /// `Interpreter::arm_raw_invocant_arrival` and consumed by whichever of the
    /// two compiled-method binders runs. Always `None` outside the window
    /// between one method-call opcode's arm and its matching disarm.
    pub(crate) pending_raw_invocant:
        Option<Box<crate::vm::vm_raw_invocant_arrival::PendingRawInvocant>>,
    /// Companion to `pending_call_arg_sources` (§1.4/§1.5): the compiler-baked
    /// `arg-source name -> caller local slot` for the current call, decoded from the
    /// `Pair(name, Int(slot))` arg-source entries. Set alongside the names by
    /// `decode_arg_sources`, taken with them by `bind_function_args_values`.
    pub(crate) pending_call_arg_source_slots: std::collections::HashMap<String, u32>,
    /// Bitmask of the CURRENT call's argument positions that were written as a
    /// literal, published by `exec_call_func_op` from the call opcode's
    /// `literal_native_args` and restored when that call returns. Multi
    /// dispatch reads it in `unwrap_varref_for_dispatch` to give a literal the
    /// native `var_type` a source variable would have carried, so
    /// `multi d(int)` / `multi d(Int)` called as `d(5)` picks `int` as rakudo
    /// does. Zero for every call site with no literal argument, which is the
    /// common case and costs one `u32` store per call.
    pub(crate) literal_native_args: u32,
    /// The CURRENT call site's `OpCode::CallFunc::static_arg_types`, published
    /// by `exec_call_func_op` and restored when that call returns. The callee's
    /// binder TAKES it (resetting it to `false`, so a call its body makes
    /// through any other route never inherits it) and reports a binding
    /// failure as the compile-time `X::TypeCheck::Argument` only when it was
    /// set (#10640; see `Interpreter::enhance_binding_error`).
    pub(crate) static_call_args: bool,
    /// `rw-arg writeback source name -> caller local slot`, captured at arg-binding
    /// time (clobber-safe: before the callee body runs) from
    /// `pending_call_arg_source_slots`. The value also records the call-frame depth
    /// that owns the slot. The rw writeback drain (`apply_pending_rw_writeback`)
    /// prefers this slot over the by-name resolution only at that depth, so a
    /// nested frame with a same-named local cannot consume the pending writeback.
    pub(crate) pending_rw_writeback_slots: rw_writeback_slots::RwWritebackSlots,
    /// The callsite line a pending test assertion should report, set by every
    /// call dispatch and cleared by `exec_nqp_op`.
    ///
    /// `pub(crate)` only so `vm_jit_layout` can take its `offset_of!`: the Tier
    /// B `nqp::*_i` fast path clears it with one inline store rather than
    /// letting a JIT-compiled nqp op leave a stale line behind. Everything else
    /// still goes through `set_pending_callsite_line`.
    pub(crate) test_pending_callsite_line: Option<i64>,
    /// Operand buffer reused by every `OpCode::NqpOp` execution.
    ///
    /// An nqp op's operand list is statically shaped and dies with the op, so
    /// it needs a buffer, not an allocation: the old `CallFunc` path built up
    /// to four `Vec`s per op (drain, `VarRef` unwrap, callsite-line sanitize,
    /// `Proxy` fetch) for ops as small as `nqp::add_i`. An op that re-enters
    /// the VM (`nqp::atkey` reaching an `AT-KEY` method) finds this empty and
    /// allocates its own, which is simply dropped when the outer op restores
    /// its buffer — nesting costs an allocation, it does not corrupt anything.
    pub(crate) nqp_arg_scratch: Vec<Value>,
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
    /// Recycled *argument* buffers for the named/spec light call paths, which
    /// need a contiguous `Vec<Value>` of the drained arguments rather than a
    /// frame's slot array. These used to borrow the locals pool, which conflated
    /// two different things: ADR-0077 makes `Locals` a window into one
    /// contiguous stack, and a window cannot be handed out as an owned buffer.
    /// Bounded, and cleared before being returned to the pool.
    pub(crate) args_scratch_pool: Vec<Vec<Value>>,
    block_stack: Vec<CodeFrame>,
    block_scope_depth: usize,
    /// A sigilless name (`my \foo = Obj.new`) whose assignment `CheckReadOnly`
    /// let through because the bound object has a user `STORE`: the store that
    /// follows routes through `STORE` instead of rebinding the name (#9551).
    pub(crate) pending_sigilless_store: Option<String>,
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
    /// State behind `nqp::getcomp("Raku")`'s compiler object and the
    /// `nqp::ctx` family: the persistent eval contexts a REPL keeps between
    /// `.eval` calls (ADR-0122, `runtime::repl_compiler`). Boxed: it is
    /// idle in almost every program, and `Interpreter` lives on the stack.
    pub(crate) repl_compiler: Box<repl_compiler::ReplCompilerState>,
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
    /// When true, rw routine calls should not auto-FETCH Proxy return values.
    pub(crate) in_lvalue_assignment: bool,
    /// When true, a bare block is evaluating the tail of an `is rw` routine
    /// and scalar reads must retain their storage container. This is set only
    /// around the indirect block call; ordinary block calls still
    /// decontainerize as usual.
    pub(crate) rw_return_context: bool,
    /// When set, `does` on a routine parameter inside `trait_mod:<is>` will
    /// store the resulting Mixin value for writeback to the outer scope.
    pub(crate) trait_mod_writeback_key: Option<String>,
    /// The captured Mixin value from a trait_mod `does` writeback: whichever
    /// `does`/`but` ran LAST while `trait_mod_writeback_key` was armed,
    /// regardless of its target. `apply_attribute_traits` reads this to
    /// detect and invoke a role's `compose` hook (AttrX::Lazy's shape mixes a
    /// role into `$class.HOW` specifically so this fires).
    pub(crate) trait_mod_writeback_value: Option<Value>,
    /// Like `trait_mod_writeback_value`, but captured ONLY when the `does`
    /// target is (or wraps) an Attribute instance — i.e. NOT a `$class.HOW`
    /// mixin, which persists through its own dedicated path
    /// (`eval_does_values`'s `how_target_from_value` branch writes
    /// `registry.class_how_values`). A handler that mixes into both `$attr`
    /// AND `$class.HOW` (AttrX::Lazy: `$attr does LazyAttribute; ...
    /// $class.HOW does LazyAttributeContainerHOW`) needs both captured
    /// separately: `trait_mod_writeback_value` for whichever ran last (to
    /// find the compose hook), this field for the attribute's OWN resulting
    /// value, cached as its `^attributes` meta-object. Conflating the two
    /// into one slot lost the attribute's own mixin whenever a HOW-mixin ran
    /// last, corrupting the cache a later re-registration of the class reuses
    /// (#8806).
    pub(crate) trait_mod_attr_writeback_value: Option<Value>,
    /// The value passed to CORE's `trait_mod:<is>(Attribute:D $attr, :$default!)`
    /// candidate (see `runtime::run::TRAIT_MOD_IS_DEFAULT_PRELUDE`), set by the
    /// native primitive `__mutsu_attribute_set_default` behind it
    /// (`vm::vm_trait_mod_does_ops::try_trait_mod_set_default`). A distribution's
    /// own custom attribute trait handler may re-dispatch to this builtin
    /// candidate as an ordinary function call to reuse `is default(...)`'s
    /// semantics on an `Attribute` object it already holds (ASN::BER's
    /// `DefaultValue` role: `trait_mod:<is>($attr, :default($v))` from inside its
    /// own `is default-value(...)` handler) — mutsu otherwise only recognizes
    /// `is default(...)` as parse-time sugar on a `has` declaration
    /// (`CompiledAttrDecl::is_default`), which a runtime call can't reach.
    /// `apply_class_body_attribute_traits` drains this after dispatching an
    /// attribute's custom traits and folds it into the attribute's compiled
    /// default, exactly as if `is default(...)` had been written directly.
    pub(crate) trait_mod_default_writeback: Option<Value>,
    /// When true, hash indexing with a missing key autovivifies (creates an
    /// empty Hash entry and returns it).  Set during reduce with `is raw`
    /// callbacks so that container semantics are preserved.
    pub(crate) hash_autovivify: bool,
    /// Stack of caller environments for $CALLER:: / $DYNAMIC:: resolution.
    /// Each entry is a snapshot of the env at the point a sub/function was called.
    caller_env_stack: Vec<Env>,
    /// Recursion guards for `.raku`/`.gist` renders of self-referencing
    /// structures (the `guards` subsystem, ADR-10779).
    pub(crate) raku_cycle_guards: raku_cycle_guards::RakuCycleGuards,
    /// Last expression value from VM execution, used by REPL for auto-display.
    pub(crate) last_value: Option<Value>,
    /// Pending env updates from regex code blocks, to be synced to VM locals.
    pub(crate) pending_local_updates: Vec<(String, Value)>,

    // === Merged VM execution registers (CP-3 collapse: the bytecode VM was
    // dissolved into the Interpreter; these were the per-execution fields of the
    // former `VM` struct). The Interpreter IS the bytecode VM now. ===
    pub(crate) stack: Vec<Value>,
    pub(crate) locals: Locals,
    /// Every live TRIR frame's slots and operand stacks (ADR-0110). One
    /// contiguous region per bank, like `Locals`: a call extends it, a return
    /// truncates it, and an `is rw` native parameter is an index into it.
    /// Its boxed halves are GC roots (`gc_roots.rs`).
    pub(crate) trir: crate::trir::frame::TrStacks,
    /// ADR-0110 §3.1: each TRIR chunk's free variables, resolved once and
    /// kept as the `unit_lexicals` CELLS they live in — so a write from
    /// anywhere is seen without re-resolving. Keyed by the chunk's address
    /// and validated against [`LexicalState::unit_lexical_gen`](crate::runtime::lexical_state::LexicalState::unit_lexical_gen) and the
    /// package the bindings were resolved under; resolving
    /// them per call cost three string-keyed hash lookups plus two
    /// thread-local interner hits, which is more than `nom-ws`'s whole body.
    /// Keyed by [`crate::trir::TrChunk::id`] — a monotonic counter, not the
    /// chunk's address, which the allocator may reuse after an `EVAL`'s
    /// compiled routines are dropped.
    pub(crate) trir_outer_cache:
        rustc_hash::FxHashMap<u64, (u64, crate::symbol::Symbol, Vec<Value>)>,
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
    pub(crate) call_frames: Vec<crate::vm::VmCallFrame>,
    /// Calls left before the next ADR-0100 native-stack headroom check.
    ///
    /// Reading the stack pointer at every call boundary cost ~4% on a
    /// call-dominated workload (`fib(32)`), because the address-of forces a
    /// stack slot and acts as an optimization barrier in the hottest
    /// functions mutsu has. Counting down an integer field instead keeps the
    /// hot path to a decrement and a predictable branch, and the real check
    /// runs once per [`crate::vm::vm_stack_guard::STACK_CHECK_INTERVAL`]
    /// calls -- which is why the guard's reserve has to absorb a whole
    /// interval's worth of frames. See `vm::vm_stack_guard`.
    pub(crate) stack_check_countdown: u32,
    /// The function table inline CATCH/CONTROL handler entries share while it
    /// is unchanged, keyed by its `CompiledFns::id`. See
    /// `Interpreter::shared_fns_snapshot`.
    pub(crate) handler_fns_snapshot: Option<(u64, std::sync::Arc<crate::opcode::CompiledFns>)>,
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
    /// `(code, ip)` of the numeric infix op (`+`, `==`, ...) the interpreter
    /// loop is executing -- `code` in the [`Self::current_code`] encoding --
    /// or `(0, 0)` outside one. Saved, set and restored around that op by
    /// `exec_one_dispatch`, so a JIT shim (which bypasses the loop) never sees
    /// a site of its own. Only read on the cold path, to name the variable in
    /// the "Use of uninitialized value $x ... in numeric context" warning
    /// (#9359); the reader checks `code` against `current_code`, so code a
    /// nested call runs meanwhile cannot misread it.
    pub(crate) numeric_op_site: (usize, usize),
    /// When `Some`, a *carrier* (EVAL / interpreter fallback) is running and
    /// every by-name env write through `set_env_with_main_alias` logs its name
    /// here. On carrier return, exactly these names are written back into the
    /// caller's slots (`writeback_carrier_writes`). See docs/vm-single-store.md
    /// Slice B.
    pub(crate) carrier_writes: Option<std::collections::HashSet<String>>,
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
    #[cfg(feature = "jit")]
    pub(crate) jit_error: Option<RuntimeError>,
    /// Backs `vm_call_state_guard::MarkContextGuard`: the whole "mark
    /// context" one-shot flag family, packed into one `u16` bitfield plus the
    /// one non-`Copy` member (`array_share_source`) — see
    /// `crate::runtime::mark_context` for the layout and for why the pack
    /// happened (#7738). Read a flag through its accessor
    /// (`Interpreter::bind_context` et al.), which hands out a `MarkFlag`
    /// with the same `get`/`set` API the separate `Cell<bool>` fields had.
    ///
    /// It is `Box`-backed (a HEAP allocation separate from `Interpreter`'s
    /// own, not embedded directly in this struct) so the guard's `Drop` impl
    /// can restore it via a raw pointer taken straight into that separate
    /// allocation. A plain `Cell<T>` field is NOT enough: Miri's
    /// Stacked-Borrows retagging does not carve out an embedded Cell's own
    /// byte range as exempt from a later `&mut Interpreter` call's Unique
    /// retag over the WHOLE struct, so a raw pointer into `Interpreter`
    /// itself -- even one that only ever touches a Cell field -- still goes
    /// stale. A `Box`'s heap allocation is a separate Stacked-Borrows
    /// allocation entirely, immune to retags of `Interpreter`'s own memory
    /// (the same reason `runtime::accessors_stack::CurrentPackageGuard`'s
    /// `Arc<RwLock<String>>`/`Arc<AtomicU32>` backing works) — see
    /// `crate::vm::vm_call_state_guard`'s module doc for the full history.
    pub(crate) mark_ctx: Box<crate::runtime::mark_context::MarkContextState>,
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
    /// Slice 2a: cheap gate — `true` once any `__mutsu_array_share::` marker has
    /// been set, so the `SetLocal` write-through fast path only pays the marker
    /// lookup when at least one `=`-array-shared scalar exists.
    pub(crate) array_share_active: bool,
    /// Set by `StashVarDeclInit`: the raw, uncoerced initializer of the `@`/`%`
    /// declaration currently being processed, so `ApplyVarTrait`'s
    /// custom-container branches can hand the class's `STORE` the RHS with its
    /// original scalar-vs-list shape intact (raku passes `'x'` bare but
    /// `('x','y')` as a List). Taken by the trait op; `None` for every
    /// declaration that does not carry a type-named `is` trait.
    pub(crate) vardecl_init_raw: Option<Value>,
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
    ///
    /// A **set**, not a list: the entries are an unordered pending-work set (each
    /// names a distinct variable, so no two of them target the same slot and the
    /// drain order cannot matter), every producer already deduplicated against it
    /// before pushing, and retain-on-miss means it is long-lived — after loading a
    /// handful of modules it holds hundreds of names that no frame will ever own
    /// (enum values, constants, exported symbols; see
    /// `apply_pending_caller_var_writeback_slow`). A `Vec` made both the
    /// dedup-on-insert and the drain linear in that accumulated size.
    pub(crate) pending_caller_var_writeback: rustc_hash::FxHashSet<String>,
    /// Appended every time a resume-safe `CONTROL` handler is run INLINE at a
    /// warn raise site (`try_control_inline`) and writes one of the
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
    /// Slots of the current call's scalar `is rw` parameters that its body
    /// may rebind with `:=`, each with the value it held when first rebound
    /// (#10361; see `vm_rw_param_rebind`). Saved per call frame.
    pub(crate) rw_param_rebinds: Vec<(u32, Option<Value>)>,
    pub(crate) outer_scope_locals: Vec<Vec<Value>>,
    /// Stack of captured ENTER-phaser values for blocks whose textually-last
    /// statement is an ENTER phaser (its entry-time value becomes the block
    /// result). Pushed by `PushEnterResult` in the ENTER section and popped by
    /// `LoadEnterResult` at the end of the block body.
    pub(crate) enter_result_stack: Vec<Value>,
    pub(crate) pending_alias_bind_names: Vec<(String, String)>,
    /// Depth of `with_nested_registers` re-entry (nested VM runs: closure
    /// bodies dispatched from native code, EVAL, dies-ok blocks, ...). The
    /// uncaught-CX::Return -> X::ControlFlow::Return conversion in `run_inner`
    /// only fires at the TRUE top level (depth 0): inside a nested run the
    /// signal's target routine may well be an outer VM frame, so it must keep
    /// propagating (a tap/quit callback's `return` targeting the sub that
    /// called `.emit`, for example).
    pub(crate) nested_run_depth: u32,
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
    /// Resolution and compile caches: derived state, rebuilt on demand (the
    /// `caches` subsystem, ADR-10779).
    pub(crate) caches: resolution_caches::ResolutionCaches,
    /// Regex, grammar and slang state (the `regex` subsystem, ADR-10779).
    pub(crate) regex_state: regex_grammar_state::RegexGrammarState,
    /// Supply/react/gather/lazy-pull state (the `async` subsystem, ADR-10779).
    pub(crate) async_state: async_state::AsyncState,
    /// Cross-thread variable sharing and lock bookkeeping (the `threads`
    /// subsystem, ADR-10779).
    pub(crate) threads: thread_sharing::ThreadSharing,
    /// Topic, given/when and for/loop bookkeeping (the `topic` subsystem,
    /// ADR-10779).
    pub(crate) topic_state: topic_state::TopicState,
    /// Control flow, exceptions, phasers and program exit (the `control`
    /// subsystem, ADR-10779).
    pub(crate) control: control_state::ControlState,
    /// Dispatch state: the multi/method/wrap/samewith stacks, `wrap` chains,
    /// operator tables and dispatch flags (the `dispatch` subsystem, ADR-10779).
    pub(crate) dispatch: dispatch_state::DispatchState,
    /// Package/unit/`our`/`state` variable storage and lexical bookkeeping that
    /// lives outside the frames (the `lexicals` subsystem, ADR-10779).
    pub(crate) lexicals: lexical_state::LexicalState,
    /// Output sinks, IO handles, process paths and TAP state (the `io`
    /// subsystem, ADR-10779).
    pub(crate) io: io_state::IoState,
    /// The compilation unit's declarator docs (`#|`/`#=`) and the `.WHY`
    /// caches over them (ADR-0136, ADR-10779).
    pub(crate) declarator_docs: declarator_docs::DeclaratorDocs,
    /// Module loading, import/export bookkeeping and pragmas (the `module`
    /// subsystem, ADR-10779).
    pub(crate) module: module_state::ModuleState,
    /// The type registry and the class/role/enum/subset declaration state
    /// (the `types` subsystem, ADR-10779).
    pub(crate) types: type_state::TypeState,
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
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ContainerTypeInfo {
    pub(crate) value_type: String,
    pub(crate) key_type: Option<String>,
    pub(crate) declared_type: Option<String>,
}

/// [`ContainerTypeInfo`] with its two type names still borrowed from the
/// constraint text they were sliced out of — see
/// [`Interpreter::container_constraint_parts`], which is where the reason
/// lives.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ContainerConstraintParts<'a> {
    pub(crate) value_type: &'a str,
    pub(crate) key_type: Option<&'a str>,
    pub(crate) declared_type: Option<String>,
}

impl ContainerConstraintParts<'_> {
    /// Copy the borrowed names out, for the callers that keep the answer past
    /// the constraint text's borrow.
    pub(crate) fn into_owned(self) -> ContainerTypeInfo {
        ContainerTypeInfo {
            value_type: self.value_type.to_string(),
            key_type: self.key_type.map(str::to_string),
            declared_type: self.declared_type,
        }
    }
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
        // A captured variable promoted to a shared cell (escape analysis)
        // holds the very container the variable's own store path consults, so
        // its descriptor (`is Map`, element type, ...) is read through the
        // cell rather than lost behind it (#9488).
        ValueView::ContainerRef(cell) => {
            let inner = cell.lock().unwrap().clone();
            container_type_metadata_with(&inner, instance_meta)
        }
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

/// Which compilation unit each `EVAL` unit was compiled inside, keyed by the
/// unit's `?FILE` name (`EVAL_0`, ...). Process-global, like the counter that
/// mints those names: the names are unique for the life of the process, and an
/// EVAL unit's parent never changes once recorded.
static EVAL_UNIT_PARENTS: std::sync::LazyLock<
    std::sync::RwLock<rustc_hash::FxHashMap<Symbol, Symbol>>,
> = std::sync::LazyLock::new(|| std::sync::RwLock::new(rustc_hash::FxHashMap::default()));

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
    // The three copy-on-write registry tables: an `Arc` bump each, not a copy
    // (see `Registry::functions`). The functions table is the versioned
    // `FunctionTable`, so putting this snapshot back also puts back the version
    // its content was stamped with (#8314).
    Arc<crate::runtime::function_table::FunctionTable>,
    Arc<rustc_hash::FxHashMap<Symbol, Arc<FunctionDef>>>,
    Arc<rustc_hash::FxHashSet<String>>,
    Arc<crate::runtime::registry::TokenDefsMap>,
    Arc<rustc_hash::FxHashSet<String>>,
    Arc<rustc_hash::FxHashMap<Symbol, Arc<FunctionDef>>>, // our-scoped functions snapshot
    std::sync::Arc<std::collections::HashMap<String, HashSet<Symbol>>>, // user_declared_infix_ops snapshot
    Arc<HashSet<Symbol>>,                  // imported routine aliases snapshot
    Arc<HashMap<Symbol, HashSet<String>>>, // imported exported-proto tags snapshot
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
    /// Function definitions hidden by an imported proto/multi family in this
    /// scope. Imports merge multi candidates into one package key, so the
    /// ordinary key snapshot cannot restore an enclosing candidate that the
    /// import intentionally shadowed.
    pub(crate) shadowed_functions: HashMap<Symbol, Arc<FunctionDef>>,
    /// Proto definitions hidden by an imported proto in this scope.
    pub(crate) shadowed_proto_functions: HashMap<Symbol, Arc<FunctionDef>>,
    /// Fully-qualified imported proto names already given shadowing semantics
    /// in this scope. Multiple imports in one scope still merge candidates;
    /// only the first import hides the enclosing family.
    pub(crate) shadowed_proto_names: HashSet<String>,
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
    /// The value each of `imported_env_keys` held before this scope first
    /// imported over it, for the keys that already had one. The block's
    /// import shadows that outer binding, so `pop_import_scope` puts it back
    /// instead of removing the key (`use M :t; { use M } t` still sees the
    /// outer import of `t`).
    pub(crate) shadowed_env_values: HashMap<Symbol, Value>,
    /// Imported environment aliases visible before this scope was pushed.
    pub(crate) imported_env_aliases: HashMap<Symbol, Symbol>,
    /// Imported routine aliases visible before this scope was pushed. The
    /// registry snapshot alone cannot distinguish an imported alias from a
    /// declaration made in this scope when the names collide.
    pub(crate) imported_routine_aliases: std::sync::Arc<HashSet<Symbol>>,
    /// The routine aliases (`Pkg::name`) imported while this scope was the
    /// innermost one, including re-imports of an alias an enclosing scope
    /// already had: the block's own `MY::` lists exactly these (#10626).
    pub(crate) own_routine_imports: HashSet<Symbol>,
    /// Export tags inherited by local multis extending imported exported protos.
    pub(crate) imported_exported_proto_tags: std::sync::Arc<HashMap<Symbol, HashSet<String>>>,
    pub(crate) newline_mode: NewlineMode,
    pub(crate) strict_mode: bool,
    pub(crate) fatal_mode: bool,
    pub(crate) lexical_fatal_mode: bool,
    pub(crate) monkey_typing: bool,
    /// Whether the pop also rolls the class registry back to `classes`.
    ///
    /// True for a `use`-containing block, whose class imports are lexical to it.
    /// False for the BEGIN-time preload scope (`push_preload_scope`), which
    /// exists only to contain the `GLOBAL::` routine and proto aliases mutsu's
    /// sub hoisting installs while a module body loads: a package a module
    /// declares is installed into GLOBAL by the load itself in raku, and it is
    /// precisely what the preload is hoisting the load in order to publish.
    pub(crate) scope_classes: bool,
    /// LEAVE phasers a `use` in this block attached to it at "compile time"
    /// (`$*R.find-attach-target('block').add-leave-phaser(...)`, see
    /// `runtime::attach_target`), in attach order. Run LIFO when the scope
    /// closes, on every exit path (`OpCode::ImportScope`).
    pub(crate) leave_phasers: Vec<Value>,
    /// The compilation unit whose code opened this scope
    /// (`executing_unit_sym_for_module_load` at the push). An import made here
    /// shadows that unit's own top-level routines, but not another unit's
    /// (#11103), and a `need`/`use` it runs merges into it (ADR-11136).
    pub(crate) unit: Symbol,
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

/// Rakudo's default `$*TOLERANCE`, the relative tolerance `infix:<=~=>`/`≅`
/// (and `Complex`'s real-coercion check) compare against. Declared in
/// `PROCESS::` by the setting, so a program that never touches it still reads
/// `1e-15` from `$*TOLERANCE` — mutsu materializes it lazily in
/// `Interpreter::lazy_magic_dynamic_var`. This constant is the *same* value,
/// used by the operator implementations as the fallback for the case where the
/// dynamic lookup cannot see it (e.g. `get_dynamic_var`, which walks only the
/// caller stack and never the lazy magic table).
pub(crate) const DEFAULT_TOLERANCE: f64 = 1e-15;

/// Rakudo's default `$*DEFAULT-READ-ELEMS`, used by `IO::Handle.read` and
/// compatible user-defined handle implementations.
pub(crate) const DEFAULT_READ_ELEMS: i64 = 65536;

/// Reserved pseudo-unit key mainline's own captured `my` lexicals are stored
/// under in `Interpreter::unit_lexicals` (ADR-0024). Contains `<`/`>`, which
/// cannot appear in a real Raku package name, so no user `package`/`module`/
/// `class` can collide with it.
/// One `let`/`temp` save (`Interpreter::let_saves`).
#[derive(Clone)]
pub(crate) struct LetSaveEntry {
    /// The saved variable (unused for an element save).
    pub(crate) name: String,
    /// The value to restore.
    pub(crate) value: Value,
    /// `temp` (always restore) rather than `let` (restore on failure only).
    pub(crate) is_temp: bool,
    /// Compiler-baked local slot of `name` (§1.4/§1.5): the scope-exit
    /// restore writes `locals[slot]` directly instead of resolving the name to
    /// the OUTER slot via `find_local_slot`. `None` for a non-local target
    /// (by-name fallback).
    pub(crate) slot: Option<u32>,
    /// An element save (`temp @a[i]`, `temp $t[1]<k>[1]`): the container the
    /// element lives in and its key. The restore writes `value` back into that
    /// element in place, so the rest of the container -- and every other name
    /// bound to it -- is untouched (#9434).
    pub(crate) elem: Option<(Value, Value)>,
}

pub(crate) const MAINLINE_UNIT_KEY: &str = "UNIT<mainline>";

/// Prefix of the reserved pseudo-unit key a named sub declared inside a *bare
/// block* stores its own captured block lexicals under (ADR-0024's
/// "subs declared inside blocks" follow-up). One bucket per sub rather than
/// one shared bucket, because a block scope — unlike mainline — is not
/// unique: two sibling blocks each declaring `my $x` and each declaring a sub
/// that captures it are two different bindings, and a single bucket would fuse
/// them. Contains `<`/`>` for the same reason [`MAINLINE_UNIT_KEY`] does.
pub(crate) const BLOCK_LEXICAL_UNIT_PREFIX: &str = "UNIT<block ";

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
    use crate::value::{SubData, Value, ValueMap};
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
        // A REPL line's tail is its value; a program's tail is sunk.
        interp.run_value_tail("3 + 4").unwrap();
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
        compiled.binding_descs = vec![
            crate::binding_desc::BindingDesc::new(crate::binding_desc::BindingFlags::new(
                true, false,
            )),
            crate::binding_desc::BindingDesc::default(),
            crate::binding_desc::BindingDesc::default(),
        ];
        compiled.ops = vec![
            OpCode::GetGlobal(0),
            OpCode::GetArrayVar(1),
            OpCode::SetGlobal(0),
            OpCode::SetLocal(2),
        ];

        let block = crate::gc::Gc::new(SubData {
            package: Symbol::intern("GLOBAL"),
            name: Symbol::intern("__protect_test__"),
            params: crate::value::empty_params(),
            param_defs: crate::value::empty_param_defs(),
            body: std::sync::Arc::new(vec![Stmt::Expr(Expr::Literal(Value::int(0)))]),
            is_rw: false,
            is_raw: false,
            env,
            assumed_positional: vec![],
            assumed_named: ValueMap::default(),
            id: 1,
            is_direct_code: false,
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
            own_cell_captures: Vec::new(),
            upvalues: Vec::new(),
            captured_fatal_mode: false,
            param_name_syms_cache: std::sync::OnceLock::new(),
            source_file_sym_cache: std::sync::OnceLock::new(),
            state_scope_guard: None,
            captured_readonly: None,
            routine_cell: Default::default(),
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
