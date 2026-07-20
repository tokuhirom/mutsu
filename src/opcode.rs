use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use crate::ast::{ParamDef, Stmt};
use crate::symbol::Symbol;
use crate::value::{Value, ValueView};

/// Monotonic, process-global flag: set at compile time when any compiled code
/// contains an op that can read a *caller frame's* lexical by dynamic name --
/// `CALLER::`/`OUTER::` access, symbolic dereference `::($name)`, pseudo-stash
/// access, indirect code lookup, or an `EVAL`/`EVALFILE` call (which compiles
/// and runs a string in the caller's lexical scope).
///
/// The compiled-function light call path uses this to decide whether it may
/// skip writing a *slot-only* parameter (one not in `needs_env_sync`, read only
/// via `GetLocal`) into the interpreter's shared `env`: if no such reflective
/// reader exists anywhere in the program, the param can stay purely in the VM's
/// `locals` (dual-store decoupling, docs/vm-dual-store.md). The flag is
/// monotonic and global, so `true` only ever forces the (correct) full param
/// write -- an over-set is conservative, never wrong.
static REFLECTIVE_NAME_ACCESS_SEEN: AtomicBool = AtomicBool::new(false);

/// True if any compiled code may read a caller frame's lexical by dynamic name.
/// See [`REFLECTIVE_NAME_ACCESS_SEEN`].
#[inline]
pub(crate) fn reflective_name_access_possible() -> bool {
    REFLECTIVE_NAME_ACCESS_SEEN.load(Ordering::Relaxed)
}

/// `(B)` per-store env-write gate (docs/lexical-scope-slot-campaign.md, "The
/// `(B)` per-store env-write gate"). Default OFF is byte-identical to the
/// pre-gate build: `exec_set_local_op_inner` mirrors every plain-lexical store
/// into the name-keyed `env`. Under `MUTSU_GATE_LOCAL_ENV_WRITE=1` the mirror is
/// skipped for slot-authoritative plain lexicals (not captured/reflective/sync),
/// so the slot is the single source of truth and env-COW can drop the write. This
/// is the burndown gate — flip and delete once all four env-mirror consumers
/// (#1 block-restore, #2 cross-thread, #3 call-return reconcile, #4 curry) are
/// slot-authoritative. The t/ ON survey is clean; the roast ON survey (2026-07-20)
/// surfaced a residual set (mixhash/sethash `%h<a>--`, state-in-regex, interpolated
/// phasers, WHICH/skip/e) being burned down before the default flip. Cached like
/// `jit_enabled()`.
#[inline]
pub(crate) fn gate_local_env_write() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| {
        matches!(
            std::env::var("MUTSU_GATE_LOCAL_ENV_WRITE").ok().as_deref(),
            Some("1") | Some("on") | Some("ON")
        )
    })
}

/// Base binary operation for a fused compound-assignment opcode
/// (`$x OP= rhs`). Each variant maps to the same `exec_*_op` the plain
/// `Binary` path uses, so the fused op shares exact operator semantics.
/// See `OpCode::AtomicCompoundVar`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CompoundBaseOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Concat,
    BitAnd,
    BitOr,
    BitXor,
    BitShiftLeft,
    BitShiftRight,
    IntDiv,
    IntMod,
    Gcd,
    Lcm,
    InfixMin,
    InfixMax,
    StringRepeat,
}

impl CompoundBaseOp {
    /// Map a base binary `OpCode` to its fusable `CompoundBaseOp`, or `None`
    /// if compound-assignment fusion is not supported for that operator.
    pub(crate) fn from_opcode(op: &OpCode) -> Option<CompoundBaseOp> {
        Some(match op {
            OpCode::Add => CompoundBaseOp::Add,
            OpCode::Sub => CompoundBaseOp::Sub,
            OpCode::Mul => CompoundBaseOp::Mul,
            OpCode::Div => CompoundBaseOp::Div,
            OpCode::Mod => CompoundBaseOp::Mod,
            OpCode::Pow => CompoundBaseOp::Pow,
            OpCode::Concat => CompoundBaseOp::Concat,
            OpCode::BitAnd => CompoundBaseOp::BitAnd,
            OpCode::BitOr => CompoundBaseOp::BitOr,
            OpCode::BitXor => CompoundBaseOp::BitXor,
            OpCode::BitShiftLeft => CompoundBaseOp::BitShiftLeft,
            OpCode::BitShiftRight => CompoundBaseOp::BitShiftRight,
            OpCode::IntDiv => CompoundBaseOp::IntDiv,
            OpCode::IntMod => CompoundBaseOp::IntMod,
            OpCode::Gcd => CompoundBaseOp::Gcd,
            OpCode::Lcm => CompoundBaseOp::Lcm,
            OpCode::InfixMin => CompoundBaseOp::InfixMin,
            OpCode::InfixMax => CompoundBaseOp::InfixMax,
            OpCode::StringRepeat => CompoundBaseOp::StringRepeat,
            _ => return None,
        })
    }

    /// The `infix:<OP=>` sub name a user can declare to override this
    /// compound-assignment operator directly (distinct from overriding the
    /// base `infix:<OP>`, e.g. `multi sub infix:<+=> ($a is rw, $b) { ... }`
    /// — roast S06-operator-overloading/infix.t).
    pub(crate) fn user_infix_name(self) -> &'static str {
        use CompoundBaseOp::*;
        match self {
            Add => "infix:<+=>",
            Sub => "infix:<-=>",
            Mul => "infix:<*=>",
            Div => "infix:</=>",
            Mod => "infix:<%=>",
            Pow => "infix:<**=>",
            Concat => "infix:<~=>",
            BitAnd => "infix:<+&=>",
            BitOr => "infix:<+|=>",
            BitXor => "infix:<+^=>",
            BitShiftLeft => "infix:<+<=>",
            BitShiftRight => "infix:<+>=>",
            IntDiv => "infix:<div=>",
            IntMod => "infix:<mod=>",
            Gcd => "infix:<gcd=>",
            Lcm => "infix:<lcm=>",
            InfixMin => "infix:<min=>",
            InfixMax => "infix:<max=>",
            StringRepeat => "infix:<x=>",
        }
    }
}

/// Payload of `OpCode::ForLoop`, boxed to keep `size_of::<OpCode>()` small.
/// This is by far the widest instruction (it used to carry three `Vec<String>`s
/// plus two `Option<String>`s inline, padding EVERY opcode in every
/// `Vec<OpCode>` to 192 bytes — see docs/opcode-design-review.md). The VM
/// borrows the boxed spec directly, so executing a `for` loop no longer clones
/// any of these fields.
#[derive(Debug, Clone)]
pub(crate) struct ForLoopSpec {
    pub(crate) param_idx: Option<u32>,
    pub(crate) param_local: Option<u32>,
    pub(crate) body_end: u32,
    pub(crate) label: Option<String>,
    pub(crate) arity: u32,
    pub(crate) collect: bool,
    /// Restore outer `$_` after loop execution (used by postfix/do-for semantics).
    pub(crate) restore_topic: bool,
    /// When true, run the loop body in a spawned thread (race for / hyper for).
    pub(crate) threaded: bool,
    /// When true, the named param is writable (via `<->`, `is rw`, or `is copy`).
    pub(crate) is_rw: bool,
    /// When true, write back modifications to the source container.
    pub(crate) do_writeback: bool,
    /// Param names for multi-param rw for loops (used for writeback).
    pub(crate) rw_param_names: Vec<String>,
    /// When true, the iterable is from .kv (key-value pairs).
    /// Writeback only applies to value params (odd-indexed in the chunk).
    pub(crate) kv_mode: bool,
    /// Variable names for per-element writeback when the iterable is a list
    /// of scalar variables (e.g. `for ($a, $b, $c) { $_++ }`).
    pub(crate) source_var_names: Vec<String>,
    /// Compiler-baked local slot for each `source_var_names` entry (§1.5): the
    /// per-element writeback (`write_back_to_source_var`) writes the mutated
    /// loop value straight into `locals[slot]` instead of re-resolving the
    /// target name via `update_local_if_exists`. `None` for a target with no
    /// local slot (`our`/global/undeclared), which keeps the by-name path.
    /// Parallel to `source_var_names`.
    pub(crate) source_var_locals: Vec<Option<u32>>,
    /// When true, Junction items are expanded into their eigenstates
    /// (parameter type is Any or more specific, not Mu or Junction).
    pub(crate) autothread_junctions: bool,
    /// When true, the block explicitly declared zero parameters (`-> {}`).
    /// Passing any argument should throw "Too many positionals passed".
    pub(crate) explicit_zero_params: bool,
    /// Names of multi-param bindings (for `-> $a, \b, $c` loops).
    /// Used to temporarily clear sigilless readonly flags before binding.
    pub(crate) multi_param_names: Vec<String>,
    /// When true, the iterable is a `.pairs`/`.antipairs` transform: the
    /// loop variable is a `Pair` that *wraps* the source element, not the
    /// element itself. The plain (topic/named) per-element source writeback
    /// is suppressed (it would overwrite the element with the Pair); the
    /// source tag is still kept so the Pair's rw `.value` alias can detect
    /// immutability and propagate.
    pub(crate) loop_var_wraps_element: bool,
    /// When true, the iterable is `%h.values` / `$b.values` on a variable:
    /// the loop variable (`$_` / a plain named param) aliases the container's
    /// *value*, so a `$_ = ...` topic assignment writes back to the source by
    /// key order (`$_ = X for %h.values` mutates `%h`; `for $b.values` mutates
    /// a mutable MixHash/BagHash). The VM branches on the runtime container
    /// type. Distinguished from bare `for %h` (Pairs, no value writeback) and
    /// `.keys` (read-only).
    pub(crate) values_mode: bool,
    /// The bare source array variable name for `for @a` (without sigil), when
    /// the iterable is a single plain array variable. Enables live-array
    /// iteration: if the loop body pushes onto `@a`, the loop keeps yielding
    /// the newly-appended tail (raku semantics). `None` for any non-trivial
    /// iterable. Separate from `source_var_names` (a per-index scalar-list
    /// writeback mechanism that must NOT be populated for a `@`-source).
    pub(crate) single_array_source: Option<String>,
    /// Compiler-baked local slot for `single_array_source` (§1.5): the
    /// live-array re-read reads `locals[slot]` directly instead of resolving
    /// the source name via `find_local_slot`. `None` when the source is not a
    /// resolvable local (keeps the by-name + env fallback).
    pub(crate) single_array_source_local: Option<u32>,
    /// When true, the loop body declares one or more routines (`sub`/`token`/…)
    /// at its top level. Such declarations are lexically scoped to the loop
    /// body in Raku: they are hoisted (visible before their textual position,
    /// via `RegisterSub` ops emitted at body start) and must NOT leak past the
    /// loop. The VM snapshots the routine registry before the loop and restores
    /// it after, but only when this flag is set — hot numeric loops (the common
    /// case, no nested `sub`) skip the snapshot entirely and pay zero cost.
    pub(crate) body_declares_routines: bool,
}

/// Payload of `OpCode::RuntimeHasDecl`. A `has $.x` that reaches the VM (rather
/// than being collected declaratively by `register_class_decl`) only arises from
/// mainline / EVAL'd source — e.g. `class Foo { BEGIN EVAL q[has $.x] }`. At
/// runtime the op checks whether a class is currently being defined
/// (`Interpreter::defining_class`): if so it registers the attribute onto that
/// class; otherwise it throws the pre-built `error` (`X::Attribute::NoPackage`
/// or `X::Attribute::Package`). Boxed to keep `size_of::<OpCode>()` small.
#[derive(Debug, Clone)]
pub(crate) struct RuntimeHasDeclSpec {
    pub(crate) attr_name: String,
    pub(crate) is_public: bool,
    pub(crate) sigil: char,
    pub(crate) is_rw: bool,
    pub(crate) is_readonly: bool,
    pub(crate) is_required: Option<Option<String>>,
    pub(crate) is_built: Option<bool>,
    pub(crate) type_constraint: Option<String>,
    pub(crate) type_smiley: Option<String>,
    pub(crate) default: Option<crate::ast::Expr>,
    /// The `X::Attribute::*` error to throw when this `has` runs outside a
    /// class-definition context.
    pub(crate) error: Value,
}

/// Bytecode operations for the VM.
#[derive(Debug, Clone)]
pub(crate) enum OpCode {
    // -- Constants --
    LoadConst(u32),
    LoadNil,
    LoadTrue,
    LoadFalse,

    // -- Variables --
    GetLocal(u32),
    /// Like GetLocal but does NOT resolve HashEntryRef values.
    /// Used by `=:=` to compare raw container references.
    GetLocalRaw(u32),
    SetLocal(u32),
    /// `SetLocal` fused with the declaration markers that always precede it in a
    /// `my $x = <expr>` (ADR-0006 §2.3 peephole): `MarkExplicitInitializerContext`
    /// (only when `explicit_init`) + `MarkVarDeclContext` + `SetLocal(slot)`.
    /// Three dispatches per declaration collapse into one; the VM sets the same
    /// two context flags before running the identical `SetLocal` body, so the
    /// semantics are unchanged. Fusion happens in `emit()`, which can only see —
    /// and therefore only ever rewrite — a marker pair it just emitted itself.
    SetLocalDecl {
        slot: u32,
        explicit_init: bool,
    },
    GetGlobal(u32),
    /// Read a captured read-only scalar free variable by index from this frame's
    /// upvalue array (`self.upvalues`). Emitted in place of `GetGlobal` for a
    /// closure's read-only plain-scalar free variables (see
    /// `CompiledCode::compute_upvalues`). `index` indexes `upvalue_syms` / the
    /// runtime upvalue array; a `ContainerRef` upvalue is auto-dereferenced.
    /// `name_idx` is the original name constant: when `index` is out of range for
    /// the live `self.upvalues` (a non-standard execution path — control handler,
    /// phaser, nested-register run — that did not install this closure's upvalue
    /// array), execution falls back to a `GetGlobal(name_idx)` env lookup. Env is
    /// retained as the capture source, so the fallback is always correct.
    GetUpvalue {
        index: u32,
        name_idx: u32,
    },
    /// Load `self` from the captured environment for a `$.attr` accessor.
    /// Raises X::Syntax::NoSelf (the operand is the constant index of the
    /// accessor's display name, e.g. `$.a`) when `self` is unavailable.
    GetSelfOrNoSelf(u32),
    SetGlobal(u32),
    /// Like SetGlobal but skips @/% coercion (used for `constant @x` / `constant %x`).
    SetGlobalRaw(u32),
    /// Verify that a dynamic variable (`$*x` / `@*x` / `%*x`) is in scope before a
    /// genuine assignment to it. Throws X::Dynamic::NotFound when it was never
    /// declared (`my $*x`) nor is a built-in dynamic var. Emitted only for plain
    /// `Stmt::Assign` / `Expr::AssignExpr` to a `*`-twigil name (NOT for param
    /// binding, element auto-viv, or `my` declarations). Operand: constant index
    /// of the assignment target name (sigil-stripped, e.g. `*PATH` / `%*OPTS`).
    CheckDynamicVarDeclared(u32),
    /// Load the value of an `our`-scoped variable from the persistent our_vars store.
    /// Falls back to Nil if not found. Used for `our` redeclarations without initializer.
    GetOurVar(u32),
    /// Coerce top-of-stack value to a List (ArrayKind::List).
    /// Used for `constant @x = ...` where the @-sigil should produce a List, not an Array.
    CoerceToList,
    /// Mark that the next SetLocal should treat the value as a constant
    /// (skip @/% container coercion). Similar to MarkBindContext.
    MarkConstantContext,
    /// Mark that the next SetLocal came from an explicit initializer (`= expr`).
    MarkExplicitInitializerContext,
    /// Mark that the next SetLocal is from a `my` VarDecl (allows overwriting
    /// immutable Blob containers when the local slot is reused in a loop).
    MarkVarDeclContext,
    /// Mark that the next SetLocal declares a *shaped* (fixed-dimension) array
    /// (`my @a[5]`, `my int @a[3;3] = ...`). The shape comes from the declaration
    /// itself, so SetLocal must KEEP it (unlike `my @u = @shaped`, which copies
    /// values and drops shaped-ness).
    MarkShapedDeclContext,
    SetVarType {
        name_idx: u32,
        tc_idx: u32,
    },
    SetTopic,
    SaveTopic,
    RestoreTopic,
    /// Enter a pointy-topic scope (`if COND -> $_`, `with COND -> $_`): save the
    /// current `$_` and `topic_source_var`, then clear `topic_source_var` so the
    /// fresh `$_` binding shadows an enclosing `given`'s topic without writing
    /// back to its source variable. Paired with `ExitPointyTopic`.
    EnterPointyTopic,
    ExitPointyTopic,
    GetArrayVar(u32),
    GetHashVar(u32),
    GetBareWord(u32),
    GetPseudoStash(u32),

    // -- Arithmetic --
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Negate,
    IntBitNeg,  // +^ prefix: integer bitwise negation
    BoolBitNeg, // ?^ prefix: boolean bitwise negation
    StrBitNeg,  // ~^ prefix: string/buffer bitwise negation
    MakeSlip,   // | prefix: convert array/list to Slip for flattening
    DeSlip,     // demote a top-level Slip VALUE to a Seq so it is NOT flattened
    // by a `**@`-slurpy consumer (say/put/print/note). A `|EXPR` pipe-slip is
    // left untouched (still flattens); an ordinary `.Slip`/`slip(...)` value is
    // kept whole. See exec_say_op / flatten_slip_args.
    Decont, // strip ONE level of Scalar for slurpy flattening (NOT the
    // recursive Value::descalarize; touches no ArrayKind flag — see decont family note)
    /// Itemize (containerize) an Array/List value so it behaves as a single
    /// item in list context. Emitted when `$` variable values are used inside
    /// `ArrayLiteral` or assigned to `@`/`%` targets.
    Itemize,
    /// De-itemize a `for … -> @a` chunk element while preserving the source
    /// array's element type (see `Expr::DeitemizeForBind`). Falls back to plain
    /// list flattening for non-array values.
    DeitemizeForBind,
    /// Strip ONE level of itemization for a zen slice (`$a[]` / `$a<>`):
    /// an itemized Array/List drops its Scalar container (kind flag) so a
    /// following list context flattens it; a `Scalar` wrapper unwraps.
    /// Everything else passes through unchanged.
    DeitemizeZen,
    /// Like `Itemize`, but skips itemization when the named scalar variable is
    /// bound (`:=`) to a Positional value. A bound scalar is NOT a Scalar
    /// container, so `@a = $bound` must flatten (matching Raku). The argument is
    /// the constant-pool index of the variable name. Emitted for `@a = $var`.
    ItemizeVar(u32),
    /// Wrap the top-of-stack value in a Scalar container.
    /// Used for `my $ = expr` (anonymous scalar) in argument position,
    /// so the anonymous container is preserved in immutable List contexts.
    WrapScalar,
    /// Wrap the top-of-stack value in a typed `ContainerRef` cell and register
    /// its `of`-type constraint (the u32 is the constant-pool index of the type
    /// name). Emitted for a typed anonymous scalar `my T $` used as a value, so
    /// the constraint travels with the value (e.g. into a `Pair` value) and
    /// `.value = ...` can raise X::TypeCheck::Assignment.
    WrapTypedContainer(u32),
    /// Recursively flatten a list value into a real Array (like *@ slurpy).
    /// Used to populate @_ in bare if blocks.
    FlattenSlurpy,

    // -- Logic / coercion --
    Not,
    BoolCoerce,
    WrapVarRef(u32),
    /// Signal that the next SetLocal is a `:=` bind (preserve container type for `@` vars).
    MarkBindContext,
    /// Signal that the next SetLocal binds a `$` scalar to a Positional value via
    /// `:=`, so it must be recorded as decontainerized (so `@a = $bound` flattens).
    MarkScalarBindContext,
    /// Signal that the next SetLocal is a `:=` rebind (not a VarDecl).
    /// Triggers cleanup of old bind pairs and reverse aliases.
    MarkRebindContext,
    /// Emitted immediately before a `CallMethod`/`CallMethodMut` whose result is
    /// wanted as a *container* rather than a value copy (a `:=` bind RHS like
    /// `my $ref := $obj.attr`, or the inner call of a `.VAR` chain like
    /// `$obj.attr.VAR`). When the call resolves to a public attribute accessor
    /// read, the attribute slot is promoted to a shared `ContainerRef` cell and
    /// the cell itself is returned, giving the caller the attribute's container
    /// identity. Consumed (and unconditionally cleared) at CallMethod entry, so
    /// it cannot leak past the one dispatch it was emitted for.
    MarkAccessorRefContext,
    /// Slice 2a/2b (`docs/scalar-array-sharing.md`): signal that the next
    /// SetLocal/AssignExpr assigns to a `$` scalar via plain `=` and that the
    /// named source variable's container should be shared by reference. The
    /// operand is a constant-pool index for the source name (`@z`/`%h` for a
    /// whole-container RHS, or a scalar name for a chained `$r = $q`). The source
    /// is promoted to a shared `ContainerRef` cell so structural mutations
    /// (`.push`) through either name are seen by both. A no-op when the source
    /// does not hold a container (so a plain `$x = $y` stays a copy).
    MarkArrayShareSource(u32),

    /// Slice 2b (`docs/scalar-array-sharing.md`): flag the upcoming
    /// `IndexAssignExprNamed` as a `=`-reference share of an array/hash element
    /// (`@aoa[i] = @row` / `%h<k> = @row`). The RHS is compiled as a `:=` bind so
    /// the element holds a shared `ContainerRef` cell and the source is promoted,
    /// but this marker records the element as a *value* share (not a bind) so a
    /// later non-share reassignment (`@aoa[i] = 42`) REPLACES the slot instead of
    /// writing through the shared cell (raku value semantics).
    MarkElementShare,

    // -- String --
    Concat,

    // -- Numeric comparison --
    NumEq,
    NumNe,
    /// Native-int-aware `!=`.  Flags encode signedness of each operand:
    /// bit 0 = left is unsigned, bit 1 = right is unsigned.
    /// When cross-signed and the signed operand is negative, returns False
    /// (matching Rakudo's MoarVM behaviour for native int registers).
    NumNeNative(u8),
    NumLt,
    NumLe,
    NumGt,
    NumGe,
    ApproxEq,
    /// Container identity (`=:=`).
    /// The `u8` flags encode containerisation of operands:
    /// bit 0 = left operand is containerised,
    /// bit 1 = right operand is containerised.
    /// When an operand is containerised (came from a variable / index),
    /// non-reference values on the stack can never be the same container,
    /// so the operator returns False for non-Arc value types.
    ContainerEq(u8),
    /// Container identity (`=:=`) when both operands are named variables.
    /// The VM checks the alias table to see if the two variable names
    /// resolve to the same binding root.  Falls back to `values_identical`
    /// for reference types (Array, Hash, Sub, Instance, …).
    ContainerEqNamed {
        left_name_idx: u32,
        right_name_idx: u32,
    },
    /// Container identity (`=:=`) when one or both operands are array/hash
    /// index expressions.  Carries encoded source names (e.g. "@a\0idx\01")
    /// for both sides.  The VM checks if one side has a binding sentinel
    /// pointing to the other's source.
    ContainerEqIndexed {
        left_name_idx: u32,
        right_name_idx: u32,
    },
    /// Container identity (`=:=`) using raw container values.
    /// Compares HashEntryRef values by checking if they
    /// point to the same hash slot (Arc::ptr_eq + key equality).
    ContainerEqRaw,

    // -- String comparison --
    StrEq,
    StrNe,
    StrLt,
    StrGt,
    StrLe,
    StrGe,

    // -- Generic ordering (cmp-based) --
    Before,
    After,

    // -- Three-way comparison --
    Spaceship,
    Cmp,
    Coll,
    Unicmp,
    Leg,

    // -- Identity/value equality --
    StrictEq,
    StrictNe,
    Eqv,
    /// Smart match with compiled RHS expression at [ip+1..rhs_end).
    SmartMatchExpr {
        rhs_end: u32,
        negate: bool,
        /// Variable name for LHS (to write back after s/// substitution)
        lhs_var: Option<String>,
        /// Compile-time-resolved local slot for `lhs_var`, when it names a
        /// current-scope local (§1.5: bakes the scope-correct slot so the
        /// modified-topic writeback does not re-resolve the name at run time —
        /// see docs/lexical-scope-slot-campaign.md). `None` for a global / not-a-
        /// local LHS, where the writeback stays env-by-name.
        lhs_slot: Option<u32>,
        /// True when RHS was originally `m//` (MatchRegex), which affects
        /// failure return value: `m//` failure returns False, bare `//` returns Nil.
        rhs_is_match_regex: bool,
        /// True when the LHS is a literal (non-lvalue). A destructive `s///`/`tr///`
        /// that matches against a literal must throw X::Assignment::RO.
        lhs_is_literal: bool,
        /// True when the RHS is a plain `Regex` literal. Compile-time half
        /// of the Slice 6.3 step 2 gate that lets the smartmatch op skip its
        /// conservative post-match `env_dirty` re-sync (the runtime half checks
        /// `pending_local_updates` / `$/`-as-local). Excludes RegexWithAdverbs,
        /// named/Sub regex, substitution, transliteration, value smartmatch.
        rhs_pure_regex: bool,
    },
    /// Scalarize a multi-match regex result: Nil -> 0, Positional -> elems, Match -> 1.
    ScalarizeRegexMatchResult,

    // -- Divisibility --
    DivisibleBy,
    NotDivisibleBy,

    // -- Keyword math --
    IntDiv,
    IntMod,
    Gcd,
    Lcm,
    InfixMin,
    InfixMax,

    // -- Repetition --
    StringRepeat,
    ListRepeat,
    FunctionCompose,

    // -- Mixin --
    ButMixin,
    /// Like ButMixin but checks for duplicate type conflicts (used for
    /// per-element tuple expansion: `True but (1, "x")`).
    ButMixinTupleElem,
    // -- Type check --
    Isa,
    Does,
    /// `$x does R` in-place mixin. `.0` = constant index of the target variable
    /// name; `.1` = compiler-baked local slot for that name (§1.5), `None` when the
    /// target is not a resolvable local (falls back to the by-name writeback).
    DoesVar(u32, Option<u32>),
    /// Set/clear the in_does_rhs flag so role calls return Pairs instead of
    /// throwing X::Coerce::Impossible during `does` RHS evaluation.
    SetDoesContext(bool),

    // -- Pair --
    MakePair,
    /// Convert Pair(k,v) → ValuePair(Str(k),v) so it's treated as positional arg
    ContainerizePair,

    // -- Bitwise --
    BitAnd,
    BitOr,
    BitXor,
    BitShiftLeft,
    BitShiftRight,
    BoolBitOr,
    BoolBitAnd,
    BoolBitXor,
    StrBitAnd,
    StrBitOr,
    StrBitXor,
    StrShiftLeft,
    StrShiftRight,

    // -- Set operations --
    SetElem,
    SetCont,
    SetUnion,
    SetAddition,
    SetIntersect,
    SetMultiply,
    SetDiff,
    SetSymDiff,
    SetSubset,
    SetSuperset,
    SetStrictSubset,
    SetStrictSuperset,
    JunctionAny,
    JunctionAll,
    JunctionOne,
    /// Multi-operand junction: pop `count` values, check for user-defined
    /// infix:<|>/<&>/<^> override (list-associative), or build junction.
    JunctionAnyN(u32),
    JunctionAllN(u32),
    JunctionOneN(u32),

    // -- Sequence --
    Sequence {
        exclude_end: bool,
    },

    // -- Control flow --
    /// No-op label marker for `goto`.
    Label(u32),
    /// Jump to `Label` by runtime-evaluated name on stack.
    Goto,
    Jump(i32),
    JumpIfFalse(i32),
    JumpIfTrue(i32),
    /// Jump if top of stack is not nil/defined (without popping)
    JumpIfNotNil(i32),
    /// Call .defined on top of stack, replace with Bool result
    CallDefined,

    // -- Logical --
    /// Logical XOR: pops two values, returns truthy one if exactly one is truthy, else Nil/falsy
    XorXor,

    // -- Stack manipulation --
    Dup,
    Pop,
    /// Pop with sink context — throws unhandled Failures when fatal_mode is active.
    /// The bool is `true` when the sunk value is a syntactically fresh rvalue
    /// (e.g. a method call / `Foo.new`) that may invoke a user-defined `sink`
    /// method; `false` for bare variables / function-call returns whose values
    /// are (or may be) container-wrapped and must not auto-sink (Raku does not
    /// sink container-wrapped values).
    SinkPop(bool),
    /// Statement-prefix `quietly`: push a warning-suppression frame so any
    /// warning raised while evaluating the following expression is silenced
    /// (the warn still resumes in place with its resume value, matching Raku's
    /// CONTROL/CX::Warn `.resume`). Balanced by `WarnSuppressPop`. Unlike the
    /// `quietly(&block)` builtin, this runs the guarded expression INLINE in the
    /// current lexical scope, so `quietly my $x = ...` leaks `$x` to the
    /// enclosing scope as Raku requires.
    WarnSuppressPush,
    /// Pop the warning-suppression frame pushed by `WarnSuppressPush`.
    WarnSuppressPop,
    /// Peek the top of stack (without popping) and throw it if it is an
    /// unhandled Failure. Used at the tail of a try/CATCH body so a trailing
    /// `fail`/Failure value is thrown into the block's CATCH handler while a
    /// normal trailing value is retained as the block's return value.
    ThrowIfFailure,

    // -- Range creation --
    MakeRange,
    MakeRangeExcl,
    MakeRangeExclStart,
    MakeRangeExclBoth,

    // -- Composite --
    MakeArray(u32),
    /// Like MakeArray but creates a true Array (from [...] literals) instead of a List.
    MakeRealArray(u32),
    /// Like MakeRealArray but never flattens a single element (from `[x,]` trailing comma).
    MakeRealArrayNoFlatten(u32),
    MakeHash(u32),
    /// Build a Hash from `count` Pair values on the stack (from `%(k=>v, ...)` syntax).
    MakeHashFromPairs(u32),
    /// Create a Capture from `count` items on stack. Pair values become named args,
    /// non-Pair values become positional args. Slip values are flattened.
    MakeCapture(u32),

    // -- I/O --
    Say(u32),
    Put(u32),
    Print(u32),
    Note(u32),

    // -- Calls (args compiled to bytecode, dispatch delegated to interpreter) --
    /// Expression-level function call: pop `arity` args, call name, push result.
    CallFunc {
        name_idx: u32,
        arity: u32,
        arg_sources_idx: Option<u32>,
    },
    /// Expression-level function call whose literal named args travel
    /// out-of-band: `arity` values on the stack, of which the positions
    /// listed in `CompiledCode::named_arg_specs[spec_idx]` are named-arg
    /// VALUES (no Pair boxing at the call site). The light-call fast path
    /// binds them by `Symbol`; every other dispatch route materializes the
    /// Pairs in place on the stack and delegates to the `CallFunc` logic.
    CallFuncNamed {
        name_idx: u32,
        arity: u32,
        spec_idx: u32,
        arg_sources_idx: Option<u32>,
    },
    /// Method call: pop `arity` args + target, call method, push result.
    CallMethod {
        name_idx: u32,
        arity: u32,
        modifier_idx: Option<u32>,
        /// When true, the method name was quoted (e.g. `."DEFINITE"()`),
        /// bypassing pseudo-method macros.
        quoted: bool,
        /// Optional arg sources for `is rw` parameter writeback support.
        arg_sources_idx: Option<u32>,
    },
    /// Method call with writeback: target is a variable that may be mutated.
    /// Fast path for @arr.push(val) — directly appends to the array Arc,
    /// bypassing full method dispatch. Stack: [val] -> [array].
    ArrayPush {
        target_name_idx: u32,
        /// When the pushed argument is a bare container variable (`@a.push(@b)` /
        /// `@a.push(%h)`), this carries that source variable's name. The pushed
        /// element then shares a `ContainerRef` cell with the source, so later
        /// mutations of the source (`@b.push(4)`, `@b = (...)`) propagate to the
        /// stored element — Raku's non-flattening `**@` slurpy stores the
        /// container itself, not a snapshot. `None` for scalar / expression args.
        value_source_idx: Option<u32>,
    },
    CallMethodMut {
        name_idx: u32,
        arity: u32,
        target_name_idx: u32,
        modifier_idx: Option<u32>,
        /// When true, the method name was quoted (e.g. `."DEFINITE"()`),
        /// bypassing pseudo-method macros.
        quoted: bool,
        /// Optional arg sources for `is rw` parameter writeback support.
        arg_sources_idx: Option<u32>,
    },
    /// Dynamic method call: method name is evaluated at runtime.
    /// Stack layout: [target, name_str, arg0, arg1, ...]
    CallMethodDynamic {
        arity: u32,
        modifier_idx: Option<u32>,
    },
    /// Dynamic method call on a variable target (allows mutation/writeback).
    /// Stack layout: [target, name_str, arg0, arg1, ...]
    CallMethodDynamicMut {
        arity: u32,
        target_name_idx: u32,
        modifier_idx: Option<u32>,
    },
    /// Statement-level call: pop `arity` args, call name (no push).
    ExecCall {
        name_idx: u32,
        arity: u32,
        arg_sources_idx: Option<u32>,
    },
    /// Statement-level call with positional/named values encoded as Pair.
    ///
    /// `slip_positions_idx` indexes a constant `Array` of the argument
    /// positions written as `|EXPR`. Those — and only those — spread into the
    /// argument list: a Slip an ordinary argument merely evaluated to
    /// (`is-deeply $s.Slip, $t.Slip, 'name'`) stays one argument, as in Rakudo.
    /// `keep_value` (tail position: the call's value is the body's result)
    /// pushes the call result onto the stack; plain statement position leaves
    /// the stack untouched.
    ExecCallPairs {
        name_idx: u32,
        arity: u32,
        slip_positions_idx: Option<u32>,
        keep_value: bool,
    },
    BlockScope {
        pre_end: u32,
        enter_end: u32,
        body_end: u32,
        keep_start: u32,
        undo_start: u32,
        post_start: u32,
        end: u32,
        /// True when this scope is a genuine bare block statement (`{ ... }`)
        /// from source, as opposed to a synthesized control-flow body (if/while/
        /// loop branch) or a routine/do-block body. A genuine bare block is a
        /// callframe in Raku, so it must contribute an anonymous frame to a
        /// backtrace captured while executing inside it.
        is_bare_block: bool,
    },
    /// Lightweight block scope for an `if`/`unless`/`else` branch body that
    /// declares a block-local `my`. Unlike `BlockScope` (which does a full
    /// env+locals save/restore and would revert `:=` bindings the branch makes
    /// to *outer* variables), this only applies the loop bodies' *shadow-only*
    /// restore: a body-local `my $x` that shadows an enclosing same-named
    /// binding is recorded at declaration and the outer value is restored on
    /// exit. Names bound with `:=` to an outer var (not `my`-declared in the
    /// branch) are never recorded, so they survive. Runs the body in
    /// `ip+1 .. body_end`.
    BlockLocalScope {
        body_end: u32,
    },
    /// Check the top-of-stack value; if falsy, throw X::Phaser::PrePost.
    /// `is_pre` distinguishes PRE (true) from POST (false). `condition_idx` is
    /// the constant index of the condition's source text (e.g. `0`), used for
    /// the exception's `condition` attribute and message; `None` when unknown.
    CheckPhaser {
        is_pre: bool,
        condition_idx: Option<u32>,
    },
    /// Marks the start of an individual LEAVE phaser body within the
    /// KEEP/UNDO queue. `next` points to the start of the next LEAVE
    /// phaser (or the end of the queue). Used by the VM to continue
    /// running remaining LEAVE phasers when one throws an exception.
    LeaveGuard {
        next: u32,
    },
    /// Pop the top of the value stack and push it onto the ENTER-result stack.
    /// Emitted at the end of the ENTER section for an ENTER phaser that is the
    /// textually-last statement of its block, so the phaser's entry-time value
    /// can later become the block's result value (Raku semantics: a trailing
    /// `ENTER` phaser provides the block return value).
    PushEnterResult,
    /// Pop the top of the ENTER-result stack and push it onto the value stack.
    /// Emitted at the end of the block body when the block's textually-last
    /// statement is an ENTER phaser, materializing its captured value as the
    /// block result.
    LoadEnterResult,
    DoBlockExpr {
        body_end: u32,
        label: Option<String>,
        scope_isolate: bool,
        /// Constant-pool index of a `Array` of the scalar/array variable
        /// names the block declares with `my`/`state` (sigil-keyed as stored in
        /// env). On a `scope_isolate` exit those names revert to their pre-block
        /// values (block-local declarations don't leak), while mutations of OUTER
        /// variables persist. `u32::MAX` when there are none / not isolated.
        isolate_decls_idx: u32,
    },
    OnceExpr {
        body_end: u32,
    },
    DoGivenExpr {
        body_end: u32,
    },
    /// Create a lazy gather list from `stmt_pool[.0]`. `.1` indexes the
    /// analysis-only escaping closure compiled from the same body
    /// (`surface_stashed_body_free_vars`): exec boxes the captured-and-mutated
    /// lexicals it names (`box_captured_lexicals`) BEFORE snapshotting the env,
    /// so a lazy pull after the frame moves on reads the live cell, not a stale
    /// by-value copy.
    MakeGather(u32, Option<u32>),
    /// Force eager evaluation of the top-of-stack value (LazyList → Array)
    Eager,
    CallOnValue {
        arity: u32,
        arg_sources_idx: Option<u32>,
    },
    CallOnCodeVar {
        name_idx: u32,
        arity: u32,
        arg_sources_idx: Option<u32>,
    },
    /// Third field: true when this is a bare block `{ }`, false for `sub { }`.
    MakeAnonSub(u32, Option<u32>, bool),
    /// Third field: true when generated by Whatever-currying (WhateverCode).
    MakeAnonSubParams(u32, Option<u32>, bool),
    /// Third field: true when generated by Whatever-currying (WhateverCode).
    MakeLambda(u32, Option<u32>, bool),
    MakeBlockClosure(u32, Option<u32>),
    // -- Indexing --
    /// `is_positional` is true when the subscript was `[...]` (positional),
    /// false when `{...}` or `<...>` (associative).
    Index {
        is_positional: bool,
    },
    /// Auto-vivifying index that does NOT create the hash entry if missing.
    /// Returns a HashEntryRef that defers creation until write.
    /// Used for the outermost level of `:=` bind so that binding alone
    /// does not autovivify (e.g. `my $b := %h<a><b>` keeps %h empty).
    IndexAutovivifyLazy,
    /// Like IndexAutovivifyLazy, but the index is the TERMINAL element of a `:=`
    /// bind RHS. A container-valued (Array/Hash) leaf is promoted to a
    /// `ContainerRef` cell — not kept as a traversal back-reference.
    IndexAutovivifyLazyTerminal,
    /// `%h<k>:delete` / `@a[i]:delete`. First field is the container variable's
    /// name (const-pool index); the optional second is its compile-time-resolved
    /// local slot (§1.5: the mutated container is written back through this exact
    /// slot instead of a by-name `code.locals` search — docs/lexical-scope-slot-
    /// campaign.md). `None` for a non-local / EVAL-carrier container.
    DeleteIndexNamed(u32, Option<u32>),
    DeleteIndexExpr,
    /// Multi-dimensional indexing: @a[$x;$y;$z]
    /// Stack: [target, dim0, dim1, ..., dimN] → [result]
    MultiDimIndex(u32),
    /// Multi-dimensional index assignment: @a[$x;$y;$z] = value
    /// Stack: [value, dim0, dim1, ..., dimN] (target by name)
    MultiDimIndexAssign {
        name_idx: u32,
        ndims: u32,
    },
    /// Multi-dimensional index assignment (generic target)
    /// Stack: [target, dim0, ..., dimN, value]
    MultiDimIndexAssignGeneric(u32),
    /// Multi-dimensional index as an lvalue (`:=` bind RHS, or a raw `\target` /
    /// `is rw` argument). Descends the nested array/hash through all (scalar)
    /// dimensions, promoting the leaf to a shared `ContainerRef` cell so a later
    /// assignment writes through to the real container. If any dimension is a
    /// slice (Whatever / list), it can't collapse to a single cell, so the read
    /// value is pushed instead (a non-aliasing fallback).
    /// Stack: [target, dim0, ..., dimN] → [ContainerRef | value]
    MultiDimIndexBindRef(u32),
    /// Hash hyperslice: recursively iterate hash with given adverb mode.
    /// Stack: [target] → [result list]
    HyperSlice(u8),

    // -- String interpolation --
    StringConcat(u32),

    // -- Loop control --
    Last(Option<String>),
    Next(Option<String>),
    Redo(Option<String>),

    // -- Given/When control --
    Proceed,
    Succeed,
    /// `done` — terminate the innermost react event loop
    ReactDone,
    /// Tag the current value as coming from a named container (for Scalar binding).
    /// The second field is the compile-time-resolved local slot for the source
    /// name (§1.5 slot baking; `None` = non-local): with shadow slots active the
    /// container writeback targets `locals[slot]` instead of the ambiguous
    /// by-name (`position`) resolution.
    TagContainerRef(u32, Option<u32>),
    /// Tag the current value as coming from a reversed named container (for
    /// `@a.reverse` writeback); same slot-baking contract as `TagContainerRef`.
    TagContainerRefReversed(u32, Option<u32>),
    /// Topicalize a container *element* (`given %h<k>` / `given @a[i]`) as an
    /// lvalue: pop the index from the stack, read element `container[index]`,
    /// push it as the topic value, and record the (container, index) source so
    /// the `given`/`with` body's final `$_` (after `$_ = ...` or `.push`) is
    /// written back to that element. `positional` is true for `@a[i]`, false for
    /// `%h<k>`. The operand is the constant index of the container variable name.
    TagElementSource {
        container_idx: u32,
        positional: bool,
    },

    /// Clear an aggregate variable (@/%) in-place so references see the change.
    UndefineAggregate(u32),

    // -- Unary coercion --
    NumCoerce,
    StrCoerce,
    UptoRange,

    // -- Prefix increment/decrement (returns NEW value) --
    // Optional second field: the compile-time-resolved local slot for the named
    // scalar (§1.5, mirrors PostIncrement/PostDecrement — docs/lexical-scope-slot-
    // campaign.md). `None` for a non-local / temp-value target (env-by-name).
    PreIncrement(u32, Option<u32>),
    PreDecrement(u32, Option<u32>),
    PreIncrementIndex(u32),
    PreDecrementIndex(u32),

    // -- Variable access --
    GetCaptureVar(u32),
    GetCodeVar(u32),

    // -- Postfix operators --
    // The optional second field is the compile-time-resolved local slot for the
    // named scalar (§1.5: bakes the scope-correct slot so the RMW writeback
    // mirrors the exact slot instead of a by-name `code.locals` search, which is
    // ambiguous once a name occupies several slots — docs/lexical-scope-slot-
    // campaign.md). `None` for a non-local (global / `our` / dynamic / a temp
    // value target), where the writeback stays env-by-name.
    PostIncrement(u32, Option<u32>),
    PostDecrement(u32, Option<u32>),
    PostIncrementIndex(u32),
    PostDecrementIndex(u32),
    /// Named index assignment: `var[idx] = value` where `var` is a known
    /// variable name. `is_positional` records whether the subscript was
    /// `[...]` (positional) or `{...}`/`<...>` (associative); used to
    /// choose Array vs Hash when autovivifying a missing variable.
    IndexAssignExprNamed {
        name_idx: u32,
        is_positional: bool,
        /// §1.4 shadow-slot: the compiler-resolved local slot for the target var
        /// (`local_map[name]` at emit time), or `None` for a non-local target
        /// (global/dynamic/undeclared). The exec prefers this baked slot over the
        /// by-name `find_local_slot` (position = outer) so a shadowing inner-block
        /// `my $a` writes its own slot. Byte-identical with shadows off (baked ==
        /// position). See docs/lexical-scope-slot-campaign.md.
        target_slot: Option<u32>,
    },
    IndexAssignPseudoStashNamed {
        stash_name_idx: u32,
        key_name_idx: u32,
    },
    /// Element-for-mutation load for `@a[i].push(...)` / `%h<k>.pop` etc.:
    /// read the element like a plain subscript; with `autoviv` set (push/
    /// append/unshift/prepend), a missing element (Nil / Any / Mu hole) is
    /// autovivified to a fresh empty Array through the normal index-assign
    /// machinery and the stored shared node is yielded. Elements of a
    /// parameterized container (`my Array of Int @x`) get the element type
    /// tagged onto their node so the method's own type check fires. The
    /// following method call mutates the element's node in place (container
    /// identity §3.2), so no post-call writeback is emitted.
    /// Stack: [container, key] → [element]
    IndexElemAutoviv {
        name_idx: u32,
        is_positional: bool,
        /// §1.4 shadow-slot (same contract as `IndexAssignExprNamed`).
        target_slot: Option<u32>,
        /// True for push/append/unshift/prepend (Raku autovivifies);
        /// false for pop/shift/splice (Raku dies without growing).
        autoviv: bool,
        /// Autovivify a missing element to an empty Hash instead of an
        /// empty Array. Used for the *intermediate* levels of a nested
        /// subscript chain (`%h<a><b>.push`): the fresh container's kind
        /// follows the NEXT subscript (positional → Array, associative →
        /// Hash), while the final level always vivifies an Array.
        viv_hash: bool,
    },

    // -- Assignment as expression --
    AssignExpr(u32),
    /// `.=` metaop on the topic `$_` (`$_ = $_.meth`). Like `AssignExpr` of `_`,
    /// but bypasses the read-only mark a whole-container topic (`given @a`) puts
    /// on `$_` and, for such a topic, writes the reassigned value straight through
    /// to the `@`/`%` source container. The operand (the method result) is on the
    /// stack; the constant index names `_`.
    TopicDotAssign(u32),
    /// Assignment as expression for local variable (indexed slot)
    AssignExprLocal(u32),
    /// Fused compound assignment to a NAMED (env) scalar: `$x OP= rhs`.
    /// The rhs has already been compiled and sits on top of the stack.
    /// Performs a read-modify-write of the named variable (`old OP rhs`),
    /// using an atomic locked RMW when the variable holds a shared
    /// `ContainerRef` cell (Track C cross-thread atomicity), and leaves the
    /// new value on the stack. Emitted only for plain env-named scalars
    /// (local slots and literal `$x = $x + y` are excluded for perf).
    AtomicCompoundVar {
        name_idx: u32,
        op: CompoundBaseOp,
    },
    /// Nested index assignment: `var[outer][inner] = value` (sigil included in name).
    /// `outer_positional` is true if the outer subscript was `[...]` (positional),
    /// false if `{...}` / `<...>` (associative). `inner_positional` is the same
    /// for the inner subscript. Used to decide autovivification kind
    /// (Array vs Hash) for missing intermediate containers.
    IndexAssignExprNested {
        name_idx: u32,
        outer_positional: bool,
        inner_positional: bool,
    },
    /// Deep nested index assignment (3+ levels): @a[i][j][k]... = val
    /// Stack: [value, idx_n (outermost), idx_n-1, ..., idx_1 (innermost)]
    /// `depth` is the total number of subscript levels.
    /// `positional_flags_idx` is a constant index holding a Array of booleans
    /// encoding is_positional for each level from innermost to outermost.
    IndexAssignDeepNested {
        name_idx: u32,
        depth: u32,
        positional_flags_idx: u32,
    },
    /// Generic index assignment on a stack-computed target.
    /// Stack: [target, index, value] → assigns value to target[index].
    /// Supports callframe .my hash writeback for dynamic variables.
    IndexAssignGeneric,
    AssignReadOnly,
    /// Check if a variable is readonly; throw if so (for assignment to readonly params).
    CheckReadOnly(u32),
    /// Mark a variable as readonly (for `:=` binding).
    MarkVarReadonly(u32),

    // -- Loops (compound opcodes) --
    /// While loop. Condition opcodes follow at [ip+1..cond_end).
    /// Body opcodes at [cond_end..body_end). VM loops internally.
    WhileLoop {
        cond_end: u32,
        body_end: u32,
        label: Option<String>,
        collect: bool,
        isolate_topic: bool,
    },
    /// For loop. Iterable value must be on stack.
    /// Body opcodes at [ip+1..body_end). VM iterates internally.
    /// The spec is boxed to keep `size_of::<OpCode>()` small (see `ForLoopSpec`).
    ForLoop(Box<ForLoopSpec>),
    /// Restore the single named for-loop param's prior binding, deferred until
    /// after the loop's LAST/post phasers have run (which must still see the
    /// param bound to its final iteration value). Pairs with the push the
    /// ForLoop opcode performs on normal completion. No-op if nothing pending.
    RestoreForParam,
    /// C-style loop: [cond opcodes][body opcodes][step opcodes].
    /// Layout after CStyleLoop: cond at [ip+1..cond_end), body at [cond_end..step_start),
    /// step at [step_start..body_end).
    CStyleLoop {
        cond_end: u32,
        step_start: u32,
        body_end: u32,
        label: Option<String>,
        collect: bool,
    },

    // -- Given/When/Default (compound opcodes) --
    Given {
        body_end: u32,
        /// When true, the topic (`$_`) is read-only: assigning to it (`$_ = ...`)
        /// must fail. True for every topic except a bare scalar variable
        /// (`given $x { $_ = 9 }` aliases `$x` rw); `given @a`/`given 42`/
        /// `given expr()` are all read-only in Raku (container *mutation* like
        /// `.push` is still allowed and propagates).
        topic_readonly: bool,
        /// For a pointy block (`given @a -> @p { ... }`), the env name of the
        /// parameter aliased to the topic. When set, the topic-source writeback
        /// reads this parameter's final value (instead of `$_`) and writes it
        /// back to the source, so `@p.push` / `@p[0]=v` propagate to `@a`. The
        /// parser desugars `-> @p` into `@p := $_` at the body head and the
        /// compiler records the bound name here. `None` for non-pointy `given`.
        pointy_param_idx: Option<u32>,
    },
    When {
        body_end: u32,
    },
    Default {
        body_end: u32,
    },

    // -- Repeat loop (compound opcode) --
    RepeatLoop {
        cond_end: u32,
        body_end: u32,
        label: Option<String>,
    },

    // -- Environment variable access --
    GetEnvIndex(u32),

    // -- Exists check --
    ExistsEnvIndex(u32),
    ExistsExpr,
    /// Rich :exists adverb with flags.
    /// Stack: [target, index] or [target, index, arg] or [target] (zen).
    /// Flags: bit0=negated, bit1=has_arg, bit2=is_zen,
    ///        bits 4-7=adverb (0=None,1=Kv,2=NotKv,3=P,4=NotP,5=NotV,
    ///                         6=InvalidK,7=InvalidNotK,8=InvalidV)
    ExistsIndexAdv(u32),
    /// Variant of ExistsIndexAdv that knows the array variable name and
    /// consults the deleted-index tracker so `:delete` can report a slot
    /// as missing even though the slot still holds a type-object hole.
    /// Layout: (name_idx, flags) — same flag encoding as ExistsIndexAdv.
    ExistsIndexNamedAdv {
        name_idx: u32,
        flags: u32,
    },

    // -- Reduction ([+] @arr) --
    Reduction(u32),

    // -- Magic variables --
    RoutineMagic,
    BlockMagic,

    // -- Substitution (s///) --
    Subst {
        pattern_idx: u32,
        replacement_idx: u32,
        samecase: bool,
        sigspace: bool,
        samemark: bool,
        samespace: bool,
        global: bool,
        nth_idx: Option<u32>,
        /// Constant-pool index of the raw `:x` spec string (`"3"` / `"1..3"`),
        /// or `None` when `:x` is absent.
        x_idx: Option<u32>,
        perl5: bool,
    },

    // -- Non-destructive substitution (S///) --
    NonDestructiveSubst {
        pattern_idx: u32,
        replacement_idx: u32,
        samecase: bool,
        sigspace: bool,
        samemark: bool,
        samespace: bool,
        global: bool,
        nth_idx: Option<u32>,
        /// Constant-pool index of the raw `:x` spec string (`"3"` / `"1..3"`),
        /// or `None` when `:x` is absent.
        x_idx: Option<u32>,
        perl5: bool,
    },

    // -- Transliteration (tr///) --
    Transliterate {
        from_idx: u32,
        to_idx: u32,
        delete: bool,
        complement: bool,
        squash: bool,
        non_destructive: bool,
    },

    // -- Take (gather/take) --
    Take,

    // -- Package scope --
    PackageScope {
        name_idx: u32,
        body_end: u32,
    },
    /// Register a package name so it's accessible as a Package value.
    RegisterPackage {
        name_idx: u32,
    },
    /// Register a lexically-scoped (`my`) package type object.
    /// Same as RegisterPackage but marks the name as block-declared
    /// so it is cleaned up when the enclosing block scope exits.
    RegisterPackageMy {
        name_idx: u32,
    },
    /// Register a package as a stub (body is `...`, `!!!`, or `???`).
    RegisterPackageStub {
        name_idx: u32,
    },
    /// Clear a package stub when the package is redefined with a real body.
    ClearPackageStub {
        name_idx: u32,
    },

    // -- Phaser --
    /// Register an END phaser. `site_id` ensures register-once semantics
    /// for END phasers inside closures that may be called repeatedly.
    PhaserEnd {
        idx: u32,
        site_id: u64,
    },
    /// Marks the start of a CHECK phaser body. If an error occurs before
    /// the matching `CheckPhaserEnd`, it is wrapped in X::Comp::BeginTime.
    CheckPhaserStart {
        /// IP of the CheckPhaserEnd instruction (jump target on error).
        end_ip: u32,
    },
    /// Marks the end of a CHECK phaser body.
    CheckPhaserEnd,

    // -- HyperMethodCall (».method) --
    HyperMethodCall {
        name_idx: u32,
        arity: u32,
        modifier_idx: Option<u32>,
        quoted: bool,
        /// The lvalue variable name when the hyper target is a plain `@`/`%`
        /// variable (`@a>>++`), so a mutating hyper writes back *precisely* to
        /// that binding (cell-write if bound, COW-detach otherwise) instead of
        /// the Arc-identity scan that over-reaches COW-shared copies. `None` for
        /// non-variable targets (`@b[0]>>++`, `(1,2,3)>>.uc`).
        target_name_idx: Option<u32>,
    },
    HyperMethodCallDynamic {
        arity: u32,
        modifier_idx: Option<u32>,
    },

    // -- HyperOp (>>op<<) --
    HyperOp {
        op_idx: u32,
        dwim_left: bool,
        dwim_right: bool,
    },

    // -- HyperFuncOp (>>[&func]<<) --
    HyperFuncOp {
        name_idx: u32,
        dwim_left: bool,
        dwim_right: bool,
        /// When true, the left operand is a mutable lvalue: bind each element
        /// `rw` so a mutating code-ref (e.g. `&[+=]`) writes back, and push the
        /// (possibly mutated) left value on top of the result so the compiler
        /// can store it back into the lvalue.
        writeback: bool,
    },

    // -- MetaOp (Rop, Xop, Zop) --
    MetaOp {
        meta_idx: u32,
        op_idx: u32,
    },

    // -- List-associative n-ary MetaOp (X/Z chained: `a X b X c`) --
    // Pops `count` operands off the stack and combines them in a single
    // n-ary cross (X) or zip (Z) so the result is flat n-tuples rather than
    // left-nested pairs.
    MetaOpNary {
        meta_idx: u32,
        op_idx: u32,
        count: u32,
    },

    // -- InfixFunc (atan2, sprintf) --
    InfixFunc {
        name_idx: u32,
        right_arity: u32,
        modifier_idx: Option<u32>,
    },
    /// Stateful scalar flip-flop (ff/fff) with lazily evaluated lhs/rhs bytecode spans.
    FlipFlopExpr {
        lhs_end: u32,
        rhs_end: u32,
        site_id: u64,
        exclude_start: bool,
        exclude_end: bool,
        is_fff: bool,
    },

    // -- Exception handling --
    /// Try block layout:
    /// body at [ip+1..catch_start),
    /// catch at [catch_start..control_start),
    /// control at [control_start..body_end).
    TryCatch {
        catch_start: u32,
        control_start: u32,
        body_end: u32,
        /// True when CATCH { } is explicitly present — unhandled exceptions
        /// (no `when`/`default` match) must be re-thrown.
        explicit_catch: bool,
        /// True when this block's CONTROL handler unconditionally `.resume`s
        /// (e.g. `CONTROL { default { ...; .resume } }`) with no `when`/`succeed`
        /// exit. Such a handler can be run *inline* at a deep `warn` raise site
        /// (see `builtin_warn`) without unwinding the Rust call stack, which is
        /// what enables cross-frame resumable warns. Computed at compile time
        /// from the CONTROL block AST (the runtime cannot see the AST).
        resume_safe: bool,
        /// True when this try/catch frame is a genuine bare block statement
        /// (`{ ...; CATCH { } }`) from source. Like `BlockScope::is_bare_block`,
        /// such a block is a callframe and contributes an anonymous backtrace
        /// frame while executing inside it.
        is_bare_block: bool,
    },

    /// Push an anonymous block callframe onto the routine stack. Emitted around a
    /// genuine bare block `{ ... }` that the compiler *inlines* (tail-position
    /// blocks have no `BlockScope`/`TryCatch` boundary to carry the
    /// `is_bare_block` flag), so a backtrace captured while executing inside the
    /// inlined block still shows the block as a frame (Raku callframe semantics).
    /// Paired with `PopBlockFrame` on the normal exit; leaked frames (when the
    /// body throws past the pop) are reclaimed by the enclosing sub/try/block
    /// boundary, which truncates the routine stack to its entry depth.
    PushBlockFrame,
    /// Pop the anonymous block callframe pushed by `PushBlockFrame`.
    PopBlockFrame,

    // -- Error handling --
    Die,
    Fail,

    /// A `has`-attribute declaration that reaches runtime (mainline / EVAL'd
    /// source). Registers the attribute onto the class currently being defined,
    /// or throws the boxed `X::Attribute::*` error when not in a class body.
    RuntimeHasDecl(Box<RuntimeHasDeclSpec>),

    // -- Functions --
    Return,
    /// Return used outside a routine.
    /// The `bool` payload is `true` if the op is lexically nested inside a
    /// routine (a closure/block in a sub) — in that case `return` should
    /// perform a non-local return up to the enclosing routine, and only
    /// become an `X::ControlFlow::Return` exception when no enclosing routine
    /// is on the dynamic call stack (out-of-dynamic-scope).
    /// When `false`, the op is at top level with no lexical routine and
    /// throws `X::ControlFlow::Return` directly.
    ReturnFromNonRoutine(bool),
    RegisterSub(u32),
    RegisterToken(u32),
    RegisterProtoSub(u32),
    RegisterProtoToken(u32),
    RegisterEnum(u32),
    RegisterClass(u32),
    AugmentClass(u32),
    RegisterRole(u32),
    RegisterSubset(u32),
    SubtestScope {
        body_end: u32,
    },
    ReactScope {
        body_end: u32,
    },
    WheneverScope {
        body_idx: u32,
        param_idx: Option<u32>,
        target_var_idx: Option<u32>,
    },
    UseModule {
        name_idx: u32,
        tags_idx: Option<u32>,
    },
    ImportModule {
        name_idx: u32,
        tags_idx: Option<u32>,
    },
    NoModule(u32),
    /// `need Module;` — load module without importing exports.
    NeedModule(u32),
    UseLibPath,
    /// Save current function/class registries for lexical import scoping.
    PushImportScope,
    /// Restore function/class registries to the last saved snapshot.
    PopImportScope,

    // -- Type checking --
    /// Check that the value on top of stack matches the given type constraint.
    /// First u32 is a constant index for the type name string.
    /// Optional second u32 is a constant index for the variable name (for error messages).
    TypeCheck(u32, Option<u32>),

    /// Like TypeCheck, but for `:=` binds to a typed scalar. On a type
    /// mismatch this raises X::TypeCheck::Binding (e.g. `my Str $x := 3`)
    /// instead of X::TypeCheck::Assignment. First u32 is the type name
    /// constant index; optional second u32 is the variable name index.
    TypeCheckBind(u32, Option<u32>),

    /// Set a pragma value. Pops the value from the stack.
    /// The u32 is a constant index for the pragma name.
    SetPragma(u32),

    /// State variable initialization.
    /// slot = local slot index, key_idx = constant index for unique state key.
    /// Pops init value from stack.
    /// If state_vars has key: set locals[slot] = stored value (discard init).
    /// If not: set locals[slot] = init value, store in state_vars.
    StateVarInit(u32, u32),
    /// Guard for state variable initialization.
    /// Check if state key (arg 0) exists.  If yes: push stored value and jump
    /// to the absolute instruction offset (arg 1).  If no: fall through so the
    /// RHS initializer can be compiled next.
    StateVarInitGuard(u32, u32),
    /// Mark whether a declared variable should report `.VAR.dynamic` true.
    SetVarDynamic {
        name_idx: u32,
        dynamic: bool,
    },
    RegisterVarExport {
        name_idx: u32,
        tags_idx: Option<u32>,
    },
    /// Apply a custom variable trait via trait_mod:<is>.
    /// When `has_arg` is true, pops trait argument value from stack.
    /// `slot` is the compile-time-baked local slot of the declared variable
    /// (§1.5; scope-correct under shadow slots). `None` for env-only
    /// expression-position declarations, which have no local slot to bake.
    ApplyVarTrait {
        name_idx: u32,
        trait_name_idx: u32,
        has_arg: bool,
        slot: Option<u32>,
    },

    /// Get a variable from the caller's scope ($CALLER::varname).
    /// name_idx = constant index for the bare variable name (without CALLER:: prefix).
    /// depth = number of CALLER:: levels (1 for $CALLER::x, 2 for $CALLER::CALLER::x).
    GetCallerVar {
        name_idx: u32,
        depth: u32,
    },

    /// Get a variable through `$CALLERS::` — the "any caller scope" twin of
    /// [`GetCallerVar`]. A `$*`-twigil dynamic name cascades outward through the
    /// whole caller chain (`cascade = true`); a plain name resolves to the exact
    /// frame at `depth`, identical to `GetCallerVar` (`cascade = false`).
    GetCallersVar {
        name_idx: u32,
        depth: u32,
        cascade: bool,
    },

    /// Set a variable in the caller's scope ($CALLER::varname = value).
    SetCallerVar {
        name_idx: u32,
        depth: u32,
    },

    /// Bind a variable in the caller's scope to a local variable ($CALLER::target := $source).
    /// This creates an alias so that changes to source are reflected in target.
    BindCallerVar {
        target_idx: u32,
        source_idx: u32,
        depth: u32,
    },

    /// Get a variable from an outer lexical scope ($OUTER::varname).
    /// `depth` indicates how many OUTER:: prefixes (1 = $OUTER::x, 2 = $OUTER::OUTER::x).
    /// `slot` is the emit-point local slot of the binding visible `depth` lexical
    /// scopes out (resolved by walking the compiler's `local_scopes` shadow
    /// records; §1.3 S14). `None` = the name is not a local binding there (it
    /// crosses a frame boundary, or is first declared deeper than the target
    /// scope). Read only under `MUTSU_SHADOW_SLOTS`: with shadow slots a name
    /// occupies several `locals` slots and the runtime's position search always
    /// picks the outermost, wrong for any depth short of the outermost binding.
    GetOuterVar {
        name_idx: u32,
        depth: u32,
        slot: Option<u32>,
    },

    /// Get a variable by searching the dynamic call stack ($DYNAMIC::varname).
    GetDynamicVar(u32),

    /// Indirect type lookup: pop string from stack, resolve to Package value.
    IndirectTypeLookup,

    /// Indirect code lookup: pop package string from stack, resolve &name in that package context.
    IndirectCodeLookup(u32),

    /// Symbolic variable dereference: pop name string from stack, look up variable by sigil+name.
    /// `sigil_idx` indexes the constant pool for the sigil string ("$", "@", or "%").
    /// `scopes_idx` indexes [`CompiledCode::lex_scopes`] for the lexical scope chain
    /// visible at this site, which the popped name needs when it turns out to spell
    /// an `OUTER::` / `OUTERS::` lookup.
    SymbolicDeref {
        sigil_idx: u32,
        scopes_idx: u32,
    },

    /// Symbolic variable dereference store: pop value and name from stack, store value into variable.
    /// The u32 indexes the constant pool for the sigil string ("$", "@", or "%").
    SymbolicDerefStore(u32),

    /// Indirect type lookup store: pop value and name from stack, store value into variable by name.
    IndirectTypeLookupStore,

    /// Save current variable value for `let`/`temp` scope management.
    /// Pops the array index (if index_mode is true) from the stack.
    /// `is_temp`: true for `temp` (always restore), false for `let` (restore on failure only).
    LetSave {
        name_idx: u32,
        index_mode: bool,
        is_temp: bool,
        /// Compiler-baked local slot for the saved variable (§1.4/§1.5): the
        /// scope-exit restore writes `locals[slot]` directly instead of resolving
        /// the name via `find_local_slot` (position = OUTER slot, wrong for a live
        /// inner shadow). `None` for a non-local target (falls back to by-name).
        slot: Option<u32>,
    },

    /// Block with `let` scope management. Executes body, then checks
    /// the topic ($_) to decide whether to restore or discard let saves.
    LetBlock {
        body_end: u32,
    },
}

#[cfg(test)]
mod opcode_size_guard {
    use super::*;
    #[test]
    fn opcode_stays_small() {
        // Every instruction in a `Vec<OpCode>` is padded to the widest variant,
        // so one fat variant taxes the fetch/decode cache locality of ALL code.
        // `ForLoop` used to hold its 21-field spec inline (192 bytes); boxing it
        // brought `OpCode` to 48 bytes (current widest: `SmartMatchExpr`, `Subst`).
        //
        // If this assert fails because you added/widened a variant: do NOT just
        // bump the limit. First try to keep `OpCode` at the current size —
        // `Box` the payload (like `ForLoop(Box<ForLoopSpec>)`), move strings to
        // the constant pool as `u32` indices, or pack flags. Raise the limit
        // only when none of those work, and record the reasoning in
        // docs/opcode-design-review.md.
        let sz = std::mem::size_of::<OpCode>();
        assert!(sz <= 48, "size_of::<OpCode>() = {sz}, expected <= 48");
    }
}

#[cfg(test)]
mod const_pool_dedup {
    use super::*;

    #[test]
    fn equal_scalars_share_a_slot() {
        let mut code = CompiledCode::new();
        let a = code.add_constant(Value::int(42));
        let b = code.add_constant(Value::int(42));
        let s1 = code.add_constant(Value::str("elems".to_string()));
        let s2 = code.add_constant(Value::str("elems".to_string()));
        let other = code.add_constant(Value::int(7));
        assert_eq!(a, b, "the same Int shares one slot");
        assert_eq!(s1, s2, "the same Str shares one slot");
        assert_ne!(a, other);
        assert_eq!(code.constants.len(), 3, "42, \"elems\", 7");
    }

    #[test]
    fn distinct_num_bit_patterns_keep_distinct_slots() {
        let mut code = CompiledCode::new();
        let pos = code.add_constant(Value::num(0.0));
        let neg = code.add_constant(Value::num(-0.0));
        // 0.0 == -0.0 numerically, but they are distinguishable values
        // (1/0.0 vs 1/-0.0), so the pool must not merge them.
        assert_ne!(pos, neg);
    }

    #[test]
    fn identity_bearing_values_are_never_shared() {
        let mut code = CompiledCode::new();
        // Containers have an observable identity (`=:=`), so two equal-looking
        // ones must keep their own slots.
        let a = code.add_constant(Value::array(vec![Value::int(1)]));
        let b = code.add_constant(Value::array(vec![Value::int(1)]));
        assert_ne!(a, b);
    }

    #[test]
    fn finalizing_drops_the_dedup_index() {
        let mut code = CompiledCode::new();
        code.add_constant(Value::int(1));
        assert!(!code.const_index.is_empty());
        code.compute_needs_env_sync();
        assert!(
            code.const_index.is_empty(),
            "the index is compile-time scaffolding"
        );
    }
}

/// A compiled chunk of bytecode.
#[derive(Debug, Clone)]
pub(crate) struct CompiledCode {
    pub(crate) ops: Vec<OpCode>,
    /// Static ip -> source line table, parallel to `ops` (0 = unknown). Replaces
    /// the former per-statement `SetSourceLine` opcode: the line an instruction
    /// belongs to is compile-time data, so it does not need a dispatched
    /// instruction to carry it. The VM reads it with `line_at()` at the points
    /// that can *observe* a line (call/reentry boundaries, error and warning
    /// raise sites) instead of maintaining `cur_source_line` on every statement.
    pub(crate) op_lines: Vec<u32>,
    /// Compile-time cursor: the source line attached to every op emitted from
    /// now on (set by the `Stmt::SetLine` marker). Not used at runtime.
    emit_line: u32,
    pub(crate) constants: Vec<Value>,
    /// Reverse index over `constants` for pool dedup (ADR-0006 §2.4): the same
    /// literal or name string emitted at N sites shares one slot instead of
    /// pushing N copies. Compile-time only — `finalize()` drops it once the
    /// chunk stops growing, so it costs no memory in the executed code.
    const_index: rustc_hash::FxHashMap<ConstKey, u32>,
    pub(crate) stmt_pool: Vec<Stmt>,
    pub(crate) locals: Vec<String>,
    /// Pre-interned Symbol for each local name. Avoids Symbol::intern()
    /// on every env sync in hot paths.
    pub(crate) locals_sym: Vec<Symbol>,
    /// Pre-interned Symbol of the `__mutsu_sigilless_alias::<name>` env key for
    /// each local. The scalar-assignment hot path probes that key on EVERY store
    /// (to propagate the new value to a `:=` alias target); building the key with
    /// `format!` per store cost a String allocation plus a `Symbol::intern` string
    /// hash, which profiled as ~19% of bench-mandelbrot. Interned once here, the
    /// probe is a plain `Env::get_sym`.
    pub(crate) locals_alias_sym: Vec<Symbol>,
    /// Pre-interned Symbol of the `__mutsu_sigilless_readonly::<name>` env key
    /// for each local — the readonly half of the pair described above, probed on
    /// every assignment for the same reason.
    pub(crate) locals_readonly_sym: Vec<Symbol>,
    /// Pre-interned Symbols of the two per-variable metadata keys a `my`
    /// DECLARATION speculatively clears (`__mutsu_deleted_index::<name>` and
    /// `__mutsu_bound_array_slice::<name>`), so a redeclaration cannot inherit an
    /// earlier same-named variable's state. Both keys are almost never present,
    /// but the clears ran per declaration — in a loop body (`my $t = ...`) that
    /// is once per iteration.
    pub(crate) locals_deleted_index_sym: Vec<Symbol>,
    pub(crate) locals_bound_slice_sym: Vec<Symbol>,
    /// Bitmap: true if local[i] is a *plain lexical* name — the sigil-less form
    /// the compiler stores scalars under (`my $x` -> `"x"`, a scalar param
    /// `$n` -> `"n"`), with no twigil (`*d`, `^a`), no attribute (`.x`, `!x`),
    /// no `@`/`%`/`&` sigil, no `::` qualifier, not the topic `_`, and not a
    /// compiler-internal name (`__mutsu_*`, `__ANON*`).
    ///
    /// Such a name has none of the aliases the by-name env writer
    /// (`set_env_with_main_alias`) exists to maintain — no `$*d`/`*d` twigil
    /// pair, no `&infix:<+>` operator alias, no `Main::`/`GLOBAL::`/`OUR::`
    /// qualification — so its env mirror is a single Symbol-keyed insert. The
    /// predicate is a scan of the name's bytes, so it is computed once here
    /// rather than on every store (`flush_local_to_env` runs on each `my $x =
    /// ...`).
    pub(crate) plain_locals: Vec<bool>,
    /// Maps local slot indices to persistent state keys for `state` variables.
    pub(crate) state_locals: Vec<(usize, String)>,
    /// Maps local slot indices to qualified package names for `our` variables.
    /// Used by BlockScope restoration to sync local slots from their global values.
    pub(crate) our_locals: Vec<(usize, String)>,
    /// Scalar locals declared with a `:=` bind (`my $x := EXPR`). Such a binding
    /// is immutable — the local is never reassigned — so its captured snapshot in
    /// a closure can never go stale, even when the local is also handed to a call
    /// (which would otherwise veto it from `authoritative_free_vars` for fear of an
    /// `is rw` writeback). See the `own_call_arg_sources` exception in
    /// `compute_free_vars`.
    pub(crate) scalar_bind_locals: Vec<Symbol>,
    /// Compiler-authoritative positional-parameter → local-slot mapping, in the
    /// order `precompute_param_local_slots` expects (positional `param_defs`, or
    /// `params` when `param_defs` is empty). Baked at emit time from the
    /// compiler's `local_map` so `CompiledFunction::precompute_param_local_slots`
    /// does not have to re-resolve parameter names by searching `locals` (§1.5:
    /// remove name→slot runtime resolution). Empty when the compiler did not
    /// record it (e.g. hand-built `CompiledCode::new()` chunks), in which case
    /// precompute falls back to the by-name search.
    pub(crate) param_local_slots: Vec<u32>,
    /// Out-of-band lexical scope chains for `SymbolicDeref` sites (indexed by the
    /// op's `scopes_idx`). `$::($name)::x` can only be recognised as an `OUTER::`
    /// lookup once the name string exists, by which time the compile-time scope
    /// shape it must be answered against is gone — so the emit point bakes it here.
    /// See [`crate::compiler::lex_scope::LexScopeChain`].
    pub(crate) lex_scopes: Vec<Arc<crate::compiler::lex_scope::LexScopeChain>>,
    /// Pre-compiled closure bodies embedded in this code chunk.
    pub(crate) closure_compiled_codes: Vec<Arc<CompiledCode>>,
    /// Own local slots that reach an atomic-op builtin (`⚛$x`, `$x ⚛= v`,
    /// `cas($x, …)`) as the target VARIABLE. These builtins are compiled to a
    /// `__mutsu_*_var(name_str, …)` call and resolve the target by NAME from env
    /// (`atomic_current_value` falls back to `env.get(name)` for a non-`atomicint`
    /// scalar). Under `MUTSU_GATE_LOCAL_ENV_WRITE` a plain `my Int $x = 1` skips
    /// its env mirror, so the builtin reads the decl-seed placeholder. Consumed
    /// ONLY by the gated `compute_needs_env_sync` fold, which marks these slots
    /// env-synced; the default build never reads this field (byte-identical).
    pub(crate) atomic_env_sync_locals: Vec<u32>,
    /// Out-of-band named-argument specs for `CallFuncNamed` sites (indexed by
    /// the op's `spec_idx`): which of the call's stack values are named-arg
    /// VALUES and under which keys. Lets a literal `:key(val)` call site skip
    /// the per-call Pair boxing.
    pub(crate) named_arg_specs: Vec<Arc<NamedArgsSpec>>,
    /// Parallel to `closure_compiled_codes`: `closure_escapes[i]` is true if the
    /// i-th child closure was created in an *escaping position* — its value is
    /// stored/returned/bound (assignment or `:=` RHS, `return`/`fail` operand,
    /// block tail, or a literal element) rather than immediately invoked (a call
    /// argument like `lives-ok {...}` / `map {...}`, or a control-construct
    /// block). Consumed by `compute_free_vars` to decide which captured-and-
    /// mutated own-locals need a shared `ContainerRef` cell (escape analysis,
    /// replacing the old `>=2 sibling closures` proxy).
    pub(crate) closure_escapes: Vec<bool>,
    /// Whether this compiled code represents a Routine (sub/method) as opposed
    /// to a Block (bare block / pointy block).  `return` signals are caught
    /// only at routine boundaries, allowing pointy-block returns to propagate
    /// up to the enclosing routine.
    pub(crate) is_routine: bool,
    /// Source line number (1-based) where this closure/block was defined.
    pub(crate) source_line: Option<i64>,
    /// Whether this compiled code represents a pointy block (`-> { }` / `<-> { }`).
    /// Pointy blocks are NOT routine boundaries — `return` propagates through
    /// them to the enclosing routine, and `&?ROUTINE` sees the enclosing routine.
    pub(crate) is_pointy_block: bool,
    /// Whether this code contains opcodes that write to env (SetGlobal,
    /// AssignExpr, PostIncrement, etc.). Used by call_compiled_method to
    /// skip the expensive env merge when the method body is read-only.
    pub(crate) has_env_writes: bool,
    /// Whether this code reads/writes outer-scope variables via GetGlobal
    /// that are NOT method-local (attributes, params, special vars).
    /// When true, the fast method path cannot use a fresh env.
    pub(crate) may_capture_outer_vars: bool,
    /// Bitmap: true if local[i] needs to be synced to env (because it's
    /// referenced by GetGlobal/SetGlobal in this code or closures exist).
    /// Locals that are only accessed via GetLocal don't need env sync,
    /// reducing env size and clone cost.
    pub(crate) needs_env_sync: Vec<bool>,
    /// Bitmap: true if local[i]'s NAME occupies more than one `locals` slot —
    /// a genuine inner-block shadow under the `MUTSU_SHADOW_SLOTS` gate (§1.4).
    /// The name-keyed env can hold only ONE value per name, so the whole-locals
    /// env broadcast (`sync_env_from_locals` and the regex-interpolation sync)
    /// must skip these slots: pushing an arbitrary (last-iterated) same-named
    /// slot clobbers the live value with an uninitialized/stale sibling's. The
    /// per-write mirror (`flush_local_to_env`) keeps env tracking the live
    /// slot's writes instead. With the gate off `alloc_local` get-or-creates by
    /// name, so names are unique and this is all-false (byte-identical).
    pub(crate) dup_named_locals: Vec<bool>,
    /// Names `my`-declared (or `constant`-declared) in THIS code's body — the
    /// block's own fresh lexical bindings. The closure-exit caller-writeback
    /// scan must not propagate them to a same-named caller lexical: with the
    /// flattened env a `.map({ my $spec = ... })` inside a method otherwise
    /// clobbers the calling method's `$spec` parameter on block exit (how
    /// zef's `provides-spec-matcher` corrupted `contains-spec` and dropped
    /// JSON::OptIn from the prereq list). A declared name that is ALSO a free
    /// var (used before its declaration, so it refers to the outer binding)
    /// keeps the writeback.
    pub(crate) my_declared_sym: rustc_hash::FxHashSet<Symbol>,
    /// Free variables this code (and its nested closures) reference from an
    /// enclosing scope: names used via GetGlobal-family ops that are not this
    /// code's own locals. For a closure body this is the set of captured
    /// lexicals whose per-instance mutable state actually matters, so the
    /// closure-call path can persist/restore only these instead of iterating
    /// the entire (~100-entry) captured env. Empty until `compute_free_vars`
    /// runs (during `compute_needs_env_sync`).
    pub(crate) free_var_syms: Vec<Symbol>,
    /// §1.3 closure-capture slot bake: parallel to `free_var_syms`, the CREATING
    /// frame's compile-time local slot for each free variable (`local_map` at the
    /// closure's emit point, baked by `Compiler::add_closure_code_baked`), or
    /// `None` when the name is not a local there (an ancestor lexical / global).
    /// Under `MUTSU_SHADOW_SLOTS` a name can occupy several creator slots, so the
    /// runtime capture paths (`capture_closure_env`, `capture_upvalues`,
    /// `box_captured_lexicals`) must resolve the emit-point slot, not an
    /// `rposition` name search (which always picks the innermost shadow, wrong
    /// for a closure created outside that shadow's block). Read only when shadow
    /// slots are active; empty for hand-built chunks (falls back to the name
    /// search).
    pub(crate) free_var_parent_slots: Vec<Option<u32>>,
    /// Parallel to `upvalue_syms`: the creating frame's compile-time local slot
    /// for each upvalue, baked exactly like `free_var_parent_slots` (see there).
    pub(crate) upvalue_parent_slots: Vec<Option<u32>>,
    /// Bare names this code reads through an `$OUTER::` reference (`$OUTER::x` →
    /// `x`). Populated by `compute_free_vars` from `GetOuterVar` ops. The closure
    /// snapshots each such name's captured enclosing value under a reserved
    /// `__mutsu_outer::<name>` env key so `get_outer_var` can resolve it even after
    /// the running frame overwrites the plain name (e.g. a fresh topic `$_`).
    pub(crate) outer_ref_names: Vec<String>,
    /// Free variables (names not in this code's own locals) that this code or a
    /// nested closure *writes* (assign / inc-dec / bind). Folded up from nested
    /// closures so an enclosing scope can tell which of *its* locals are mutated
    /// from inside a closure. Used to compute `captured_mutated_locals`.
    pub(crate) free_var_writes: Vec<Symbol>,
    /// Free `@`/`%` container variables this code mutates IN PLACE (via a mutating
    /// method like `push`/`append`, or an element/index assignment) without ever
    /// rebinding the whole container by name. Such mutations are NOT `SetGlobal`
    /// name-writes, so they never appear in `free_var_writes`; this set captures
    /// them separately so a nested named sub that mutates a captured outer
    /// container (e.g. a user `trait_mod:<is>` pushing to an outer `@names`) can
    /// have that container boxed into a shared `ContainerRef` cell at its
    /// declaration site (see `needs_cell_named_sub` / box_decl_local_cell and
    /// docs/captured-outer-cell-sharing.md §7.2). Kept SEPARATE from
    /// `free_var_writes` so it never perturbs the default-build precise-writeback
    /// drain (which keys on `free_var_writes`); it only feeds the gated cell
    /// boxing.
    pub(crate) free_var_container_writes: Vec<Symbol>,
    /// Write contributions of directly-nested *named subs* (declared in this
    /// scope), each a `(free_var_writes, needs_cell_named_sub_free)` pair copied
    /// from the sub's finalized `CompiledCode`. A named sub is always reachable
    /// (callable any time after declaration) and — unlike a closure — has no
    /// runtime creation op (`RegisterSub` is hoisted to the top of the scope,
    /// before the captured local is even declared). So `compute_free_vars` uses
    /// these to compute `needs_cell_named_sub`, and the VM boxes those locals into
    /// a shared cell at their *declaration site* (see
    /// docs/captured-outer-cell-sharing.md), letting `via(); via()` accumulate
    /// through a shared cell instead of the `env_dirty` blanket reconcile. Kept
    /// SEPARATE from the closure-driven `needs_cell_locals`: closures are boxed
    /// precisely at their creation op (`box_captured_lexicals`, scoped to the exact
    /// captured slot), whereas named-sub boxing happens at the declaration site, so
    /// it must only fire for locals a named sub actually *writes* — never for an
    /// unrelated same-named local in a sibling block (which would wrongly box e.g.
    /// a `let`-restored variable; same-named `my` locals share one slot).
    pub(crate) named_sub_captures: Vec<(Vec<Symbol>, Vec<Symbol>)>,
    /// Own locals that a directly-nested named sub WRITES (computed from
    /// `named_sub_captures`). The VM boxes these into a shared `ContainerRef` cell
    /// at their declaration site (`box_decl_local_cell`). Distinct from
    /// `needs_cell_locals` (closure-driven) — see `named_sub_captures`.
    pub(crate) needs_cell_named_sub: Vec<Symbol>,
    /// Named-sub writes of a NON-own (ancestor) lexical, bubbled up so the ancestor
    /// that declares the local folds it into its own `needs_cell_named_sub`
    /// (mirrors `needs_cell_free_vars` for closures).
    pub(crate) needs_cell_named_sub_free: Vec<Symbol>,
    /// Free-variable captures of directly-nested *`our`-scoped* named subs. Unlike
    /// a plain `my sub`, an `our sub` is installed into the package registry and
    /// stays callable *after* its declaring block exits, but a registry routine has
    /// no per-sub closure env. So the lexicals it READS (not just writes) must be
    /// boxed into a shared `ContainerRef` cell at their declaration site AND
    /// persisted into `Interpreter::escaped_our_lexical_cells`, so a call made after
    /// the block reads the live cell instead of `Nil`. Each entry is one our-sub's
    /// full free-var set (reads ∪ writes ∪ bubbled ancestor cell-needs). Computed
    /// into `needs_cell_escaping_our_sub` / `_free` by `compute_free_vars`.
    pub(crate) escaping_our_sub_captures: Vec<Vec<Symbol>>,
    /// Own locals captured (read or written) by a directly-nested `our`-scoped named
    /// sub. The VM boxes these at their declaration site and persists the cell so the
    /// escaped sub resolves them after the block exits. See `escaping_our_sub_captures`.
    pub(crate) needs_cell_escaping_our_sub: Vec<Symbol>,
    /// Escaping-our-sub captures of a NON-own (ancestor) lexical, bubbled up so the
    /// ancestor that declares the local folds it into its own
    /// `needs_cell_escaping_our_sub` (mirrors `needs_cell_named_sub_free`).
    pub(crate) needs_cell_escaping_our_sub_free: Vec<Symbol>,
    /// Own locals that are BOTH captured by a nested closure AND mutated after
    /// their declaration (reassigned/inc-dec in this scope, or written from
    /// inside a nested closure). Such a local must be a shared container so the
    /// closure observes the mutation and sibling closures share one cell (Raku
    /// "a closure captures the container"). The VM boxes these into a
    /// `ContainerRef` at closure-capture time (see `box_captured_lexicals`).
    /// Declaration-only / read-only captures are excluded on purpose: boxing
    /// them is unnecessary and trips ContainerRef-unaware paths.
    pub(crate) captured_mutated_locals: Vec<Symbol>,
    /// Subset of `captured_mutated_locals` captured by at least one child closure
    /// whose value **escapes** the creating frame (its `closure_escapes` bit is
    /// set — stored/returned/bound rather than immediately invoked). These
    /// genuinely need a shared `ContainerRef` cell so the escaping closure (and
    /// any siblings) observe mutations even after the declaring frame exits
    /// (Phase 1 / lever C, non-loop case). The VM boxes these regardless of loop
    /// context (see `box_captured_lexicals`). This escape signal replaces the old
    /// `>=2 distinct sibling closures` proxy: it both subsumes the sibling
    /// getter+setter case (both are assigned, so both escape) AND fixes the
    /// single escaping closure (`&f = sub {...}`) that the >=2 proxy missed —
    /// while keeping immediately-invoked closures (`lives-ok {...}` / `map {...}`,
    /// call args / control blocks) non-boxed, avoiding the broad-boxing
    /// perf/correctness regression (see #2749).
    pub(crate) needs_cell_locals: Vec<Symbol>,
    /// The subset of this code's own `free_var_syms` whose captured value is
    /// **authoritative**: the CREATING frame declares them as plain lexicals and
    /// provably never mutates them after this closure captured them, so the
    /// by-value snapshot in the closure's env can never go stale.
    ///
    /// Baked by the creator's `compute_free_vars` (which is the only place that
    /// knows its own `captured_mutated` set) into each nested closure it embeds.
    ///
    /// The closure dispatch installs exactly these with OVERWRITE semantics, so a
    /// same-named lexical in whatever frame happens to be calling can no longer
    /// shadow the closure's own binding. Everything else — a capture the creator
    /// mutates (its snapshot may be stale, so the live value must come from the
    /// shared cell or the env chain), a free var inherited from an ancestor rather
    /// than declared by the creator, and all non-plain-lexical names (dynamics,
    /// the topic, `self`, `__mutsu_*`) — keeps the don't-overwrite merge.
    pub(crate) authoritative_free_vars: Vec<Symbol>,
    /// Locals whose own `my $x = ...` *initializer* creates a closure that
    /// captures `$x` itself — the self-recursive closure (`my $f = -> $n { ...
    /// $f($n-1) ... }`). The capture op runs BEFORE the declaration's store, so
    /// the closure snapshots `$x` while it is still unset; only a shared cell can
    /// carry the value the store is about to write. They are therefore boxed (the
    /// store-after-capture rule adds them to `captured_mutated_locals`), and the
    /// declaration's usual "clear the stale ContainerRef, this is a fresh binding"
    /// step must be SKIPPED for them — that step exists for a loop redeclaration
    /// re-boxing a *previous iteration's* cell, but here the cell was boxed by
    /// this very declaration's initializer, so clearing it orphans the closure's
    /// capture and `$f` reads back as `Any`.
    pub(crate) self_capture_decl_locals: Vec<Symbol>,
    /// `&`-sigiled lexicals (params like `&x1`, `my &f = ...`) visible in the
    /// ENCLOSING scopes at this closure's definition point, threaded down by
    /// `compile_closure_body` (transitively, so a grandchild still sees an
    /// ancestor's `&`-param). A bare call `x1(...)` records the callee only as a
    /// call opcode's name constant — there is no separate read op — yet at
    /// runtime it resolves against the lexical `&x1` before the global function
    /// registry. `compute_free_vars` uses this set to decide which callee names
    /// are really code-variable reads that must be captured: registering EVERY
    /// called name would bloat `free_var_syms` with `&say` etc. on every closure
    /// (a per-call `free_at_entry` cost), so only names matching a declared
    /// `&`-lexical count.
    pub(crate) outer_code_var_names: std::collections::HashSet<String>,
    /// Free variables (names NOT in this code's own locals) that must become a
    /// shared `ContainerRef` cell in whichever *ancestor* frame declares them,
    /// because they are captured-and-mutated by an ESCAPING closure somewhere in
    /// this code's closure subtree. This bubbles the escape signal up through
    /// intermediate NON-escaping closures (e.g. a `map {...}` block — itself an
    /// immediately-invoked call arg — that contains `start { $outer++ }`): the
    /// `start` escapes, so `$outer` needs a cell, but the enclosing `map` block
    /// doesn't escape and would otherwise hide that requirement. The ancestor
    /// that owns the local folds these into its own `needs_cell_locals`
    /// (see `compute_free_vars`).
    pub(crate) needs_cell_free_vars: Vec<Symbol>,
    /// True if this code contains any call opcode (function/method/closure
    /// invocation). Set during `emit()`. The closure exit-writeback skip uses
    /// this as the "is this a leaf closure" test: a non-leaf closure may have a
    /// nested call write back an arbitrary captured variable, so it cannot skip
    /// the caller writeback even when its own free variables are unchanged.
    /// Distinct from `has_env_writes`, which lists only *some* call opcodes.
    pub(crate) has_calls: bool,
    /// True if this code object directly contains a `once { ... }` (`OnceExpr`).
    /// Set during `emit()`. Callers use it to keep `once`-bearing routines off the
    /// fast/light call paths, which skip the routine clone-id setup the `once`
    /// store keys on (see `once_scope_key`).
    pub(crate) has_once: bool,
    /// True if this code runs an inline body that reads the frame's lexicals *by
    /// name* through a path the `free_var_syms` op-scan cannot see: loop/block
    /// bodies that thread control temps through `env` by name (`ForLoop`,
    /// `BlockScope`, `BlockLocalScope`), and the two ops that stash a body in the
    /// `stmt_pool` and compile/run it at runtime against the live env
    /// (`MakeGather`, `WheneverScope`). For such a frame `free_var_syms` is
    /// *incomplete*, so two consumers fall back to the whole env: the dual-store
    /// flush blanket (`compute_needs_env_sync`) marks every local env-synced, and
    /// the closure upvalue capture (`capture_closure_env`, single-store Slice E)
    /// snapshots the whole env instead of just the free vars. Set during
    /// `compute_needs_env_sync`.
    pub(crate) captures_env_by_name: bool,
    /// Compile-time identity fingerprint for each `Stmt::SubDecl` in `stmt_pool`,
    /// keyed by its pool index. A `RegisterSub(idx)` opcode re-executes whenever
    /// its enclosing frame runs (e.g. a `my sub` inside a hot routine), but the
    /// declaration it installs is constant. The fingerprint lets the registrar
    /// recognize an idempotent re-registration in O(1) — without re-deriving the
    /// `FunctionDef` from the AST — and skip perturbing the resolution caches.
    /// Computed once here at compile time (see `add_stmt`).
    pub(crate) sub_fingerprints: std::collections::HashMap<u32, u64>,
    /// Ordered list of this closure's read-only plain-lexical free variables that
    /// have been promoted to index-based upvalues. Index `i` in this list is the
    /// operand of the `GetUpvalue(i)` ops that `compute_upvalues` rewrites in
    /// `ops`, and the slot the runtime upvalue array (`SubData::upvalues` /
    /// `Interpreter::upvalues`) is built against. Empty for non-closure code and
    /// for closures with no upvalue-eligible free variables. Populated by
    /// `compute_upvalues` (called only for anonymous-closure bodies).
    pub(crate) upvalue_syms: Vec<Symbol>,
    /// Names declared with `my` in this body that have NO compiled local slot —
    /// i.e. env-only lexicals emitted via `SetVarDynamic` (e.g. a `my @x`
    /// declared inside a statement-modifier condition like
    /// `next unless my @x = ...`). The scoped-overlay return merges exclude
    /// slotted locals via `code.locals` and the light path also excludes these
    /// via `CompiledFunction::declared_locals`; the method-dispatch merge
    /// (`merge_method_env`) has no `CompiledFunction`, so it consults this set to
    /// avoid leaking a callee's env-only `my` back into a same-named caller
    /// lexical across (self-)recursion. Populated by `compute_needs_env_sync`.
    pub(crate) env_only_decls: Vec<String>,
    /// Lazily-built `Symbol` per constant-pool slot (see `const_sym`). Sized on
    /// first use; each slot interns on first access. Cloning a chunk clones the
    /// already-resolved entries (cheap: `Symbol` is a `u32`).
    pub(crate) const_syms: std::sync::OnceLock<Box<[std::sync::OnceLock<Symbol>]>>,
    /// Lazily-built attribute-cell key per local slot (see `local_attr_key`):
    /// `Some((bare attribute Symbol, is_private))` when the slot's name is an
    /// attribute twigil (`!x`, `$.x`, `@!a`, …), `None` otherwise. Resolving it
    /// once per chunk keeps the twigil string parse *and* `Symbol::intern` off
    /// the per-access `$!x` / `$.x` read-write path (ADR-0006 §2.4).
    pub(crate) local_attr_keys: std::sync::OnceLock<LocalAttrKeys>,
    /// Per-chunk JIT hotness counter and compiled-entry cache (ADR-0004 J1).
    pub(crate) jit: JitCodeState,
}

/// The per-local-slot attribute-key table of a chunk (see
/// [`CompiledCode::local_attr_keys`]): one entry per slot, `Some((bare attribute
/// `Symbol`, is_private))` for an attribute twigil and `None` otherwise.
pub(crate) type LocalAttrKeys = Box<[Option<(Symbol, bool)>]>;

/// JIT hotness/entry state carried on each `CompiledCode` (ADR-0004 layer 4).
/// `entry` caches the compiled native entry so the per-call cost once compiled
/// is a single atomic load: 0 = cold (counting), `JIT_ENTRY_BAILOUT` = chunk
/// contains an unsupported opcode (never retry), any other value = the native
/// function pointer. Cloning a chunk resets the state — a clone is a distinct
/// compilation identity (the global fingerprint cache still avoids recompiles).
#[derive(Debug, Default)]
pub(crate) struct JitCodeState {
    pub(crate) calls: std::sync::atomic::AtomicU32,
    pub(crate) entry: std::sync::atomic::AtomicU64,
    /// Per-sub-range JIT state for compound-loop bodies (ADR-0004 J4b
    /// hot-loop entry): `(start, end) -> state`, resolved and populated by
    /// `vm_jit::try_enter_range` on each `run_range` call once the JIT is on.
    /// A linear-scan Vec: chunks hold only a handful of distinct hot ranges.
    pub(crate) ranges: std::sync::Mutex<JitRangeTable>,
    /// Lock-free read cache over `ranges` for *settled* sub-ranges (ADR-0004
    /// J4d): once a range's entry word is final (a compiled function pointer
    /// or `JIT_ENTRY_BAILOUT`), it is published here as a write-once
    /// `(key, entry)` pair so the per-iteration `run_range` hook resolves it
    /// with a couple of atomic loads instead of a mutex + linear scan (which
    /// profiled at ~12% of a hot numeric loop). `key` packs
    /// `(start << 32 | end) + 1` (never 0, so 0 = empty slot); the writer
    /// stores `entry` before releasing `key`, and readers acquire `key` first,
    /// so a visible key implies a visible entry. Slots are claimed under the
    /// `ranges` mutex; ranges beyond the fixed capacity simply stay on the
    /// mutex path (correct, just slower).
    pub(crate) range_cache: [(std::sync::atomic::AtomicU64, std::sync::atomic::AtomicU64); 4],
}

/// The per-chunk range table: `(start, end)` keys to shared range states.
pub(crate) type JitRangeTable = Vec<((u32, u32), std::sync::Arc<JitRangeState>)>;

/// Hotness counter and compiled-entry cache for one `[start, end)` opcode
/// sub-range (a compound loop's body/cond), same encoding as the chunk-level
/// `JitCodeState` (`entry`: 0 = cold, `JIT_ENTRY_BAILOUT` = rejected, other =
/// native function pointer).
#[derive(Debug, Default)]
pub(crate) struct JitRangeState {
    pub(crate) calls: std::sync::atomic::AtomicU32,
    pub(crate) entry: std::sync::atomic::AtomicU64,
}

impl Clone for JitCodeState {
    fn clone(&self) -> Self {
        Self::default()
    }
}

/// Dedup key for the constant pool (ADR-0006 §2.4).
///
/// Only *value-identical-is-indistinguishable* scalars are keyed: two `Str`
/// constants with the same text are interchangeable, whereas a container or an
/// Instance has an observable identity and must keep its own slot (they simply
/// get no key and are always pushed).
///
/// `Num` is keyed by its bit pattern, so `0.0` and `-0.0` stay distinct slots
/// and NaN never dedups against anything (`to_bits` of two NaNs may differ,
/// and a NaN key would never be looked up by an equal key anyway).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ConstKey {
    Int(i64),
    Num(u64),
    Str(Arc<String>),
    Bool(bool),
    Rat(i64, i64),
}

impl ConstKey {
    fn of(value: &Value) -> Option<Self> {
        match value.view() {
            ValueView::Int(i) => Some(ConstKey::Int(i)),
            ValueView::Num(n) => Some(ConstKey::Num(n.to_bits())),
            ValueView::Str(s) => Some(ConstKey::Str(Arc::clone(&s))),
            ValueView::Bool(b) => Some(ConstKey::Bool(b)),
            ValueView::Rat(n, d) => Some(ConstKey::Rat(n, d)),
            _ => None,
        }
    }
}

impl CompiledCode {
    pub(crate) fn new() -> Self {
        Self {
            ops: Vec::new(),
            op_lines: Vec::new(),
            emit_line: 0,
            constants: Vec::new(),
            const_index: rustc_hash::FxHashMap::default(),
            stmt_pool: Vec::new(),
            locals: Vec::new(),
            locals_sym: Vec::new(),
            locals_alias_sym: Vec::new(),
            locals_readonly_sym: Vec::new(),
            locals_deleted_index_sym: Vec::new(),
            locals_bound_slice_sym: Vec::new(),
            plain_locals: Vec::new(),
            state_locals: Vec::new(),
            our_locals: Vec::new(),
            scalar_bind_locals: Vec::new(),
            param_local_slots: Vec::new(),
            lex_scopes: Vec::new(),
            closure_compiled_codes: Vec::new(),
            atomic_env_sync_locals: Vec::new(),
            named_arg_specs: Vec::new(),
            closure_escapes: Vec::new(),
            is_routine: false,
            has_once: false,
            source_line: None,
            is_pointy_block: false,
            has_env_writes: false,
            may_capture_outer_vars: false,
            needs_env_sync: Vec::new(),
            dup_named_locals: Vec::new(),
            my_declared_sym: rustc_hash::FxHashSet::default(),
            free_var_syms: Vec::new(),
            free_var_parent_slots: Vec::new(),
            upvalue_parent_slots: Vec::new(),
            outer_ref_names: Vec::new(),
            free_var_writes: Vec::new(),
            free_var_container_writes: Vec::new(),
            named_sub_captures: Vec::new(),
            needs_cell_named_sub: Vec::new(),
            needs_cell_named_sub_free: Vec::new(),
            escaping_our_sub_captures: Vec::new(),
            needs_cell_escaping_our_sub: Vec::new(),
            needs_cell_escaping_our_sub_free: Vec::new(),
            captured_mutated_locals: Vec::new(),
            needs_cell_locals: Vec::new(),
            authoritative_free_vars: Vec::new(),
            self_capture_decl_locals: Vec::new(),
            outer_code_var_names: std::collections::HashSet::new(),
            needs_cell_free_vars: Vec::new(),
            has_calls: false,
            captures_env_by_name: false,
            sub_fingerprints: std::collections::HashMap::new(),
            upvalue_syms: Vec::new(),
            env_only_decls: Vec::new(),
            const_syms: std::sync::OnceLock::new(),
            local_attr_keys: std::sync::OnceLock::new(),
            jit: JitCodeState::default(),
        }
    }

    /// The attribute-cell key of local slot `idx`, or `None` when that slot is
    /// not an attribute twigil. Built once per chunk (see `local_attr_keys`): the
    /// VM's `$!x` / `$.x` read and write paths would otherwise re-parse the
    /// twigil and re-intern the bare name on every access.
    pub(crate) fn local_attr_key(&self, idx: usize) -> Option<(Symbol, bool)> {
        let slots = self.local_attr_keys.get_or_init(|| {
            self.locals
                .iter()
                .map(|name| {
                    crate::value::attr_twigil_base(name)
                        .map(|(bare, is_private)| (Symbol::intern(bare), is_private))
                })
                .collect()
        });
        slots.get(idx).copied().flatten()
    }

    /// The `Symbol` for the string constant at `idx`, interned once per slot
    /// via a lazily-built side table. Keeps `Symbol::intern` (a thread-local
    /// hash lookup) off the per-call dispatch path: method names are string
    /// constants that would otherwise be re-interned on every `CallMethod`.
    pub(crate) fn const_sym(&self, idx: u32) -> Symbol {
        let resolve = |i: usize| match self.constants[i].view() {
            ValueView::Str(s) => Symbol::intern(s.as_str()),
            _ => unreachable!("expected string constant"),
        };
        let slots = self.const_syms.get_or_init(|| {
            (0..self.constants.len())
                .map(|_| std::sync::OnceLock::new())
                .collect()
        });
        match slots.get(idx as usize) {
            Some(slot) => *slot.get_or_init(|| resolve(idx as usize)),
            // A constant appended after the table was sized (compile-time
            // chunks are finalized before execution, so this is defensive):
            // fall back to a plain intern.
            None => resolve(idx as usize),
        }
    }

    /// Scan opcodes to detect if this code references outer-scope variables
    /// that aren't method-local (attributes, params, or special vars).
    pub(crate) fn compute_may_capture_outer_vars(&mut self) {
        let locals_set: std::collections::HashSet<&str> =
            self.locals.iter().map(|s| s.as_str()).collect();
        for op in &self.ops {
            let name_idx = match op {
                OpCode::GetGlobal(idx)
                | OpCode::SetGlobal(idx)
                | OpCode::SetGlobalRaw(idx)
                | OpCode::PostIncrement(idx, _)
                | OpCode::PostDecrement(idx, _)
                | OpCode::PreIncrement(idx, _)
                | OpCode::PreDecrement(idx, _)
                | OpCode::GetArrayVar(idx)
                | OpCode::GetHashVar(idx) => Some(*idx),
                OpCode::AssignExpr(idx) | OpCode::TopicDotAssign(idx) => Some(*idx),
                _ => None,
            };
            if let Some(idx) = name_idx
                && let Some(ValueView::Str(name)) =
                    self.constants.get(idx as usize).map(Value::view)
            {
                let name = name.as_str();
                if locals_set.contains(name) {
                    continue;
                }
                // Skip known method-specific/internal names
                if Self::is_non_lexical_name(name) {
                    continue;
                }
                self.may_capture_outer_vars = true;
                return;
            }
        }
    }

    /// The interned name of local `idx`. Served from the pre-interned table; a
    /// hand-built chunk that never ran `compute_locals_sym` falls back to
    /// interning on the spot, so a by-Symbol slot match is never silently missed.
    pub(crate) fn local_sym(&self, idx: usize) -> Option<Symbol> {
        match self.locals_sym.get(idx) {
            Some(sym) => Some(*sym),
            None => self.locals.get(idx).map(|n| Symbol::intern(n)),
        }
    }

    /// Pre-intern all local names as Symbols.
    /// The interned `__mutsu_sigilless_alias::<name>` env key of local `idx`.
    /// Served from the pre-interned table; a hand-built chunk that never ran
    /// `compute_locals_sym` falls back to interning it on the spot, so the probe
    /// is never silently skipped.
    pub(crate) fn alias_sym(&self, idx: usize) -> Option<Symbol> {
        match self.locals_alias_sym.get(idx) {
            Some(sym) => Some(*sym),
            None => self
                .locals
                .get(idx)
                .map(|n| Symbol::intern(&crate::runtime::sigilless_alias_key(n))),
        }
    }

    /// The interned `__mutsu_sigilless_readonly::<name>` env key of local `idx`.
    /// See [`CompiledCode::alias_sym`].
    pub(crate) fn readonly_sym(&self, idx: usize) -> Option<Symbol> {
        match self.locals_readonly_sym.get(idx) {
            Some(sym) => Some(*sym),
            None => self
                .locals
                .get(idx)
                .map(|n| Symbol::intern(&crate::runtime::sigilless_readonly_key(n))),
        }
    }

    /// The interned `__mutsu_deleted_index::<name>` env key of local `idx`.
    /// See [`CompiledCode::alias_sym`].
    pub(crate) fn deleted_index_sym(&self, idx: usize) -> Option<Symbol> {
        match self.locals_deleted_index_sym.get(idx) {
            Some(sym) => Some(*sym),
            None => self
                .locals
                .get(idx)
                .map(|n| Symbol::intern(&crate::runtime::deleted_index_key(n))),
        }
    }

    /// The interned `__mutsu_bound_array_slice::<name>` env key of local `idx`.
    /// See [`CompiledCode::alias_sym`].
    pub(crate) fn bound_slice_sym(&self, idx: usize) -> Option<Symbol> {
        match self.locals_bound_slice_sym.get(idx) {
            Some(sym) => Some(*sym),
            None => self
                .locals
                .get(idx)
                .map(|n| Symbol::intern(&crate::runtime::bound_array_slice_key(n))),
        }
    }

    pub(crate) fn compute_locals_sym(&mut self) {
        self.locals_sym = self.locals.iter().map(|s| Symbol::intern(s)).collect();
        self.locals_alias_sym = self
            .locals
            .iter()
            .map(|s| Symbol::intern(&crate::runtime::sigilless_alias_key(s)))
            .collect();
        self.locals_readonly_sym = self
            .locals
            .iter()
            .map(|s| Symbol::intern(&crate::runtime::sigilless_readonly_key(s)))
            .collect();
        self.locals_deleted_index_sym = self
            .locals
            .iter()
            .map(|s| Symbol::intern(&crate::runtime::deleted_index_key(s)))
            .collect();
        self.locals_bound_slice_sym = self
            .locals
            .iter()
            .map(|s| Symbol::intern(&crate::runtime::bound_array_slice_key(s)))
            .collect();
    }

    /// Compute which locals need to be synced to env.
    /// A local needs env sync if it's referenced by GetGlobal/SetGlobal/etc.
    /// in this code. Locals only accessed via GetLocal don't need env sync,
    /// which reduces env size and makes method call env clones cheaper.
    pub(crate) fn compute_needs_env_sync(&mut self) {
        // The ip -> line table must stay index-aligned with `ops`: a code path
        // that pushes/removes an op without going through `emit()` (or without
        // mirroring the change into `op_lines`) would shift every later line.
        // A chunk built entirely by hand never calls `emit()`, so an empty table
        // is also valid — `line_at` then reports "no line information".
        debug_assert!(
            self.op_lines.is_empty() || self.op_lines.len() == self.ops.len(),
            "op_lines desynced from ops ({} vs {})",
            self.op_lines.len(),
            self.ops.len()
        );
        // This chunk is finalized: drop the constant-pool dedup index, which is
        // compile-time scaffolding (ADR-0006 §2.4). A constant added afterwards
        // (a runtime-built chunk being patched) simply takes a fresh slot.
        self.const_index = rustc_hash::FxHashMap::default();
        self.compute_locals_sym();
        self.compute_free_vars();
        // Collect env-only `my` declarations so the method-dispatch return merge
        // can treat them as callee-local and not propagate them into a same-named
        // caller lexical across (self-)recursion. Two sources:
        //  (1) top-level `SetVarDynamic` ops (a `my $x`/`@x`/`%x` with no slot),
        //  (2) `my` declarations inside a *deferred* body stashed in `stmt_pool`
        //      (a `gather`/block/`while` body run by-name against the method env,
        //      e.g. zef `!find-prereq-candidates`'s `my @needed`), which never
        //      reaches the top-level op scan.
        let mut decls: std::collections::HashSet<String> = std::collections::HashSet::new();
        for op in &self.ops {
            if let OpCode::SetVarDynamic { name_idx, .. } = op
                && let Some(crate::value::ValueView::Str(name)) =
                    self.constants.get(*name_idx as usize).map(Value::view)
            {
                decls.insert(name.to_string());
            }
        }
        crate::ast::collect_all_my_decl_names(&self.stmt_pool, &mut decls);
        // Keep only names that are NOT compiled local slots (those are already
        // excluded from the merge via `method_local_keys`/`code.locals`).
        self.env_only_decls = decls
            .into_iter()
            .filter(|n| !self.locals.iter().any(|l| l == n))
            .collect();
        // Always scan for reflective caller-lexical access (independent of the
        // needs_env_sync early returns below), so the global flag is set even for
        // loop/block or zero-local frames.
        self.scan_reflective_name_access();
        // Conservative fallback: code that runs inline control-flow bodies with
        // their own env/locals juggling (for/while/loop bodies, which the
        // loop-phaser desugaring threads state through by name via `env`, e.g.
        // the `__mutsu_loop_first_`/`__mutsu_loop_ran_` control temps) cannot
        // safely treat any local as slot-only -- a slot value may not survive the
        // loop's per-iteration env round-trips. The same applies to the two ops
        // that stash a body in the `stmt_pool` and compile/run it at runtime
        // against the live env by name -- `MakeGather` (a gather block,
        // vm_register_ops::exec_make_gather_op) and `WheneverScope` (a
        // `whenever`/`supply` body, exec_whenever_scope_op): the body is not in
        // `closure_compiled_codes`, so the nested-closure free-var scan below
        // cannot see which lexicals it reads. For such a frame `free_var_syms` is
        // incomplete, so this flag drives the whole-env fallback in two places:
        // the dual-store flush blanket here, and the closure upvalue capture
        // (`capture_closure_env`). Computed unconditionally (before the n==0
        // early return) so zero-local closures wrapping a `whenever`/`gather` are
        // covered too. Recursion-heavy code without these (e.g. `fib`) is
        // unaffected and still skips the per-call flush for its slot-only params.
        self.captures_env_by_name = self.ops.iter().any(|op| {
            matches!(
                op,
                OpCode::ForLoop(..)
                    | OpCode::BlockScope { .. }
                    | OpCode::BlockLocalScope { .. }
                    | OpCode::MakeGather(..)
                    | OpCode::WheneverScope { .. }
            )
        });
        let n = self.locals.len();
        self.needs_env_sync = vec![false; n];
        // §1.4 shadow slots: flag every slot whose name occupies more than one
        // `locals` slot (a genuine inner-block shadow under MUTSU_SHADOW_SLOTS)
        // so the whole-locals env broadcasts skip them — see `dup_named_locals`.
        // Computed before the early returns below so it is populated for
        // BlockScope-carrying frames too (the very frames that shadow).
        self.dup_named_locals = vec![false; n];
        {
            let mut first_seen: std::collections::HashMap<&str, usize> =
                std::collections::HashMap::with_capacity(n);
            let mut dups: Vec<usize> = Vec::new();
            for (i, name) in self.locals.iter().enumerate() {
                match first_seen.get(name.as_str()) {
                    Some(&first) => {
                        dups.push(first);
                        dups.push(i);
                    }
                    None => {
                        first_seen.insert(name.as_str(), i);
                    }
                }
            }
            for i in dups {
                self.dup_named_locals[i] = true;
            }
        }
        if n == 0 {
            return;
        }
        if self.captures_env_by_name {
            self.needs_env_sync.iter_mut().for_each(|b| *b = true);
            return;
        }
        let locals_map: std::collections::HashMap<&str, usize> = self
            .locals
            .iter()
            .enumerate()
            .map(|(i, name)| (name.as_str(), i))
            .collect();
        // Single-store Slice E Part 2: a nested closure no longer reads its free
        // variables from this frame's flushed env — `capture_closure_env` reads
        // this frame's own locals straight from the slot store (the live upvalue).
        // So a local being a closure free variable no longer forces an env flush
        // here. (Mutation propagation back to the parent still flows through the
        // reverse env_dirty path, and a captured-and-mutated local is boxed into a
        // shared `ContainerRef` cell by `box_captured_lexicals`, so the closure and
        // parent share one cell.) Only a local genuinely read/written *by name* in
        // this frame (below) still needs the slot mirrored into env.
        for op in &self.ops {
            let name_idx = match op {
                OpCode::GetGlobal(idx)
                | OpCode::SetGlobal(idx)
                | OpCode::SetGlobalRaw(idx)
                | OpCode::PostIncrement(idx, _)
                | OpCode::PostDecrement(idx, _)
                | OpCode::PreIncrement(idx, _)
                | OpCode::PreDecrement(idx, _)
                | OpCode::GetArrayVar(idx)
                | OpCode::GetHashVar(idx)
                | OpCode::AssignExpr(idx)
                | OpCode::TopicDotAssign(idx) => Some(*idx),
                _ => None,
            };
            if let Some(idx) = name_idx
                && let Some(ValueView::Str(name)) =
                    self.constants.get(idx as usize).map(Value::view)
                && let Some(&slot) = locals_map.get(name.as_str())
            {
                self.needs_env_sync[slot] = true;
            }
        }
        // `(B)` per-store env-write gate — closure-capture cluster fold.
        // A nested closure normally reads its free variables straight from this
        // frame's live slot store (`capture_closure_env`), so a closure-captured
        // local does NOT force an env flush in the default build (the J4d env
        // decoupling). But when such a closure is handed to a by-name slow-path
        // consumer — the `.map`/`.grep` fast loop, for instance, pre-inserts the
        // closure's captured env into `self.env` only for keys ABSENT there, so it
        // reads a captured free var back from THIS frame's env by name — the env
        // mirror must be current. Under `MUTSU_GATE_LOCAL_ENV_WRITE` a plain
        // lexical's store skips that mirror, leaving the decl-seed `Any`, so the
        // consumer reads a stale value. Fold every nested-closure free var that is
        // one of this frame's own locals back into `needs_env_sync` so its store
        // keeps mirroring. Gated on the flag so the default build is byte-identical
        // AND perf-neutral (no extra `flush_local_to_env` work when the gate is
        // off); the cost only lands with the gate, and it never touches a
        // hot-arithmetic loop local (those are not closure free variables).
        if gate_local_env_write() {
            // Atomic-op targets (`⚛$x`, `$x ⚛= v`, `cas($x, …)`) resolve their
            // variable by NAME from env in the `__mutsu_*_var` builtin. Keep the
            // mirror current so a non-`atomicint` scalar is not read as its
            // decl-seed placeholder under the gate. (Recorded at emit time in
            // `atomic_env_sync_locals`; an `atomicint` reads the shared store
            // first, so folding it here is harmless.)
            for &slot in &self.atomic_env_sync_locals {
                if let Some(b) = self.needs_env_sync.get_mut(slot as usize) {
                    *b = true;
                }
            }
            for nested in &self.closure_compiled_codes {
                for sym in &nested.free_var_syms {
                    if let Some(&slot) = sym.with_str(|s| locals_map.get(s)) {
                        self.needs_env_sync[slot] = true;
                    }
                }
                // A nested closure whose ONLY use of an outer SCALAR is an
                // in-place container mutation (`$bh<a>:delete`, `$h<k> = v`,
                // `$a[i]++`) never records that scalar in `free_var_syms`: those
                // ops are classified as container mutations, and the
                // container-write free-var set is filtered to `@`/`%` aggregates
                // (a scalar holding a Bag/Hash/Array is neither). Under the gate
                // the outer `my $bh = <a a b>.BagHash` skips its env mirror, so
                // when the closure runs by-name in a carrier (`lives-ok { … }`)
                // its `:delete` reads the decl-seed `Any` from env and the
                // mutation vanishes. Fold such scalars into `needs_env_sync` too,
                // so the outer store keeps mirroring the live container. Gate-ON
                // only, so the default build is byte-identical / perf-neutral.
                for op in &nested.ops {
                    if let Some(idx) = nested.op_container_mutate_const_idx(op)
                        && let Some(ValueView::Str(name)) =
                            nested.constants.get(idx as usize).map(Value::view)
                        && !name.starts_with('@')
                        && !name.starts_with('%')
                        && !name.starts_with('&')
                        && !nested.locals.iter().any(|l| l.as_str() == name.as_str())
                        && let Some(&slot) = locals_map.get(name.as_str())
                    {
                        self.needs_env_sync[slot] = true;
                    }
                }
            }
            // A NAMED sub (`sub f { ... }`) is not embedded in
            // `closure_compiled_codes` — it is registered from `stmt_pool` via a
            // `RegisterSub` op and compiled lazily, so this frame cannot see which
            // enclosing lexicals its body reads by name. Such a sub reads an outer
            // lexical (`my $base = 100; sub f { $base + 1 }`) from this frame's env
            // by name at call time, which the gate would leave stale. A class/role
            // METHOD body captures an outer lexical the same way (`my $base = 100;
            // class T { method calc($n) { $base + $n } }`) and is likewise compiled
            // lazily off the class/role registration op, invisible here. Without the
            // body's free-var set available, conservatively keep every local of a
            // frame that defines a named sub or a class/role env-synced. Gate-ON
            // only, so the default build is byte-identical/perf-neutral; the
            // top-level/main frame (the usual definer) is never a hot arithmetic
            // loop.
            let defines_lazy_body = self.ops.iter().any(|op| {
                matches!(
                    op,
                    OpCode::RegisterSub(_)
                        | OpCode::RegisterClass(_)
                        | OpCode::RegisterRole(_)
                        // A deferred END body (`PhaserEnd`, run after the frame
                        // exits) and a compile-time BEGIN/CHECK body (`CheckPhaser`)
                        // reconstruct the installing frame's lexicals BY NAME from
                        // env, not from `self.locals` — exactly like a lazy sub
                        // body. Under the gate a top-level `my $hist` mutated only
                        // through these phasers skips its env mirror, so each phaser
                        // reads a stale value and the accumulation is lost (roast
                        // S04-phasers/interpolate.t: END sees `E`, not `BCIE`).
                        | OpCode::PhaserEnd { .. }
                        | OpCode::CheckPhaser { .. }
                )
            });
            // A frame that installs a *resume-safe* CONTROL handler
            // (`CONTROL { default { $out ~= .Str; .resume } }`) has its handler run
            // INLINE at a deep `warn` raise site (`try_resume_safe_control_inline`),
            // which reconstructs the installing frame's locals FROM ENV by name (the
            // cross-frame store) because `self.locals` is the deep raise-site frame.
            // Under the gate a plain `my $out = ''` in this frame skips its env
            // mirror, so the handler reconstructs a stale `$out` and its `~=` is
            // lost. Keep every local of such a frame env-synced (gate-ON only; the
            // installing frame is a block/main frame, never a hot loop).
            let installs_resume_control = self.ops.iter().any(|op| {
                matches!(
                    op,
                    OpCode::TryCatch {
                        resume_safe: true,
                        control_start,
                        body_end,
                        ..
                    } if control_start < body_end
                )
            });
            // A frame that constructs a regex value which interpolates a lexical
            // (`/ ... $script ... /`) may have that regex matched in a DIFFERENT
            // frame — e.g. `like $err, / ... $script ... /` matches inside `like`,
            // whose `interpolate_regex_scalars` resolves `$script` from the
            // name-keyed env (the cross-frame store), not this frame's slots. Under
            // the gate a plain `my $script = ...` skips its env mirror, so the
            // interpolation reads a stale/empty value. Keep every local of a frame
            // that holds an interpolating regex constant env-synced (gate-ON only;
            // the pattern is checked with the same conservative `regex_pattern_is_
            // static` used for the match cache, so a static regex folds nothing).
            let holds_interpolating_regex = self.constants.iter().any(|c| {
                matches!(
                    c.view(),
                    ValueView::Regex(p)
                        if !crate::runtime::regex_parse::regex_pattern_is_static(p.as_str())
                )
            });
            // A frame that runs a substitution with a DYNAMIC replacement
            // (`s/^(.)/{ $a++ }/`, `s/x/$a/`) re-entrantly evaluates that
            // replacement, which reads/writes the referenced lexicals BY NAME from
            // the env (the closure-carried cross-frame store), not this frame's
            // slots. `holds_interpolating_regex` only catches a dynamic *pattern*;
            // a static pattern with a code/interpolated replacement slips past it.
            // Under the gate a `state $a = 0` (or plain `my`) in this frame skips
            // its env mirror, so the replacement reads a stale value and the
            // closure's state save-back stores it back (roast S04-declarations/
            // state.t: `state $a` bumped inside `s///` stays 0). Keep every local
            // of such a frame env-synced (gate-ON only; a substitution frame is not
            // a hot arithmetic loop). A purely literal replacement folds nothing.
            let holds_dynamic_substitution = self.ops.iter().any(|op| {
                let repl_idx = match op {
                    OpCode::Subst {
                        replacement_idx, ..
                    }
                    | OpCode::NonDestructiveSubst {
                        replacement_idx, ..
                    } => *replacement_idx,
                    _ => return false,
                };
                self.constants
                    .get(repl_idx as usize)
                    .map(|c| match c.view() {
                        ValueView::Str(s) => s.contains(['$', '@', '%', '&', '{']),
                        _ => true,
                    })
                    .unwrap_or(false)
            });
            if defines_lazy_body
                || installs_resume_control
                || holds_interpolating_regex
                || holds_dynamic_substitution
            {
                self.needs_env_sync.iter_mut().for_each(|b| *b = true);
            }
        }
    }

    /// Scan this code's ops for reflective by-name access to a caller frame's
    /// lexicals (`CALLER::`/`OUTER::`, symbolic deref, pseudo-stash, indirect
    /// code lookup, `EVAL`/`EVALFILE`) and set the process-global
    /// [`REFLECTIVE_NAME_ACCESS_SEEN`] flag. Runs unconditionally at finalize
    /// (before the `needs_env_sync` early returns) so the flag covers loop/block
    /// frames and zero-local frames too. Monotonic: only ever sets `true`.
    pub(crate) fn scan_reflective_name_access(&self) {
        if REFLECTIVE_NAME_ACCESS_SEEN.load(Ordering::Relaxed) {
            return;
        }
        for op in &self.ops {
            let reflective = match op {
                OpCode::GetCallerVar { .. }
                | OpCode::GetCallersVar { .. }
                | OpCode::GetOuterVar { .. }
                | OpCode::GetPseudoStash(_)
                | OpCode::SymbolicDeref { .. }
                | OpCode::SymbolicDerefStore(_)
                | OpCode::IndirectCodeLookup(_) => true,
                OpCode::CallFunc { name_idx, .. } | OpCode::CallFuncNamed { name_idx, .. } => {
                    matches!(
                        self.constants.get(*name_idx as usize).map(Value::view),
                        Some(ValueView::Str(name)) if name.as_str() == "EVAL" || name.as_str() == "EVALFILE"
                    )
                }
                _ => false,
            };
            if reflective {
                REFLECTIVE_NAME_ACCESS_SEEN.store(true, Ordering::Relaxed);
                return;
            }
        }
    }

    /// The constant-pool index naming the variable an op reads/writes by name,
    /// for the GetGlobal-family opcodes that resolve against the env.
    fn op_name_const_idx(op: &OpCode) -> Option<u32> {
        match op {
            OpCode::GetGlobal(idx)
            | OpCode::SetGlobal(idx)
            | OpCode::SetGlobalRaw(idx)
            | OpCode::PostIncrement(idx, _)
            | OpCode::PostDecrement(idx, _)
            | OpCode::PreIncrement(idx, _)
            | OpCode::PreDecrement(idx, _)
            | OpCode::GetArrayVar(idx)
            | OpCode::GetHashVar(idx)
            | OpCode::AssignExpr(idx)
            | OpCode::TopicDotAssign(idx)
            | OpCode::AtomicCompoundVar { name_idx: idx, .. } => Some(*idx),
            _ => None,
        }
    }

    /// The constant-pool index naming the variable an op *writes* by name
    /// (assignment / increment / decrement). Subset of `op_name_const_idx` that
    /// excludes pure reads. Used to compute `free_var_writes` /
    /// `captured_mutated_locals`. NOTE: declaration (`SetLocal` after
    /// `MarkVarDeclContext`) and own-local reassignment (`AssignExprLocal`) are
    /// slot-based and handled separately by the caller.
    /// A name that is NOT a lexical free variable: an attribute accessor
    /// (`$.x` → `.x`, `$!x` → `!x`, `@.x`/`@!x`/`%.x`/`%!x`), `self`, a special
    /// twigil var (`$*foo`, `$?FILE`, `$^a`), or a compiler-internal temporary.
    /// Such names resolve via `self`/dynamic scope/internals, never the
    /// enclosing lexical env, so they must be excluded from the free-var
    /// read/write classification (otherwise e.g. `method inc { $.count++ }`
    /// mis-records `.count` as a captured-outer write and the redispatch
    /// writeback corrupts the rw attribute update — #3658).
    pub(crate) fn is_non_lexical_name(name: &str) -> bool {
        name == "self"
            || name == "__ANON_STATE__"
            || name == "?CLASS"
            || name == "?ROLE"
            || name == "_"
            || name == "!"
            || name == "/"
            || name == "__mutsu_callable_id"
            || name.starts_with('!')
            || name.starts_with('.')
            || name.starts_with("@!")
            || name.starts_with("@.")
            || name.starts_with("%!")
            || name.starts_with("%.")
            || name.starts_with("__mutsu_")
            || name.starts_with('*')
            || name.starts_with('?')
            || name.starts_with('^')
    }

    /// A name that names an instance attribute via its twigil (`$!x` → `!x`,
    /// `$.x` → `.x`, and the `@!`/`@.`/`%!`/`%.` array/hash forms). These resolve
    /// through `self`, never the enclosing lexical env, so a *write* to one is an
    /// attribute mutation — NOT a captured-outer free-var write. Narrower than
    /// `is_non_lexical_name`: it deliberately does NOT cover dynamic (`$*x`) or
    /// special (`$?x`/`$^x`) vars, which must stay in `free_var_writes` to keep
    /// the redispatch writeback gate (#3658).
    fn is_attribute_accessor_name(name: &str) -> bool {
        name == "self"
            || name.starts_with('!')
            || name.starts_with('.')
            || name.starts_with("@!")
            || name.starts_with("@.")
            || name.starts_with("%!")
            || name.starts_with("%.")
    }

    fn op_name_write_const_idx(op: &OpCode) -> Option<u32> {
        match op {
            OpCode::SetGlobal(idx)
            | OpCode::SetGlobalRaw(idx)
            | OpCode::PostIncrement(idx, _)
            | OpCode::PostDecrement(idx, _)
            | OpCode::PreIncrement(idx, _)
            | OpCode::PreDecrement(idx, _)
            | OpCode::AssignExpr(idx)
            | OpCode::TopicDotAssign(idx)
            | OpCode::AtomicCompoundVar { name_idx: idx, .. } => Some(*idx),
            _ => None,
        }
    }

    /// Array/hash methods that mutate the receiver in place. A `CallMethodMut`
    /// with one of these on a `@`/`%` target is an in-place container write (not a
    /// name rebind), so it must count toward `free_var_container_writes`.
    fn is_mutating_container_method(method: &str) -> bool {
        matches!(
            method,
            "push" | "pop" | "shift" | "unshift" | "append" | "prepend" | "splice"
        )
    }

    /// The constant-pool index naming a `@`/`%` container that `op` mutates IN
    /// PLACE (element/index assignment, or a mutating method like `push`). These
    /// are deliberately excluded from `op_name_write_const_idx` (they are not
    /// `SetGlobal` name rebinds); the caller filters for non-own `@`/`%` names and
    /// records them in `free_var_container_writes` to drive cell boxing.
    fn op_container_mutate_const_idx(&self, op: &OpCode) -> Option<u32> {
        match op {
            OpCode::IndexAssignExprNamed { name_idx, .. }
            | OpCode::IndexAssignExprNested { name_idx, .. }
            | OpCode::IndexAssignDeepNested { name_idx, .. }
            | OpCode::IndexElemAutoviv { name_idx, .. } => Some(*name_idx),
            // Element increment/decrement (`@a[$i]++`, `%h{$k}--`) and element
            // delete (`@a[$i]:delete`, `%h{$k}:delete`) mutate the container in
            // place exactly like element-assign — without these a closure whose
            // ONLY use of an outer aggregate is `%h{$k}++` / `:delete` never
            // captured it, so the mutation vanished once the closure escaped its
            // declaring frame (Track B T6 probe).
            OpCode::PostIncrementIndex(name_idx)
            | OpCode::PostDecrementIndex(name_idx)
            | OpCode::PreIncrementIndex(name_idx)
            | OpCode::PreDecrementIndex(name_idx) => Some(*name_idx),
            OpCode::DeleteIndexNamed(name_idx, _) => Some(*name_idx),
            OpCode::IndexAssignPseudoStashNamed { stash_name_idx, .. } => Some(*stash_name_idx),
            OpCode::ArrayPush {
                target_name_idx, ..
            } => Some(*target_name_idx),
            OpCode::CallMethodMut {
                name_idx,
                target_name_idx,
                ..
            } => {
                if let Some(ValueView::Str(method)) =
                    self.constants.get(*name_idx as usize).map(Value::view)
                    && Self::is_mutating_container_method(&method)
                {
                    Some(*target_name_idx)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Compute `free_var_syms`: the names this code references from an enclosing
    /// scope (GetGlobal-family ops whose name is not one of this code's own
    /// locals), unioned with the free variables of nested closures that are not
    /// resolved by this code's locals. Nested closures have already had their
    /// own `free_var_syms` computed (they are finalized before being embedded),
    /// so they are folded in directly without re-walking their ops.
    /// Extract sigil'd variable references (`$r`, `@a`, `%h`, `&rule`) from a
    /// regex pattern string, for closure free-variable analysis. Conservative: it
    /// over-approximates (an extra captured var is harmless), and skips capture
    /// references like `$0`/`$<name>` (sigil not followed by an identifier start).
    fn regex_interpolated_var_names(pattern: &str) -> Vec<String> {
        let bytes = pattern.as_bytes();
        let mut names = Vec::new();
        let mut i = 0;
        while i < bytes.len() {
            let c = bytes[i];
            if matches!(c, b'$' | b'@' | b'%' | b'&') {
                let start_ident = i + 1;
                if start_ident < bytes.len()
                    && (bytes[start_ident].is_ascii_alphabetic() || bytes[start_ident] == b'_')
                {
                    let mut j = start_ident;
                    while j < bytes.len() && (bytes[j].is_ascii_alphanumeric() || bytes[j] == b'_')
                    {
                        j += 1;
                    }
                    // Scalars ($x) are stored sigil-less in `locals`/`env` ("x"),
                    // while @/%/& lexicals keep their sigil. Match that convention
                    // so the free-var name lines up with the local slot / env key.
                    let name = if c == b'$' {
                        pattern[start_ident..j].to_string()
                    } else {
                        pattern[i..j].to_string()
                    };
                    names.push(name);
                    i = j;
                    continue;
                }
            }
            i += 1;
        }
        names
    }

    /// The constant-pool index of a call op's *argument source names*. A lexical
    /// that reaches a call as an argument can be written through by an `is rw` /
    /// `is raw` parameter (`cas($x, $old, $new)` is the canonical sink), and that
    /// write is invisible to the name-write op scan — the arg is only ever *read*
    /// by name at the call site. So a local that appears here can go stale behind
    /// the analysis's back and must never be vouched for.
    fn op_arg_sources_idx(op: &OpCode) -> Option<u32> {
        match op {
            OpCode::CallFunc {
                arg_sources_idx, ..
            }
            | OpCode::CallFuncNamed {
                arg_sources_idx, ..
            }
            | OpCode::CallMethod {
                arg_sources_idx, ..
            }
            | OpCode::CallMethodMut {
                arg_sources_idx, ..
            }
            | OpCode::ExecCall {
                arg_sources_idx, ..
            }
            | OpCode::CallOnValue {
                arg_sources_idx, ..
            }
            | OpCode::CallOnCodeVar {
                arg_sources_idx, ..
            } => *arg_sources_idx,
            _ => None,
        }
    }

    /// The constant-pool index of a code-variable read (`&f` as a value, or a
    /// call through a code variable, `&f.()`). The constant holds the SIGIL-LESS
    /// name (`"f"`), but the lexical itself lives under `&f` in `locals`/`env`,
    /// so the free-var scan must re-key it with the sigil before matching.
    fn op_code_var_read_const_idx(op: &OpCode) -> Option<u32> {
        match op {
            OpCode::GetCodeVar(idx) => Some(*idx),
            OpCode::CallOnCodeVar { name_idx, .. } => Some(*name_idx),
            _ => None,
        }
    }

    /// The constant-pool index of a bare call's CALLEE name (`f(...)` in
    /// expression or statement position). At runtime the callee resolves against
    /// the lexical `&f` before the global function registry, so when an
    /// enclosing scope declares `&f` (see `outer_code_var_names`) the call is
    /// really a code-variable read this closure must capture.
    fn op_callee_name_const_idx(op: &OpCode) -> Option<u32> {
        match op {
            OpCode::CallFunc { name_idx, .. }
            | OpCode::CallFuncNamed { name_idx, .. }
            | OpCode::ExecCall { name_idx, .. } => Some(*name_idx),
            _ => None,
        }
    }

    /// The `closure_compiled_codes` index an op embeds, for the ops that create a
    /// closure value (and so snapshot the creating frame's env at that point).
    fn op_closure_code_idx(op: &OpCode) -> Option<u32> {
        match op {
            OpCode::MakeAnonSub(_, cc, _)
            | OpCode::MakeAnonSubParams(_, cc, _)
            | OpCode::MakeLambda(_, cc, _)
            | OpCode::MakeBlockClosure(_, cc)
            | OpCode::MakeGather(_, cc) => *cc,
            _ => None,
        }
    }

    /// Names this code declares itself: `my` declarations (`SetVarDynamic`)
    /// and `for`-loop parameters (`ForLoop`). A lazily-forced `gather` body
    /// uses this to keep its OWN declarations out of the pull-site env merge —
    /// a body loop var that shadows a consumer-scope lexical of the same name
    /// (`for f() -> $a { ... }` pulling a gather whose body also loops `-> $a`)
    /// must not clobber the consumer's binding. Mirrors the scan in
    /// `CompiledFunction::compute_declared_locals`.
    pub(crate) fn self_declared_names(&self) -> rustc_hash::FxHashSet<Symbol> {
        let mut declared: rustc_hash::FxHashSet<Symbol> = rustc_hash::FxHashSet::default();
        for op in &self.ops {
            match op {
                OpCode::SetVarDynamic { name_idx, .. } => {
                    if let Some(ValueView::Str(name)) =
                        self.constants.get(*name_idx as usize).map(Value::view)
                    {
                        declared.insert(Symbol::intern(&name));
                    }
                }
                OpCode::ForLoop(spec) => {
                    if let Some(idx) = spec.param_idx
                        && let Some(ValueView::Str(name)) =
                            self.constants.get(idx as usize).map(Value::view)
                    {
                        declared.insert(Symbol::intern(&name));
                    }
                    for name in &spec.multi_param_names {
                        declared.insert(Symbol::intern(name));
                    }
                }
                _ => {}
            }
        }
        declared
    }

    pub(crate) fn compute_free_vars(&mut self) {
        let own: std::collections::HashSet<&str> = self.locals.iter().map(|s| s.as_str()).collect();
        let mut free: std::collections::HashSet<Symbol> = std::collections::HashSet::new();
        // Free variables this code (or a nested closure) *writes*.
        let mut free_writes: std::collections::HashSet<Symbol> = std::collections::HashSet::new();
        // Own locals that are mutated *after* declaration within this code
        // (reassigned, or inc/dec by name). Both a `my $x = e` declaration and a
        // plain `$x = e` reassignment compile to `SetLocal(slot)`; the ONLY
        // distinguisher is a preceding `MarkVarDeclContext` (declaration). A
        // `SetLocal` without a pending decl marker is a mutation.
        let mut self_mutated: std::collections::HashSet<Symbol> = std::collections::HashSet::new();
        // Free `@`/`%` containers mutated in place (push/append/element-assign).
        let mut free_container_writes: std::collections::HashSet<Symbol> =
            std::collections::HashSet::new();
        // OWN `@`/`%` containers mutated in place. A container write is not a
        // name-write, so `self_mutated` never sees it — but it still makes a
        // closure's by-value capture go stale (a `BagHash` element-assign
        // copy-on-writes, so the captured Gc stops tracking the local). Tracked
        // separately so `authoritative_free_vars` can refuse to vouch for them.
        let mut own_container_writes: std::collections::HashSet<Symbol> =
            std::collections::HashSet::new();
        // Own locals that reach a call as an argument, and so may be written
        // through an `is rw` / `is raw` parameter (`cas($x, ...)`). See
        // `op_arg_sources_idx`.
        let mut own_call_arg_sources: std::collections::HashSet<Symbol> =
            std::collections::HashSet::new();
        // Bare names read via `$OUTER::` (order-preserving, de-duplicated).
        let mut outer_ref_names: Vec<String> = Vec::new();
        // Own locals captured by a closure created since the last store/decl
        // marker — i.e. by the initializer currently being evaluated. Consumed by
        // the store that ends it (see the self-capture rule below).
        let mut captured_in_decl: std::collections::HashSet<Symbol> =
            std::collections::HashSet::new();
        // Own locals whose declaration's own initializer captured them.
        let mut self_capture_decl: std::collections::HashSet<Symbol> =
            std::collections::HashSet::new();
        let mut pending_decl = false;
        for op in &self.ops {
            // Read+write free-var set (names referenced from an enclosing scope).
            if let Some(idx) = Self::op_name_const_idx(op)
                && let Some(ValueView::Str(name)) =
                    self.constants.get(idx as usize).map(Value::view)
                && !own.contains(name.as_str())
            {
                free.insert(Symbol::intern(&name));
            }
            // A code-variable read (`&x1` as a value, `&x1.()`): the op's constant
            // is the sigil-less name, but the lexical lives under `&x1`. Without
            // this, a closure that captures a `&`-sigiled parameter never records
            // it as a free variable — `&x1` is a plain user lexical, so the
            // capture filter drops it and every read silently resolves through the
            // CALLING frame's env instead (lexical scoping degrading into dynamic
            // scoping). The closure looks right while it fires from its creator's
            // own frame — the chains agree — and breaks the first time a sibling
            // frame with same-named parameters invokes it (roast
            // integration/man-or-boy.t). Non-lexical forms (`&!attr`, `&?ROUTINE`,
            // `&*dyn`, package-qualified and operator names) keep resolving
            // against the live env by design.
            if let Some(idx) = Self::op_code_var_read_const_idx(op)
                && let Some(ValueView::Str(name)) =
                    self.constants.get(idx as usize).map(Value::view)
                && !name.contains(':')
            {
                let key = format!("&{}", name.as_str());
                if crate::env::is_plain_user_lexical(&key) && !own.contains(key.as_str()) {
                    free.insert(Symbol::intern(&key));
                }
            }
            // A bare call `x1(...)` records the read of `&x1` only as the call
            // opcode's callee name. Registering every called name would bloat
            // `free_var_syms` with `&say` etc. on every closure, so only callees
            // matching a `&`-sigiled lexical declared in an enclosing scope
            // (`outer_code_var_names`, threaded down at compile time) count as
            // code-variable reads.
            if !self.outer_code_var_names.is_empty()
                && let Some(idx) = Self::op_callee_name_const_idx(op)
                && let Some(ValueView::Str(name)) =
                    self.constants.get(idx as usize).map(Value::view)
            {
                let key = format!("&{}", name.as_str());
                if self.outer_code_var_names.contains(&key) && !own.contains(key.as_str()) {
                    free.insert(Symbol::intern(&key));
                }
            }
            // Own locals reaching a call as arguments: an `is rw` parameter can
            // write straight back into them without any name-write op here.
            if let Some(idx) = Self::op_arg_sources_idx(op)
                && let Some(ValueView::Array(items, ..)) =
                    self.constants.get(idx as usize).map(Value::view)
            {
                for item in items.iter() {
                    // Entries are either `Str(name)` or `Pair(name, Int(slot))`.
                    let name = match item.view() {
                        ValueView::Str(s) => Some(s.to_string()),
                        ValueView::Pair(k, _) => Some(k.to_string()),
                        _ => None,
                    };
                    // `&`-sigiled arguments are exempt from the rw-arg-sink rule:
                    // Raku rejects `is rw` on a non-scalar parameter ("Can only
                    // use 'is rw' on a scalar ('$' sigil) parameter"), so a code
                    // variable handed to a call cannot be rebound through one.
                    // Without the exemption, a routine that both captures its
                    // `&`-params in a closure and forwards them to calls (the
                    // man-or-boy `A`) could never vouch for them. A direct
                    // `&f = ...` rebind is a name-write and is still caught; an
                    // `is raw` param rebinding a passed `&`-arg remains a known
                    // gap of this analysis.
                    if let Some(name) = name
                        && own.contains(name.as_str())
                        && !name.starts_with('&')
                    {
                        own_call_arg_sources.insert(Symbol::intern(&name));
                    }
                }
            }
            // Own-container in-place mutation: not a name-write, so `self_mutated`
            // misses it, but it still invalidates a closure's by-value capture.
            if let Some(idx) = self.op_container_mutate_const_idx(op)
                && let Some(ValueView::Str(name)) =
                    self.constants.get(idx as usize).map(Value::view)
                && own.contains(name.as_str())
            {
                own_container_writes.insert(Symbol::intern(&name));
            }
            // Free-var container in-place mutation (push/append/element-assign):
            // NOT a name-write, so tracked separately for cell boxing.
            if let Some(idx) = self.op_container_mutate_const_idx(op)
                && let Some(ValueView::Str(name)) =
                    self.constants.get(idx as usize).map(Value::view)
                && (name.starts_with('@') || name.starts_with('%'))
                && !own.contains(name.as_str())
            {
                free_container_writes.insert(Symbol::intern(&name));
                // A container mutated ONLY via element-assign (`%h{$k} = v`,
                // `@a[$i] = v`) must ALSO be captured as a free variable, exactly
                // like one mutated via a method (`%h.push`, already a name-read op).
                // Otherwise the closure never carries the container, so a wrapper's
                // hash element-assign is lost on return (its copy-on-write result is
                // neither shared with the caller nor persisted for writeback). An
                // array happened to survive via shared in-place mutation, but a hash
                // element-assign copy-on-writes and needs the capture + writeback.
                free.insert(Symbol::intern(&name));
            }
            // Name-based writes: either a free-var write or an own-local mutation.
            if let Some(idx) = Self::op_name_write_const_idx(op)
                && let Some(ValueView::Str(name)) =
                    self.constants.get(idx as usize).map(Value::view)
            {
                if own.contains(name.as_str()) {
                    self_mutated.insert(Symbol::intern(&name));
                } else if !Self::is_attribute_accessor_name(&name) {
                    // Attribute accessors (`$.count++` → name `.count`, `$!x`, the
                    // `@.`/`@!`/`%.`/`%!` forms) resolve via `self`, NOT the
                    // enclosing lexical env, so they must not count as free-var
                    // writes — else the redispatch writeback mis-propagates the
                    // attribute name as a caller var (#3658). Dynamic vars (`$*x`)
                    // and special twigils stay in `free_var_writes` deliberately:
                    // they still need the writeback gate (reduce-time dynamic-var
                    // scoping in grammar actions, t/grammar-reduce-time-dynvar.t),
                    // so they are NOT excluded here.
                    free_writes.insert(Symbol::intern(&name));
                }
            }
            // `$OUTER::x` reads the enclosing lexical scope's binding of `x`.
            // OUTER:: is a *lexical* construct, so the enclosing binding must be
            // captured into this closure's env (`get_outer_var` resolves it there
            // once the defining block has exited). The op scan above does not treat
            // `GetOuterVar` as a name-read, so register the bare name here. (CALLER::
            // is dynamic-scope — resolved against the live call stack — so it is
            // deliberately NOT captured.)
            if let OpCode::GetOuterVar { name_idx, .. } = op
                && let Some(name) = self
                    .constants
                    .get(*name_idx as usize)
                    .and_then(|v| v.as_str())
            {
                if !outer_ref_names.iter().any(|n| n == name) {
                    outer_ref_names.push(name.to_string());
                }
                if !own.contains(name) {
                    free.insert(Symbol::intern(name));
                }
            }
            // `$::($name)::x` is the same lexical read, with the target name known
            // only at run time — so, unlike the `GetOuterVar` case above, there is
            // no single name to snapshot. Claim every name the site's baked scope
            // chain declares, which is exactly the set the deref could resolve to.
            // Without this the enclosing binding is never snapshotted and
            // `get_outer_var` falls through to the runtime scope stack, which inside
            // a stored closure holds the CALLER's blocks — dynamic scope, and a
            // different answer than the literal spelling gives.
            //
            // Only `outer_ref_names` is claimed, not `free`: a symbolic deref sets
            // the reflective-access flag, which already makes `capture_closure_env`
            // fall back to a whole-env `clone_env`, so every enclosing name is in the
            // captured env regardless. Adding them to `free` would buy nothing and
            // would freeze snapshots of names the body never reads.
            if let OpCode::SymbolicDeref { scopes_idx, .. } = op
                && let Some(chain) = self.lex_scopes.get(*scopes_idx as usize)
            {
                for name in chain.declared_names() {
                    if !outer_ref_names.iter().any(|n| n == name) {
                        outer_ref_names.push(name.to_string());
                    }
                }
            }
            // Self-capturing declaration: `my $f = -> $n { ... $f($n-1) ... }`.
            // The initializer's closure-creation op snapshots the env BEFORE the
            // declaration's store runs, so the closure captures `$f` while it is
            // still unset. `pending_decl` cannot see this — it only separates
            // `my $x = e` from `$x = e`, and both are "declarations" here. Treat
            // the store as a mutation *from the closure's point of view*, which is
            // what earns the local a shared cell (`captured_mutated` → the escape
            // analysis then puts it in `needs_cell`, since an assigned closure
            // escapes). The window is deliberately narrow: only closures created
            // by the initializer that this very store completes.
            if let Some(cc_idx) = Self::op_closure_code_idx(op)
                && let Some(nested) = self.closure_compiled_codes.get(cc_idx as usize)
            {
                for sym in &nested.free_var_syms {
                    if sym.with_str(|s| own.contains(s)) {
                        captured_in_decl.insert(*sym);
                    }
                }
            }
            let store_slot = match op {
                OpCode::SetLocal(slot) | OpCode::AssignExprLocal(slot) => Some(*slot),
                OpCode::SetLocalDecl { slot, .. } => Some(*slot),
                _ => None,
            };
            if let Some(slot) = store_slot {
                if (matches!(op, OpCode::SetLocalDecl { .. }) || pending_decl)
                    && let Some(name) = self.locals.get(slot as usize)
                {
                    let sym = Symbol::intern(name);
                    if captured_in_decl.contains(&sym) {
                        self_mutated.insert(sym);
                        self_capture_decl.insert(sym);
                    }
                }
                // The store ends this initializer: later closures belong to the
                // next one.
                captured_in_decl.clear();
            }
            match op {
                OpCode::MarkVarDeclContext => pending_decl = true,
                OpCode::SetLocal(slot) => {
                    if !pending_decl && let Some(name) = self.locals.get(*slot as usize) {
                        // Reassignment of an own local (declaration consumes the
                        // pending marker instead).
                        self_mutated.insert(Symbol::intern(name));
                    }
                    pending_decl = false;
                }
                // The fused declaration (ADR-0006 §2.3) carries the marker with
                // it, so it is a declaration, never a reassignment.
                OpCode::SetLocalDecl { .. } => pending_decl = false,
                OpCode::AssignExprLocal(slot) => {
                    if let Some(name) = self.locals.get(*slot as usize) {
                        self_mutated.insert(Symbol::intern(name));
                    }
                    pending_decl = false;
                }
                _ => {}
            }
        }
        // Regex literals interpolate lexical variables at match time (`/<$r>/`,
        // `/$x/`, `/<&rule>/`). Those reads happen inside the regex engine, not via
        // a name-const op, so the op scan above misses them. A closure that stores
        // such a regex (e.g. `-> $r { * ~~ /<$r>/ }`) must still capture `$r`, so
        // scan every regex constant for sigil'd variable references and treat them
        // as free vars (unless this body declares them).
        for c in &self.constants {
            let pattern = match c.view() {
                ValueView::Regex(s) => Some(s.clone()),
                ValueView::RegexWithAdverbs(a) => Some(a.pattern.clone()),
                _ => None,
            };
            if let Some(pattern) = pattern {
                for name in Self::regex_interpolated_var_names(&pattern) {
                    if !own.contains(name.as_str()) {
                        free.insert(Symbol::intern(&name));
                    }
                }
            }
        }
        // Fold nested closures: their free vars are ours unless we declare them;
        // their free-var *writes* of one of our locals make that local mutated.
        for nested in &self.closure_compiled_codes {
            for sym in &nested.free_var_syms {
                if !sym.with_str(|s| own.contains(s)) {
                    free.insert(*sym);
                }
            }
            for sym in &nested.free_var_writes {
                if sym.with_str(|s| own.contains(s)) {
                    self_mutated.insert(*sym);
                } else {
                    free_writes.insert(*sym);
                }
            }
            // A nested closure that mutates an outer container in place keeps that
            // container free here unless we own it (it stays a container-write
            // contribution either way — own ones are handled by the cell at decl).
            for sym in &nested.free_var_container_writes {
                if sym.with_str(|s| own.contains(s)) {
                    own_container_writes.insert(*sym);
                } else {
                    free_container_writes.insert(*sym);
                }
            }
        }
        // Own locals captured by a nested closure AND mutated -> must be boxed
        // into a shared container at capture time. `captured_mutated` drives the
        // loop (path A) boxing and the VM's capture filter. `needs_cell` is the
        // escape-analysis subset: captured-and-mutated locals closed over by at
        // least one child closure whose value ESCAPES the frame
        // (`closure_escapes[i]` — stored/returned/bound, not immediately
        // invoked). This replaces the old `>=2 distinct sibling closures` proxy.
        let mut captured_mutated: std::collections::HashSet<Symbol> =
            std::collections::HashSet::new();
        let mut needs_cell: std::collections::HashSet<Symbol> = std::collections::HashSet::new();
        // Free vars that must be cells in an ancestor (escape bubbling up through
        // this frame's NON-escaping closures — see `needs_cell_free_vars`).
        let mut needs_cell_free: std::collections::HashSet<Symbol> =
            std::collections::HashSet::new();
        for (i, nested) in self.closure_compiled_codes.iter().enumerate() {
            let escapes = self.closure_escapes.get(i).copied().unwrap_or(false);
            for sym in &nested.free_var_syms {
                let is_own = sym.with_str(|s| own.contains(s));
                if is_own && self_mutated.contains(sym) {
                    captured_mutated.insert(*sym);
                    if escapes {
                        needs_cell.insert(*sym);
                    }
                }
                // An escaping child closure that captures-and-mutates a var which
                // is NOT our local: that var needs a cell in the ancestor that
                // owns it. Bubble it up. (Mutation is checked against the union
                // of free-var writes folded in above.)
                if escapes && !is_own && free_writes.contains(sym) {
                    needs_cell_free.insert(*sym);
                }
            }
            // Bubble cell requirements that originated deeper in the subtree.
            for sym in &nested.needs_cell_free_vars {
                if sym.with_str(|s| own.contains(s)) {
                    // We declare this local; it must be a shared cell here.
                    captured_mutated.insert(*sym);
                    needs_cell.insert(*sym);
                } else {
                    // Still a free var here; keep bubbling toward the owner.
                    needs_cell_free.insert(*sym);
                }
            }
        }
        // Escaping-our-sub cell requirements bubbled up from nested scopes (an
        // `our sub` declared inside a nested block whose captured lexical we own).
        let mut nceos: std::collections::HashSet<Symbol> = std::collections::HashSet::new();
        let mut nceos_free: std::collections::HashSet<Symbol> = std::collections::HashSet::new();
        for nested in &self.closure_compiled_codes {
            for sym in &nested.needs_cell_escaping_our_sub_free {
                if sym.with_str(|s| own.contains(s)) {
                    nceos.insert(*sym);
                } else {
                    nceos_free.insert(*sym);
                }
            }
        }
        // Named-sub decl-site boxing (kept entirely separate from the closure
        // analysis above): a local that a directly-nested named sub WRITES must be
        // a shared cell so the sub's by-name env write and the owner's slot read
        // alias one cell, enabling cross-call accumulation. Only WRITES count (a
        // read-only capture works through the env snapshot); only own locals are
        // boxed here, non-own (ancestor) writes bubble up via
        // `needs_cell_named_sub_free`. This never touches `needs_cell`/closures, so
        // it cannot over-box an unrelated same-named local (e.g. a `let`-restored
        // var in a sibling block).
        let mut ncns: std::collections::HashSet<Symbol> = std::collections::HashSet::new();
        let mut ncns_free: std::collections::HashSet<Symbol> = std::collections::HashSet::new();
        for (nf_writes, nf_ncns_free) in &self.named_sub_captures {
            for sym in nf_writes {
                if sym.with_str(|s| own.contains(s)) {
                    ncns.insert(*sym);
                } else {
                    ncns_free.insert(*sym);
                }
            }
            for sym in nf_ncns_free {
                if sym.with_str(|s| own.contains(s)) {
                    ncns.insert(*sym);
                } else {
                    ncns_free.insert(*sym);
                }
            }
        }
        self.needs_cell_named_sub = ncns.into_iter().collect();
        self.needs_cell_named_sub_free = ncns_free.into_iter().collect();
        // Escaping-our-sub decl-site boxing: a local that a directly-nested
        // `our sub` READS or WRITES must be boxed into a shared cell AND persisted
        // (the registry routine outlives the block, with no closure env), so a call
        // after the block reads the live cell. Both reads and writes count here —
        // unlike `needs_cell_named_sub` (writes only), because a read-only capture
        // would otherwise resolve to `Nil` once the block scope is gone.
        // EXCLUDE `our`-declared vars: an `our sub` that reads an `our $msg` in the
        // same package block resolves it through the package/our store (handled by
        // the existing `GetGlobal` fallbacks), NOT the escaping-my-lexical cell. The
        // captured name maps to a local slot that is also recorded in `our_locals`.
        let our_slots: std::collections::HashSet<usize> =
            self.our_locals.iter().map(|(slot, _)| *slot).collect();
        for syms in &self.escaping_our_sub_captures {
            for sym in syms {
                let is_our_local = sym.with_str(|s| {
                    self.locals
                        .iter()
                        .position(|l| l == s)
                        .is_some_and(|slot| our_slots.contains(&slot))
                });
                if is_our_local {
                    continue;
                }
                if sym.with_str(|s| own.contains(s)) {
                    nceos.insert(*sym);
                } else {
                    nceos_free.insert(*sym);
                }
            }
        }
        self.needs_cell_escaping_our_sub = nceos.into_iter().collect();
        self.needs_cell_escaping_our_sub_free = nceos_free.into_iter().collect();
        self.free_var_syms = free.into_iter().collect();
        self.outer_ref_names = outer_ref_names;
        self.free_var_writes = free_writes.into_iter().collect();
        self.free_var_container_writes = free_container_writes.into_iter().collect();
        self.captured_mutated_locals = captured_mutated.into_iter().collect();
        self.needs_cell_locals = needs_cell.into_iter().collect();
        // A self-capturing declaration only matters when the local actually gets a
        // cell — otherwise there is no cell for the declaration to preserve.
        self.self_capture_decl_locals = self_capture_decl
            .into_iter()
            .filter(|sym| self.needs_cell_locals.contains(sym))
            .collect();
        self.needs_cell_free_vars = needs_cell_free.into_iter().collect();
        // Tell each closure we embed which of ITS free variables we (the creating
        // frame) vouch for: a plain lexical we declare and never mutate after the
        // capture op runs. Only such a capture can be installed with overwrite
        // semantics at call time — see `authoritative_free_vars`. This is the one
        // place that knows `captured_mutated`, and the nested codes are still
        // uniquely owned here, so `make_mut` does not clone.
        let vouched: std::collections::HashSet<Symbol> = self
            .locals
            .iter()
            .filter(|name| crate::env::is_plain_user_lexical(name))
            .map(|name| Symbol::intern(name))
            .filter(|sym| {
                // Reassigned / inc-dec'd by name (directly or by a nested closure).
                !self.captured_mutated_locals.contains(sym)
                    // Mutated in place as a container — invisible to `self_mutated`,
                    // but a `%h<k> = v` that copy-on-writes still strands the capture.
                    && !own_container_writes.contains(sym)
                    // Handed to a call, where an `is rw` param can write it back —
                    // making a by-value overwrite-install go stale (`my $x = ...;
                    // my $c = -> { $x }; mutate($x); $c()` must see the writeback).
                    // EXCEPTION: a `:=`-bound scalar is an immutable binding — it is
                    // never reassigned and (when bound to a value/attribute result,
                    // the zef `my $path := $candi.uri` shape) has no source container
                    // an rw param could write through, so its captured snapshot can
                    // never go stale. Vouching for it lets a closure that carries it
                    // into a foreign frame with a same-named parameter still resolve
                    // its own lexical binding (the misresolution otherwise picks up
                    // the callee's `$path`). See `scalar_bind_locals`.
                    && (!own_call_arg_sources.contains(sym)
                        || self.scalar_bind_locals.contains(sym))
            })
            .collect();
        for nested in &mut self.closure_compiled_codes {
            let authoritative: Vec<Symbol> = nested
                .free_var_syms
                .iter()
                .filter(|sym| vouched.contains(sym))
                .copied()
                .collect();
            if authoritative.is_empty() && nested.authoritative_free_vars.is_empty() {
                continue;
            }
            let nested_mut = Arc::make_mut(nested);
            nested_mut.authoritative_free_vars = authoritative;
            // Transitive vouching: a vouched capture stays authoritative
            // arbitrarily deep in this closure's subtree, as long as no
            // intermediate closure redeclares the name. The vouch already
            // guarantees the ENTIRE subtree never writes the name (nested
            // free-var writes fold into `captured_mutated` here, at the
            // declaring frame), so a grandchild's snapshot — taken from its own
            // creator's frame, where this same vouched value was installed —
            // cannot go stale either. Without this, a closure created inside
            // another closure's frame lost its authority the moment it escaped:
            // the middle frame cannot vouch (the name is not ITS local), so a
            // same-named lexical in the eventual calling frame shadowed the
            // capture again — the exact #4510 bug, one level deeper.
            let names = nested_mut.authoritative_free_vars.clone();
            for child in nested_mut.closure_compiled_codes.iter_mut() {
                Self::propagate_authoritative_down(child, &names);
            }
        }
    }

    /// Append `names` to the authoritative set of `cc` (when it captures them)
    /// and recurse into its closure subtree, stopping at any level that
    /// redeclares a name — from there down the name is a different binding.
    /// See the transitive-vouching comment in `compute_free_vars`.
    fn propagate_authoritative_down(cc: &mut std::sync::Arc<CompiledCode>, names: &[Symbol]) {
        let live: Vec<Symbol> = names
            .iter()
            .filter(|sym| sym.with_str(|s| !cc.locals.iter().any(|l| l == s)))
            .copied()
            .collect();
        if live.is_empty() {
            return;
        }
        let cc_mut = Arc::make_mut(cc);
        for sym in &live {
            if cc_mut.free_var_syms.contains(sym) && !cc_mut.authoritative_free_vars.contains(sym) {
                cc_mut.authoritative_free_vars.push(*sym);
            }
        }
        for child in cc_mut.closure_compiled_codes.iter_mut() {
            Self::propagate_authoritative_down(child, &live);
        }
    }

    /// The constant-pool index of a *pure scalar read* of a lexical by name — the
    /// only op `compute_upvalues` rewrites to `GetUpvalue` (Phase 1). Deliberately
    /// a strict subset of [`Self::op_name_const_idx`]: it excludes every
    /// read-write / write op (`PostIncrement`, `AssignExpr`, …) and the array/hash
    /// reads (`GetArrayVar`/`GetHashVar`), so only a scalar free variable the
    /// closure never mutates is ever a candidate.
    fn op_upvalue_read_const_idx(op: &OpCode) -> Option<u32> {
        match op {
            OpCode::GetGlobal(idx) => Some(*idx),
            _ => None,
        }
    }

    /// Promote this closure's *read-only plain-lexical* free variables to
    /// index-based upvalues: rewrite their pure-read ops to `GetUpvalue(i)` and
    /// record the captured order in `upvalue_syms`. Must run AFTER
    /// `compute_free_vars` / `compute_needs_env_sync` (it consumes `free_var_syms`,
    /// `free_var_writes`, `free_var_container_writes`, `captures_env_by_name`).
    ///
    /// Conservative by design (Phase 1):
    /// - Skips entirely when `captures_env_by_name` (loop/block/gather/whenever or
    ///   reflective bodies read lexicals by name through paths the op-scan cannot
    ///   rewrite).
    /// - A variable is eligible only if it is a plain user lexical, is NEVER
    ///   written anywhere in the closure subtree (not in `free_var_writes` /
    ///   `free_var_container_writes`), and appears in this code's ops only through
    ///   pure-read ops. Read-only capture means the by-value-or-cell snapshot in
    ///   the upvalue array observes the creator's container correctly (a mutated
    ///   capture is boxed into a shared `ContainerRef` cell, which the snapshot
    ///   clones), so reads stay coherent without any write-back.
    pub(crate) fn compute_upvalues(&mut self, runtime_bound: &std::collections::HashSet<Symbol>) {
        if self.captures_env_by_name {
            return;
        }
        let own: std::collections::HashSet<&str> = self.locals.iter().map(|s| s.as_str()).collect();
        let written: std::collections::HashSet<Symbol> = self
            .free_var_writes
            .iter()
            .chain(self.free_var_container_writes.iter())
            .copied()
            .collect();
        // Eligible = free, read-only, plain *scalar* user lexical, not an own
        // local. Scalars are stored sigil-less ("$x" -> "x"); arrays/hashes/subs
        // ("@a"/"%h"/"&f") are excluded in Phase 1 (their reads use distinct ops
        // and shared-container semantics handled separately). `runtime_bound`
        // excludes names this body binds at call time but that read via GetGlobal
        // (sub-signature capture params like `|c(Str $x)`), which only LOOK free.
        let eligible: std::collections::HashSet<Symbol> = self
            .free_var_syms
            .iter()
            .copied()
            .filter(|sym| !written.contains(sym))
            .filter(|sym| !runtime_bound.contains(sym))
            .filter(|sym| {
                sym.with_str(|s| {
                    crate::env::is_plain_user_lexical(s)
                        && !s.starts_with(['@', '%', '&'])
                        && !own.contains(s)
                })
            })
            .collect();
        if eligible.is_empty() {
            return;
        }
        // Assign indices in first-read order so the rewrite and the captured
        // `upvalue_syms` array stay aligned and deterministic. Record the exact op
        // positions to rewrite in the same pass (avoids a second self.ops borrow).
        let mut index_of: std::collections::HashMap<Symbol, u32> = std::collections::HashMap::new();
        let mut syms: Vec<Symbol> = Vec::new();
        let mut rewrites: Vec<(usize, u32, u32)> = Vec::new();
        for (op_pos, op) in self.ops.iter().enumerate() {
            if let Some(idx) = Self::op_upvalue_read_const_idx(op)
                && let Some(ValueView::Str(name)) =
                    self.constants.get(idx as usize).map(Value::view)
            {
                let sym = Symbol::intern(&name);
                if eligible.contains(&sym) {
                    let uv = *index_of.entry(sym).or_insert_with(|| {
                        let n = syms.len() as u32;
                        syms.push(sym);
                        n
                    });
                    rewrites.push((op_pos, uv, idx));
                }
            }
        }
        if syms.is_empty() {
            return;
        }
        for (op_pos, uv, name_idx) in rewrites {
            self.ops[op_pos] = OpCode::GetUpvalue {
                index: uv,
                name_idx,
            };
        }
        self.upvalue_syms = syms;
    }

    /// Store a compiled closure body and return its index. `escapes` records
    /// whether the closure was created in an escaping position (see
    /// `closure_escapes`); the two Vecs are kept index-aligned in lockstep.
    pub(crate) fn add_closure_code(&mut self, code: CompiledCode, escapes: bool) -> u32 {
        let idx = self.closure_compiled_codes.len() as u32;
        self.closure_compiled_codes.push(Arc::new(code));
        self.closure_escapes.push(escapes);
        idx
    }

    /// Bake one emit point's lexical scope chain, returning the index a
    /// `SymbolicDeref` carries. Not deduped: symbolic deref is rare, and two
    /// sites almost never share a chain anyway.
    pub(crate) fn add_lex_scope_chain(
        &mut self,
        chain: crate::compiler::lex_scope::LexScopeChain,
    ) -> u32 {
        let idx = self.lex_scopes.len() as u32;
        self.lex_scopes.push(Arc::new(chain));
        idx
    }

    pub(crate) fn emit(&mut self, op: OpCode) -> usize {
        if matches!(op, OpCode::OnceExpr { .. }) {
            self.has_once = true;
        }
        if !self.has_calls {
            // Every call opcode -- any of these can invoke a callee that writes
            // back an arbitrary captured variable into this frame's env. Keep
            // this list exhaustive: the closure writeback-skip's soundness
            // depends on it (a missed variant silently drops outward mutations).
            self.has_calls = matches!(
                op,
                OpCode::CallDefined
                    | OpCode::CallFunc { .. }
                    | OpCode::CallFuncNamed { .. }
                    | OpCode::CallMethod { .. }
                    | OpCode::CallMethodMut { .. }
                    | OpCode::CallMethodDynamic { .. }
                    | OpCode::CallMethodDynamicMut { .. }
                    | OpCode::ExecCall { .. }
                    | OpCode::ExecCallPairs { .. }
                    | OpCode::CallOnValue { .. }
                    | OpCode::CallOnCodeVar { .. }
                    | OpCode::HyperMethodCall { .. }
                    | OpCode::HyperMethodCallDynamic { .. }
                    // The I/O ops internally dispatch a user `$*OUT`/`$*ERR`
                    // override's `print` (plus `.Str`/`.gist` coercion of the
                    // arguments), which can write captured-outer/`our`/dynamic
                    // variables into this frame's env — omitting them made
                    // `can_skip_merge` restore a stale env snapshot on method
                    // exit and drop those writes (advent2010-day14).
                    | OpCode::Say(_)
                    | OpCode::Put(_)
                    | OpCode::Print(_)
                    | OpCode::Note(_)
            );
        }
        if !self.has_env_writes {
            self.has_env_writes = matches!(
                op,
                OpCode::SetGlobal(_)
                    | OpCode::SetGlobalRaw(_)
                    | OpCode::AssignExpr(_)
                    | OpCode::TopicDotAssign(_)
                    | OpCode::AssignExprLocal(_)
                    | OpCode::AtomicCompoundVar { .. }
                    | OpCode::IndexAssignExprNamed { .. }
                    | OpCode::IndexAssignExprNested { .. }
                    | OpCode::IndexAssignDeepNested { .. }
                    | OpCode::IndexAssignGeneric
                    | OpCode::IndexAssignPseudoStashNamed { .. }
                    | OpCode::IndexElemAutoviv { .. }
                    | OpCode::PostIncrement(..)
                    | OpCode::PostDecrement(..)
                    | OpCode::PostIncrementIndex(_)
                    | OpCode::PostDecrementIndex(_)
                    | OpCode::PreIncrement(..)
                    | OpCode::PreDecrement(..)
                    | OpCode::PreIncrementIndex(_)
                    | OpCode::PreDecrementIndex(_)
                    | OpCode::MultiDimIndexAssign { .. }
                    | OpCode::MultiDimIndexAssignGeneric(_)
                    | OpCode::CallFunc { .. }
                    | OpCode::CallFuncNamed { .. }
                    | OpCode::CallMethod { .. }
                    | OpCode::CallMethodMut { .. }
                    | OpCode::CallMethodDynamic { .. }
                    | OpCode::CallMethodDynamicMut { .. }
                    | OpCode::ExecCall { .. }
                    | OpCode::ExecCallPairs { .. }
                    | OpCode::HyperMethodCall { .. }
                    | OpCode::HyperMethodCallDynamic { .. }
                    | OpCode::BlockScope { .. }
                    | OpCode::BlockLocalScope { .. }
                    | OpCode::RegisterSub(_)
                    | OpCode::RegisterClass(_)
                    | OpCode::RegisterRole(_)
                    | OpCode::RegisterEnum(_)
                    | OpCode::RegisterPackage { .. }
                    | OpCode::RegisterPackageMy { .. }
            );
        }
        // Peephole (ADR-0006 §2.3): a `my $x = <expr>` declaration always ends in
        // `MarkExplicitInitializerContext; MarkVarDeclContext; SetLocal(slot)` —
        // two dispatches whose only effect is to set two flags the `SetLocal` body
        // reads and clears. Fuse them into one instruction.
        //
        // Safe because the fusion only ever rewrites markers this `emit()` just
        // appended, with nothing in between: no jump can target the middle of the
        // pair (a target is only ever recorded at the tail *between* statements,
        // and the three ops are emitted back-to-back), and a jump landing on the
        // first marker lands on the fused instruction instead, which does exactly
        // what falling through the pair into `SetLocal` did.
        if let OpCode::SetLocal(slot) = op
            && let Some(fused) = self.fuse_decl_markers(slot)
        {
            return fused;
        }
        let idx = self.ops.len();
        self.ops.push(op);
        self.op_lines.push(self.emit_line);
        idx
    }

    /// Replace a just-emitted trailing declaration-marker run with the fused
    /// `SetLocalDecl`. Returns the index of the fused instruction, or `None` when
    /// the tail is not a declaration (an ordinary assignment `$x = 1`).
    fn fuse_decl_markers(&mut self, slot: u32) -> Option<usize> {
        let n = self.ops.len();
        if n == 0 || !matches!(self.ops[n - 1], OpCode::MarkVarDeclContext) {
            return None;
        }
        let explicit_init =
            n >= 2 && matches!(self.ops[n - 2], OpCode::MarkExplicitInitializerContext);
        let keep = if explicit_init { n - 2 } else { n - 1 };
        self.ops.truncate(keep);
        self.op_lines.truncate(keep);
        let idx = self.ops.len();
        self.ops.push(OpCode::SetLocalDecl {
            slot,
            explicit_init,
        });
        self.op_lines.push(self.emit_line);
        Some(idx)
    }

    /// Attach `line` to every op emitted from now on (the compile-time half of
    /// the ip -> line table). Called for the `Stmt::SetLine` marker the parser
    /// inserts before each statement, and once at a sub/block body's start so
    /// its prologue ops carry the declaration line.
    pub(crate) fn set_emit_line(&mut self, line: i64) {
        self.emit_line = u32::try_from(line).unwrap_or(0);
    }

    /// The source line of the instruction at `ip`, or `None` when this chunk has
    /// no line information for it (a hand-built chunk, or a prologue emitted
    /// before any line marker). `None` means "leave the current line alone" —
    /// never report line 0.
    #[inline]
    pub(crate) fn line_at(&self, ip: usize) -> Option<i64> {
        match self.op_lines.get(ip) {
            Some(&0) | None => None,
            Some(&line) => Some(line as i64),
        }
    }

    /// Patch a jump instruction at `idx` to point to the current position.
    pub(crate) fn patch_jump(&mut self, idx: usize) {
        let target = self.ops.len() as i32;
        match &mut self.ops[idx] {
            OpCode::Jump(offset)
            | OpCode::JumpIfFalse(offset)
            | OpCode::JumpIfTrue(offset)
            | OpCode::JumpIfNotNil(offset) => {
                *offset = target;
            }
            _ => panic!("patch_jump on non-jump opcode"),
        }
    }

    /// Patch a jump instruction at `idx` to point to the given `target` position.
    pub(crate) fn patch_jump_to(&mut self, idx: usize, target: usize) {
        let target = target as i32;
        match &mut self.ops[idx] {
            OpCode::Jump(offset)
            | OpCode::JumpIfFalse(offset)
            | OpCode::JumpIfTrue(offset)
            | OpCode::JumpIfNotNil(offset) => {
                *offset = target;
            }
            _ => panic!("patch_jump_to on non-jump opcode"),
        }
    }

    #[allow(dead_code)]
    pub(crate) fn current_pos(&self) -> usize {
        self.ops.len()
    }

    /// Patch the body_end field of a loop opcode.
    pub(crate) fn patch_loop_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::WhileLoop { body_end, .. } => *body_end = target,
            OpCode::ForLoop(spec) => spec.body_end = target,
            OpCode::CStyleLoop { body_end, .. } => *body_end = target,
            OpCode::RepeatLoop { body_end, .. } => *body_end = target,
            OpCode::BlockScope { end, .. } => *end = target,
            _ => panic!("patch_loop_end on non-loop opcode"),
        }
    }

    pub(crate) fn patch_block_local_body_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::BlockLocalScope { body_end } => *body_end = target,
            _ => panic!("patch_block_local_body_end on non-BlockLocalScope opcode"),
        }
    }

    pub(crate) fn patch_block_pre_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::BlockScope { pre_end, .. } => *pre_end = target,
            _ => panic!("patch_block_pre_end on non-BlockScope opcode"),
        }
    }

    pub(crate) fn patch_block_enter_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::BlockScope { enter_end, .. } => *enter_end = target,
            _ => panic!("patch_block_enter_end on non-BlockScope opcode"),
        }
    }

    pub(crate) fn patch_block_body_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::BlockScope { body_end, .. } => *body_end = target,
            _ => panic!("patch_block_body_end on non-BlockScope opcode"),
        }
    }

    pub(crate) fn patch_block_keep_start(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::BlockScope { keep_start, .. } => *keep_start = target,
            _ => panic!("patch_block_keep_start on non-BlockScope opcode"),
        }
    }

    pub(crate) fn patch_block_undo_start(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::BlockScope { undo_start, .. } => *undo_start = target,
            _ => panic!("patch_block_undo_start on non-BlockScope opcode"),
        }
    }

    pub(crate) fn patch_block_post_start(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::BlockScope { post_start, .. } => *post_start = target,
            _ => panic!("patch_block_post_start on non-BlockScope opcode"),
        }
    }

    pub(crate) fn patch_leave_guard_next(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::LeaveGuard { next, .. } => *next = target,
            _ => panic!("patch_leave_guard_next on non-LeaveGuard opcode"),
        }
    }

    pub(crate) fn patch_repeat_cond_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::RepeatLoop { cond_end, .. } => *cond_end = target,
            _ => panic!("patch_repeat_cond_end on non-RepeatLoop opcode"),
        }
    }

    pub(crate) fn patch_body_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::Given { body_end, .. } => *body_end = target,
            OpCode::When { body_end, .. } => *body_end = target,
            OpCode::Default { body_end, .. } => *body_end = target,
            OpCode::PackageScope { body_end, .. } => *body_end = target,
            OpCode::DoBlockExpr { body_end, .. } => *body_end = target,
            OpCode::OnceExpr { body_end, .. } => *body_end = target,
            OpCode::DoGivenExpr { body_end, .. } => *body_end = target,
            OpCode::SubtestScope { body_end, .. } => *body_end = target,
            OpCode::ReactScope { body_end, .. } => *body_end = target,
            _ => panic!("patch_body_end on opcode without body_end"),
        }
    }

    pub(crate) fn patch_smart_match_rhs_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::SmartMatchExpr { rhs_end, .. } => *rhs_end = target,
            _ => panic!("patch_smart_match_rhs_end on non-SmartMatchExpr opcode"),
        }
    }

    pub(crate) fn patch_flip_flop_lhs_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::FlipFlopExpr { lhs_end, .. } => *lhs_end = target,
            _ => panic!("patch_flip_flop_lhs_end on non-FlipFlopExpr opcode"),
        }
    }

    pub(crate) fn patch_flip_flop_rhs_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::FlipFlopExpr { rhs_end, .. } => *rhs_end = target,
            _ => panic!("patch_flip_flop_rhs_end on non-FlipFlopExpr opcode"),
        }
    }

    pub(crate) fn patch_let_block_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::LetBlock { body_end, .. } => *body_end = target,
            _ => panic!("patch_let_block_end on non-LetBlock opcode"),
        }
    }

    pub(crate) fn patch_try_catch_start(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::TryCatch { catch_start, .. } => *catch_start = target,
            _ => panic!("patch_try_catch_start on non-TryCatch opcode"),
        }
    }

    pub(crate) fn patch_try_body_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::TryCatch { body_end, .. } => *body_end = target,
            _ => panic!("patch_try_body_end on non-TryCatch opcode"),
        }
    }

    pub(crate) fn patch_try_control_start(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::TryCatch { control_start, .. } => *control_start = target,
            _ => panic!("patch_try_control_start on non-TryCatch opcode"),
        }
    }

    pub(crate) fn patch_while_cond_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::WhileLoop { cond_end, .. } => *cond_end = target,
            _ => panic!("patch_while_cond_end on non-WhileLoop opcode"),
        }
    }

    pub(crate) fn patch_cstyle_cond_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::CStyleLoop { cond_end, .. } => *cond_end = target,
            _ => panic!("patch_cstyle_cond_end on non-CStyleLoop opcode"),
        }
    }

    pub(crate) fn patch_cstyle_step_start(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::CStyleLoop { step_start, .. } => *step_start = target,
            _ => panic!("patch_cstyle_step_start on non-CStyleLoop opcode"),
        }
    }

    /// Intern `value` into the constant pool, sharing the slot of an identical
    /// scalar constant already in it (ADR-0006 §2.4). The compiler pushes the
    /// same name/literal from many emit sites (a method name string per call
    /// site, `Value::NIL` per implicit return, ...), so the pool is heavily
    /// duplicated without this.
    ///
    /// Values with an observable identity (containers, Instances, Regex, ...)
    /// get no key and always take a fresh slot.
    pub(crate) fn add_constant(&mut self, value: Value) -> u32 {
        let Some(key) = ConstKey::of(&value) else {
            let idx = self.constants.len() as u32;
            self.constants.push(value);
            crate::vm::vm_stats::record_const_add(false);
            return idx;
        };
        if let Some(&idx) = self.const_index.get(&key) {
            crate::vm::vm_stats::record_const_add(true);
            return idx;
        }
        let idx = self.constants.len() as u32;
        self.constants.push(value);
        self.const_index.insert(key, idx);
        crate::vm::vm_stats::record_const_add(false);
        idx
    }

    /// Register a `CallFuncNamed` site's out-of-band named-arg spec, returning
    /// the index the op carries as `spec_idx`.
    pub(crate) fn add_named_arg_spec(&mut self, spec: NamedArgsSpec) -> u32 {
        let idx = self.named_arg_specs.len() as u32;
        self.named_arg_specs.push(Arc::new(spec));
        idx
    }

    pub(crate) fn add_stmt(&mut self, stmt: Stmt) -> u32 {
        let idx = self.stmt_pool.len() as u32;
        // Record a declaration fingerprint for SubDecls so a re-executed
        // `RegisterSub(idx)` (e.g. a `my sub` inside a hot routine) can be
        // recognized as an idempotent no-op without re-deriving the FunctionDef.
        if let Stmt::SubDecl {
            params,
            param_defs,
            body,
            return_type,
            multi,
            is_rw,
            is_raw,
            name_expr,
            ..
        } = &stmt
            && name_expr.is_none()
        {
            let fp = crate::ast::sub_registration_fingerprint(
                params,
                param_defs,
                body,
                return_type.as_ref(),
                *multi,
                *is_rw,
                *is_raw,
            );
            self.sub_fingerprints.insert(idx, fp);
        }
        self.stmt_pool.push(stmt);
        idx
    }
}

/// The compiled-functions table of a program (function key → compiled body).
/// `FxHashMap`: this map is probed on the light-call cache hit path of every
/// function call (ADR-0004 J4d), where std SipHash over the string key was a
/// measured ~5% of a recursion-heavy workload. The keys are internal,
/// compiler-generated strings, so HashDoS resistance buys nothing here.
///
/// Keyed by `Symbol` (S1b, docs/perf-callpath-scouting.md §3.1): the formatted
/// key strings are interned once at compile time, so a light-call cache hit
/// compares a `u32` symbol id instead of memcmp-ing a ~20-byte key string, and
/// the caches store the resolved key as a `Copy` `Symbol` (no per-entry `String`
/// allocation). The slow resolution path probes candidate keys via
/// `Symbol::lookup` (no interning of names that turn out not to exist), so a
/// missed probe never grows the global symbol table.
pub(crate) type CompiledFns = rustc_hash::FxHashMap<crate::symbol::Symbol, CompiledFunction>;

/// Out-of-band named-argument spec for a `CallFuncNamed` site: which of the
/// call's stack values are named-arg values, and under which keys.
#[derive(Clone, Debug)]
pub(crate) struct NamedArgsSpec {
    /// In argument (stack) order.
    pub(crate) entries: Vec<NamedArgEntry>,
}

/// One named argument of a [`NamedArgsSpec`].
#[derive(Clone, Debug)]
pub(crate) struct NamedArgEntry {
    /// Position among the call's `arity` stack values.
    pub(crate) pos: u32,
    /// The interned key, for the light path's `Symbol` compare.
    pub(crate) sym: Symbol,
    /// The key string, for fallback Pair materialization.
    pub(crate) key: String,
}

/// Precomputed bind plan for the light named-call path: what
/// `call_compiled_function_light`'s binding loop needs per call, derived once
/// per `CompiledFunction` instead of re-deriving match keys / locals slots /
/// env-mirror gates from strings on every call. Built whenever the signature
/// has at least one named parameter (all-named or mixed positional+named).
#[derive(Clone, Debug)]
pub(crate) struct NamedCallPlan {
    /// One entry per parameter, in `param_defs` order.
    pub(crate) params: Vec<LightParamBind>,
    /// Whether the body reads `@_` (has a `@_` local), so the caller's
    /// positional args must be materialized into it.
    pub(crate) uses_arg_array: bool,
    /// Number of positional (non-named) parameters, for arity errors.
    pub(crate) positional_count: usize,
}

/// One parameter's bind entry in a [`NamedCallPlan`].
#[derive(Clone, Debug)]
pub(crate) enum LightParamBind {
    Positional(PositionalParamBind),
    Named(NamedParamBind),
}

/// Per-positional-parameter entry of a [`NamedCallPlan`] (mixed signatures).
#[derive(Clone, Debug)]
pub(crate) struct PositionalParamBind {
    /// The parameter's locals slot (by `pd.name`), when it has one.
    pub(crate) slot: Option<usize>,
    /// Whether the bound value must also be written into the overlay env.
    pub(crate) needs_env: bool,
    /// Whether the parameter is required (missing => arity error).
    pub(crate) required: bool,
}

/// Per-parameter entry of a [`NamedCallPlan`].
#[derive(Clone, Debug)]
pub(crate) struct NamedParamBind {
    /// The key a caller's `:key(value)` pair must carry (sigil/twigil stripped).
    pub(crate) match_key: String,
    /// `match_key` interned, for the spec-based (out-of-band) named lookup.
    pub(crate) match_key_sym: Symbol,
    /// The parameter's locals slot (by `pd.name`), when it has one.
    pub(crate) slot: Option<usize>,
    /// Whether the bound value must also be written into the overlay env
    /// (a name-based reader exists for the slot / the param has no slot).
    pub(crate) needs_env: bool,
    /// Whether the parameter is required (missing => X::AdHoc).
    pub(crate) required: bool,
    /// `sub_signature` alias keys that also match this param
    /// (e.g. `colour` for `:color(:$colour)`).
    pub(crate) alias_keys: Vec<String>,
    /// On a match, every `sub_signature` name is additionally bound to the
    /// value: (bind name, its locals slot).
    pub(crate) alias_binds: Vec<(String, Option<usize>)>,
    /// `outer_sub_signature` alias keys (sigils trimmed) that also match.
    pub(crate) outer_alias_keys: Vec<String>,
}

/// A compiled function body (SubDecl compiled to bytecode).
#[derive(Debug, Clone)]
pub(crate) struct CompiledFunction {
    pub(crate) code: CompiledCode,
    /// Source file the routine was declared in (None = main script); flows
    /// from `FunctionDef::source_file` for backtrace frame attribution.
    pub(crate) source_file: Option<String>,
    pub(crate) params: Vec<String>,
    pub(crate) param_defs: Vec<ParamDef>,
    pub(crate) return_type: Option<String>,
    pub(crate) fingerprint: u64,
    /// When true, this sub has an explicit empty signature `()` and should reject any arguments.
    pub(crate) empty_sig: bool,
    /// When true, this sub is declared `is rw`.
    #[allow(dead_code)]
    pub(crate) is_rw: bool,
    /// When true, this sub is declared `is raw` and Proxy values should NOT be auto-FETCHed.
    pub(crate) is_raw: bool,
    /// Pre-computed mapping from positional parameter index to locals slot index.
    /// Used by the positional light call fast path to avoid name-based lookup per call.
    pub(crate) param_local_slots: Option<Vec<usize>>,
    /// True if the function body contains inner sub declarations or closures.
    /// When true, parameters must be written to env (not just locals) so that
    /// nested functions can capture them via closure.
    pub(crate) has_inner_subs: bool,
    /// True if the function body *directly* declares a lexical routine via a
    /// top-level `RegisterSub` / `RegisterSubset` opcode (`my sub`, a bare
    /// nested `sub`/`regex`/`token`/`rule`, or a `subset`). Such a routine is
    /// lexically scoped to this body and — unless it escapes by being returned —
    /// must be removed from the (program-global) routine registry when the call
    /// returns. A `my sub` nested inside a `{ }` block within the body is not
    /// counted here: `BlockScope` already restores the registry for it.
    pub(crate) declares_inner_routines: bool,
    /// Pre-computed bind plan for the light named-call path
    /// (`call_compiled_function_light`): per-parameter match keys, locals
    /// slots, and env-mirror gates that the binding loop would otherwise
    /// recompute from strings on every call. `Some` exactly when every
    /// parameter is named (the light path's eligibility precondition).
    pub(crate) named_call_plan: Option<Box<NamedCallPlan>>,
    /// Deprecation info: (kind, name, package, message).
    /// When set, every call records a deprecation event.
    pub(crate) deprecated_info: Option<(String, String, String, String)>,
    /// Set of variable names declared locally in this function (via `my`).
    /// Used by the positional light call path to distinguish function-local vars
    /// (which should be restored after recursive calls) from captured outer vars
    /// (which should keep their modified values).
    ///
    /// `Symbol`-keyed: the call paths test it against the callee's env-overlay
    /// keys, which are already `Symbol`s, so the return merge compares `u32`s
    /// instead of rebuilding a `HashSet<&str>` (SipHash) on every call.
    pub(crate) declared_locals: Option<rustc_hash::FxHashSet<Symbol>>,
    /// Pre-interned `param_defs[i].name`, parallel to `param_defs`. The call
    /// paths mark every param read-only on entry; without this each mark would
    /// re-hash the parameter name string on every call.
    pub(crate) param_name_syms: Vec<Symbol>,
    /// The package this routine was declared in (e.g. `"P"` for a sub in
    /// `package P { ... }`, `"GLOBAL"` for a top-level sub). The dispatch sets
    /// `current_package` from this on entry so package-scoped variable
    /// resolution (`our $x` / a `package { my $x }` lexical read or written from
    /// inside the sub) works on EVERY call — not just the first OTF compile,
    /// which was the only path that previously used the defining package. Call
    /// sites pass the *caller's* package, which is wrong for a by-name call into
    /// another package; this authoritative field fixes all of them at once.
    pub(crate) package: String,
}

impl CompiledFunction {
    /// Pre-compute the mapping from positional parameter index to locals slot index.
    ///
    /// Prefers the compiler-baked `code.param_local_slots` (authoritative slots
    /// recorded from `local_map` at emit time — §1.5, no name search). Falls back
    /// to the legacy by-name `locals.position` search only for hand-built code
    /// chunks that never recorded it.
    pub(crate) fn precompute_param_local_slots(&mut self) {
        if !self.code.param_local_slots.is_empty() {
            self.param_local_slots = Some(
                self.code
                    .param_local_slots
                    .iter()
                    .map(|&s| s as usize)
                    .collect(),
            );
            return;
        }
        let mut slots = Vec::new();
        if !self.param_defs.is_empty() {
            for pd in &self.param_defs {
                if pd.named {
                    continue;
                }
                if let Some(slot) = self.code.locals.iter().position(|n| n == &pd.name) {
                    slots.push(slot);
                }
            }
        } else {
            for param in &self.params {
                if let Some(slot) = self.code.locals.iter().position(|n| n == param) {
                    slots.push(slot);
                }
            }
        }
        if !slots.is_empty() {
            self.param_local_slots = Some(slots);
        }
    }

    /// Pre-compute the light named-call bind plan (see [`NamedCallPlan`]).
    pub(crate) fn precompute_named_call_plan(&mut self) {
        if self.param_defs.is_empty() || !self.param_defs.iter().any(|pd| pd.named) {
            return;
        }
        let slot_of = |name: &str| self.code.locals.iter().position(|n| n == name);
        // A slot's bound value must be mirrored into the overlay env when a
        // name-based reader exists for it (same compile-time analysis the
        // body's SetLocal flush uses); a param with no slot is env-only.
        let needs_env_of = |slot: Option<usize>| match slot {
            Some(s) => self.code.needs_env_sync.get(s).copied().unwrap_or(true),
            None => true,
        };
        let mut params = Vec::with_capacity(self.param_defs.len());
        let mut positional_count = 0usize;
        for pd in &self.param_defs {
            if !pd.named {
                positional_count += 1;
                let slot = slot_of(&pd.name);
                params.push(LightParamBind::Positional(PositionalParamBind {
                    slot,
                    needs_env: needs_env_of(slot),
                    // A positional is required unless explicitly optional
                    // (`$x?`) or defaulted (`pd.required` is the NAMED `!`
                    // marker and is false for a plain `$x`).
                    required: !pd.optional_marker && pd.default.is_none(),
                }));
                continue;
            }
            let match_key = pd
                .name
                .strip_prefix('@')
                .or_else(|| pd.name.strip_prefix('%'))
                .unwrap_or(&pd.name);
            let match_key = match_key
                .strip_prefix('!')
                .or_else(|| match_key.strip_prefix('.'))
                .unwrap_or(match_key)
                .to_string();
            let slot = slot_of(&pd.name);
            let mut alias_keys = Vec::new();
            let mut alias_binds = Vec::new();
            if let Some(ref sub_params) = pd.sub_signature {
                // A named alias can chain: `:type(:class($kind))` nests further
                // renames. Walk every level so all caller-facing alias keys
                // (`class`) match and the innermost variable (`$kind`, the one
                // the body reads) is bound. A worklist avoids recursion here.
                let mut worklist: Vec<&crate::ast::ParamDef> = sub_params.iter().collect();
                let mut idx = 0;
                while idx < worklist.len() {
                    let sub_pd = worklist[idx];
                    idx += 1;
                    if sub_pd.named {
                        alias_keys.push(
                            sub_pd
                                .name
                                .strip_prefix(':')
                                .unwrap_or(&sub_pd.name)
                                .to_string(),
                        );
                    }
                    // On a match (by any key), every sub-signature name is
                    // bound to the value — e.g. `:color(:$colour)` binds both.
                    alias_binds.push((sub_pd.name.clone(), slot_of(&sub_pd.name)));
                    if let Some(ref nested) = sub_pd.sub_signature {
                        worklist.extend(nested.iter());
                    }
                }
            }
            let mut outer_alias_keys = Vec::new();
            if let Some(ref outer) = pd.outer_sub_signature {
                for outer_pd in outer {
                    outer_alias_keys.push(
                        outer_pd
                            .name
                            .trim_start_matches(|c: char| "$@%&".contains(c))
                            .to_string(),
                    );
                }
            }
            params.push(LightParamBind::Named(NamedParamBind {
                match_key_sym: Symbol::intern(&match_key),
                match_key,
                slot,
                needs_env: needs_env_of(slot),
                required: pd.required,
                alias_keys,
                alias_binds,
                outer_alias_keys,
            }));
        }
        self.named_call_plan = Some(Box::new(NamedCallPlan {
            params,
            uses_arg_array: self.code.locals.iter().any(|n| n == "@_"),
            positional_count,
        }));
    }

    /// Detect whether the function body contains inner sub declarations or closures.
    pub(crate) fn detect_inner_subs(&mut self) {
        self.has_inner_subs = !self.code.closure_compiled_codes.is_empty()
            || self.code.ops.iter().any(|op| {
                matches!(
                    op,
                    OpCode::RegisterSub(..)
                        | OpCode::RegisterSubset(..)
                        | OpCode::RegisterClass(..)
                        | OpCode::RegisterRole(..)
                        // CallOnValue/CallOnCodeVar may invoke closures that do `return`
                        // targeting an outer routine, requiring the routine stack.
                        | OpCode::CallOnValue { .. }
                        | OpCode::CallOnCodeVar { .. }
                        // ForLoop may have FIRST/NEXT/LAST phasers that need proper state
                        | OpCode::ForLoop(..)
                )
            });
        // A routine declared directly in this body (not inside a nested
        // BlockScope, which restores the registry itself) is lexical to the
        // body and must be unregistered on return unless it escapes.
        self.declares_inner_routines = self
            .code
            .ops
            .iter()
            .any(|op| matches!(op, OpCode::RegisterSub(..) | OpCode::RegisterSubset(..)));
    }

    /// Compute the set of variable names declared locally in this function
    /// (via SetVarDynamic opcode, which is emitted for `my` declarations).
    /// Also includes parameter names. Used to distinguish function-local vars
    /// from captured outer vars in the positional light call path.
    pub(crate) fn compute_declared_locals(&mut self) {
        let mut declared: std::collections::HashSet<String> = std::collections::HashSet::new();
        // Parameters are always function-local (including sub-signature params)
        Self::collect_param_names(&self.param_defs, &mut declared);
        for p in &self.params {
            declared.insert(p.clone());
        }
        // Scan opcodes for SetVarDynamic which marks `my` declarations, and
        // ForLoop which binds its loop parameter(s). A `for` param (`-> $idx`) is
        // callee-local: it is bound by the ForLoop op into env, not via a `my`
        // (SetVarDynamic) or the function signature, so without collecting it here
        // the scoped-overlay return merge would leak the callee's last loop value
        // into a caller lexical of the same name (recursion into a same-named
        // `for` loop that early-returns).
        for op in &self.code.ops {
            match op {
                OpCode::SetVarDynamic { name_idx, .. } => {
                    if let Some(crate::value::ValueView::Str(name)) = self
                        .code
                        .constants
                        .get(*name_idx as usize)
                        .map(crate::value::Value::view)
                    {
                        declared.insert(name.to_string());
                    }
                }
                OpCode::ForLoop(spec) => {
                    if let Some(idx) = spec.param_idx
                        && let Some(crate::value::ValueView::Str(name)) = self
                            .code
                            .constants
                            .get(idx as usize)
                            .map(crate::value::Value::view)
                    {
                        declared.insert(name.to_string());
                    }
                    for name in &spec.multi_param_names {
                        declared.insert(name.clone());
                    }
                }
                _ => {}
            }
        }
        self.declared_locals = Some(declared.iter().map(|n| Symbol::intern(n)).collect());
    }

    /// Pre-intern the parameter names (see `param_name_syms`).
    pub(crate) fn precompute_param_name_syms(&mut self) {
        self.param_name_syms = self
            .param_defs
            .iter()
            .map(|pd| Symbol::intern(&pd.name))
            .collect();
    }

    /// True if `sym` names a *callee-local* of this function — a parameter, a
    /// `my` declaration, or a `for`-loop parameter. The scoped-overlay return
    /// merge uses this to decide which of the callee's env writes are its own
    /// (dropped with the overlay) and which target a captured outer variable
    /// (merged back into the caller).
    ///
    /// Prefers the precomputed `declared_locals`; a hand-built chunk that never
    /// ran `compute_declared_locals` falls back to the code's own local names,
    /// exactly as the call sites did before.
    #[inline]
    pub(crate) fn is_callee_local_sym(&self, sym: Symbol) -> bool {
        match &self.declared_locals {
            Some(declared) => declared.contains(&sym),
            None if self.code.locals_sym.len() == self.code.locals.len() => {
                self.code.locals_sym.contains(&sym)
            }
            // Unfinalized chunk: `locals_sym` was never computed, so compare by name.
            None => {
                let name = sym.as_str();
                self.code.locals.iter().any(|n| n == name)
            }
        }
    }

    /// Recursively collect parameter names from param_defs, including
    /// sub-signature parameters (e.g. `[$p, *@r]` in array unpacking).
    fn collect_param_names(
        param_defs: &[crate::ast::ParamDef],
        declared: &mut std::collections::HashSet<String>,
    ) {
        for pd in param_defs {
            declared.insert(pd.name.clone());
            if let Some(ref sub_sig) = pd.sub_signature {
                Self::collect_param_names(sub_sig, declared);
            }
        }
    }
}
