use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use crate::ast::{ParamDef, Stmt};
use crate::symbol::Symbol;
use crate::value::Value;

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
    /// Like GetLocal but does NOT resolve DeferredHashAccess/HashSlotRef values.
    /// Used by `=:=` to compare raw container references.
    GetLocalRaw(u32),
    SetLocal(u32),
    GetGlobal(u32),
    /// Load `self` from the captured environment for a `$.attr` accessor.
    /// Raises X::Syntax::NoSelf (the operand is the constant index of the
    /// accessor's display name, e.g. `$.a`) when `self` is unavailable.
    GetSelfOrNoSelf(u32),
    SetGlobal(u32),
    /// Like SetGlobal but skips @/% coercion (used for `constant @x` / `constant %x`).
    SetGlobalRaw(u32),
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
    SetVarType {
        name_idx: u32,
        tc_idx: u32,
    },
    SetTopic,
    SaveTopic,
    RestoreTopic,
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
    Decont,     // strip ONE level of Value::Scalar for slurpy flattening (NOT the
    // recursive Value::descalarize; touches no ArrayKind flag — see decont family note)
    /// Itemize (containerize) an Array/List value so it behaves as a single
    /// item in list context. Emitted when `$` variable values are used inside
    /// `ArrayLiteral` or assigned to `@`/`%` targets.
    Itemize,
    /// Like `Itemize`, but skips itemization when the named scalar variable is
    /// bound (`:=`) to a Positional value. A bound scalar is NOT a Scalar
    /// container, so `@a = $bound` must flatten (matching Raku). The argument is
    /// the constant-pool index of the variable name. Emitted for `@a = $var`.
    ItemizeVar(u32),
    /// Wrap the top-of-stack value in a Value::Scalar container.
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
    /// Compares DeferredHashAccess/HashSlotRef values by checking if they
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
        /// True when RHS was originally `m//` (MatchRegex), which affects
        /// failure return value: `m//` failure returns False, bare `//` returns Nil.
        rhs_is_match_regex: bool,
        /// True when the LHS is a literal (non-lvalue). A destructive `s///`/`tr///`
        /// that matches against a literal must throw X::Assignment::RO.
        lhs_is_literal: bool,
        /// True when the RHS is a plain `Value::Regex` literal. Compile-time half
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
    DoesVar(u32),
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

    // -- Nil check (for defined-or //) --
    #[allow(dead_code)]
    IsNil,

    // -- Control flow --
    /// No-op label marker for `goto`.
    Label(u32),
    /// Jump to `Label` by runtime-evaluated name on stack.
    Goto,
    Jump(i32),
    JumpIfFalse(i32),
    JumpIfTrue(i32),
    /// Jump if top of stack is nil/undefined (without popping)
    #[allow(dead_code)]
    JumpIfNil(i32),
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
    /// Pop with sink context — throws unhandled Failures when fatal_mode is active
    SinkPop,

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
    /// Expression-level function call with capture slip: pop 1 slip + `regular_arity` args,
    /// flatten the slip into the argument list, call name, push result.
    CallFuncSlip {
        name_idx: u32,
        regular_arity: u32,
        arg_sources_idx: Option<u32>,
        /// Position of the slip argument among all compiled args (0-based).
        /// When None, slip is compiled last (legacy behavior).
        slip_pos: Option<u32>,
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
    /// Statement-level call with positional/named values encoded as Value::Pair.
    ExecCallPairs {
        name_idx: u32,
        arity: u32,
    },
    /// Call with capture slip: `regular_arity` normal args + 1 slip arg on stack.
    /// The slip arg (top of stack) is an Array whose elements are flattened into
    /// the argument list before the call.
    ExecCallSlip {
        name_idx: u32,
        regular_arity: u32,
        arg_sources_idx: Option<u32>,
        /// Position of the slip argument among all compiled args (0-based).
        slip_pos: Option<u32>,
    },
    BlockScope {
        pre_end: u32,
        enter_end: u32,
        body_end: u32,
        keep_start: u32,
        undo_start: u32,
        post_start: u32,
        end: u32,
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
    DoBlockExpr {
        body_end: u32,
        label: Option<String>,
        scope_isolate: bool,
    },
    OnceExpr {
        key_idx: u32,
        body_end: u32,
    },
    DoGivenExpr {
        body_end: u32,
    },
    MakeGather(u32),
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
    /// Like Index, but auto-vivifies intermediate hash entries and returns
    /// a `HashSlotRef` for the final key.  Used by IndexAutovivifyLazy's
    /// fallback path for non-hash targets.
    #[allow(dead_code)]
    IndexAutovivify,
    /// Like IndexAutovivify but does NOT create the hash entry if missing.
    /// Returns a HashSlotRef that defers creation until write.
    /// Used for the outermost level of `:=` bind so that binding alone
    /// does not autovivify (e.g. `my $b := %h<a><b>` keeps %h empty).
    IndexAutovivifyLazy,
    /// Like IndexAutovivifyLazy, but the index is the TERMINAL element of a `:=`
    /// bind RHS. A container-valued (Array/Hash) leaf is promoted to a
    /// `ContainerRef` cell — not kept as a traversal SlotRef.
    IndexAutovivifyLazyTerminal,
    DeleteIndexNamed(u32),
    DeleteIndexExpr,
    /// Multi-dimensional indexing: @a[$x;$y;$z]
    /// Stack: [target, dim0, dim1, ..., dimN] → [result]
    #[allow(dead_code)]
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
    /// Hash hyperslice: recursively iterate hash with given adverb mode.
    /// Stack: [target] → [result list]
    HyperSlice(u8),
    /// Hash hyperindex: drill into nested hash by key path.
    /// Stack: [target, keys] → [value]
    HyperIndex,

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
    /// Tag the current value as coming from a named container (for Scalar binding)
    TagContainerRef(u32),
    /// Tag the current value as coming from a reversed named container (for `@a.reverse` writeback)
    TagContainerRefReversed(u32),
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
    PreIncrement(u32),
    PreDecrement(u32),
    PreIncrementIndex(u32),
    PreDecrementIndex(u32),

    // -- Variable access --
    GetCaptureVar(u32),
    GetCodeVar(u32),

    // -- Postfix operators --
    PostIncrement(u32),
    PostDecrement(u32),
    PostIncrementIndex(u32),
    PostDecrementIndex(u32),
    /// Named index assignment: `var[idx] = value` where `var` is a known
    /// variable name. `is_positional` records whether the subscript was
    /// `[...]` (positional) or `{...}`/`<...>` (associative); used to
    /// choose Array vs Hash when autovivifying a missing variable.
    IndexAssignExprNamed {
        name_idx: u32,
        is_positional: bool,
    },
    IndexAssignPseudoStashNamed {
        stash_name_idx: u32,
        key_name_idx: u32,
    },

    // -- Assignment as expression --
    AssignExpr(u32),
    /// Assignment as expression for local variable (indexed slot)
    AssignExprLocal(u32),
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
    /// `positional_flags_idx` is a constant index holding a Value::Array of booleans
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
    ForLoop {
        param_idx: Option<u32>,
        param_local: Option<u32>,
        body_end: u32,
        label: Option<String>,
        arity: u32,
        collect: bool,
        /// Restore outer `$_` after loop execution (used by postfix/do-for semantics).
        restore_topic: bool,
        /// When true, run the loop body in a spawned thread (race for / hyper for).
        threaded: bool,
        /// When true, the named param is writable (via `<->`, `is rw`, or `is copy`).
        is_rw: bool,
        /// When true, write back modifications to the source container.
        do_writeback: bool,
        /// Param names for multi-param rw for loops (used for writeback).
        rw_param_names: Vec<String>,
        /// When true, the iterable is from .kv (key-value pairs).
        /// Writeback only applies to value params (odd-indexed in the chunk).
        kv_mode: bool,
        /// Variable names for per-element writeback when the iterable is a list
        /// of scalar variables (e.g. `for ($a, $b, $c) { $_++ }`).
        source_var_names: Vec<String>,
        /// When true, Junction items are expanded into their eigenstates
        /// (parameter type is Any or more specific, not Mu or Junction).
        autothread_junctions: bool,
        /// When true, the block explicitly declared zero parameters (`-> {}`).
        /// Passing any argument should throw "Too many positionals passed".
        explicit_zero_params: bool,
        /// Names of multi-param bindings (for `-> $a, \b, $c` loops).
        /// Used to temporarily clear sigilless readonly flags before binding.
        multi_param_names: Vec<String>,
        /// When true, the iterable is a `.pairs`/`.antipairs` transform: the
        /// loop variable is a `Pair` that *wraps* the source element, not the
        /// element itself. The plain (topic/named) per-element source writeback
        /// is suppressed (it would overwrite the element with the Pair); the
        /// source tag is still kept so the Pair's rw `.value` alias can detect
        /// immutability and propagate.
        loop_var_wraps_element: bool,
    },
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
        x_count: Option<u32>,
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
        x_count: Option<u32>,
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
    },

    // -- Error handling --
    Die,
    Fail,

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
    ApplyVarTrait {
        name_idx: u32,
        trait_name_idx: u32,
        has_arg: bool,
    },

    /// Get a variable from the caller's scope ($CALLER::varname).
    /// name_idx = constant index for the bare variable name (without CALLER:: prefix).
    /// depth = number of CALLER:: levels (1 for $CALLER::x, 2 for $CALLER::CALLER::x).
    GetCallerVar {
        name_idx: u32,
        depth: u32,
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
    GetOuterVar {
        name_idx: u32,
        depth: u32,
    },

    /// Get a variable by searching the dynamic call stack ($DYNAMIC::varname).
    GetDynamicVar(u32),

    /// Indirect type lookup: pop string from stack, resolve to Package value.
    IndirectTypeLookup,

    /// Indirect code lookup: pop package string from stack, resolve &name in that package context.
    IndirectCodeLookup(u32),

    /// Symbolic variable dereference: pop name string from stack, look up variable by sigil+name.
    /// The u32 indexes the constant pool for the sigil string ("$", "@", or "%").
    SymbolicDeref(u32),

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
    },

    /// Block with `let` scope management. Executes body, then checks
    /// the topic ($_) to decide whether to restore or discard let saves.
    LetBlock {
        body_end: u32,
    },

    /// Set the current source line number (for deprecation tracking, error messages, etc.).
    SetSourceLine(i64),
}

/// A compiled chunk of bytecode.
#[derive(Debug, Clone)]
pub(crate) struct CompiledCode {
    pub(crate) ops: Vec<OpCode>,
    pub(crate) constants: Vec<Value>,
    pub(crate) stmt_pool: Vec<Stmt>,
    pub(crate) locals: Vec<String>,
    /// Pre-interned Symbol for each local name. Avoids Symbol::intern()
    /// on every env sync in hot paths.
    pub(crate) locals_sym: Vec<Symbol>,
    /// Bitmap: true if local[i] is eligible for SetLocal fast path
    /// (simple $-prefixed scalar, no twigils, no ::, no _ topic, no ./! attrs).
    pub(crate) simple_locals: Vec<bool>,
    /// Maps local slot indices to persistent state keys for `state` variables.
    pub(crate) state_locals: Vec<(usize, String)>,
    /// Maps local slot indices to qualified package names for `our` variables.
    /// Used by BlockScope restoration to sync local slots from their global values.
    pub(crate) our_locals: Vec<(usize, String)>,
    /// Pre-compiled closure bodies embedded in this code chunk.
    pub(crate) closure_compiled_codes: Vec<Arc<CompiledCode>>,
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
    /// Free variables this code (and its nested closures) reference from an
    /// enclosing scope: names used via GetGlobal-family ops that are not this
    /// code's own locals. For a closure body this is the set of captured
    /// lexicals whose per-instance mutable state actually matters, so the
    /// closure-call path can persist/restore only these instead of iterating
    /// the entire (~100-entry) captured env. Empty until `compute_free_vars`
    /// runs (during `compute_needs_env_sync`).
    pub(crate) free_var_syms: Vec<Symbol>,
    /// Free variables (names not in this code's own locals) that this code or a
    /// nested closure *writes* (assign / inc-dec / bind). Folded up from nested
    /// closures so an enclosing scope can tell which of *its* locals are mutated
    /// from inside a closure. Used to compute `captured_mutated_locals`.
    pub(crate) free_var_writes: Vec<Symbol>,
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
}

impl CompiledCode {
    pub(crate) fn new() -> Self {
        Self {
            ops: Vec::new(),
            constants: Vec::new(),
            stmt_pool: Vec::new(),
            locals: Vec::new(),
            locals_sym: Vec::new(),
            simple_locals: Vec::new(),
            state_locals: Vec::new(),
            our_locals: Vec::new(),
            closure_compiled_codes: Vec::new(),
            closure_escapes: Vec::new(),
            is_routine: false,
            source_line: None,
            is_pointy_block: false,
            has_env_writes: false,
            may_capture_outer_vars: false,
            needs_env_sync: Vec::new(),
            free_var_syms: Vec::new(),
            free_var_writes: Vec::new(),
            captured_mutated_locals: Vec::new(),
            needs_cell_locals: Vec::new(),
            needs_cell_free_vars: Vec::new(),
            has_calls: false,
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
                | OpCode::PostIncrement(idx)
                | OpCode::PostDecrement(idx)
                | OpCode::PreIncrement(idx)
                | OpCode::PreDecrement(idx)
                | OpCode::GetArrayVar(idx)
                | OpCode::GetHashVar(idx) => Some(*idx),
                OpCode::AssignExpr(idx) => Some(*idx),
                _ => None,
            };
            if let Some(idx) = name_idx
                && let Some(Value::Str(name)) = self.constants.get(idx as usize)
            {
                let name = name.as_str();
                if locals_set.contains(name) {
                    continue;
                }
                // Skip known method-specific/internal names
                if name == "self"
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
                    || name.starts_with("?")
                    || name.starts_with('^')
                {
                    continue;
                }
                self.may_capture_outer_vars = true;
                return;
            }
        }
    }

    /// Pre-intern all local names as Symbols.
    pub(crate) fn compute_locals_sym(&mut self) {
        self.locals_sym = self.locals.iter().map(|s| Symbol::intern(s)).collect();
    }

    /// Compute which locals need to be synced to env.
    /// A local needs env sync if it's referenced by GetGlobal/SetGlobal/etc.
    /// in this code. Locals only accessed via GetLocal don't need env sync,
    /// which reduces env size and makes method call env clones cheaper.
    pub(crate) fn compute_needs_env_sync(&mut self) {
        self.compute_locals_sym();
        self.compute_free_vars();
        // Always scan for reflective caller-lexical access (independent of the
        // needs_env_sync early returns below), so the global flag is set even for
        // loop/block or zero-local frames.
        self.scan_reflective_name_access();
        let n = self.locals.len();
        self.needs_env_sync = vec![false; n];
        if n == 0 {
            return;
        }
        // Conservative fallback: code that runs inline control-flow bodies with
        // their own env/locals juggling (for/while/loop bodies, which the
        // loop-phaser desugaring threads state through by name via `env`, e.g.
        // the `__mutsu_loop_first_`/`__mutsu_loop_ran_` control temps) cannot
        // safely treat any local as slot-only -- a slot value may not survive the
        // loop's per-iteration env round-trips. The same applies to `MakeGather`:
        // a gather block compiles its body inline and snapshots the *whole*
        // interpreter env by name (vm_register_ops::exec_make_gather_op), but the
        // body is not registered in `closure_compiled_codes`, so the nested-closure
        // free-var scan below cannot see which locals it reads. Mark every local
        // env-synced so the dual-store flush gate (vm_env_helpers) keeps the full
        // flush for such frames. Recursion-heavy code without loops/gather (e.g.
        // `fib`) is unaffected and still skips the per-call flush for its slot-only
        // params.
        let captures_env_by_name = self.ops.iter().any(|op| {
            matches!(
                op,
                OpCode::ForLoop { .. }
                    | OpCode::BlockScope { .. }
                    | OpCode::BlockLocalScope { .. }
                    | OpCode::MakeGather(_)
            )
        });
        if captures_env_by_name {
            self.needs_env_sync.iter_mut().for_each(|b| *b = true);
            return;
        }
        let locals_map: std::collections::HashMap<&str, usize> = self
            .locals
            .iter()
            .enumerate()
            .map(|(i, name)| (name.as_str(), i))
            .collect();
        // A nested closure created in this frame captures the env at creation
        // time and later reads its free variables from that snapshot by name.
        // Any local of *this* frame that is a free variable of some nested
        // closure must therefore be flushed to env before the capture. This
        // replaces the old conservative `fill(true)` (which mirrored *every*
        // local to env whenever any closure existed) with the exact set of
        // locals a closure can actually observe.
        for nested in &self.closure_compiled_codes {
            for sym in &nested.free_var_syms {
                if let Some(slot) = sym.with_str(|s| locals_map.get(s).copied()) {
                    self.needs_env_sync[slot] = true;
                }
            }
        }
        for op in &self.ops {
            let name_idx = match op {
                OpCode::GetGlobal(idx)
                | OpCode::SetGlobal(idx)
                | OpCode::SetGlobalRaw(idx)
                | OpCode::PostIncrement(idx)
                | OpCode::PostDecrement(idx)
                | OpCode::PreIncrement(idx)
                | OpCode::PreDecrement(idx)
                | OpCode::GetArrayVar(idx)
                | OpCode::GetHashVar(idx)
                | OpCode::AssignExpr(idx) => Some(*idx),
                _ => None,
            };
            if let Some(idx) = name_idx
                && let Some(Value::Str(name)) = self.constants.get(idx as usize)
                && let Some(&slot) = locals_map.get(name.as_str())
            {
                self.needs_env_sync[slot] = true;
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
                | OpCode::GetOuterVar { .. }
                | OpCode::GetPseudoStash(_)
                | OpCode::SymbolicDeref(_)
                | OpCode::SymbolicDerefStore(_)
                | OpCode::IndirectCodeLookup(_) => true,
                OpCode::CallFunc { name_idx, .. } | OpCode::CallFuncSlip { name_idx, .. } => {
                    matches!(
                        self.constants.get(*name_idx as usize),
                        Some(Value::Str(name)) if name.as_str() == "EVAL" || name.as_str() == "EVALFILE"
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
            | OpCode::PostIncrement(idx)
            | OpCode::PostDecrement(idx)
            | OpCode::PreIncrement(idx)
            | OpCode::PreDecrement(idx)
            | OpCode::GetArrayVar(idx)
            | OpCode::GetHashVar(idx)
            | OpCode::AssignExpr(idx) => Some(*idx),
            _ => None,
        }
    }

    /// The constant-pool index naming the variable an op *writes* by name
    /// (assignment / increment / decrement). Subset of `op_name_const_idx` that
    /// excludes pure reads. Used to compute `free_var_writes` /
    /// `captured_mutated_locals`. NOTE: declaration (`SetLocal` after
    /// `MarkVarDeclContext`) and own-local reassignment (`AssignExprLocal`) are
    /// slot-based and handled separately by the caller.
    fn op_name_write_const_idx(op: &OpCode) -> Option<u32> {
        match op {
            OpCode::SetGlobal(idx)
            | OpCode::SetGlobalRaw(idx)
            | OpCode::PostIncrement(idx)
            | OpCode::PostDecrement(idx)
            | OpCode::PreIncrement(idx)
            | OpCode::PreDecrement(idx)
            | OpCode::AssignExpr(idx) => Some(*idx),
            _ => None,
        }
    }

    /// Compute `free_var_syms`: the names this code references from an enclosing
    /// scope (GetGlobal-family ops whose name is not one of this code's own
    /// locals), unioned with the free variables of nested closures that are not
    /// resolved by this code's locals. Nested closures have already had their
    /// own `free_var_syms` computed (they are finalized before being embedded),
    /// so they are folded in directly without re-walking their ops.
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
        let mut pending_decl = false;
        for op in &self.ops {
            // Read+write free-var set (names referenced from an enclosing scope).
            if let Some(idx) = Self::op_name_const_idx(op)
                && let Some(Value::Str(name)) = self.constants.get(idx as usize)
                && !own.contains(name.as_str())
            {
                free.insert(Symbol::intern(name));
            }
            // Name-based writes: either a free-var write or an own-local mutation.
            if let Some(idx) = Self::op_name_write_const_idx(op)
                && let Some(Value::Str(name)) = self.constants.get(idx as usize)
            {
                if own.contains(name.as_str()) {
                    self_mutated.insert(Symbol::intern(name));
                } else {
                    free_writes.insert(Symbol::intern(name));
                }
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
                OpCode::AssignExprLocal(slot) => {
                    if let Some(name) = self.locals.get(*slot as usize) {
                        self_mutated.insert(Symbol::intern(name));
                    }
                    pending_decl = false;
                }
                _ => {}
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
        self.free_var_syms = free.into_iter().collect();
        self.free_var_writes = free_writes.into_iter().collect();
        self.captured_mutated_locals = captured_mutated.into_iter().collect();
        self.needs_cell_locals = needs_cell.into_iter().collect();
        self.needs_cell_free_vars = needs_cell_free.into_iter().collect();
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

    pub(crate) fn emit(&mut self, op: OpCode) -> usize {
        if !self.has_calls {
            // Every call opcode -- any of these can invoke a callee that writes
            // back an arbitrary captured variable into this frame's env. Keep
            // this list exhaustive: the closure writeback-skip's soundness
            // depends on it (a missed variant silently drops outward mutations).
            self.has_calls = matches!(
                op,
                OpCode::CallDefined
                    | OpCode::CallFunc { .. }
                    | OpCode::CallFuncSlip { .. }
                    | OpCode::CallMethod { .. }
                    | OpCode::CallMethodMut { .. }
                    | OpCode::CallMethodDynamic { .. }
                    | OpCode::CallMethodDynamicMut { .. }
                    | OpCode::ExecCall { .. }
                    | OpCode::ExecCallPairs { .. }
                    | OpCode::ExecCallSlip { .. }
                    | OpCode::CallOnValue { .. }
                    | OpCode::CallOnCodeVar { .. }
                    | OpCode::HyperMethodCall { .. }
                    | OpCode::HyperMethodCallDynamic { .. }
            );
        }
        if !self.has_env_writes {
            self.has_env_writes = matches!(
                op,
                OpCode::SetGlobal(_)
                    | OpCode::SetGlobalRaw(_)
                    | OpCode::AssignExpr(_)
                    | OpCode::AssignExprLocal(_)
                    | OpCode::IndexAssignExprNamed { .. }
                    | OpCode::IndexAssignExprNested { .. }
                    | OpCode::IndexAssignDeepNested { .. }
                    | OpCode::IndexAssignGeneric
                    | OpCode::IndexAssignPseudoStashNamed { .. }
                    | OpCode::PostIncrement(_)
                    | OpCode::PostDecrement(_)
                    | OpCode::PostIncrementIndex(_)
                    | OpCode::PostDecrementIndex(_)
                    | OpCode::PreIncrement(_)
                    | OpCode::PreDecrement(_)
                    | OpCode::PreIncrementIndex(_)
                    | OpCode::PreDecrementIndex(_)
                    | OpCode::MultiDimIndexAssign { .. }
                    | OpCode::MultiDimIndexAssignGeneric(_)
                    | OpCode::CallFunc { .. }
                    | OpCode::CallFuncSlip { .. }
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
        let idx = self.ops.len();
        self.ops.push(op);
        idx
    }

    /// Patch a jump instruction at `idx` to point to the current position.
    pub(crate) fn patch_jump(&mut self, idx: usize) {
        let target = self.ops.len() as i32;
        match &mut self.ops[idx] {
            OpCode::Jump(offset)
            | OpCode::JumpIfFalse(offset)
            | OpCode::JumpIfTrue(offset)
            | OpCode::JumpIfNil(offset)
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
            | OpCode::JumpIfNil(offset)
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
            OpCode::ForLoop { body_end, .. } => *body_end = target,
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

    pub(crate) fn add_constant(&mut self, value: Value) -> u32 {
        let idx = self.constants.len() as u32;
        self.constants.push(value);
        idx
    }

    pub(crate) fn add_stmt(&mut self, stmt: Stmt) -> u32 {
        let idx = self.stmt_pool.len() as u32;
        self.stmt_pool.push(stmt);
        idx
    }
}

/// A compiled function body (SubDecl compiled to bytecode).
#[derive(Debug, Clone)]
pub(crate) struct CompiledFunction {
    pub(crate) code: CompiledCode,
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
    /// Pre-computed mapping for named parameters: (match_key, local_slot, sub_sig_slots).
    /// sub_sig_slots is a list of (inner_key, inner_slot) for sub_signature aliases.
    /// Used by the OTF named call fast path to avoid name-based lookup per call.
    #[allow(clippy::type_complexity)]
    pub(crate) named_param_slots: Option<Vec<(String, usize, Vec<(String, usize)>)>>,
    /// Deprecation info: (kind, name, package, message).
    /// When set, every call records a deprecation event.
    pub(crate) deprecated_info: Option<(String, String, String, String)>,
    /// Set of variable names declared locally in this function (via `my`).
    /// Used by the positional light call path to distinguish function-local vars
    /// (which should be restored after recursive calls) from captured outer vars
    /// (which should keep their modified values).
    pub(crate) declared_locals: Option<std::collections::HashSet<String>>,
}

impl CompiledFunction {
    /// Pre-compute the mapping from positional parameter index to locals slot index.
    pub(crate) fn precompute_param_local_slots(&mut self) {
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

    /// Pre-compute the mapping for named parameters: match_key -> (local_slot, sub_sig_slots).
    pub(crate) fn precompute_named_param_slots(&mut self) {
        if self.param_defs.is_empty() || !self.param_defs.iter().all(|pd| pd.named) {
            return;
        }
        let mut slots = Vec::new();
        for pd in &self.param_defs {
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
            let slot = self
                .code
                .locals
                .iter()
                .position(|n| n == &pd.name)
                .unwrap_or(0);
            // Pre-compute sub_signature alias slots
            let mut sub_sig_slots = Vec::new();
            if let Some(ref sub_params) = pd.sub_signature {
                for sub_pd in sub_params {
                    if !sub_pd.named {
                        continue;
                    }
                    let inner_key = sub_pd
                        .name
                        .strip_prefix(':')
                        .unwrap_or(&sub_pd.name)
                        .to_string();
                    let inner_slot = self
                        .code
                        .locals
                        .iter()
                        .position(|n| n == &sub_pd.name)
                        .unwrap_or(0);
                    sub_sig_slots.push((inner_key, inner_slot));
                }
            }
            slots.push((match_key, slot, sub_sig_slots));
        }
        if !slots.is_empty() {
            self.named_param_slots = Some(slots);
        }
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
                        | OpCode::ForLoop { .. }
                )
            });
    }

    /// Compute the set of variable names declared locally in this function
    /// (via SetVarDynamic opcode, which is emitted for `my` declarations).
    /// Also includes parameter names. Used to distinguish function-local vars
    /// from captured outer vars in the positional light call path.
    pub(crate) fn compute_declared_locals(&mut self) {
        let mut declared = std::collections::HashSet::new();
        // Parameters are always function-local (including sub-signature params)
        Self::collect_param_names(&self.param_defs, &mut declared);
        for p in &self.params {
            declared.insert(p.clone());
        }
        // Scan opcodes for SetVarDynamic which marks `my` declarations
        for op in &self.code.ops {
            if let OpCode::SetVarDynamic { name_idx, .. } = op
                && let Some(crate::value::Value::Str(name)) =
                    self.code.constants.get(*name_idx as usize)
            {
                declared.insert(name.to_string());
            }
        }
        self.declared_locals = Some(declared);
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
