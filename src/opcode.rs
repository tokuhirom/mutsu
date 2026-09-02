use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use crate::ast::{Expr, ParamDef, ReadonlyKind, Stmt};
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

/// Which bracket a subscript was written with. Carried in bits 8-9 of the
/// `ExistsIndexAdv` / `ExistsIndexNamedAdv` flag word so the VM can pick the
/// subscript protocol from the *syntax* rather than guessing from the index's
/// runtime type: `$c[0]` is `EXISTS-POS`/`AT-POS` and `$c{0}` is
/// `EXISTS-KEY`/`AT-KEY`, however the index happens to be typed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SubscriptKind {
    /// The subscript's bracket is not recorded (a zen slice, or a target the
    /// compiler did not recognise as an `Index`). The VM falls back to the
    /// index-type heuristic.
    Unknown = 0,
    /// Written with `[...]`.
    Positional = 1,
    /// Written with `{...}` or `<...>`.
    Associative = 2,
}

impl SubscriptKind {
    const SHIFT: u32 = 8;
    const MASK: u32 = 0b11;

    /// The kind of a subscript written with `[...]` when `is_positional`.
    #[inline]
    pub fn from_is_positional(is_positional: bool) -> Self {
        if is_positional {
            SubscriptKind::Positional
        } else {
            SubscriptKind::Associative
        }
    }

    /// This kind's contribution to an `ExistsIndexAdv` flag word.
    #[inline]
    pub fn to_flag_bits(self) -> u32 {
        (self as u32) << Self::SHIFT
    }

    /// The kind recorded in an `ExistsIndexAdv` flag word.
    #[inline]
    pub fn from_flags(flags: u32) -> Self {
        match (flags >> Self::SHIFT) & Self::MASK {
            1 => SubscriptKind::Positional,
            2 => SubscriptKind::Associative,
            _ => SubscriptKind::Unknown,
        }
    }
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

/// LHS writeback target of `OpCode::SmartMatchExpr`, boxed to keep the opcode
/// at 48 bytes. A destructive RHS (`s///` / `tr///`) mutates the topic alias,
/// and the modified topic must flow back into the LHS lvalue.
#[derive(Debug, Clone)]
pub(crate) enum SmartMatchLhs {
    /// `$x ~~ s///` — write the modified topic back into the named variable.
    /// `slot` is the compile-time-resolved local slot when the name is a
    /// current-scope local (§1.5: bakes the scope-correct slot so the
    /// writeback does not re-resolve the name at run time — see
    /// docs/lexical-scope-slot-campaign.md); `None` keeps the env-by-name path.
    /// `implicit_topic` marks the LHS the compiler SYNTHESIZED for a bare
    /// regex (`/a/`, `if /a/`, `so /a/`) rather than one the source wrote.
    /// Rakudo coerces that subject quietly, while an explicit `$_ ~~ /a/`
    /// warns -- see `Interpreter::quiet_topic_for_regex_match`.
    Var {
        name: String,
        slot: Option<u32>,
        implicit_topic: bool,
    },
    /// `$obj.meth ~~ s///` — the LHS is a zero-arg method call on a variable
    /// (an `is rw` accessor in Raku, whose returned container the substitution
    /// writes through). mutsu's method calls return plain values, so the VM
    /// re-invokes the accessor as an lvalue (`$obj.meth = <modified topic>`)
    /// after a destructive RHS actually modified the topic (Text::CSV's
    /// `$f.text ~~ s{ <[\ \t]>+ $ } = ""` allow_whitespace trimming).
    Method {
        obj: String,
        obj_slot: Option<u32>,
        method: String,
    },
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
    /// Local slot the frame holds for the TOPIC (`_`), when it has one — a
    /// `sub f ($_) { … }` parameter or a `my $_`. Distinct from `param_local`,
    /// which is the *named* loop parameter's slot.
    ///
    /// A `for` block binds `$_` as its own implicit parameter, but mutsu keeps
    /// the loop topic in `env`. When the enclosing frame also has a `_` slot the
    /// body reads that slot (`GetLocal`), so the loop's topic went unseen and
    /// `sub f($_) { for 1,2,3 { say $_ } }` printed the argument three times.
    /// The loop mirrors each item into this slot and restores the entry value on
    /// exit, exactly as `exec_given_op` does for `given`/`with`. Only set for an
    /// implicit-topic loop (a named parameter does not rebind `$_`).
    pub(crate) topic_local: Option<u32>,
    /// Local slot that owns the iterable container (`$b` in `for $b.pairs`,
    /// `%h` in `for %h.values`). Pair/value alias writeback reaches this source
    /// through env today, so ADR-0018 keeps exactly this slot synchronized.
    pub(crate) source_container_local: Option<u32>,
    pub(crate) body_end: u32,
    /// Local slot holding the materialized callable for an inline `for` block
    /// that references `&?BLOCK`.
    pub(crate) block_callable_local: Option<u32>,
    pub(crate) label: Option<String>,
    pub(crate) arity: u32,
    pub(crate) collect: bool,
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
    /// When true, the loop block's explicit signature declares zero *positional*
    /// parameters -- rakudo's `.count` is 0. Both `-> { ... }` and a signature
    /// whose only parameters are named slurpies (`-> *%h { ... }`) qualify.
    /// The loop still hands the block one element per iteration, so the first
    /// invocation dies with "Too many positionals passed" as soon as the source
    /// has any element at all (an empty source runs the block zero times and is
    /// fine). Computed by `Compiler::for_zero_positional_params`.
    pub(crate) zero_positional_params: bool,
    /// Names of multi-param bindings (for `-> $a, \b, $c` loops).
    /// Used to temporarily clear sigilless readonly flags before binding.
    pub(crate) multi_param_names: Vec<String>,
    /// Compiler-baked local slot for each `multi_param_names` entry, when the
    /// name already has one in the enclosing scope. A multi-param loop binds
    /// its parameters via a plain `Stmt::Assign` (`build_for_bind_stmts`), not
    /// a `my`-style declaration, so it does NOT get a fresh shadow slot: the
    /// bind resolves to whatever slot `name` already occupies (an enclosing
    /// `my $v`) and overwrites it in place for the loop's duration. `None`
    /// when `name` has no local slot at all (the bind target is a global).
    /// Parallel to `multi_param_names`. Lets the VM restore the pre-loop value
    /// straight into that slot after the loop, mirroring `param_local` for the
    /// single-param form — see `todo/tickets/for-multi-param-shadow-clobbers-outer-lexical.md`.
    pub(crate) multi_param_locals: Vec<Option<u32>>,
    /// Declared type constraint of the single named loop parameter
    /// (`for @a -> Int $x { ... }`), if any. `None` for an untyped param, a
    /// multi-param loop, or no named param at all. Checked once per
    /// iteration against the bound item, raising `X::TypeCheck::Binding::Parameter`
    /// on mismatch (`todo/tickets/for-loop-multi-param-types-unenforced.md`).
    pub(crate) param_type_constraint: Option<String>,
    /// Declared type constraints for a multi-param loop (`-> Str $k, Int $v`),
    /// parallel to `multi_param_names`. `None` per-entry for an untyped param.
    pub(crate) multi_param_type_constraints: Vec<Option<String>>,
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
    /// The source expression is a direct smartmatch. A successful Match has
    /// an empty list value in this context; an itemized scalar variable still
    /// yields the Match as one item.
    pub(crate) direct_smartmatch: bool,
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
    /// When true, every item this loop yields is provably a bare VALUE with no
    /// container of its own — the iterable is a list built entirely out of
    /// literals (`for 1, 2`, `for <a b>`) or a `.keys` read. Raku aliases the
    /// implicit topic `$_` straight to such an item, so `$_ = ...` is
    /// X::AdHoc "Cannot assign to an immutable value" and `$_.VAR.^name` is the
    /// item's own type rather than `Scalar`.
    ///
    /// Deliberately a *provable* compile-time property rather than a runtime
    /// "is this a container?" test: mutsu stores real `Array`/`Hash` elements
    /// bare (see `todo/deep/element-itemization-lost-in-scalar-binding.md` /
    /// ADR-0040), so a runtime test would also mark genuinely writable element
    /// topics (`for @a[0..1]`, `for @a.map(...)`) read-only. A mixed list
    /// (`for 1, $a`) stays writable, which is lax but never wrong.
    pub(crate) source_items_are_bare: bool,
    /// Whether the single named loop parameter is *sigilless* (`-> \v`).
    ///
    /// ADR-0045 slice 5 rejects an `is rw` / `<->` bind against a source whose
    /// items are provably bare values, at bind time, with raku's
    /// `X::Parameter::RW`. A sigilless parameter sets the same
    /// [`Self::do_writeback`] but must NOT take that rejection: raku binds
    /// `-> \v` to a bare item happily and only dies if the body *assigns*
    /// through it ("Cannot modify an immutable Int"). The parameter name alone
    /// cannot tell the two apart -- the AST stores `\v` as plain `"v"` -- so
    /// the compiler records the distinction here.
    pub(crate) param_sigilless: bool,
}

impl ForLoopSpec {
    /// Whether the value handed to one iteration of the body is a *chunk array*
    /// of `arity` source elements rather than the bare source element.
    ///
    /// A signature with more than one parameter always gets an array, even at
    /// `arity == 1`: a trailing slurpy (`-> $a, *@rest`) makes rakudo consume
    /// exactly one element per iteration while the binder still reads `$a` out
    /// of slot 0 and hands the (empty) remainder to `*@rest`. Handing that
    /// binder a bare element instead would be ambiguous whenever the element is
    /// itself a list — `for (1,2),(3,4) -> $a, *@rest` must bind `$a` to the
    /// whole `(1,2)`, not to `1`.
    pub(crate) fn chunks_items(&self) -> bool {
        self.arity > 1 || self.multi_param_names.len() > 1
    }
}

/// Precompiled replacements for `CompiledAttrDecl::from_stmt`'s AST-only
/// declaration-time expressions (`is_default`, `default`, `where_constraint`),
/// supplied by a caller with compiler access at plan-lowering time (ADR-0019
/// D2c-1/D2c-4). Each `None` field falls back to `DeclTraitArg::Ast` wrapping
/// the raw AST expression — the same guarded-fallback shape every other
/// ADR-0019 plan cutover uses. `#[derive(Default)]` gives every non-plan
/// registration path (role bodies pre-D2c-4, `augment class`, mainline/EVAL
/// `has`) a one-line "no chunks available" value.
#[derive(Default)]
pub(crate) struct AttrDeclChunks {
    pub(crate) is_default: Option<DeclTraitArg>,
    pub(crate) default: Option<DeclTraitArg>,
    pub(crate) where_constraint: Option<DeclTraitArg>,
}

/// A typed mirror of `Stmt::HasDecl` (ADR-0019 D2b), built once by
/// [`CompiledAttrDecl::from_stmt`] instead of being re-destructured with an
/// 18-field pattern at each of the class-body, role-body, augment, and
/// mainline/EVAL `has`-registration sites. `name` is the resolved (twigil-free)
/// attribute name and `is_default` is unboxed, matching what every consumer
/// actually reads; everything else mirrors the AST field for field.
/// `default`/`where_constraint` are `DeclTraitArg` rather than a raw `Expr`
/// (ADR-0019 D2c-4, matching `ClassAttributeDef`'s own D2c-2 field type) —
/// every reader runs them through `Interpreter::eval_decl_trait_arg`/
/// `.literal()` instead of matching `Expr::Literal` directly.
#[derive(Debug, Clone)]
pub(crate) struct CompiledAttrDecl {
    pub(crate) name: String,
    pub(crate) is_public: bool,
    pub(crate) default: Option<DeclTraitArg>,
    pub(crate) handles: Vec<crate::ast::HandleSpec>,
    pub(crate) is_rw: bool,
    pub(crate) is_readonly: bool,
    pub(crate) type_constraint: Option<String>,
    pub(crate) type_smiley: Option<String>,
    pub(crate) is_required: Option<Option<String>>,
    pub(crate) sigil: char,
    pub(crate) where_constraint: Option<DeclTraitArg>,
    pub(crate) is_alias: bool,
    pub(crate) is_our: bool,
    pub(crate) is_my: bool,
    /// The `is default(...)` trait argument (ADR-0019 D2c). `Ast` unless a
    /// caller with compiler access at plan-lowering time supplied a
    /// precompiled `Literal`/`Compiled` replacement — see [`Self::from_stmt`].
    pub(crate) is_default: Option<DeclTraitArg>,
    pub(crate) is_type: Option<String>,
    pub(crate) deprecated_message: Option<String>,
    pub(crate) is_built: Option<bool>,
    pub(crate) unknown_traits: Vec<(String, String, Option<crate::ast::Expr>)>,
    /// Declared shape dimensions for an `@`-sigil attribute (`has @.a[2]`),
    /// extracted once from the raw `default` expression's compiler-generated
    /// `Array.new(:shape(...))` pattern (ADR-0019 D2c-4, the D2a precompute
    /// pattern) — `default` above no longer carries a raw `Expr` a consumer
    /// could re-inspect at construction time.
    pub(crate) declared_shape: Option<Vec<usize>>,
}

impl CompiledAttrDecl {
    /// Build a typed descriptor from a `Stmt::HasDecl`. Panics on any other
    /// statement kind — every call site already matched on `Stmt::HasDecl`
    /// before reaching here.
    ///
    /// `chunks` supplies precompiled replacements for the trait/expr fields,
    /// looked up or built by the caller (by attribute name) from a
    /// `CompiledClassDeclPlan`/`CompiledRoleDeclPlan` built at compile time.
    /// Pass `AttrDeclChunks::default()` when no such plan is available
    /// (registration paths that still walk a raw AST body, e.g. `augment
    /// class`) — each field then keeps its raw expression as
    /// `DeclTraitArg::Ast`, the same fallback used elsewhere for
    /// not-yet-migrated declaration kinds.
    pub(crate) fn from_stmt(stmt: &Stmt, chunks: AttrDeclChunks) -> CompiledAttrDecl {
        let Stmt::HasDecl {
            name,
            is_public,
            default,
            handles,
            is_rw,
            is_readonly,
            type_constraint,
            type_smiley,
            is_required,
            sigil,
            where_constraint,
            is_alias,
            is_our,
            is_my,
            is_default,
            is_type,
            deprecated_message,
            is_built,
            unknown_traits,
        } = stmt
        else {
            unreachable!("CompiledAttrDecl::from_stmt called on a non-HasDecl statement");
        };
        let declared_shape = attr_declared_shape(default.as_ref());
        CompiledAttrDecl {
            name: name.resolve(),
            is_public: *is_public,
            default: chunks
                .default
                .or_else(|| default.clone().map(|e| DeclTraitArg::Ast(Box::new(e)))),
            handles: handles.clone(),
            is_rw: *is_rw,
            is_readonly: *is_readonly,
            type_constraint: type_constraint.clone(),
            type_smiley: type_smiley.clone(),
            is_required: is_required.clone(),
            sigil: *sigil,
            where_constraint: chunks.where_constraint.or_else(|| {
                where_constraint
                    .as_deref()
                    .cloned()
                    .map(|e| DeclTraitArg::Ast(Box::new(e)))
            }),
            is_alias: *is_alias,
            is_our: *is_our,
            is_my: *is_my,
            is_default: chunks
                .is_default
                .or_else(|| is_default.clone().map(|e| DeclTraitArg::Ast(Box::new(e)))),
            is_type: is_type.clone(),
            deprecated_message: deprecated_message.clone(),
            is_built: *is_built,
            unknown_traits: unknown_traits.clone(),
            declared_shape,
        }
    }
}

/// A typed mirror of `Stmt::MethodDecl` (ADR-0019 D3-2), built once by
/// [`CompiledMethodDecl::from_stmt`] instead of being re-destructured with a
/// 19-field pattern at each of the class-body, role-body, and augment
/// `method`/`submethod`-registration sites (`ANALYSIS §1.1`'s drift between
/// those three walkers is exactly what independently-drifted destructuring
/// produces — see the D3 scoping note above). `params: Vec<String>` is
/// dropped: every existing site already ignores it (the parameter names are
/// recomputed from `param_defs`), so mirroring it here would carry a field no
/// consumer reads. `name_expr` is kept only for its `is_some()` check — the
/// resolved runtime name itself comes from the D3-1 `method_name_chunks`
/// cursor, not from re-evaluating this field.
#[derive(Debug, Clone)]
pub(crate) struct CompiledMethodDecl {
    pub(crate) name: Symbol,
    pub(crate) name_expr: Option<Expr>,
    pub(crate) param_defs: Vec<ParamDef>,
    pub(crate) body: Vec<Stmt>,
    pub(crate) multi: bool,
    pub(crate) is_rw: bool,
    pub(crate) is_private: bool,
    pub(crate) is_our: bool,
    pub(crate) is_my: bool,
    pub(crate) is_submethod: bool,
    pub(crate) our_variable_form: bool,
    pub(crate) return_type: Option<String>,
    pub(crate) is_default_candidate: bool,
    pub(crate) deprecated_message: Option<String>,
    pub(crate) handles: Vec<crate::ast::HandleSpec>,
    pub(crate) custom_traits: Vec<(String, Option<Expr>)>,
    pub(crate) is_export: bool,
    pub(crate) export_tags: Vec<String>,
    /// Main-pass-compiled bytecode key for this method's body (ADR-0019
    /// D3-8a), keyed into the program's [`CompiledFns`] table exactly like a
    /// `sub`'s [`CompiledSubDeclPlan::compiled_routine_keys`]. `None` when
    /// the declaration has no statically-known name/package (a computed
    /// `method ::($name) {...}` or a class with a computed `::($n)` name) —
    /// those keep using the registration-time throwaway-compiler fallback
    /// (`compile_method_def_in_place_with_dist`). D3-8a only *populates*
    /// this field; nothing reads it yet (that is D3-8b/c's cutover).
    pub(crate) compiled_routine_key: Option<Symbol>,
    /// ADR-0019 D3-9: whether this method's body reads a bare `@_`
    /// (`method_signature_shared::auto_signature_uses`'s `positional`
    /// flag), precomputed once here instead of re-scanning `body` at every
    /// registration (`class_body_method_decl`/`registration_class_augment.rs`
    /// each used to call `auto_signature_uses` themselves, on every class
    /// declaration — including one declared inside a loop). Only meaningful
    /// together with an empty `param_defs` (an explicit signature, even
    /// `()`, opts out) — callers already gate on that themselves, mirroring
    /// `apply_auto_positional_slurpy`'s own `original_param_defs_is_empty`
    /// guard.
    pub(crate) uses_bare_positional_args: bool,
}

impl CompiledMethodDecl {
    /// Build a typed descriptor from a `Stmt::MethodDecl`. Panics on any
    /// other statement kind — every call site already matched on
    /// `Stmt::MethodDecl` before reaching here.
    pub(crate) fn from_stmt(stmt: &Stmt) -> CompiledMethodDecl {
        let Stmt::MethodDecl {
            name,
            name_expr,
            params: _,
            param_defs,
            body,
            multi,
            is_rw,
            is_private,
            is_our,
            is_my,
            is_submethod,
            our_variable_form,
            return_type,
            is_default_candidate,
            deprecated_message,
            handles,
            custom_traits,
            is_export,
            export_tags,
        } = stmt
        else {
            unreachable!("CompiledMethodDecl::from_stmt called on a non-MethodDecl statement");
        };
        let uses_bare_positional_args = crate::method_signature_shared::auto_signature_uses(body).0;
        CompiledMethodDecl {
            name: *name,
            name_expr: name_expr.clone(),
            param_defs: param_defs.clone(),
            body: body.clone(),
            multi: *multi,
            is_rw: *is_rw,
            is_private: *is_private,
            is_our: *is_our,
            is_my: *is_my,
            is_submethod: *is_submethod,
            our_variable_form: *our_variable_form,
            return_type: return_type.clone(),
            is_default_candidate: *is_default_candidate,
            deprecated_message: deprecated_message.clone(),
            handles: handles.clone(),
            custom_traits: custom_traits.clone(),
            is_export: *is_export,
            export_tags: export_tags.clone(),
            compiled_routine_key: None,
            uses_bare_positional_args,
        }
    }
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
    pub(crate) decl: CompiledAttrDecl,
    /// The `X::Attribute::*` error to throw when this `has` runs outside a
    /// class-definition context.
    pub(crate) error: Value,
}

/// Slot marker in [`OpCode::LoadRegexClosure`]'s capture list: the captured
/// name has no local slot in the creating frame and must be read from `env`.
pub(crate) const NOT_A_LOCAL: u32 = u32::MAX;

/// Bytecode operations for the VM.
#[derive(Debug, Clone)]
pub(crate) enum OpCode {
    // -- Constants --
    LoadConst(u32),
    /// Load a *code-bearing* regex literal as the closure it is.
    ///
    /// A Raku regex closes over the scope it was written in, but mutsu stores a
    /// regex as a pattern string, so code embedded in the pattern (`{ … }`,
    /// `<?{ … }>`, `:my`/`:let` initializers) would otherwise resolve its free
    /// variables against whatever env is live at *match* time — losing them
    /// whenever the regex is stored and matched from another frame. This op
    /// snapshots those lexicals out of the creating frame and attaches them to
    /// the value (`Value::RegexCaptured`, which still views as a plain Regex).
    ///
    /// `captures` pairs each capture's env key (`$x` -> `x`, `@x`/`%x`/`&x`
    /// keep their sigil) with its local slot in the creating frame, or
    /// [`NOT_A_LOCAL`] when the name is only reachable through `env`.
    LoadRegexClosure {
        const_idx: u32,
        captures: Arc<Vec<(Symbol, u32)>>,
    },
    LoadNil,
    LoadTrue,
    LoadFalse,

    // -- Variables --
    GetLocal(u32),
    /// Fused `GetLocal(slot); MetaAssignIdentity(identity)` (emit-time peephole,
    /// same shape as `SetLocalDecl`). `$i += 1` on a local is the single most
    /// common compound assignment, so the metaop's identity seed must not cost
    /// it an extra dispatch.
    GetLocalMetaAssign {
        slot: u32,
        identity: crate::token_kind::MetaAssignIdentity,
    },
    /// Like GetLocal but does NOT resolve HashEntryRef values.
    /// Used by `=:=` to compare raw container references.
    GetLocalRaw(u32),
    /// The subscript-chain read of a local: the full `GetLocal` resolution
    /// (binding aliases, shared/atomic storage, env container adoption, lazy
    /// thunks, `ContainerRef` deref) EXCEPT that a deferred `HashEntryRef` bind
    /// token is pushed as-is instead of being resolved to its current value.
    ///
    /// Emitted for the target of a subscript compiled in container mode (a `:=`
    /// bind RHS, or a `return-rw` operand): `my $x := %h<a>; my $y := $x<b>` and
    /// `sub f(\c) is rw { return-rw c<b> }` called with a not-yet-existent
    /// `%h<a>` both need the token itself, so `IndexAutovivifyLazy` can extend
    /// its path and the eventual write autovivifies the whole chain. Resolving
    /// it to `Any` (what a value read correctly does) severs the chain.
    /// Unlike `GetLocalRaw` this keeps the env fallback, which a method frame's
    /// parameter slot depends on.
    GetLocalDeferred(u32),
    /// Load a scalar variable's container for `take-rw`.  A plain scalar is
    /// promoted to a shared cell in its authoritative store; an already-bound
    /// scalar keeps its existing cell.  Ordinary reads must continue to
    /// decontainerize, so this is deliberately a distinct lvalue opcode.
    GetScalarContainer {
        name_idx: u32,
        local_idx: Option<u32>,
    },
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
    /// `our $x = <expr>` for a plain untyped scalar (no `:=` bind, no type
    /// constraint, no container sigil, not a `constant`): installs ONE shared
    /// `ContainerRef` cell under the lexical local slot, the bare env name,
    /// and the package-qualified name (`qualified_idx`) — see
    /// `Interpreter::exec_declare_our_scalar_op`. `our $x` and `$Pkg::x` (or
    /// `$GLOBAL::x` at file scope) then name the SAME container, so a write
    /// through either name is visible through the other via the existing
    /// generic `ContainerRef` read/write-through chokepoints (`GetLocal`/
    /// `SetLocal`/`GetGlobal`/`SetGlobal`) — no bespoke sync code needed.
    /// Replaces the old two-store `Dup; SetLocalDecl(slot); SetGlobal(q)`
    /// sequence for exactly this case; every other `our` shape (typed,
    /// `@`/`%`/`&`-sigiled, `constant`, `:=` bound, or shadowing an outer
    /// `constant`) keeps the old sequence unchanged.
    DeclareOurScalar {
        slot: u32,
        qualified_idx: u32,
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
    /// [`Self::SetVarType`] for a scalar `my`/`state` declaration LEXICALLY
    /// INSIDE a routine: registers the constraint in the env-scoped
    /// `__mutsu_type::` metadata ONLY (exactly like a typed parameter), never
    /// in the global name-keyed `var_type_constraints` map. The env entry dies
    /// with the routine frame (and travels with a captured closure env), so
    /// the constraint cannot leak onto a same-named variable in another frame
    /// (`todo/deep/bare-name-type-constraint-store-is-scope-blind.md`).
    SetVarTypeScoped {
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
    /// Replace the role *group* type object on the stack with the INDIVIDUAL
    /// parametric role that was just declared (the group's current candidate).
    /// Emitted right after a `role` declaration used in expression position, so
    /// `(role R { ... })` evaluates to a `ParametricRoleHOW`-backed role like it
    /// does in Rakudo, while the installed name `R` keeps resolving to the
    /// `ParametricRoleGroupHOW` group.
    RoleGroupToCandidate,
    /// Push the type object of the class most recently registered by
    /// `RegisterClass` in THIS bytecode stream (no intervening opcode may run
    /// between the two). Emitted right after a NAMED `class` declaration used
    /// in expression position (`(class A { ... })`), so the expression
    /// evaluates to the type object the declaration just created — never a
    /// name-based lookup of the bareword `A`, which can resolve to an
    /// unrelated, same-named class from a completely different scope (e.g. a
    /// class re-declared inside `EVAL`'d code that runs in a different
    /// package than the caller — see
    /// `news/2026-08/class-decl-expr-is-not-a-name-lookup.md`).
    PushLastRegisteredClass,
    /// Push the role group type object most recently installed by
    /// `RegisterRole`. Emitted immediately after a named role declaration in
    /// expression position so lookup uses the declaration's actual qualified
    /// registry key rather than its potentially shadowed bare source name.
    PushLastRegisteredRole,

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
    /// Read a `ContainerRef` on the stack top through its cell, pushing the
    /// plain value it holds (a no-op for everything else). Emitted where a
    /// compiler-synthesized temp must hold a *value snapshot* rather than an
    /// alias — the `++`/`--` lowerings on an rw-accessor lvalue, whose temp
    /// global would otherwise be bound to the accessor's own container and, on
    /// the next iteration of a loop, be written *through* by `SetGlobal`,
    /// storing the cell into itself.
    DerefContainer,
    Decont, // strip ONE level of Scalar for slurpy flattening (NOT the
    // recursive Value::descalarize; touches no ArrayKind flag — see decont family note)
    /// Snapshot a list's elements to plain VALUES: pop a list/array and push a
    /// fresh real array where every element is read through its `ContainerRef`
    /// cell (`:=` / list-element container alias) and descalarized. Used by
    /// list assignment (`($a, $b) = ($b, $a)`) to buffer the RHS value list
    /// BEFORE writing any LHS container, so a write cannot corrupt a later read
    /// of an aliased element. Bounded (only the elements already reified are
    /// touched), so it stays lazy-safe when applied to a finite prefix slice.
    DecontListElems,
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
    /// Tag the top-of-stack value with the variable name it was read from
    /// (`name_idx` constant), for `is rw`/`is raw`/`:=` aliasing and
    /// list-element container capture. `slot` is the emitting frame's local
    /// slot for that name at this site (shadow-slot-exact), or `u32::MAX`
    /// when the source is not a local of this frame.
    WrapVarRef {
        name_idx: u32,
        slot: u32,
    },
    /// Resolve a `WrapVarRef`-tagged top-of-stack value to the *shared cell* of
    /// the variable it names, boxing the variable's local slot into a
    /// `ContainerRef` if it is not one already (`capture_var_cell_inner` with
    /// `box_type_objects`, i.e. exactly what `MakeArray` does per List element).
    ///
    /// Emitted for a `return-rw` operand that names a plain scalar lexical
    /// (ADR-0059 Slice 2): `sub f() { return-rw $v }` must hand the caller `$v`'s
    /// container, so a later `my $r := f(); $r = 5` — or an element write through
    /// a returned list of containers — writes `$v` itself. `MakeArray`/
    /// `MakeCapture`/`MakePair` consume the `VarRef` tag inline; this opcode is
    /// the standalone spelling for the one-value case.
    CaptureVarCell,
    /// Signal that the next SetLocal is a `:=` bind (preserve container type for `@` vars).
    MarkBindContext,
    /// Signal that the next SetLocal binds a `$` scalar to a Positional value via
    /// `:=`, so it must be recorded as decontainerized (so `@a = $bound` flattens).
    MarkScalarBindContext,
    /// Marks the next SetLocal/SetGlobal as a raw (non-itemizing) bind of a
    /// sigilless target (`-> \v` loop-param binds). Skips scalar-store
    /// itemization ONLY — no readonly/decont/bind side effects.
    MarkParamRawBindContext,
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
        /// LHS writeback target (boxed to keep `OpCode` at 48 bytes). `None`
        /// when the LHS is not a writable-through-name/-accessor expression.
        lhs: Option<Box<SmartMatchLhs>>,
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
    /// Build a data-minted Pair (ADR-0021 I2): always the positional
    /// (`ValuePair`) flavour. Used by every fat-arrow `a => b` compile
    /// EXCEPT argument-position named-arg synthesis, which uses
    /// `MakeNamedArg` instead.
    MakePair,
    /// Build a named-argument-flavour Pair (ADR-0021 I2/I3): emitted only
    /// by compiled argument-position synthesis (a literal `key => value` /
    /// `:key(value)` written directly in an argument list, or a Capture
    /// literal's named-lane element `\(:$a)`) that intends the in-band
    /// named marker to reach the callee's binder. Same payload and runtime
    /// shape as `MakePair` (pop value, pop key, push a Pair) — only the
    /// resulting flavour differs.
    MakeNamedArg,
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
    /// Pop with sink context — throws unhandled Failures.
    /// The first bool (`user_sink`) is `true` when the sunk value is a
    /// syntactically fresh rvalue (e.g. a method call / `Foo.new`) that may
    /// invoke a user-defined `sink` method; `false` for bare variables /
    /// function-call returns whose values are (or may be) container-wrapped
    /// and must not auto-sink (Raku does not sink container-wrapped values).
    /// The second bool (`may_explode_failure`) is `false` only for a bare
    /// container read (`$f;`, `@a;`, `%h;`): Raku's optimizer recognizes a
    /// pure variable mention as "Useless use ... in sink context" and never
    /// actually forces it, so a stored unhandled `Failure` must not explode
    /// merely because the bare mention was reached — Raku decides a
    /// Failure's fate at *construction* time (throwing immediately there
    /// under `use fatal`, matched by the various `self.fatal_mode`
    /// assignment-time checks), not by re-examining it at every later
    /// mention. Every other sunk shape (fresh calls, method calls, `sink`
    /// prefix, ...) keeps `true`, matching prior behavior.
    SinkPop(bool, bool),
    /// Pop the value of an assignment used as a statement (`%h{$k} = ...;`,
    /// `@a[$i] = ...;`). Rakudo treats assignment statements as wanted, not
    /// sunk: an assigned unhandled Failure is stored, never thrown — except
    /// under `use fatal`, where it still explodes. Unlike `SinkPop` this arm
    /// must not force lazy values or run user `sink` methods either.
    SinkPopAssign,
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
    /// Stack layout: [target, name_value, arg0, arg1, ...]
    CallMethodDynamic {
        arity: u32,
        modifier_idx: Option<u32>,
        quoted: bool,
        /// ADR-0054 S3: `|EXPR` positions (and, incidentally, rw-arg source
        /// names) baked the same way `CallMethod`'s does.
        arg_sources_idx: Option<u32>,
    },
    /// Dynamic method call on a variable target (allows mutation/writeback).
    /// Stack layout: [target, name_value, arg0, arg1, ...]
    CallMethodDynamicMut {
        arity: u32,
        target_name_idx: u32,
        modifier_idx: Option<u32>,
        quoted: bool,
        /// ADR-0054 S3: `|EXPR` positions (and, incidentally, rw-arg source
        /// names) baked the same way `CallMethodMut`'s does.
        arg_sources_idx: Option<u32>,
    },
    /// Statement-level call: pop `arity` args, call name (no push).
    ExecCall {
        name_idx: u32,
        arity: u32,
        arg_sources_idx: Option<u32>,
    },
    /// Statement-level call with positional/named values encoded as Pair.
    ///
    /// ADR-0054 Slice 4: `arg_sources_idx` is the SAME per-argument-position
    /// descriptor `CallFunc`/`CallMethod`/etc. carry (see their doc comments
    /// and `decode_arg_slip_positions`) — a `|EXPR` position is a `TRUE`
    /// entry. Those — and only those — spread into the argument list: a Slip
    /// an ordinary argument merely evaluated to (`is-deeply $s.Slip,
    /// $t.Slip, 'name'`) stays one argument, as in Rakudo. Before Slice 4
    /// this carried a dedicated `slip_positions_idx` (a constant array of
    /// bare integer positions, decoded by the now-deleted
    /// `spread_slip_positions`); it collapsed into this table so a call site
    /// has exactly one syntax descriptor instead of two. `keep_value` (tail
    /// position: the call's value is the body's result) pushes the call
    /// result onto the stack; plain statement position leaves the stack
    /// untouched.
    ExecCallPairs {
        name_idx: u32,
        arity: u32,
        arg_sources_idx: Option<u32>,
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
    /// `succeed_boundary` says whether *this* block is where a `when`/`default`
    /// succeed signal stops. A bare block or an `if`/`unless`/`else` branch is
    /// such a boundary (`given 5 { if c { when Int {...} }; say "after" }` still
    /// runs the `say`), so the signal is absorbed. A `when`/`default`/`given`
    /// body is NOT: the signal has to reach the enclosing `When`/`Default`/
    /// `Given` op, which turns it into that construct's value.
    BlockLocalScope {
        body_end: u32,
        succeed_boundary: bool,
    },
    /// Succeed barrier around a block body that lexically contains a top-level
    /// `when`/`default`. The succeed a matched `when` raises unwinds only to the
    /// innermost block *containing* the `when` statement, so
    /// `given 5 { if c { when Int { } }; say "after" }` still runs the `say`, and
    /// `given 5 { while ... { when Int { } }; say "after" }` keeps looping. The
    /// op adds no scoping of its own — the body's own `BlockScope` /
    /// `BlockLocalScope` (when there is one) still does that. It runs
    /// `ip+1 .. body_end` and absorbs a succeed signal, resetting the
    /// `when_matched` flag so an enclosing `given` does not break out of its body.
    SucceedBarrier {
        body_end: u32,
    },
    /// Drop the `state` variables initialized in `ip+1 .. body_end` from the
    /// state store, so their declarations re-run their initializers.
    ///
    /// Raku clones a block every time its ENCLOSING block runs, and a `state`
    /// cell belongs to the clone — so a `state` inside an `if` branch or a bare
    /// nested block restarts on each execution of that construct
    /// (`sub f { if 1 { state $n; say ++$n } }` says `1` on every call). A real
    /// closure gets this from its per-clone `state_scope_id`, and a loop body
    /// from `reset_state_locals_in_range` at loop-statement entry (iterations of
    /// ONE execution share the clone); an inline-compiled `if` branch or bare
    /// block has neither, hence this op at its entry. Emitted only when the body
    /// declares a `state` at its own level — a nested loop/if/block inside it
    /// resets through its own entry.
    ResetStateLocals {
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
    /// A `BEGIN <expr>` whose value the phaser lifter could not hoist out of its
    /// enclosing routine (module bodies are not lifted at all — see
    /// `compile_expr_phaser`). The body still runs at most once: the result is
    /// memoized in the `once` store under a compile-time site id, so unlike
    /// `OnceExpr` the memo is shared by every clone of the enclosing code object,
    /// which is what BEGIN means (one value, baked in at compile time).
    BeginOnceExpr {
        body_end: u32,
        site_id: u64,
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
    ///
    /// `is_positional` mirrors [`OpCode::Index`]: true for `[...]`, false for
    /// `{...}` / `<...>`. It is what the deferred token's path step records, so
    /// a positional step over a not-yet-existent container walk-creates an
    /// `Array` rather than a `Hash` keyed by the stringified index.
    IndexAutovivifyLazy {
        is_positional: bool,
    },
    /// Like IndexAutovivifyLazy, but the index is the TERMINAL element of a `:=`
    /// bind RHS. A container-valued (Array/Hash) leaf is promoted to a
    /// `ContainerRef` cell — not kept as a traversal back-reference.
    ///
    /// `sigilless` marks the bind target as a SIGILLESS term (`my \a := (5,
    /// 6)[0]`, and each sigilless target of a list-destructuring bind). A
    /// sigilless term IS whatever it is bound to, so rakudo settles its
    /// mutability from the bound thing: an `Array` element is a container and
    /// writes through, while a `List` element is a plain value and `a = 10`
    /// dies with "Cannot modify an immutable Int". Promoting a `List`'s scalar
    /// leaf to a fresh cell would make the second case look writable, so this
    /// flag suppresses the promotion for an immutable `List` — an element that
    /// already IS a container (a captured source cell from `($x, $y)`, a nested
    /// `Array`/`Hash`) is handed back unchanged and stays writable.
    ///
    /// The flag is deliberately narrow. The same over-promotion makes
    /// `my $x := (5, 6)[0]; $x = 10` and a `List`-element loop parameter
    /// wrongly writable, but suppressing it there breaks consumers that lean on
    /// the promotion (a chunked `for @flat -> \a, \b` binding, `.kv` on a
    /// mutable QuantHash or a `Pair`); see
    /// `todo/deep/immutable-list-element-bind-is-writable.md`.
    IndexAutovivifyLazyTerminal {
        is_positional: bool,
        sigilless: bool,
    },
    /// `%h<k>:delete` / `@a[i]:delete`. First field is the container variable's
    /// name (const-pool index); the optional second is its compile-time-resolved
    /// local slot (§1.5: the mutated container is written back through this exact
    /// slot instead of a by-name `code.locals` search — docs/lexical-scope-slot-
    /// campaign.md). `None` for a non-local / EVAL-carrier container.
    DeleteIndexNamed(u32, Option<u32>),
    DeleteIndexExpr,
    /// Multi-dimensional indexing: @a[$x;$y;$z]
    /// Stack: [target, dim0, dim1, ..., dimN] → [result]
    ///
    /// `is_positional` records the bracket kind — see `MultiDimIndexAssign`.
    /// An associative multi-dim read is a slice even when every dimension is a
    /// single key, so it hands back a `List` (`%h{1;2}` is `(5,)`).
    MultiDimIndex {
        ndims: u32,
        is_positional: bool,
    },
    /// Multi-dimensional index assignment: @a[$x;$y;$z] = value
    /// Stack: [value, dim0, dim1, ..., dimN] (target by name)
    ///
    /// `is_positional` records the subscript's bracket kind: `[...]` walks a
    /// (possibly shaped) Positional, while `{...}` / `<...>` walks a chain of
    /// nested Hash keys — an Associative has no shape, so each level
    /// autovivifies a Hash and stringifies its key.
    MultiDimIndexAssign {
        name_idx: u32,
        ndims: u32,
        is_positional: bool,
    },
    /// Multi-dimensional index assignment (generic target)
    /// Stack: [target, dim0, ..., dimN, value]
    MultiDimIndexAssignGeneric {
        ndims: u32,
        is_positional: bool,
    },
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
    /// The `supply { ... }` desugar's own `done` terminator (see
    /// `ast::Stmt::SupplyBodyDone`) — ends just the current closure's
    /// synchronous execution, distinct from both `Return` and `ReactDone`.
    SupplyBodyDone,
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

    /// METAOP_ASSIGN identity substitution for the LHS of `$x OP= $y`: replace a
    /// top-of-stack type object with the operator's zero-argument value (`0` for
    /// `+`/`-`, `1` for `*`/`**`), or throw for the operators that have none
    /// (`/`, `%`). A no-op for a concrete value. This is what keeps the bare
    /// arithmetic infixes free to reject an uninitialized operand outright.
    MetaAssignIdentity(crate::token_kind::MetaAssignIdentity),

    // -- Prefix increment/decrement (returns NEW value) --
    // Optional second field: the compile-time-resolved local slot for the named
    // scalar (§1.5, mirrors PostIncrement/PostDecrement — docs/lexical-scope-slot-
    // campaign.md). `None` for a non-local / temp-value target (env-by-name).
    PreIncrement(u32, Option<u32>),
    PreDecrement(u32, Option<u32>),
    PreIncrementIndex(u32, Option<u32>),
    PreDecrementIndex(u32, Option<u32>),

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
    /// `$c[i]++` / `%h<k>--`. The optional second field is the same §1.5
    /// compile-time-resolved local slot as `PostIncrement`'s: the *base
    /// container's* slot. Without it the VM located the container by name, which
    /// picks the FIRST `code.locals` entry with that name — so an inner
    /// `my $b = [0, 3]` shadowing an outer `my $b` inside the same frame
    /// (a bare block, which shares the enclosing frame's locals) incremented the
    /// OUTER array's element.
    PostIncrementIndex(u32, Option<u32>),
    PostDecrementIndex(u32, Option<u32>),
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
    /// Runtime-key variant of `IndexAssignPseudoStashNamed` (e.g.
    /// `PROCESS::{$k} = v`, how a `//=`/`||=` compound assign desugars the
    /// subscript into a temp). Stack: `[..., value, key]`.
    IndexAssignPseudoStashKeyed {
        stash_name_idx: u32,
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
    ///
    /// `identity` carries the METAOP_ASSIGN zero-argument seed the unfused form
    /// would have applied through `OpCode::MetaAssignIdentity`; it is `None`
    /// for a literal `$x = $x OP y` (which has no metaop semantics) and for the
    /// operators that have no identity of their own (`~=`, `min=`, ...).
    AtomicCompoundVar {
        name_idx: u32,
        op: CompoundBaseOp,
        identity: Option<crate::token_kind::MetaAssignIdentity>,
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
    /// Settle a just-declared sigilless term's mutability from what it was
    /// actually bound to, marking it readonly when that is a plain VALUE.
    ///
    /// Raku decides a sigilless name's mutability from the binding, not from the
    /// syntax of the right-hand side: `my \x := @a[0]` aliases a real element
    /// container and `x = 9` writes through, while `my \x := 5` and
    /// `my \x := $s.uc` bind an immutable value and the same write dies with
    /// "Cannot modify an immutable Int (5)". The compiler cannot tell an `is rw`
    /// accessor call from an ordinary one, so the test is necessarily made here.
    ///
    /// Emitted right after the declaration, this writes the ordinary
    /// `__mutsu_sigilless_readonly::<name>` marker the parser used to set
    /// statically, so every existing consumer (`CheckReadOnly` in any frame, the
    /// `++`/`--` mutability gate, the redeclaration clear) keeps working
    /// unchanged. It only ever RAISES the marker: a bind whose source is itself
    /// readonly has already set it, and that must not be lowered.
    MarkSigillessBind(u32),
    /// Mark a variable as readonly (for `:=` binding / `constant`). The
    /// [`ReadonlyKind`] records *why*, which decides the exception an
    /// assignment through the name throws.
    MarkVarReadonly(u32, ReadonlyKind),

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
        /// parser emits a synthetic bound declaration at the body head and the
        /// compiler records the declared name here. `None` for non-pointy `given`.
        pointy_param_idx: Option<u32>,
    },
    When {
        body_end: u32,
        /// True for the postfix `STMT when COND` spelling. Rakudo lowers that
        /// to a plain conditional, so it is not a `when` *clause*: a `proceed`
        /// raised inside it must keep unwinding to the nearest real `when`
        /// clause instead of being consumed here. See `Stmt::When`'s field.
        statement_modifier: bool,
    },
    Default {
        body_end: u32,
    },
    /// Push the value a *non-matching* `when` clause evaluates to when the
    /// clause is used as a TERM (`say (when 42 { 43 })`). A matching clause
    /// never reaches this op — it unwinds via `succeed` carrying its own value.
    /// Raku boxes a type-object matcher's `nqp::istype` result as `Int 0` and
    /// everything else as `Bool::False`; `exec_when_op` already records which,
    /// so this consumes that one-shot record (defaulting to `False`).
    PushWhenNonmatch,

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
    ///                         6=InvalidK,7=InvalidNotK,8=InvalidV),
    ///        bits 8-9=subscript kind (0=Unknown, 1=Positional `[ ]`,
    ///                                 2=Associative `{ }` / `< >`)
    ///
    /// The subscript kind is what lets the VM tell `$c[0]` from `$c{0}` on a
    /// target that is Associative but not Positional: raku reads the former
    /// through `Any.EXISTS-POS` (the value is a one-element list holding
    /// itself) and the latter as a key lookup.
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
    /// Record the declarator keyword (`package`/`module`/`grammar`) of a bare
    /// `Stmt::Package` so `.HOW` reports the matching metaclass.
    SetPackageKind {
        name_idx: u32,
        kind: crate::ast::PackageKind,
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
    /// Switch the runtime `current_package` for the rest of the compilation
    /// unit, mirroring what the compiler does to its own `current_package` at a
    /// `unit module Foo;` / `unit package Foo;` declaration. Routine and package
    /// registration is keyed off the *runtime* package, so without this a unit
    /// module's routines register as `GLOBAL::name` and stay callable by their
    /// bare name from every consumer, `is export` or not (PLAN 8.22).
    SetCurrentPackage {
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
        /// ADR-0054 S3: `|EXPR` positions, baked the same way `CallMethod`'s
        /// `arg_sources_idx` is.
        arg_sources_idx: Option<u32>,
    },
    HyperMethodCallDynamic {
        arity: u32,
        modifier_idx: Option<u32>,
        /// ADR-0054 S3: `|EXPR` positions, baked the same way `CallMethod`'s
        /// `arg_sources_idx` is.
        arg_sources_idx: Option<u32>,
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

    // -- X/Z meta-assignment (`@a X[+=] @b`, `@a Z[+=] @b`) --
    // The inner op is an in-place assignment operator. Each cross (X) or zip
    // (Z) pair mutates the corresponding left cell with the base op, in place.
    // Pops right and left, then pushes TWO values: the result Seq (the per-op
    // assignment values, bottom) and the mutated left container (top). The
    // compiler always pairs this with a store of the mutated container back
    // into the left lvalue, leaving the result Seq as the expression value.
    MetaOpAssign {
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
        /// True when this block's CONTROL handler has an arm that can match a
        /// `CX::Take` — an explicit `when CX::Take` or a catch-all `default`.
        /// Raku's `take` ALWAYS raises a `CX::Take` control exception, which
        /// `gather` is merely the outermost handler of; a lexically nearer
        /// CONTROL block sees it first and can `.resume` it (discarding the
        /// value) or handle it without resuming (abandoning the block). mutsu
        /// takes directly into the gather buffer for speed, so this flag marks
        /// the rare blocks where the control-exception route must be taken
        /// instead. Computed at compile time from the CONTROL block AST.
        control_handles_take: bool,
        /// True when this try/catch frame is a genuine bare block statement
        /// (`{ ...; CATCH { } }`) from source. Like `BlockScope::is_bare_block`,
        /// such a block is a callframe and contributes an anonymous backtrace
        /// frame while executing inside it.
        is_bare_block: bool,
        /// True only for a genuine `try` block/expression, which *traps*: an
        /// exception no handler matched is swallowed into `$!`. The same opcode
        /// is also emitted as an implicit wrapper around any block or routine
        /// body that merely contains a `CATCH`/`CONTROL` phaser; such a region
        /// does not trap, so `{ die "x"; CONTROL { } }` must propagate.
        traps: bool,
    },

    /// Bracket `[ip+1..body_end)` with a routine-registry save/restore, so a
    /// `sub` declared inside the range stops being callable when the range ends.
    ///
    /// A statement-level `{ ... }` gets this for free from `BlockScope`, but a
    /// block compiled as a *callable* (a closure body) or *inline* (a
    /// value-producing block) has no `BlockScope`, so its declarations used to
    /// outlive it — and, because the EVAL parser's operator pre-seed is built by
    /// walking the whole registry, a leaked `sub infix:<@>` also changed how a
    /// later `EVAL` string *parsed*. Emitted only when the body actually
    /// declares a routine, so the ordinary block call path is unchanged.
    ///
    /// The value the range leaves on the stack is untouched, and the restore
    /// runs on the error path too, so `return`/`die` escaping the body still
    /// unwinds the registry.
    RoutineScope {
        body_end: u32,
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
    /// The first `bool` payload is `true` if the op is lexically nested inside
    /// a routine (a closure/block in a sub) — in that case `return` should
    /// perform a non-local return up to the enclosing routine, and only
    /// become an `X::ControlFlow::Return` exception when no enclosing routine
    /// is on the dynamic call stack (out-of-dynamic-scope).
    /// When `false`, the op is at top level with no lexical routine and
    /// throws `X::ControlFlow::Return` directly.
    ///
    /// The second `bool` (only meaningful when the first is `false`) is
    /// ADR-0037 §2.3's dead-routine-context classification: `EVAL ..., context
    /// => $ctx` where `$ctx` names a routine that already exited the dynamic
    /// call stack decides this *eagerly*, at EVAL entry, rather than by
    /// unwinding a real signal — so the `X::ControlFlow::Return` thrown here
    /// still needs `out-of-dynamic-scope` set and rakudo's fuller wording,
    /// exactly like a signal that genuinely escaped every frame.
    ReturnFromNonRoutine(bool, bool),
    RegisterDecl(u32),
    RegisterEnum(u32),
    AugmentClass(u32),
    RegisterSubset(u32),
    SubtestScope {
        body_end: u32,
    },
    ReactScope {
        body_end: u32,
    },
    WheneverScope {
        body_idx: u32,
        /// Analysis-only compiled form of the stmt-pool body. It is never
        /// executed, but supplies precise free-variable parent slots to
        /// ADR-0018's env-consumer analysis.
        analysis_cc_idx: u32,
        param_idx: Option<u32>,
        /// Whether this statement is the operand of `do` and therefore leaves
        /// the newly-created Tap on the value stack. A statement-form
        /// `whenever` deliberately sinks that value.
        yields_value: bool,
        /// Constant index of the pointy param's declared type constraint
        /// (`whenever $s -> Int $x { }`), if any.
        param_type_idx: Option<u32>,
    },
    UseModule {
        name_idx: u32,
        tags_idx: Option<u32>,
        /// Number of `use`-argument values pushed on the stack immediately
        /// before this op (`use Foo "a", "b"` / `use Foo <a b c>`). Popped by
        /// the VM and handed to the module's `sub EXPORT`, if any.
        arg_count: u16,
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
    /// slot = local slot index, key_idx = interned `Symbol` id (see
    /// `Symbol::from_id`/`Symbol::id`) for the unique state key — not a
    /// constant-pool index.
    /// Pops init value from stack.
    /// If state_vars has key: set locals[slot] = stored value (discard init).
    /// If not: set locals[slot] = init value, store in state_vars.
    StateVarInit(u32, u32),
    /// Guard for state variable initialization.
    /// Check if state key (arg 0, an interned `Symbol` id like `StateVarInit`)
    /// exists. If yes: push stored value and jump to the absolute instruction
    /// offset (arg 1). If no: fall through so the RHS initializer can be
    /// compiled next.
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

    /// Get a caller-frame lexical when the `CALLER::` site sits inside an
    /// *immediate* block (a bare block / `if` / `for` / `while` body, run in
    /// place). Such a block's dynamic caller IS its lexical parent, so `CALLER::`
    /// there resolves lexically — exactly like [`GetOuterVar`] — rather than
    /// against the runtime call stack (which the block never pushed a frame onto).
    /// Unlike `GetOuterVar` it still enforces the `CALLER::` dynamic-ness contract:
    /// a binding present in the target scope but not declared `is dynamic` throws
    /// X::Caller::NotDynamic (raku: `if 1 { $CALLER::nd }` on a plain lexical). The
    /// compiler only emits this when the target scope DECLARES the name (a
    /// not-declared `CALLER::` is a quiet Nil constant, same as `OUTER::`).
    GetCallerOuterVar {
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

    #[test]
    fn env_consumers_publish_only_their_selected_slots() {
        let mut code = CompiledCode::new();
        code.locals.push("x".to_string());
        code.locals.push("slot_only".to_string());
        let mut gather_body = CompiledCode::new();
        // `free_var_parent_slots` is built by mapping over `free_var_syms`
        // (`add_closure_code_baked`), so the two are index-aligned and the same
        // length. Seed both: the slot alone is a state no compiler run produces,
        // and the consumer-slot fold walks the syms to reach it.
        gather_body.free_var_syms.push(Symbol::intern("x"));
        gather_body.free_var_parent_slots.push(Some(0));
        code.closure_compiled_codes
            .push(std::sync::Arc::new(gather_body));
        code.ops.push(OpCode::MakeGather(0, Some(0)));
        code.compute_needs_env_sync();

        assert_eq!(code.env_consumer_slots.gather, vec![true, false]);
        assert!(code.env_consumer_slots.for_loop.is_empty());
        assert!(code.env_consumer_slots.block_scope.is_empty());
        assert!(code.env_consumer_slots.block_local_scope.is_empty());
        assert!(code.env_consumer_slots.whenever.is_empty());
        assert_eq!(code.needs_env_sync, vec![true, false]);
    }

    #[test]
    fn block_scope_does_not_force_unrelated_slots_into_env_sync() {
        let mut code = CompiledCode::new();
        code.locals.push("inside".to_string());
        code.locals.push("unrelated".to_string());
        code.ops.push(OpCode::BlockScope {
            pre_end: 1,
            enter_end: 1,
            body_end: 3,
            keep_start: 3,
            undo_start: 3,
            post_start: 3,
            end: 3,
            is_bare_block: true,
        });
        code.ops.push(OpCode::LoadNil);
        code.ops.push(OpCode::SetLocalDecl {
            slot: 0,
            explicit_init: true,
        });
        code.compute_needs_env_sync();

        assert_eq!(code.env_consumer_slots.block_scope, vec![true, false]);
        assert_eq!(code.needs_env_sync, vec![true, false]);
    }
}

/// A compiled chunk of bytecode.
#[derive(Debug, Clone, Default)]
pub(crate) struct EnvConsumerSlots {
    pub(crate) for_loop: Vec<bool>,
    pub(crate) block_scope: Vec<bool>,
    pub(crate) block_local_scope: Vec<bool>,
    pub(crate) gather: Vec<bool>,
    pub(crate) whenever: Vec<bool>,
    /// Slots read/written (by name or by local index) inside a `package`/
    /// `module`/`class`-via-`Stmt::Package` body (`OpCode::PackageScope`).
    /// `exec_package_scope_op` reconciles its outer scope through `env`
    /// exactly the way `BlockScope`/`BlockLocalScope` do (see its own
    /// `restored_env` bookkeeping), but this case was missing from the
    /// pre-ADR-0018 fold entirely — a plain scalar referenced ONLY inside such
    /// a body defaulted to `needs_env_sync = false`, so its per-store env
    /// mirror was skipped and `exec_package_scope_op`'s restore read a stale
    /// decl-seed placeholder out of `env` instead of the slot's live value
    /// (`todo/tickets/package-block-resets-an-outer-lexical-declared-before-any-env-flush.md`).
    pub(crate) package_scope: Vec<bool>,
}

/// A declaration-time expression lowered to its own bytecode chunk (ADR-0019 C5).
///
/// A computed routine name (`sub ::($name) {...}`) and a custom trait's argument
/// (`is native(LIB)`, `is symbol('foo')`, `is nonesuch($x)`) are ordinary
/// expressions that have to run when the declaration registers. They used to be
/// handed to the runtime as an `Expr` and compiled on demand at every
/// registration; the compiler now lowers them once, and registration runs the
/// chunk through the VM's normal re-entrant bytecode entry.
#[derive(Debug, Clone)]
pub(crate) struct CompiledDeclExpr {
    pub(crate) code: Arc<CompiledCode>,
    pub(crate) fns: Arc<CompiledFns>,
}

/// Extract shape dimensions from a default expression that matches the
/// pattern `Array.new(:shape(N))` or `Array.new(:shape(N, M, ...))`, as
/// generated for `has @.a[2]` or `has @.a[2;3]` declarations. A pure
/// syntactic fact about the raw `default` expression (ADR-0019 D2c-4,
/// following the D2a precompute pattern), so it is derived once here —
/// before `default` is lowered to a `DeclTraitArg` and this pattern-match
/// becomes unreachable — instead of on every instance construction via
/// `DeclTraitArg::as_expr()` (which panics on a `Compiled` chunk).
fn attr_declared_shape(default: Option<&Expr>) -> Option<Vec<usize>> {
    let expr = default?;
    // Match Array.new(:shape(...)) or Array.new(:shape(...), :data(...))
    let Expr::MethodCall {
        target, name, args, ..
    } = expr
    else {
        return None;
    };
    if name.resolve() != "new" {
        return None;
    }
    if !matches!(target.as_ref(), Expr::BareWord(s) if s == "Array") {
        return None;
    }
    // Find the :shape(...) pair in args
    for arg in args {
        if let Expr::Binary {
            left,
            op: crate::token_kind::TokenKind::FatArrow,
            right,
        } = arg
            && let Expr::Literal(lit) = left.as_ref()
            && let ValueView::Str(key) = lit.view()
            && key.as_str() == "shape"
        {
            return attr_shape_dims_from_expr(right);
        }
    }
    None
}

fn attr_shape_dims_from_expr(expr: &Expr) -> Option<Vec<usize>> {
    match expr {
        Expr::Literal(lit) => match lit.view() {
            ValueView::Int(n) if n >= 0 => Some(vec![n as usize]),
            _ => None,
        },
        Expr::ArrayLiteral(items) => {
            let mut dims = Vec::new();
            for item in items {
                if let Expr::Literal(lit) = item
                    && let ValueView::Int(n) = lit.view()
                {
                    if n >= 0 {
                        dims.push(n as usize);
                    } else {
                        return None;
                    }
                } else {
                    return None;
                }
            }
            if dims.is_empty() { None } else { Some(dims) }
        }
        _ => None,
    }
}

/// The argument of a declaration trait.
///
/// `Literal` and `Compiled` are the ADR-0019 plan path: a constant argument
/// (`is symbol('foo')`) is already a value and needs no chunk at all, everything
/// else runs as bytecode. `Ast` remains for the declaration kinds whose
/// registration still walks a source declaration (the prelude's
/// forward-declaration pass and the class/role method walkers, migrated in
/// phase D); it is the existing fallback narrowed to those callers, not a new one.
#[derive(Debug, Clone)]
pub(crate) enum DeclTraitArg {
    Literal(Value),
    Compiled(CompiledDeclExpr),
    Ast(Box<Expr>),
}

impl DeclTraitArg {
    /// The argument's value when it is a compile-time constant. Declaration
    /// machinery that only needs a literal (the `EXPORTHOW::DECLARE` keyword)
    /// reads it without running anything.
    pub(crate) fn literal(&self) -> Option<&Value> {
        match self {
            DeclTraitArg::Literal(value) => Some(value),
            DeclTraitArg::Ast(expr) => match &**expr {
                Expr::Literal(value) => Some(value),
                _ => None,
            },
            DeclTraitArg::Compiled(_) => None,
        }
    }

    /// Reconstruct an `Expr` from the argument. An escape valve for the few
    /// remaining consumers that store an `Expr` rather than evaluating it
    /// immediately (the role attribute-default registry tables, ADR-0019
    /// D2c-3) — never called on a `Compiled` chunk, since nothing on those
    /// still-AST-walking paths produces one.
    pub(crate) fn as_expr(&self) -> Expr {
        match self {
            DeclTraitArg::Literal(value) => Expr::Literal(value.clone()),
            DeclTraitArg::Ast(expr) => (**expr).clone(),
            DeclTraitArg::Compiled(_) => {
                unreachable!("DeclTraitArg::as_expr called on a Compiled chunk")
            }
        }
    }
}

/// A declaration's custom traits as registration consumes them: the trait name
/// plus its optional argument.
pub(crate) type DeclTraits = [(String, Option<DeclTraitArg>)];

/// Adapt AST-shaped custom traits for a registration path that has not been
/// migrated to declaration plans yet.
pub(crate) fn decl_traits_from_ast(
    traits: &[(String, Option<Expr>)],
) -> Vec<(String, Option<DeclTraitArg>)> {
    traits
        .iter()
        .map(|(name, arg)| {
            (
                name.clone(),
                arg.clone().map(|e| DeclTraitArg::Ast(Box::new(e))),
            )
        })
        .collect()
}

/// Pair a declaration's trait names with the arguments the compiler lowered for
/// them. `lowered` is index-aligned with `custom_traits`; a lowering site may
/// append an argument-less marker trait (`__lexical_hoist`) after building the
/// list, so a shorter list pads with `None` rather than misaligning.
fn zip_decl_trait_args(
    custom_traits: &[(String, Option<Expr>)],
    lowered: Vec<Option<DeclTraitArg>>,
) -> Vec<(String, Option<DeclTraitArg>)> {
    debug_assert!(lowered.len() <= custom_traits.len());
    custom_traits
        .iter()
        .map(|(name, _)| name.clone())
        .zip(lowered.into_iter().chain(std::iter::repeat_with(|| None)))
        .collect()
}

#[derive(Debug, Clone)]
pub(crate) struct CompiledSubDeclPlan {
    pub(crate) name: Symbol,
    /// The compiled chunk producing a runtime-resolved routine name, for
    /// `sub ::($name) {...}`. `None` for the ordinary literal-name declaration.
    pub(crate) name_chunk: Option<CompiledDeclExpr>,
    pub(crate) params: Vec<String>,
    pub(crate) param_defs: Vec<ParamDef>,
    pub(crate) return_type: Option<String>,
    pub(crate) associativity: Option<String>,
    pub(crate) signature_alternates: Vec<(Vec<String>, Vec<ParamDef>)>,
    /// Registration metadata for each `signature_alternates` slot (index-
    /// aligned), computed at plan lowering like `routine_metadata` is for the
    /// primary signature. Alternates used to register metadata-less, which
    /// left their fingerprint/facts caches to a lazy walk over `legacy_body`
    /// — a walk a body-less plan cannot serve (ADR-0019 C6e-3c).
    pub(crate) alternate_metadata: Vec<CompiledRoutineMetadata>,
    /// Stable keys of the bytecode routines compiled for the primary signature
    /// and its alternates. The compiler keeps this association explicit; the
    /// next adapter slice preserves it while importing modules and installs
    /// through these keys directly.
    pub(crate) compiled_routine_keys: Vec<Symbol>,
    pub(crate) multi: bool,
    pub(crate) is_rw: bool,
    pub(crate) is_raw: bool,
    pub(crate) is_export: bool,
    pub(crate) export_tags: Vec<String>,
    pub(crate) is_test_assertion: bool,
    pub(crate) supersede: bool,
    pub(crate) custom_traits: Vec<(String, Option<DeclTraitArg>)>,
    pub(crate) fingerprint: Option<u64>,
    /// Registration metadata derived once while lowering the declaration.
    /// Keeping it beside the plan prevents the registry adapter from walking
    /// `legacy_body` merely to reconstruct signature and identity facts.
    pub(crate) routine_metadata: CompiledRoutineMetadata,
}

#[derive(Debug, Clone)]
pub(crate) struct CompiledRoutineMetadata {
    pub(crate) effective_param_defs: Vec<ParamDef>,
    pub(crate) empty_sig: bool,
    pub(crate) has_non_nil_return: bool,
    pub(crate) is_stub: bool,
    pub(crate) has_param_return_redeclaration: bool,
    /// The OTF-gate body predicates, computed once at plan lowering (ADR-0019
    /// C6e): registration seeds `FunctionDef::body_facts_cache` from this, so
    /// a plan-derived def never has to re-walk its body on a lazy cache miss —
    /// which a body-less def will not be able to serve once `legacy_body` is
    /// dropped.
    pub(crate) body_facts: crate::ast::RoutineBodyFacts,
    /// [`crate::ast::function_body_fingerprint`] over the declaration exactly
    /// as the installed def will carry it (plan params, *effective* param
    /// defs, body). Registration seeds `FunctionDef::body_fp_cache` from it,
    /// so multi-candidate identity, `state` scoping and wrap chains keep the
    /// original body's fingerprint once `legacy_body` is dropped (C6e-3).
    pub(crate) body_fingerprint: u64,
    /// Whether the *declared* body is empty. Registration treats an empty-body
    /// same-signature re-registration as a forward-declaration no-op; that
    /// judgment must come from the declaration, not from the (possibly
    /// dropped) `legacy_body` payload (C6e-3).
    pub(crate) body_is_empty: bool,
}

/// Registration metadata for one declared signature of a sub declaration,
/// computed at plan lowering (ADR-0019 C6e). Called once for the primary
/// signature and once per `signature_alternates` slot — the body is shared,
/// but the signature-derived fields (effective param defs, fingerprints,
/// identity) differ per slot.
pub(crate) fn compiled_routine_metadata(
    params: &[String],
    param_defs: &[ParamDef],
    body: &[Stmt],
    is_rw: bool,
    is_raw: bool,
) -> CompiledRoutineMetadata {
    // `is_rw` / `is_raw` are carried on the plan itself; the only lvalue fact
    // the body contributes is whether it spells `return-rw` (see
    // `RoutineBodyFacts::uses_return_rw`).
    let _ = (is_rw, is_raw);
    let (uses_positional, uses_named) = if params.is_empty() && param_defs.is_empty() {
        let body_shape = format!("{body:?}");
        (
            body_shape.contains("ArrayVar(\"_\")"),
            body_shape.contains("HashVar(\"_\")"),
        )
    } else {
        (false, false)
    };
    let mut effective_param_defs = param_defs.to_vec();
    if effective_param_defs.is_empty() && params.is_empty() {
        if uses_positional {
            effective_param_defs.push(implicit_legacy_param("@_"));
        }
        if uses_named {
            effective_param_defs.push(implicit_legacy_param("%_"));
        }
    }
    CompiledRoutineMetadata {
        empty_sig: params.is_empty() && effective_param_defs.is_empty(),
        has_non_nil_return: body_contains_non_nil_return(body),
        is_stub: is_stub_routine_body(body),
        // Only the Signature-literal spelling `&b:(--> Bool)` counts — see
        // `validate_callable_param_return_redeclaration`.
        has_param_return_redeclaration: param_defs.iter().any(|pd| {
            pd.type_constraint.is_some()
                && pd.sub_signature.is_none()
                && pd
                    .code_signature
                    .as_ref()
                    .is_some_and(|(_, ret)| ret.is_some())
        }),
        body_facts: crate::ast::RoutineBodyFacts {
            needs_interpreter: crate::runtime::Interpreter::function_body_needs_interpreter(body),
            declares_state: crate::runtime::Interpreter::function_body_declares_state(body),
            uses_return_rw: body_uses_return_rw(body),
            registration_identity: crate::ast::registration_identity_fingerprint(
                params,
                &effective_param_defs,
                body,
            ),
        },
        // Hash the declaration exactly as the installed def will carry it:
        // plan params + *effective* param defs + body (see the field doc).
        body_fingerprint: crate::ast::function_body_fingerprint(
            params,
            &effective_param_defs,
            body,
        ),
        body_is_empty: body.is_empty(),
        effective_param_defs,
    }
}

/// Whether a routine body contains an explicit `return-rw` call anywhere a
/// routine's return value can come from: a statement, a `return`, a branch of
/// an `if`/`given`/`when`/loop body, or a ternary arm
/// (`$flag ?? return-rw c<x> !! return-rw c<y>`). Such a routine hands its
/// caller a container without the `is rw` trait (ADR-0059).
pub(crate) fn body_uses_return_rw(stmts: &[Stmt]) -> bool {
    stmts.iter().any(stmt_uses_return_rw)
}

fn stmt_uses_return_rw(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Expr(expr) | Stmt::Return(expr) => expr_uses_return_rw(expr),
        Stmt::Call { name, .. } => name == "return-rw",
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            ..
        } => {
            expr_uses_return_rw(cond)
                || body_uses_return_rw(then_branch)
                || body_uses_return_rw(else_branch)
        }
        Stmt::While { body, .. }
        | Stmt::React { body }
        | Stmt::Whenever { body, .. }
        | Stmt::SyntheticBlock(body)
        | Stmt::Block(body)
        | Stmt::Default(body)
        | Stmt::Subtest { body, .. }
        | Stmt::Given { body, .. }
        | Stmt::When { body, .. }
        | Stmt::For { body, .. } => body_uses_return_rw(body),
        Stmt::Loop { init, body, .. } => {
            init.as_deref().is_some_and(stmt_uses_return_rw) || body_uses_return_rw(body)
        }
        Stmt::Label { stmt, .. } => stmt_uses_return_rw(stmt),
        _ => false,
    }
}

fn expr_uses_return_rw(expr: &Expr) -> bool {
    match expr {
        Expr::Call { name, args } => name == "return-rw" || args.iter().any(expr_uses_return_rw),
        Expr::MethodCall {
            target, name, args, ..
        } => {
            name == "return-rw"
                || expr_uses_return_rw(target)
                || args.iter().any(expr_uses_return_rw)
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            expr_uses_return_rw(cond)
                || expr_uses_return_rw(then_expr)
                || expr_uses_return_rw(else_expr)
        }
        Expr::DoStmt(stmt) => stmt_uses_return_rw(stmt),
        _ => false,
    }
}

fn implicit_legacy_param(name: &str) -> ParamDef {
    ParamDef {
        name: name.to_string(),
        default: None,
        multi_invocant: true,
        required: false,
        named: false,
        slurpy: true,
        double_slurpy: false,
        onearg: false,
        sigilless: false,
        type_constraint: None,
        literal_value: None,
        sub_signature: None,
        where_constraint: None,
        traits: Vec::new(),
        optional_marker: false,
        outer_sub_signature: None,
        code_signature: None,
        is_invocant: false,
        shape_constraints: None,
        block_param: false,
    }
}

fn is_stub_routine_body(body: &[Stmt]) -> bool {
    let mut semantic = body.iter().filter(|stmt| !matches!(stmt, Stmt::SetLine(_)));
    matches!(
        (semantic.next(), semantic.next()),
        (
            Some(Stmt::Expr(Expr::Call { name, .. })),
            None
        ) if name == "__mutsu_stub_die" || name == "__mutsu_stub_warn"
    )
}

/// Recursively surface `has`-attribute names nested inside a `sub` within a
/// class body (`class C { sub f { has $.x } }`), mirroring
/// `collect_nested_has_decl_stmts` below, which `class_body_plan` uses to
/// give each such nested declaration its own trailing `Attr` op (ADR-0019
/// D6-4). Descends into `sub` bodies but not into a nested `class`/`role`,
/// which owns its own attribute scope. `our`/`my` (class-level) attributes
/// are excluded here, as they are not part of per-instance `$!attr`
/// validation.
fn collect_nested_has_decl_names(stmts: &[Stmt], out: &mut Vec<Symbol>) {
    for s in stmts {
        match s {
            Stmt::ClassDecl { .. } | Stmt::RoleDecl { .. } | Stmt::HasDecl { .. } => {}
            Stmt::SubDecl { body, .. } => {
                for inner in body {
                    if let Stmt::HasDecl {
                        name,
                        is_our,
                        is_my,
                        ..
                    } = inner
                        && !*is_our
                        && !*is_my
                    {
                        out.push(*name);
                    }
                }
                collect_nested_has_decl_names(body, out);
            }
            _ => {}
        }
    }
}

/// The set of attribute names a class declares directly in its own body
/// (ADR-0019 D2a), matching `run_class_body`'s pre-scan: top-level `has`
/// declarations (after flattening `has ($a, $b)` list-form
/// `SyntheticBlock`s) plus any `has` nested inside a `sub` in the body.
/// Precomputed once at plan lowering instead of re-walked on every
/// registration.
fn class_own_attribute_names(body: &[Stmt]) -> Vec<Symbol> {
    let mut names: Vec<Symbol> = body
        .iter()
        .flat_map(|s| match s {
            Stmt::SyntheticBlock(inner) => inner.iter().collect::<Vec<_>>(),
            other => vec![other],
        })
        .filter_map(|stmt| match stmt {
            Stmt::HasDecl {
                name,
                is_our,
                is_my,
                ..
            } if !*is_our && !*is_my => Some(*name),
            _ => None,
        })
        .collect();
    collect_nested_has_decl_names(body, &mut names);
    names
}

/// Names a class body `my`/`state`-declares at its own top level (ADR-0019
/// D6-1), mirroring `persist_class_body_statics`'s `declared_statics` scan:
/// a top-level (unflattened) `Stmt::VarDecl` that is neither `our` nor
/// `dynamic`. Precomputed once at plan lowering instead of re-walked on
/// every registration.
fn class_declared_static_names(body: &[Stmt]) -> Vec<Symbol> {
    body.iter()
        .filter_map(|stmt| match stmt {
            Stmt::VarDecl {
                name,
                is_our: false,
                is_dynamic: false,
                ..
            } => Some(Symbol::intern(name)),
            _ => None,
        })
        .collect()
}

/// Pre-scan facts for a role body (ADR-0019 D2a), mirroring the combined
/// loop in `Interpreter::walk_role_body`: attribute names the role declares,
/// module names it `use`s/`need`s/`import`s, and types it declares in its
/// own body. All three are pure syntactic facts, precomputed once at plan
/// lowering instead of re-walked on every registration.
fn role_body_prescan(body: &[Stmt]) -> (Vec<Symbol>, Vec<String>, Vec<String>) {
    let mut own_attribute_names = Vec::new();
    let mut used_modules = Vec::new();
    let mut declared_types = Vec::new();
    let flattened = body.iter().flat_map(|s| match s {
        Stmt::SyntheticBlock(inner) => inner.iter().collect::<Vec<_>>(),
        other => vec![other],
    });
    for stmt in flattened {
        match stmt {
            Stmt::HasDecl { name, .. } => own_attribute_names.push(*name),
            Stmt::Use { module, .. } | Stmt::Need { module } | Stmt::Import { module, .. } => {
                used_modules.push(module.clone());
            }
            Stmt::EnumDecl { name, .. }
            | Stmt::SubsetDecl { name, .. }
            | Stmt::ClassDecl { name, .. }
            | Stmt::RoleDecl { name, .. } => declared_types.push(name.resolve()),
            _ => {}
        }
    }
    (own_attribute_names, used_modules, declared_types)
}

/// Whether the role body is a stub declaration (ADR-0019 D7-1/D9-1),
/// mirroring `Interpreter::role_body_is_stub`: any top-level statement is a
/// yada-stub call (`...`/`!!!`/`???`). Unlike the class side's
/// `is_stub_routine_body`, this does not require the stub to be the body's
/// only statement — precomputed at plan lowering instead of re-walked on
/// every registration.
fn role_body_is_stub(body: &[Stmt]) -> bool {
    body.iter().any(|s| {
        matches!(s, Stmt::Expr(Expr::Call { name, .. })
            if name == "__mutsu_stub_die" || name == "__mutsu_stub_warn")
    })
}

/// The first our-scoped declaration kind found in a role body (ADR-0019
/// D7-1/D9-1), mirroring `Interpreter::check_role_body_our_scoped_decls`'s
/// scan: an implicitly our-scoped `class`/`subset`/`enum`/`role`, or an
/// explicit `our sub`/`our variable`/`our method`/`constant`, is forbidden
/// inside a role body. `None` when the body has no violation. Precomputed
/// at plan lowering instead of re-walked on every registration;
/// `register_role_decl` raises `X::Declaration::OurScopeInRole` from this
/// fact instead of constructing it inline.
fn role_body_our_scope_violation(body: &[Stmt]) -> Option<&'static str> {
    let flattened = body.iter().flat_map(|s| match s {
        Stmt::SyntheticBlock(inner) => inner.iter().collect::<Vec<_>>(),
        other => vec![other],
    });
    for stmt in flattened {
        let declaration = match stmt {
            // A `my class`/`my subset`/`my enum`/`my role` inside a role is
            // lexically scoped and private to the role body, which is
            // allowed; only an implicitly our-scoped declaration is
            // forbidden.
            Stmt::ClassDecl {
                is_lexical: false, ..
            } => Some("class"),
            Stmt::ClassDecl { .. } => None,
            Stmt::SubsetDecl { is_my: true, .. } => None,
            Stmt::SubsetDecl { .. } => Some("subset"),
            Stmt::EnumDecl { is_my: true, .. } => None,
            Stmt::EnumDecl { .. } => Some("enum"),
            Stmt::RoleDecl { custom_traits, .. }
                if custom_traits.iter().any(|(t, _)| t == "__my_scoped") =>
            {
                None
            }
            Stmt::RoleDecl { .. } => Some("role"),
            Stmt::VarDecl {
                is_our: true,
                custom_traits,
                ..
            } => {
                if custom_traits.iter().any(|(t, _)| t == "__constant") {
                    Some("constant")
                } else {
                    Some("variable")
                }
            }
            Stmt::SubDecl { custom_traits, .. }
                if custom_traits.iter().any(|(t, _)| t == "__our_scoped") =>
            {
                Some("sub")
            }
            Stmt::MethodDecl { is_our: true, .. } => Some("method"),
            // `our $.attr` / `our @.attr` / `our %.attr` — an our-scoped
            // class attribute — is rejected exactly like `our $x`, with the
            // same generic "variable" message (verified against raku: both
            // report "Cannot declare our-scoped variable inside of a role").
            // `my $.attr` (`is_my`) and a plain `has $.attr` are unaffected.
            Stmt::HasDecl { is_our: true, .. } => Some("variable"),
            _ => None,
        };
        if declaration.is_some() {
            return declaration;
        }
    }
    None
}

/// Precompute a typed `CompiledMethodDecl` for each top-level `method`/
/// `submethod` declaration in a class/role body (ADR-0019 D3-7), in the same
/// `SyntheticBlock`-flattened order `compile_method_name_chunks` already
/// walks — so the two vecs share one position cursor at registration time.
/// This moves `CompiledMethodDecl::from_stmt`'s destructure from "once per
/// registration" (`class_body_method_decl`/`role_body_method_decl`, which can
/// re-run for a class declared inside a loop or a repeatedly-called sub) to
/// "once, at compile time".
fn compile_method_decls(body: &[Stmt]) -> Vec<CompiledMethodDecl> {
    body.iter()
        .flat_map(|s| match s {
            Stmt::SyntheticBlock(inner) => inner.iter().collect::<Vec<_>>(),
            other => vec![other],
        })
        .filter_map(|stmt| match stmt {
            Stmt::MethodDecl { .. } => Some(CompiledMethodDecl::from_stmt(stmt)),
            _ => None,
        })
        .collect()
}

fn body_contains_non_nil_return(stmts: &[Stmt]) -> bool {
    stmts.iter().any(|stmt| match stmt {
        Stmt::Return(expr) => !matches!(expr, Expr::Literal(value) if value.is_nil()),
        Stmt::If {
            then_branch,
            else_branch,
            ..
        } => body_contains_non_nil_return(then_branch) || body_contains_non_nil_return(else_branch),
        Stmt::While { body, .. }
        | Stmt::React { body }
        | Stmt::SyntheticBlock(body)
        | Stmt::Block(body)
        | Stmt::Subtest { body, .. }
        | Stmt::For { body, .. } => body_contains_non_nil_return(body),
        Stmt::Loop { init, body, .. } => {
            init.as_deref()
                .is_some_and(|stmt| body_contains_non_nil_return(std::slice::from_ref(stmt)))
                || body_contains_non_nil_return(body)
        }
        _ => false,
    })
}

#[derive(Debug, Clone)]
pub(crate) struct CompiledClassDeclPlan {
    pub(crate) name: Symbol,
    /// The compiled chunk producing a runtime-resolved type name
    /// (`class ::($name) {...}`). `None` for the ordinary literal-name form.
    pub(crate) name_chunk: Option<CompiledDeclExpr>,
    pub(crate) parents: Vec<String>,
    pub(crate) class_is_rw: bool,
    pub(crate) is_hidden: bool,
    pub(crate) is_lexical: bool,
    pub(crate) hidden_parents: Vec<String>,
    pub(crate) does_parents: Vec<String>,
    pub(crate) repr: Option<String>,
    pub(crate) language_version: String,
    pub(crate) custom_traits: Vec<(String, Option<DeclTraitArg>)>,
    pub(crate) decl_id: u64,
    /// Whether the declared body is a yada stub (ADR-0019 D1). Precomputed at
    /// plan lowering so registration never re-walks the raw body to judge
    /// this — mirrors `CompiledRoutineMetadata::is_stub` for subs.
    pub(crate) is_stub: bool,
    /// `trusts SomeClass` declarations at the top level of the body,
    /// precomputed at plan lowering (ADR-0019 D1) instead of scanning the
    /// raw body for `Stmt::TrustsDecl` at registration time.
    pub(crate) trusts: Vec<Symbol>,
    /// Attribute names this class declares directly in its own body
    /// (ADR-0019 D2a), precomputed at plan lowering instead of
    /// `run_class_body` re-scanning the (flattened, nested-sub-surfaced)
    /// body on every registration.
    pub(crate) own_attribute_names: Vec<Symbol>,
    /// Precompiled typed descriptor for each of the class's own attributes
    /// (ADR-0019 D2b remainder), keyed by attribute name — `class_body_has_decl`
    /// looks a descriptor up by the `Stmt::HasDecl` it is currently visiting
    /// instead of calling `CompiledAttrDecl::from_stmt` on the raw statement
    /// at registration time (falling back to `from_stmt` only on a lookup
    /// miss, e.g. a class-level `our`/`my` attribute, which this vec excludes
    /// the same way `own_attribute_names` does). Every declaration-time
    /// expression each descriptor carries (`is default(...)`, `default`,
    /// `where_constraint`) is precompiled to a `Literal`/`Compiled` chunk
    /// (ADR-0019 D2c-1/D2c-4).
    pub(crate) attr_decls: Vec<(Symbol, CompiledAttrDecl)>,
    /// Precompiled runtime-resolved-name chunk for each top-level `method`/
    /// `submethod` declaration in the body (ADR-0019 D3-1), one entry per
    /// method in the order `run_class_body`'s `SyntheticBlock`-flattened walk
    /// encounters them (`None` for a method with no computed name). Read by
    /// position, not by name: an indirect `method ::($name) {...}` name has
    /// no guaranteed-unique fallback name to key on the way attributes do.
    pub(crate) method_name_chunks: Vec<Option<CompiledDeclExpr>>,
    /// Precompiled typed mirror of each top-level `method`/`submethod`
    /// declaration in the body (ADR-0019 D3-7), position-aligned with
    /// `method_name_chunks` (built by the same flattened walk). Registration
    /// reads a clone by position instead of calling `CompiledMethodDecl::from_stmt`
    /// on the raw statement every time the class declaration executes.
    pub(crate) method_decls: Vec<CompiledMethodDecl>,
    /// Lexicals visible in the enclosing frame at this declaration site.
    /// Method compilation uses a fresh scope, so this distinguishes genuine
    /// outer lexicals from class-body statics and lexicals declared later.
    pub(crate) method_outer_lexical_slots: Vec<(Symbol, u32)>,
    /// Names the class body `my`/`state`-declares at its own top level
    /// (ADR-0019 D6-1), precomputed at plan lowering instead of
    /// `persist_class_body_statics` re-walking the raw body on every
    /// registration to decide which lexicals are body statics.
    pub(crate) declared_static_names: Vec<Symbol>,
    /// Precompiled argument chunks for each bracketed `is Parent[Args]`/
    /// `does Role[Args]`/`hides Parent[Args]` parent (ADR-0019 D4-2), keyed
    /// by the same concatenated parent string that `parents`/`does_parents`/
    /// `hidden_parents` use as a registry lookup key. Only entries whose
    /// bracket content parsed as a clean expression list (D4-1) appear here;
    /// Evaluated at composition time (ADR-0019 D4-3) instead of
    /// `resolve_role_candidate` re-parsing the concatenated parent string.
    pub(crate) parent_arg_chunks: Vec<(String, Vec<DeclTraitArg>)>,
    /// Ordered, typed mirror of the flattened class body (ADR-0019 D6-3a),
    /// one op per statement — the sole driver of `run_class_body`'s dispatch
    /// loop since D6-4 (the same `SyntheticBlock`-flattened top level, with
    /// nested-sub `has` declarations appended at the end — see
    /// [`class_body_plan`]). The already-typed arms (`Attr`/`Method`/
    /// `Does`/`ClassSub`) carry only a name/marker, since their real
    /// payload already lives in `attr_decls`/`method_decls`/
    /// `parent_arg_chunks`; the remaining arms carry their raw statement
    /// alongside their precompiled `chunk`.
    pub(crate) body_plan: Vec<ClassBodyOp>,
}

/// One class-body statement, typed (ADR-0019 D6-3a). See
/// [`CompiledClassDeclPlan::body_plan`].
#[derive(Debug, Clone)]
pub(crate) enum ClassBodyOp {
    /// A top-level (or nested-sub) `has` declaration. Its typed descriptor
    /// lives in `attr_decls`, keyed by this same name (ADR-0019 D10:
    /// `attr_decls` covers a class-level `our`/`my` attribute too, so no
    /// raw-statement fallback is needed here — see `class_body_has_decl`).
    Attr { name: Symbol },
    /// A `method`/`submethod` declaration. Advances the existing
    /// `method_name_chunks`/`method_decls` position cursor.
    Method,
    /// A body-level `also does Role` clause.
    Does {
        name: Symbol,
        args: Option<Vec<DeclTraitArg>>,
    },
    /// A `sub` declaration. Runs like `Other` (via `chunk`), plus carries
    /// the fact that a successful registration also needs the
    /// `class_subs` tail-probe `run_class_body` performs after executing it.
    ClassSub {
        name: Symbol,
        chunk: Option<CompiledDeclExpr>,
        raw: Stmt,
        /// See [`ClassBodyOp::Other::is_swallowable`]. Always `false` for a
        /// `sub` declaration (a `Stmt::SubDecl` never matches the
        /// BEGIN/EVAL shapes), computed here anyway so `class_body_other_stmt`
        /// (which handles both `ClassSub` and `Other`) reads one flag.
        is_swallowable: bool,
        /// See [`ClassBodyOp::Other::is_compile_time_phaser`]. Always
        /// `false` for a `sub` declaration, for the same reason as
        /// `is_swallowable` above.
        is_compile_time_phaser: bool,
    },
    /// `our &baz ::= &bar` — alias a method under a new name.
    CodeAlias {
        chunk: Option<CompiledDeclExpr>,
        raw: Stmt,
    },
    /// A `proto method`/`proto submethod` declaration.
    ProtoMethod {
        chunk: Option<CompiledDeclExpr>,
        raw: Stmt,
    },
    /// A `will leave { ... }`-style class-body-scoped LEAVE phaser.
    LeavePhaser {
        chunk: Option<CompiledDeclExpr>,
        raw: Stmt,
    },
    /// A `token`/`rule` declaration inside a class body (ADR-0019 F7 slice
    /// 2), precomputed as a typed [`CompiledTokenDeclPlan`] instead of
    /// falling into `Other`'s raw-`Stmt` fallback. A class body's own
    /// package is fixed and known at class-declaration compile time
    /// (unlike a role body's, whose composing package is only known at
    /// composition — see [`RoleBodyOp::Deferred`]/`DeferredBodyOpKind::TokenRule`,
    /// which keeps the raw-`Stmt` fallback for exactly that reason), so
    /// `run_class_body` calls `register_token_decl` straight from this
    /// plan's fields instead of OTF-recompiling the statement through
    /// `run_block_raw` on every registration. The regex body itself
    /// (`raw_body`) stays interpreter-executed, unchanged — ADR-0009's own
    /// execution model, not something this migration touches.
    TokenRule { plan: CompiledTokenDeclPlan },
    /// Everything else (`use`/`need`, nested `class`/`role`, BEGIN/CHECK,
    /// EVAL, `my`/`our` lexicals, ...).
    Other {
        chunk: Option<CompiledDeclExpr>,
        raw: Stmt,
        /// Whether `raw` is a `BEGIN` phaser or an `EVAL` call (ADR-0019
        /// D10 follow-up), precomputed here so `class_body_other_stmt`
        /// reads a flag instead of re-matching `raw`'s shape at
        /// registration time to decide whether to swallow a failure so the
        /// class still registers.
        is_swallowable: bool,
        /// Whether `raw` is a `BEGIN`/`CHECK` phaser (ADR-0019 D10
        /// follow-up), precomputed here so `class_body_other_stmt` reads a
        /// flag instead of re-matching `raw`'s shape to decide whether a
        /// `has`-attribute declaration it executes should attach to the
        /// class being defined.
        is_compile_time_phaser: bool,
    },
}

/// Lower a class body into its ordered, typed op mirror (ADR-0019 D6-3a):
/// `SyntheticBlock`-flatten the top level, classify each statement, then
/// append nested-sub `has` declarations as more `Attr` ops. Since D6-4,
/// this is the sole source `run_class_body` walks — there is no separate
/// runtime-side flatten/append pass to mirror any more.
///
/// Also tracks the running `Stmt::SetLine` value while walking the flattened
/// list, exactly like `Compiler::compile_method_body_keys` does for a method
/// declaration's own line: a class body's `token`/`rule` statement carries no
/// line of its own (see [`CompiledTokenDeclPlan::source_line`]), only the
/// `SetLine` marker immediately preceding it in the body does.
pub(crate) fn class_body_plan(body: &[Stmt]) -> Vec<ClassBodyOp> {
    let mut flattened: Vec<&Stmt> = body
        .iter()
        .flat_map(|s| match s {
            Stmt::SyntheticBlock(inner) => inner.iter().collect::<Vec<_>>(),
            other => vec![other],
        })
        .collect();
    collect_nested_has_decl_stmts(body, &mut flattened);
    let mut decl_line: Option<i64> = None;
    flattened
        .iter()
        .map(|stmt| {
            if let Stmt::SetLine(line) = stmt {
                decl_line = Some(*line);
            }
            classify_class_body_stmt(stmt, decl_line)
        })
        .collect()
}

/// `has` declarations inside a body `sub`, as statement references rather
/// than just names (unlike [`collect_nested_has_decl_names`]) — unfiltered,
/// so a class-level `our`/`my` nested `has` gets its own `Attr` op too (its
/// `raw` field is `class_body_has_decl`'s only source for it, since
/// `attr_decls` excludes it).
fn collect_nested_has_decl_stmts<'a>(stmts: &'a [Stmt], out: &mut Vec<&'a Stmt>) {
    for s in stmts {
        match s {
            Stmt::ClassDecl { .. } | Stmt::RoleDecl { .. } | Stmt::HasDecl { .. } => {}
            Stmt::SubDecl { body, .. } => {
                for inner in body {
                    if matches!(inner, Stmt::HasDecl { .. }) {
                        out.push(inner);
                    }
                }
                collect_nested_has_decl_stmts(body, out);
            }
            _ => {}
        }
    }
}

fn classify_class_body_stmt(stmt: &Stmt, decl_line: Option<i64>) -> ClassBodyOp {
    match stmt {
        Stmt::Phaser {
            kind: crate::ast::PhaserKind::Leave,
            ..
        } => ClassBodyOp::LeavePhaser {
            chunk: None,
            raw: stmt.clone(),
        },
        Stmt::HasDecl { name, .. } => ClassBodyOp::Attr { name: *name },
        Stmt::MethodDecl { .. } => ClassBodyOp::Method,
        Stmt::DoesDecl { name, .. } => ClassBodyOp::Does {
            name: *name,
            args: None,
        },
        Stmt::VarDecl {
            expr: Expr::CodeVar(_),
            name: var_name,
            ..
        } if var_name.starts_with('&') => ClassBodyOp::CodeAlias {
            chunk: None,
            raw: stmt.clone(),
        },
        Stmt::ProtoDecl {
            is_method: true, ..
        } => ClassBodyOp::ProtoMethod {
            chunk: None,
            raw: stmt.clone(),
        },
        Stmt::SubDecl { name, .. } => ClassBodyOp::ClassSub {
            name: *name,
            chunk: None,
            raw: stmt.clone(),
            is_swallowable: is_swallowable_class_body_stmt(stmt),
            is_compile_time_phaser: is_compile_time_phaser_stmt(stmt),
        },
        Stmt::TokenDecl { .. } | Stmt::RuleDecl { .. } => ClassBodyOp::TokenRule {
            plan: build_token_decl_plan(stmt, decl_line),
        },
        _ => ClassBodyOp::Other {
            chunk: None,
            raw: stmt.clone(),
            is_swallowable: is_swallowable_class_body_stmt(stmt),
            is_compile_time_phaser: is_compile_time_phaser_stmt(stmt),
        },
    }
}

/// Whether a class-body statement is a `BEGIN` phaser or an `EVAL` call
/// (ADR-0019 D10 follow-up) — see [`ClassBodyOp::Other::is_swallowable`].
fn is_swallowable_class_body_stmt(stmt: &Stmt) -> bool {
    matches!(
        stmt,
        Stmt::Phaser {
            kind: crate::ast::PhaserKind::Begin,
            ..
        }
    ) || matches!(
        stmt,
        Stmt::Call { name: fn_name, .. }
            if fn_name.resolve() == "EVAL"
    ) || matches!(
        stmt,
        Stmt::Expr(Expr::Call { name: fn_name, .. })
            if fn_name.resolve() == "EVAL"
    )
}

/// Whether a class-body statement is a `BEGIN`/`CHECK` phaser (ADR-0019 D10
/// follow-up) — see [`ClassBodyOp::Other::is_compile_time_phaser`].
fn is_compile_time_phaser_stmt(stmt: &Stmt) -> bool {
    matches!(
        stmt,
        Stmt::Phaser {
            kind: crate::ast::PhaserKind::Begin | crate::ast::PhaserKind::Check,
            ..
        }
    )
}

/// One `does`/`hides`/`is hidden` clause from a role's own body, as a typed
/// plan op (ADR-0019 D7-3) read by position during the role-body walk
/// instead of the runtime string-matching the `__mutsu_role_hides__`/
/// `__mutsu_role_hidden__` marker names the parser encodes as synthetic
/// `Stmt::DoesDecl` statements. One op per `DoesDecl` statement the
/// (`SyntheticBlock`-flattened) body contains, in source order — mirroring
/// `walk_role_body`'s own flatten exactly so the two sides' cursors agree.
#[derive(Debug, Clone)]
pub(crate) struct RoleParentOp {
    /// The parent/hidden-class name (unused when `hidden` is set — the
    /// `is hidden` marker names nothing).
    pub(crate) name: Symbol,
    /// This op is the `__mutsu_role_hides__` marker: `name` is the class
    /// this role hides (already stripped of the marker prefix).
    pub(crate) hides: bool,
    /// This op is the `__mutsu_role_hidden__` marker (`is hidden` on the
    /// role itself).
    pub(crate) hidden: bool,
    /// Precompiled bracket-argument chunks for a real `does Role[Args]`
    /// parent (ADR-0019 D4-1's `Stmt::DoesDecl::args`, compiled the same way
    /// as `parent_arg_chunks`). `None` when the bracket content did not
    /// parse as a clean expression list, or there is no bracket — the
    /// consumer falls back to the string path exactly as the class-header
    /// site (D4-3) does.
    pub(crate) args: Option<Vec<DeclTraitArg>>,
}

/// One role-body statement, typed (ADR-0019 D7-4) — the role-side twin of
/// [`ClassBodyOp`], and since D9 the sole driver of `walk_role_body`.
/// Deliberately narrower than `ClassBodyOp`: a role body has no nested-sub
/// `has` collection and no `ClassSub`/`CodeAlias`/`ProtoMethod`/`LeavePhaser`
/// arms (those class-only statement kinds fall through to `Deferred` in a
/// role body, exactly as `walk_role_body`'s own catch-all treats them), and
/// carries no compiled chunk itself — deferred-statement chunk compilation
/// is `RoleDef::deferred_body`'s own `DeferredBodyOp` (ADR-0019 D8), a
/// separate type built from this one's `Deferred` ops.
#[derive(Debug, Clone)]
pub(crate) enum RoleBodyOp {
    /// A top-level `has` declaration. Its typed descriptor lives in
    /// `attr_decls`, keyed by this same name — `compile_role_attr_decls` has
    /// always covered a role-level `our`/`my` attribute too, so no
    /// raw-statement fallback is needed here (ADR-0019 D10; see
    /// `role_body_has_decl`).
    Attr { name: Symbol },
    /// A `method`/`submethod` declaration. Advances the existing
    /// `method_name_chunks`/`method_decls` position cursor.
    Method,
    /// A `does`/`hides`/`is hidden` clause. Its typed descriptor lives in
    /// `parent_ops`, read by position via `parent_op_idx`.
    Parent,
    /// Everything else: the `__mutsu_stub_die`/`__mutsu_stub_warn` stub
    /// marker call (`is_stub` already covers this as a plan fact — see
    /// `role_body_is_stub`), `SetLine` source-line markers, and every
    /// statement `walk_role_body` defers to run at composition time (see
    /// `RoleDef::deferred_body`/`DeferredBodyOp`). Boxed: unlike `ClassBodyOp` (whose other
    /// variants also carry a same-size `Stmt`, keeping the largest/
    /// second-largest gap small), `Attr`/`Method`/`Parent` here are all
    /// marker-sized, so an unboxed `Stmt` would trip
    /// `clippy::large_enum_variant`.
    Deferred {
        raw: Box<Stmt>,
        /// Whether `raw` is itself the `__mutsu_stub_die`/`__mutsu_stub_warn`
        /// stub-marker call (ADR-0019 D10 follow-up), precomputed here so
        /// `walk_role_body` reads a flag instead of re-matching `raw`'s
        /// shape at registration time.
        is_stub_marker: bool,
    },
}

/// Lower a role body into its ordered, typed op mirror (ADR-0019 D7-4),
/// matching `walk_role_body`'s own dispatch loop exactly: a single-level
/// `SyntheticBlock`-flatten (roles have no nested-sub `has` collection —
/// `walk_role_body`'s own comment confirms it), classifying each statement
/// the same way the runtime match does.
pub(crate) fn role_body_plan(body: &[Stmt]) -> Vec<RoleBodyOp> {
    body.iter()
        .flat_map(|s| match s {
            Stmt::SyntheticBlock(inner) => inner.iter().collect::<Vec<_>>(),
            other => vec![other],
        })
        .map(classify_role_body_stmt)
        .collect()
}

fn classify_role_body_stmt(stmt: &Stmt) -> RoleBodyOp {
    match stmt {
        Stmt::HasDecl { name, .. } => RoleBodyOp::Attr { name: *name },
        Stmt::MethodDecl { .. } => RoleBodyOp::Method,
        Stmt::DoesDecl { .. } => RoleBodyOp::Parent,
        _ => RoleBodyOp::Deferred {
            is_stub_marker: is_stub_marker_stmt(stmt),
            raw: Box::new(stmt.clone()),
        },
    }
}

/// Whether `stmt` is the `__mutsu_stub_die`/`__mutsu_stub_warn` stub-marker
/// call the parser synthesizes for a stubbed role/method body (ADR-0019 D10
/// follow-up). See [`RoleBodyOp::Deferred::is_stub_marker`].
fn is_stub_marker_stmt(stmt: &Stmt) -> bool {
    matches!(
        stmt,
        Stmt::Expr(Expr::Call { name, .. })
            if name == "__mutsu_stub_die" || name == "__mutsu_stub_warn"
    )
}

/// How a deferred role-body statement's package resolves at composition
/// time (ADR-0019 D8-1), mirroring `run_composed_role_deferred_body`'s
/// `is_type_decl`/`is_regex_decl` classification.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum DeferredBodyOpKind {
    /// A nested `class`/`role` declaration — registers under the role's
    /// OWN package at composition time.
    TypeDecl,
    /// A `token`/`rule`/`regex` declaration — registers under the
    /// COMPOSING class's package, which is not known until composition;
    /// excluded from the compiled-chunk cutover, the same ADR-0009
    /// carve-out D6/D9 apply to class-body token/rule statements.
    TokenRule,
    /// Everything else — runs with whatever package was ambient when
    /// composition started.
    Plain,
}

/// One deferred role-body statement, precompiled (ADR-0019 D8-1). Every
/// composition entry point runs these ops (ADR-0019 D8-2) instead of
/// re-parsing/re-lowering the raw statement per statement on every
/// composition. Reuses [`RoleBodyOp::Deferred`]'s raw statements as input —
/// see [`deferred_body_ops`].
#[derive(Debug, Clone)]
pub(crate) struct DeferredBodyOp {
    pub(crate) kind: DeferredBodyOpKind,
    /// `Some` only for `TypeDecl`: a nested `class`/`role` in a role body
    /// always registers under the role's OWN package regardless of the
    /// composition call site, so precompiling against that fixed package is
    /// verified-correct (ADR-0019 D8-2's V1 check: a parametric role's
    /// nested class referencing a type parameter, composed at two different
    /// type arguments). `None` for `TokenRule` (composing-class package,
    /// unknown until composition) and for `Plain` (the AMBIENT package at
    /// the composition call site, also unknown until composition —
    /// freezing it to the role's own package broke `my package G { class A
    /// is Array[T] {} }`'s `G::A` qualification the V1 check caught; see
    /// `compile_role_deferred_body`'s doc comment) — both fall back to
    /// `raw`/`run_block_raw`, which recompiles under the interpreter's
    /// actual ambient package at that specific composition.
    pub(crate) chunk: Option<CompiledDeclExpr>,
    /// The name this statement declares as a plain (non-`our`,
    /// non-`dynamic`) lexical `VarDecl`, replacing
    /// `run_composed_role_deferred_body`'s own re-scan of every deferred
    /// statement for this same fact. Empty for every other statement kind.
    pub(crate) declared_vars: Vec<Symbol>,
    pub(crate) raw: Stmt,
    /// `Some` only for `TokenRule`: the `SetLine` marker immediately
    /// preceding this declaration in the role body, for `Code.line`/
    /// `Code.file` (see `CompiledTokenDeclPlan::source_line`, which this
    /// mirrors). A `TokenRule` op's `chunk` stays `None` above because the
    /// COMPOSING package is unknown at role-declaration time, but the line
    /// is a role-declaration-time fact regardless of who composes the role,
    /// so it is captured here rather than lost the way `chunk` is.
    pub(crate) source_line: Option<i64>,
}

pub(crate) fn classify_deferred_body_op_kind(stmt: &Stmt) -> DeferredBodyOpKind {
    match stmt {
        Stmt::ClassDecl { .. } | Stmt::RoleDecl { .. } => DeferredBodyOpKind::TypeDecl,
        Stmt::TokenDecl { .. } | Stmt::RuleDecl { .. } => DeferredBodyOpKind::TokenRule,
        _ => DeferredBodyOpKind::Plain,
    }
}

pub(crate) fn deferred_body_op_declared_vars(stmt: &Stmt) -> Vec<Symbol> {
    match stmt {
        Stmt::VarDecl {
            name,
            is_our: false,
            is_dynamic: false,
            ..
        } => vec![Symbol::intern(name)],
        _ => Vec::new(),
    }
}

#[derive(Debug, Clone)]
pub(crate) struct CompiledRoleDeclPlan {
    pub(crate) name: Symbol,
    pub(crate) type_params: Vec<String>,
    pub(crate) type_param_defs: Vec<ParamDef>,
    pub(crate) is_export: bool,
    pub(crate) export_tags: Vec<String>,
    pub(crate) is_rw: bool,
    pub(crate) language_version: String,
    pub(crate) custom_traits: Vec<(String, Option<DeclTraitArg>)>,
    /// Attribute names this role declares in its own body (ADR-0019 D2a),
    /// precomputed at plan lowering instead of `walk_role_body`'s pre-scan
    /// pass re-deriving it on every registration.
    pub(crate) own_attribute_names: Vec<Symbol>,
    /// Module names the body `use`s/`need`s/`import`s (ADR-0019 D2a),
    /// precomputed alongside `own_attribute_names`.
    pub(crate) body_used_modules: Vec<String>,
    /// Types the body declares itself (`my enum`, `my class`, ...)
    /// (ADR-0019 D2a), precomputed alongside `own_attribute_names`.
    pub(crate) body_declared_types: Vec<String>,
    /// Precompiled typed descriptor for each attribute the role declares in
    /// its own body (ADR-0019 D2b remainder/D2c-4), keyed by attribute name
    /// — see `CompiledClassDeclPlan::attr_decls`. Its `is default(...)`/
    /// `default`/`where_constraint` chunks are precompiled the same way the
    /// class side's are: a role attribute default referencing the role's
    /// type parameters (`is default(T)`) binds them as ordinary env
    /// variables before evaluation, not via AST substitution, so one
    /// compile-time chunk is sound across every composing class.
    pub(crate) attr_decls: Vec<(Symbol, CompiledAttrDecl)>,
    /// Lexicals visible in the enclosing frame at this declaration site.
    /// The role twin of `CompiledClassDeclPlan::method_outer_lexical_slots`:
    /// a role declared in a routine body has methods that close over that
    /// routine's `my` variables, which live only in its local slots.
    pub(crate) method_outer_lexical_slots: Vec<(Symbol, u32)>,
    /// Precompiled runtime-resolved-name chunk for each top-level `method`/
    /// `submethod` declaration in the body (ADR-0019 D3-1). See
    /// `CompiledClassDeclPlan::method_name_chunks`.
    pub(crate) method_name_chunks: Vec<Option<CompiledDeclExpr>>,
    /// Precompiled typed mirror of each top-level `method`/`submethod`
    /// declaration in the body (ADR-0019 D3-7). See
    /// `CompiledClassDeclPlan::method_decls`.
    pub(crate) method_decls: Vec<CompiledMethodDecl>,
    /// Whether the role body is a stub declaration (ADR-0019 D7-1/D9-1),
    /// precomputed at plan lowering instead of `register_role_decl`
    /// re-walking the body every registration.
    pub(crate) is_stub: bool,
    /// The first our-scoped declaration kind (`"class"`, `"variable"`, ...)
    /// found in the role body, if any (ADR-0019 D7-1/D9-1); `None` when the
    /// body has no violation. Precomputed at plan lowering instead of
    /// `check_role_body_our_scoped_decls` re-walking the body every
    /// registration; `register_role_decl` raises
    /// `X::Declaration::OurScopeInRole` from this fact.
    pub(crate) our_scope_violation: Option<&'static str>,
    /// Typed `does`/`hides`/`is hidden` ops for this role's own body
    /// (ADR-0019 D7-3), one per `DoesDecl` statement in source order; see
    /// [`RoleParentOp`].
    pub(crate) parent_ops: Vec<RoleParentOp>,
    /// Ordered, typed mirror of the (single-level flattened) role body
    /// (ADR-0019 D7-4), one op per statement — the sole driver of
    /// `walk_role_body`'s dispatch loop since D9. See [`RoleBodyOp`].
    pub(crate) body_plan: Vec<RoleBodyOp>,
    /// Precompiled per-statement chunk for each deferred (non-attribute,
    /// non-method, non-`does`) statement in the role body (ADR-0019 D8-1),
    /// derived from `body_plan`'s `Deferred` ops. `register_role_decl`
    /// copies this onto `RoleDef::deferred_body`, the authoritative
    /// execution path every composition entry point runs (ADR-0019 D8-2).
    /// See [`DeferredBodyOp`].
    pub(crate) deferred_body_ops: Vec<DeferredBodyOp>,
    /// This role declaration's identity, minted once here at plan-lowering
    /// (compile) time rather than freshly on every runtime execution of the
    /// registration op. A role body inside a repeatedly-invoked sub/block
    /// re-registers on every call (Rakudo re-runs role composition/attribute
    /// setup each time too), but the role's *identity* — used to key a
    /// `but`/`does` mixin's `.WHAT` (see `mixin_composition_key`) — must stay
    /// stable across those re-registrations, matching Rakudo's declaration-
    /// site-stable identity for both named and anonymous roles. Minting a
    /// fresh id per plan (not per registration) still gives two textually
    /// distinct `my role A {}` declarations in different scopes distinct
    /// ids, since each is its own AST node / plan entry.
    pub(crate) role_id: u64,
}

/// A package-level `proto sub`/`proto rule`/`proto token` declaration lowered
/// at compile time (ADR-0019 C8). The `{*}` placeholder in a non-trivial body
/// is rewritten to a `__PROTO_DISPATCH__()` call and compiled once, here,
/// instead of being rewritten and OTF-compiled on every call.
#[derive(Debug, Clone)]
pub(crate) struct CompiledProtoDeclPlan {
    pub(crate) name: Symbol,
    pub(crate) params: Vec<String>,
    pub(crate) param_defs: Vec<ParamDef>,
    pub(crate) return_type: Option<String>,
    pub(crate) is_export: bool,
    pub(crate) custom_traits: Vec<String>,
    /// True for `proto method`/`proto submethod`: such a proto never
    /// registers at the package level (its `{*}` dispatches over the type's
    /// method table, Phase D territory), so `compiled_routine_key` is always
    /// `None` for it.
    pub(crate) is_method: bool,
    pub(crate) is_our: bool,
    /// Compatibility payload, mirroring `CompiledRoleDeclPlan::legacy_body`:
    /// a registered `FunctionDef` still needs the raw (un-rewritten) body for
    /// the pure-interpreter fallback (`call_proto_function`, reached from the
    /// user-operator dispatch fallback) and for judging triviality
    /// (`vm_resolve_trivial_proto_candidate`). Dropping it is a later box,
    /// not this one.
    pub(crate) legacy_body: Vec<Stmt>,
    /// Stable key of the bytecode compiled for the `{*}`-rewritten body.
    /// `None` for a trivial proto (an empty body, or a body that is just a
    /// bare `{*}`), which dispatches implicitly and has no candidate body of
    /// its own to compile, and for a method proto (`is_method`).
    pub(crate) compiled_routine_key: Option<Symbol>,
}

/// A `token`/`rule` declaration plan (ADR-0019 F7). Unlike `CompiledSubDeclPlan`, a
/// token/rule body is never bytecode-compiled — that stays interpreter-executed by
/// design (ADR-0009's regex/grammar execution model) — so `raw_body` is kept as an
/// opaque payload rather than a `compiled_routine_key`, mirroring
/// `CompiledProtoDeclPlan::legacy_body`'s own precedent for the same reason.
///
/// `Stmt::TokenDecl`'s own `is_my`/`is_our` fields are not carried here: the
/// pre-existing registration path (`register_token_decl`) never read them
/// either (`exec_register_token_op`'s old match arm dropped them via `..`) —
/// this plan preserves that exact fidelity rather than inventing unread
/// fields. See `todo/deep/adr0019-f7-token-rule-declaration-typed-plan.md`
/// ("Found while scoping") for why that drop was verified benign, not a live
/// bug this box should fix.
#[derive(Debug, Clone)]
pub(crate) struct CompiledTokenDeclPlan {
    pub(crate) name: Symbol,
    pub(crate) params: Vec<String>,
    pub(crate) param_defs: Vec<ParamDef>,
    pub(crate) multi: bool,
    pub(crate) raw_body: Vec<Stmt>,
    /// The declarator keyword's source line (`Code.line`/`Code.file`), fed
    /// into the registered `FunctionDef`. Unlike `Sub`/`Method` (whose line
    /// rides on their own compiled body), a token/rule has no compiled body
    /// at all (ADR-0009), so this is the only place to carry it: a
    /// `SetLine`-tracking scan over the enclosing statement list, done by
    /// `CompiledCode::add_token_decl_plan` (top level) and `class_body_plan`
    /// (class body) — never read off `stmt` itself, which carries no line of
    /// its own. `None` when no `SetLine` preceded this declaration (e.g. a
    /// role's deferred body, recompiled standalone at composition time with
    /// no line history — see `run_composed_role_deferred_body`).
    pub(crate) source_line: Option<i64>,
}

/// Build a [`CompiledTokenDeclPlan`] from a `Stmt::TokenDecl`/`RuleDecl`.
/// Shared by `CompiledCode::add_token_decl_plan` (the top-level
/// `RegisterDecl(Token)` path, ADR-0019 F7 slice 1) and
/// `classify_class_body_stmt` (`ClassBodyOp::TokenRule`, slice 2) — a pure
/// function of the raw statement plus a precomputed source line, needing no
/// further compiler state, since a token/rule declaration has no computed
/// name/trait to compile.
fn build_token_decl_plan(stmt: &Stmt, source_line: Option<i64>) -> CompiledTokenDeclPlan {
    let (name, params, param_defs, body, multi) = match stmt {
        Stmt::TokenDecl {
            name,
            params,
            param_defs,
            body,
            multi,
            ..
        }
        | Stmt::RuleDecl {
            name,
            params,
            param_defs,
            body,
            multi,
        } => (name, params, param_defs, body, *multi),
        _ => panic!("build_token_decl_plan expects TokenDecl/RuleDecl"),
    };
    CompiledTokenDeclPlan {
        name: *name,
        params: params.clone(),
        param_defs: param_defs.clone(),
        multi,
        raw_body: body.clone(),
        source_line,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CompiledDeclPlanRef {
    Sub(u32),
    Class(u32),
    Role(u32),
    Proto(u32),
    /// A `proto token`/`proto rule` LTM marker (`Stmt::ProtoToken`), which
    /// carries only a name — no signature, body, or traits — so the name is
    /// stored inline rather than indexing a pool of its own.
    ProtoToken(Symbol),
    Token(u32),
}

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
    /// Typed declaration plans consumed through `RegisterDecl`. Unlike `stmt_pool`, every entry is
    /// known to be a sub declaration, so the VM does not inspect or execute a source statement.
    pub(crate) sub_decl_plans: Vec<CompiledSubDeclPlan>,
    pub(crate) class_decl_plans: Vec<CompiledClassDeclPlan>,
    pub(crate) role_decl_plans: Vec<CompiledRoleDeclPlan>,
    pub(crate) proto_decl_plans: Vec<CompiledProtoDeclPlan>,
    /// `token`/`rule` declaration plans (ADR-0019 F7), mirroring
    /// `proto_decl_plans`'s own shape.
    pub(crate) token_decl_plans: Vec<CompiledTokenDeclPlan>,
    /// The single declaration-registration operand pool. `RegisterDecl(i)` selects one tagged
    /// typed plan here; declaration-specific metadata stays out of the hot opcode enum.
    pub(crate) decl_plans: Vec<CompiledDeclPlanRef>,
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
    pub(crate) state_locals: Vec<(usize, Symbol)>,
    /// Maps local slot indices to qualified package names for `our` variables.
    /// Used by BlockScope restoration to sync local slots from their global values.
    pub(crate) our_locals: Vec<(usize, String)>,
    /// Names this chunk *binds as parameters* through a by-name store rather than
    /// a local slot — today the multi-parameter `for` head (`for %h.kv -> $k, $v`),
    /// whose binds the compiler desugars to plain assignments at the top of the
    /// loop body (`build_for_bind_stmts`).
    ///
    /// Such a name is declared by the loop signature, but nothing in the emitted
    /// `SetGlobal` says so, which made `use strict` reject the bind itself as a
    /// write to an undeclared variable. Recorded here (compile-time data, read
    /// only on the cold strict-check path) instead of spending an extra opcode
    /// per parameter per iteration to carry the fact at runtime.
    pub(crate) param_bind_names: Vec<String>,
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
    /// Compiled functions this closure/block body directly declares as nested
    /// `sub`s, keyed exactly as they were installed into the enclosing compile
    /// pass's functions table (post name-collision remap). `None` when the
    /// body declares no nested sub. Mirrors
    /// [`CompiledFunction::compiled_fns`] for the same reason: a `SubData`
    /// built from this code and invoked as a detached `Sub` VALUE from a
    /// foreign `CompiledFns` context cannot resolve a nested `RegisterSub`'s
    /// `compiled_routine_keys` from the caller's table alone (ADR-0019
    /// C6e-3c; see
    /// `todo/deep/c6e-legacy-body-drop-blocked-by-gate-rejected-shapes.md`).
    pub(crate) compiled_fns: Option<Arc<CompiledFns>>,
    /// Own local slots that reach an atomic-op builtin (`⚛$x`, `$x ⚛= v`,
    /// `cas($x, …)`) as the target VARIABLE. These builtins are compiled to a
    /// `__mutsu_*_var(name_str, …)` call and resolve the target by NAME from env
    /// (`atomic_current_value` falls back to `env.get(name)` for a non-`atomicint`
    /// scalar). Under the (B) per-store env-write a plain `my Int $x = 1` skips
    /// its env mirror, so the builtin would read the decl-seed placeholder.
    /// Consumed by the `compute_needs_env_sync` fold, which marks these slots
    /// env-synced so their mirror stays live for the by-name builtin.
    pub(crate) atomic_env_sync_locals: Vec<u32>,
    /// Every variable NAME that reaches an atomic-op builtin as the target,
    /// whether or not this code declares it — the free-variable analysis's view
    /// of `atomic_env_sync_locals`.
    ///
    /// `⚛$x` / `$x⚛++` / `cas($x, …)` compile to a `__mutsu_*_var("x", …)`
    /// CALL, so the op scan in `compute_free_vars` sees no name-write op and the
    /// target never counted as mutated. A closure that only ever bumps a
    /// captured `atomicint` therefore looked read-only, so `box_captured_lexicals`
    /// gave it no shared cell and the counter fell back to the name-keyed atomic
    /// lane — where an unrelated same-named lexical resets it. Folding these
    /// names into `free_var_writes` / `self_mutated` is what earns the binding a
    /// cell. Pin: `t/atomic-scalar-follows-its-binding.t`.
    pub(crate) atomic_target_syms: rustc_hash::FxHashSet<Symbol>,
    /// Every variable NAME that reaches an rw-arg-sink builtin (`cas` and
    /// siblings) as its target, at ANY nesting depth — a dedicated side
    /// channel into `needs_env_sync`, kept separate from `free`/`free_writes`
    /// on purpose.
    ///
    /// `cas $x, -> $v { ... }` passes `$x`'s name to the callee as a string
    /// constant, so the op scan never sees a read OR a write of `$x` — it is
    /// invisible to `free_var_syms`/`atomic_target_syms` alike. If a nested
    /// `start`/closure body RMWs a captured scalar this way, the outer
    /// frame's own per-store write to that scalar (after the closure was
    /// spawned) can wrongly skip its env/cross-thread-name-lane mirror,
    /// because nothing marked the slot `needs_env_sync`. Folding the name
    /// into `free` instead (as `atomic_target_syms` does for the write-path)
    /// would also change `block_captured_scalars`'s capture/cell-promotion
    /// classification for it — `cas` is deliberately kept off the
    /// cell-promoting lane (commit 85a43994e) and relies on its own
    /// name-keyed cross-thread reconciliation, so this field only affects
    /// `needs_env_sync`, never capture/ownership decisions. Bubbled
    /// transitively through nested closures in `compute_free_vars` up to
    /// (not past) the owning frame. Pin: `t/start-block-inline-arg-locals-clobber.t`.
    pub(crate) rw_arg_env_sync_syms: rustc_hash::FxHashSet<Symbol>,
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
    /// Whether the body references the topic `$_` (its constant pool contains
    /// the name `"_"`, emitted by any read/write of `$_`). A routine gets a
    /// fresh `$_` (Any), so a positional-light call must shadow the caller's
    /// topic with Any before running such a body — but only when it is actually
    /// read, so a topic-free hot loop (`fib`) skips the shadowing write and
    /// keeps the frame-reuse fast path. Computed once in `compute_needs_env_sync`.
    pub(crate) reads_topic: bool,
    /// Source line number (1-based) where this closure/block was defined.
    pub(crate) source_line: Option<i64>,
    /// Whether this compiled code represents a pointy block (`-> { }` / `<-> { }`).
    /// Pointy blocks are NOT routine boundaries — `return` propagates through
    /// them to the enclosing routine, and `&?ROUTINE` sees the enclosing routine.
    pub(crate) is_pointy_block: bool,
    /// Whether this is a single-`$`-sigiled-parameter pointy block (`-> $v {
    /// }`, compiled via `Expr::Lambda`) whose one parameter must be marked
    /// readonly (`ReadonlyKind::Alias`) at the call site. Set only for the
    /// plain, trait-less, non-sigilless, non-WhateverCode shape — a traited
    /// (`is rw`/`is copy`) or multi-param pointy block instead carries a real
    /// `ParamDef` and is marked by the ordinary signature-binding path
    /// (`binding_signature.rs`). Consumed by `call_compiled_closure_with_topic`,
    /// NOT by an injected body prologue statement — see the comment on
    /// `Compiler::compile_expr_lambda`'s `pointy_alias_param` for why a
    /// prologue statement is unsafe here (it also runs inside several
    /// `push_call_frame`-bypassing "fast native loop" paths, which would leak
    /// the mark permanently).
    pub(crate) pointy_alias_param: bool,
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
    /// Per-consumer lexical-slot synchronization sets (ADR-0018). Their union
    /// contributes to `needs_env_sync` without widening unrelated slots.
    pub(crate) env_consumer_slots: EnvConsumerSlots,
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
    /// Dynamic variables `my`-REdeclared in THIS code's body (`my $*x = ...`,
    /// env-keyed `*x`). Kept separate from `my_declared_sym`, whose consumers
    /// (e.g. ADR-0024 free-var slot binding in `vm_register_sub_ops.rs`)
    /// document that it holds only plain `my` lexicals. A fresh `my $*x`
    /// redeclaration is scoped to its block/closure invocation exactly like a
    /// plain `my`, so the map/grep inline-loop save/restore
    /// (`push_block_declared_keys`) and the closure-exit caller-writeback scan
    /// must treat it as body-private — while a plain `$*x = ...` write-through
    /// to an existing outer dynamic (never in this set) propagates out.
    /// Unlike `my_declared_sym` there is no "also a free var" exemption:
    /// every dynamic READ compiles to a by-name `GetGlobal` (no local slot),
    /// so a declared-and-read dynamic always looks like a free var, and the
    /// exemption would swallow the set whole.
    pub(crate) dynamic_declared_sym: rustc_hash::FxHashSet<Symbol>,
    /// The subset of `my_declared_sym` bound by a `my enum` — its type name and
    /// every variant name. Unlike a `my` variable these get no local slot, so
    /// every bareword read of one looks like a free variable to
    /// `compute_free_vars`; that free-var status would then exempt them from the
    /// very writeback filter `my_declared_sym` exists for. They are subtracted
    /// from `free_var_syms` instead, which is also what keeps them resolving to
    /// the block's own binding inside a `whenever` callback.
    pub(crate) my_declared_enum_sym: rustc_hash::FxHashSet<Symbol>,
    /// Names bound by a single, plain-scalar `for`-loop parameter (`for @a ->
    /// $i {...}`) declared anywhere in this compiled code. Like
    /// `my_declared_enum_sym`, these get no local slot when the name has none
    /// already (the loop's per-iteration binding writes env-only inside the
    /// `ForLoop` opcode exec, not a compiled name-write op), so a pure body
    /// read of the param looks like a free variable to `compute_free_vars`
    /// and would otherwise be rewritten to `GetUpvalue` -- resolving against
    /// whatever same-named OUTER lexical this code happened to capture,
    /// bypassing the loop's own binding entirely. Subtracted from
    /// `free_var_syms` instead. See
    /// `todo/tickets/closure-for-loop-param-hijacked-by-same-named-captured-outer.md`.
    pub(crate) for_loop_param_syms: rustc_hash::FxHashSet<Symbol>,
    /// Names this code declares in EXPRESSION position (`(my $p := ...)`,
    /// `(my $x = 1)`, compiled as `Expr::DoStmt(VarDecl)`).
    ///
    /// Such a declaration is env-only — it gets no local slot — so its store op
    /// looks exactly like a write to an enclosing same-named lexical. That is
    /// wrong on the axis that matters here: the name is this code's OWN binding,
    /// so it must not make an enclosing scope's same-named local "captured and
    /// mutated" and earn it a shared `ContainerRef` cell. It did — an unrelated
    /// later `my Pair $p` in the enclosing scope then found the cell instead of
    /// its own fresh binding (roast S02-types/pair.t #181). The enclosing scope's
    /// `captured_mutated` / `needs_cell` loop skips these names.
    ///
    /// ONLY that axis is corrected. The name stays a free variable and the store
    /// still writes through to the enclosing binding, which is both the
    /// pre-existing scope leak (`raku` keeps the outer binding, mutsu overwrites
    /// it — see `todo/tickets/expression-position-my-has-no-scope.md`) and what
    /// roast S02-types/whatever.t #45 asserts, so it must not be "fixed" here.
    pub(crate) expr_declared_syms: rustc_hash::FxHashSet<Symbol>,
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
    /// Full free-variable set (reads AND writes) of each directly-nested
    /// *registered routine*'s finalized `CompiledCode`
    /// (`CompiledFunction::code.free_var_syms`) — one entry per nested
    /// `sub`/`multi sub` declared in this scope (pushed by
    /// `Compiler::compile_sub_body_with_deprecation`) and one per method body
    /// of a `class`/`role`/`grammar` declared in this scope (pushed by
    /// `Compiler::compile_method_body`). Unlike `named_sub_captures` (writes
    /// only, drives cell-boxing), this feeds `compute_free_vars`'s ordinary
    /// `free` set the same way a nested anonymous closure's `free_var_syms`
    /// already does (see the `closure_compiled_codes` fold below).
    ///
    /// Both producers share one property that makes this channel necessary:
    /// the routine is installed into a registry (the sub table, or the type's
    /// method table) by a `RegisterDecl` op and has NO runtime
    /// closure-creation op, so it never lands in `closure_compiled_codes` and
    /// the enclosing scan cannot otherwise see which outer lexicals its body
    /// references. Without this fold, a variable referenced ONLY from inside
    /// such a body is silently missing from the closure env this code
    /// snapshots when treated as a Callable value
    /// (`MakeBlockClosure`/`MakeAnonSub`), and the body reads `Nil`/`Any` at
    /// call time — even though the ordinary "own local referenced by a nested
    /// routine" case (handled by `compute_needs_env_sync`'s
    /// `defines_lazy_body` env-sync gate) works fine. See
    /// `news/2026-08/nested-named-sub-free-var-capture.md` and
    /// `news/2026-08/class-method-in-block-free-var-capture.md`.
    pub(crate) nested_routine_free_reads: Vec<Vec<Symbol>>,
    /// Own locals that a directly-nested named sub WRITES (computed from
    /// `named_sub_captures`). The VM boxes these into a shared `ContainerRef` cell
    /// at their declaration site (`box_decl_local_cell`). Distinct from
    /// `needs_cell_locals` (closure-driven) — see `named_sub_captures`.
    pub(crate) needs_cell_named_sub: Vec<Symbol>,
    /// Exact owner slots whose containers are captured by `WrapVarRef` in ANY
    /// nested compiled code (a directly nested named sub, a pointy block, an
    /// anon `sub {}`, a bare block, a class/role method, `start`/`supply` —
    /// ADR-0032 D2). Kept slot-addressed so a same-named lexical in another
    /// block is not boxed at its declaration site. Named `_ref_capture_` (not
    /// `_named_sub_`) because this is populated by [`Compiler::emit_wrap_var_ref`]
    /// (D1) at every WrapVarRef emit site and bubbled to the owning frame by
    /// [`Compiler::bubble_container_ref_capture_syms`] (D2), not by a
    /// named-sub-specific peephole.
    pub(crate) needs_cell_ref_capture_slots: Vec<u32>,
    /// Free variables whose raw container is consumed by `WrapVarRef`. Runtime
    /// reference wrapping may read a captured env cell only for this explicit
    /// set; ordinary same-named env cells must not override a shadow value.
    /// Populated at emission time by [`Compiler::emit_wrap_var_ref`] (ADR-0032
    /// D1) whenever the name is not a local of the emitting frame, and
    /// bubbled transitively across nested-code boundaries by
    /// [`Compiler::bubble_container_ref_capture_syms`] until it reaches the
    /// frame that owns the name (see `needs_cell_ref_capture_slots`).
    pub(crate) container_ref_capture_syms: Vec<Symbol>,
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
    /// Own locals interpolated into a regex constant of this same frame
    /// (`rx/ $word /`) AND mutated after the regex is constructed. A regex
    /// literal loaded via `OpCode::LoadRegexClosure` closes over its defining
    /// scope's *bindings*, not a value snapshot (`my $x = 1; my $re = rx/ abc
    /// <?{ $x == 2 }> /; $x = 2; "abc" ~~ $re` must match — see
    /// `todo/tickets/stored-regex-loses-its-defining-scope-lexicals.md`). Only a
    /// name in this set is boxed into a shared `ContainerRef` cell at capture
    /// time (`capture_regex_closure`); an unmutated capture stays a cheap
    /// by-value snapshot, since nothing can ever change it. Deliberately kept
    /// separate from `needs_cell_locals` (closure-escape driven) and
    /// `needs_cell_named_sub` (named-sub-write driven) — over-boxing an
    /// unrelated same-named local through the wrong signal is the historical
    /// bug class in this area (see the `needs_cell_locals` doc comment above).
    pub(crate) needs_cell_regex: Vec<Symbol>,
    /// Frame lexicals that a `class`/`role` body's methods WRITE. A method is
    /// installed by `RegisterClass`/`RegisterRole` and is invoked with no
    /// closure-creation op, so the capture analysis behind
    /// `box_captured_lexicals` never sees these writes. Such a name therefore
    /// keeps the name-keyed `shared_vars` lane that `clone_for_thread_for_block`
    /// otherwise retires for a block's own captures (PLAN.md §6): it is the only
    /// mechanism that carries a `submethod DESTROY { $a++ }` write on a worker
    /// back to the parent. Populated by `record_type_body_captures`.
    pub(crate) type_body_written_lexicals: Vec<Symbol>,
    /// True when this closure was compiled in a position that hands it to a
    /// THREAD (`start { ... }`, `Thread.start`, `Promise.start`). A plain
    /// escaping position (stored/returned) is not enough: this gates boxing a
    /// type-constrained scalar into a shared cell, which is required for the
    /// parent to observe a worker's write (the name-keyed `shared_vars` lane
    /// no longer carries a spawned block's own captured scalars, PLAN.md §6)
    /// but must NOT happen for a same-frame closure, because `cas` resolves its
    /// target BY NAME and is not cell-aware (roast S17-lowlevel/cas.t).
    pub(crate) thread_escaping: bool,
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
    /// True if this code observes its caller frame: a `callframe`/`callframes`
    /// call, or a `CALLER::` pseudo-package read/write op. Set during `emit()`.
    /// Such a body must be invoked through a frame-pushing call path
    /// (`push_caller_env`), so the fast/light frameless paths exclude it —
    /// otherwise `callframe(1)`/`CALLER::` resolve against the *grand*-caller.
    /// (Historically this was masked by an unconditional `fn_resolve_gen` bump
    /// after every interpreter-native call, which kept such routines
    /// permanently out of the name-keyed call caches by accident.)
    pub(crate) uses_callframe: bool,
    /// True if this code directly calls `callsame`/`nextsame`/`callwith`/
    /// `nextwith`. Set during `emit()`. The compiled method fast paths
    /// (`call_compiled_method`/`call_compiled_method_fast`) push a
    /// `SamewithContext` only when this is set, so a plain method call that
    /// never defers pays no per-call String/Vec clone — see
    /// `todo/tickets/callsame-to-native-mu-methods-nil.md` for why an
    /// unconditional push was rejected on hot-path cost grounds.
    pub(crate) uses_dispatcher: bool,
    /// True if this code is the body of a `supply { … }` block — the lambda
    /// `Supply.on-demand` is handed, recognised by its generated emitter
    /// parameter (`__mutsu_supply_emitter_N`, see `supply_method_call`).
    ///
    /// Such a body is a scope of its own that the caller never re-enters, so the
    /// names it declares with `my` are private to it: they must not be written
    /// back to the caller on exit, and the `whenever` closures created inside it
    /// own them (see `exec_whenever_scope_op`). A `react { … }` block is NOT one
    /// of these — it compiles inline into the enclosing frame, so its `my`
    /// declarations genuinely ARE that frame's lexicals and stay shared. Set
    /// during `compute_needs_env_sync`.
    pub(crate) is_supply_block_body: bool,
    /// ADR-0037 Slice 4: for an `EVAL ..., context => $ctx` unit whose `$ctx`
    /// named a *live* routine (`EvalContextRoutineState::Live(Some(id))`),
    /// the resolved registration clone id (`Interpreter::registration_clone_id`)
    /// of that routine. Set once, post-compile, by `compile_block_value_opts`
    /// -- not an `OpCode::Return` payload, to keep `OpCode::Return` itself
    /// payload-free (the `opcode_size_guard` test pins `size_of::<OpCode>()`).
    /// `OpCode::Return`'s exec arm reads this straight off `code` and stamps
    /// it onto the raised `RuntimeError::return_signal` as
    /// `return_target_callable_id`, so the signal unwinds past any
    /// intervening routine boundary to the one this id names (mirroring how
    /// a bare block's `return` already inherits its captured
    /// `__mutsu_callable_id`, see `vm_closure_dispatch.rs`). `None` for every
    /// ordinary (non-EVAL, or EVAL-without-a-live-context-target) chunk.
    pub(crate) eval_context_target_callable_id: Option<u64>,
    /// The generated emitter parameter of a `supply { … }` body
    /// (`__mutsu_supply_emitter_N`), interned. `Some` exactly when
    /// `is_supply_block_body`.
    ///
    /// The name is unique per *parse site* but shared by every runtime *instance*
    /// of that site, so the `whenever` closures a body creates must own it — see
    /// `exec_whenever_scope_op`. It has no compiled local slot (a `supply` body's
    /// `locals` is empty), so it cannot be recovered from `locals_sym`.
    pub(crate) supply_emitter_sym: Option<Symbol>,
    /// Lexicals an *enclosing* `whenever` callback already owns, handed down so a
    /// `whenever` nested inside that callback's body owns them too.
    ///
    /// A `whenever` registered at the top level of a `supply { … }` body gets its
    /// owned set computed from the body's own `CompiledCode`
    /// (`exec_whenever_scope_op`). A `whenever` registered from *inside* another
    /// `whenever`'s body cannot: the chunk it runs in is a fresh compile of that
    /// callback's AST, which knows nothing of the supply body that created it. So
    /// the callback's `SubData::authoritative_captures` — exactly the set the
    /// supply body vouched for — rides along here (see
    /// `Interpreter::pending_whenever_inherited_owned`). Without it the nested
    /// callback's `emit` re-resolved the shared emitter name against whichever
    /// sibling instance of the same parse site happened to be dispatching it
    /// (Cro's `whenever $handler.invoke(…) { emit $response }` ping-ponged with
    /// the delegated route set forever).
    pub(crate) inherited_owned_lexicals: Vec<Symbol>,
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
    /// Lazily-built `Symbol` sets over [`free_var_syms`](Self::free_var_syms)
    /// and [`locals_sym`](Self::locals_sym), for `capture_closure_env`'s
    /// membership tests. Both are pure functions of the chunk, but the capture
    /// used to rebuild them (two `HashSet` allocations plus their fills) on
    /// EVERY closure creation — see `capture_free_var_set` / `capture_local_set`.
    pub(crate) free_var_sym_set: std::sync::OnceLock<rustc_hash::FxHashSet<Symbol>>,
    pub(crate) local_sym_set: std::sync::OnceLock<rustc_hash::FxHashSet<Symbol>>,
    /// Lazily-built shared body per `stmt_pool` slot (see `closure_body_arc`).
    /// A `SubData`'s body used to be deep-cloned out of the pool on every
    /// closure creation; the `Arc` is built once per slot instead.
    pub(crate) stmt_pool_bodies: std::sync::OnceLock<StmtPoolBodies>,
    /// Per-chunk JIT hotness counter and compiled-entry cache (ADR-0004 J1).
    pub(crate) jit: JitCodeState,
}

/// The per-local-slot attribute-key table of a chunk (see
/// [`CompiledCode::local_attr_keys`]): one entry per slot, `Some((bare attribute
/// `Symbol`, is_private))` for an attribute twigil and `None` otherwise.
pub(crate) type LocalAttrKeys = Box<[Option<(Symbol, bool)>]>;

/// The per-`stmt_pool`-slot shared closure body of a chunk (see
/// [`CompiledCode::closure_body_arc`]): one lazily-filled slot per pool entry,
/// each holding the `Arc` every closure created from that entry shares.
pub(crate) type StmtPoolBodies = Box<[std::sync::OnceLock<std::sync::Arc<Vec<Stmt>>>]>;

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
    /// True when this body declares a routine (`sub`/`subset`) directly in its
    /// own scope (not inside a nested `BlockScope`, which restores the
    /// registry itself) — such a routine is lexical to this body and must be
    /// unregistered when the body returns unless it escapes via the return
    /// value. Shared by `CompiledFunction::detect_inner_subs` (the sub/closure
    /// call paths) and method dispatch (`vm_method_dispatch.rs`), which has no
    /// `CompiledFunction` wrapper of its own to cache this on.
    pub(crate) fn declares_inner_routines(&self) -> bool {
        self.ops.iter().any(|op| match op {
            OpCode::RegisterDecl(idx) => {
                matches!(
                    self.decl_plans.get(*idx as usize),
                    Some(CompiledDeclPlanRef::Sub(_))
                )
            }
            OpCode::RegisterSubset(..) => true,
            _ => false,
        })
    }

    pub(crate) fn remap_sub_decl_compiled_routine_keys(
        &mut self,
        remap: &rustc_hash::FxHashMap<Symbol, Symbol>,
    ) {
        for plan in &mut self.sub_decl_plans {
            for key in &mut plan.compiled_routine_keys {
                if let Some(remapped) = remap.get(key) {
                    *key = *remapped;
                }
            }
        }
        for plan in &mut self.proto_decl_plans {
            if let Some(key) = &mut plan.compiled_routine_key
                && let Some(remapped) = remap.get(key)
            {
                *key = *remapped;
            }
        }
        // ADR-0019 D3-8a: a class/role method body's main-pass-compiled key
        // (`CompiledMethodDecl::compiled_routine_key`) lives one level deeper
        // than a sub's, inside each declaration plan's `method_decls`. Nested
        // compilation-unit import must keep these in step with the sub/proto
        // keys above, or a class/role declared inside an `EVAL`'d or
        // otherwise-imported unit would carry a stale key pointing at nothing
        // in the merged `CompiledFns` table.
        for plan in &mut self.class_decl_plans {
            for method in &mut plan.method_decls {
                if let Some(key) = &mut method.compiled_routine_key
                    && let Some(remapped) = remap.get(key)
                {
                    *key = *remapped;
                }
            }
        }
        for plan in &mut self.role_decl_plans {
            for method in &mut plan.method_decls {
                if let Some(key) = &mut method.compiled_routine_key
                    && let Some(remapped) = remap.get(key)
                {
                    *key = *remapped;
                }
            }
        }
        for nested in &mut self.closure_compiled_codes {
            Arc::make_mut(nested).remap_sub_decl_compiled_routine_keys(remap);
        }
    }

    fn mark_name_access_slots(&self, start: usize, end: usize, slots: &mut [bool]) {
        let end = end.min(self.ops.len());
        for op in &self.ops[start.min(end)..end] {
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
                | OpCode::TopicDotAssign(idx)
                | OpCode::IndexAssignExprNested { name_idx: idx, .. }
                | OpCode::IndexAssignDeepNested { name_idx: idx, .. }
                | OpCode::MultiDimIndexAssign { name_idx: idx, .. } => Some(*idx),
                OpCode::AtomicCompoundVar { name_idx, .. } => Some(*name_idx),
                _ => None,
            };
            let Some(name_idx) = name_idx else { continue };
            let Some(ValueView::Str(name)) = self.constants.get(name_idx as usize).map(Value::view)
            else {
                continue;
            };
            for (slot, local) in self.locals.iter().enumerate() {
                if local == name.as_str() {
                    slots[slot] = true;
                }
            }
        }
    }

    fn mark_local_access_slots(&self, start: usize, end: usize, slots: &mut [bool]) {
        let end = end.min(self.ops.len());
        for op in &self.ops[start.min(end)..end] {
            let slot = match op {
                OpCode::GetLocal(slot)
                | OpCode::GetLocalRaw(slot)
                | OpCode::SetLocal(slot)
                | OpCode::AssignExprLocal(slot)
                | OpCode::StateVarInit(slot, _) => Some(*slot),
                OpCode::GetLocalMetaAssign { slot, .. } | OpCode::SetLocalDecl { slot, .. } => {
                    Some(*slot)
                }
                _ => None,
            };
            if let Some(needed) = slot.and_then(|slot| slots.get_mut(slot as usize)) {
                *needed = true;
            }
        }
    }

    fn mark_same_named_slot_peers(&self, slots: &mut [bool]) {
        let selected_names: std::collections::HashSet<&str> = slots
            .iter()
            .enumerate()
            .filter(|(_, selected)| **selected)
            .map(|(slot, _)| self.locals[slot].as_str())
            .collect();
        for (slot, name) in self.locals.iter().enumerate() {
            if selected_names.contains(name.as_str()) {
                slots[slot] = true;
            }
        }
    }

    fn mark_closure_parent_slots(&self, cc_idx: u32, slots: &mut [bool]) {
        let Some(cc) = self.closure_compiled_codes.get(cc_idx as usize) else {
            return;
        };
        for (idx, sym) in cc.free_var_syms.iter().enumerate() {
            // A self-referential stored body can be analyzed while its binding
            // initializer is still being compiled (`my @a := gather { ... @a
            // ... }`), leaving the baked parent slot absent even though the
            // declaration slot is already present in this frame. Resolve that
            // exact same-name slot here; it is still a per-consumer slot, not a
            // frame-wide fallback.
            let slot = cc
                .free_var_parent_slots
                .get(idx)
                .copied()
                .flatten()
                .map(|slot| slot as usize)
                .or_else(|| sym.with_str(|name| self.locals.iter().rposition(|n| n == name)));
            if let Some(needed) = slot.and_then(|slot| slots.get_mut(slot)) {
                *needed = true;
            }
        }
    }

    pub(crate) fn new() -> Self {
        Self {
            ops: Vec::new(),
            op_lines: Vec::new(),
            emit_line: 0,
            constants: Vec::new(),
            const_index: rustc_hash::FxHashMap::default(),
            stmt_pool: Vec::new(),
            sub_decl_plans: Vec::new(),
            class_decl_plans: Vec::new(),
            role_decl_plans: Vec::new(),
            proto_decl_plans: Vec::new(),
            token_decl_plans: Vec::new(),
            decl_plans: Vec::new(),
            locals: Vec::new(),
            locals_sym: Vec::new(),
            locals_alias_sym: Vec::new(),
            locals_readonly_sym: Vec::new(),
            locals_deleted_index_sym: Vec::new(),
            locals_bound_slice_sym: Vec::new(),
            plain_locals: Vec::new(),
            state_locals: Vec::new(),
            our_locals: Vec::new(),
            param_bind_names: Vec::new(),
            scalar_bind_locals: Vec::new(),
            param_local_slots: Vec::new(),
            lex_scopes: Vec::new(),
            closure_compiled_codes: Vec::new(),
            compiled_fns: None,
            atomic_env_sync_locals: Vec::new(),
            atomic_target_syms: rustc_hash::FxHashSet::default(),
            rw_arg_env_sync_syms: rustc_hash::FxHashSet::default(),
            named_arg_specs: Vec::new(),
            closure_escapes: Vec::new(),
            is_routine: false,
            reads_topic: false,
            has_once: false,
            uses_callframe: false,
            uses_dispatcher: false,
            source_line: None,
            is_pointy_block: false,
            pointy_alias_param: false,
            has_env_writes: false,
            may_capture_outer_vars: false,
            needs_env_sync: Vec::new(),
            env_consumer_slots: EnvConsumerSlots::default(),
            dup_named_locals: Vec::new(),
            is_supply_block_body: false,
            eval_context_target_callable_id: None,
            supply_emitter_sym: None,
            inherited_owned_lexicals: Vec::new(),
            my_declared_sym: rustc_hash::FxHashSet::default(),
            dynamic_declared_sym: rustc_hash::FxHashSet::default(),
            my_declared_enum_sym: rustc_hash::FxHashSet::default(),
            for_loop_param_syms: rustc_hash::FxHashSet::default(),
            expr_declared_syms: rustc_hash::FxHashSet::default(),
            free_var_syms: Vec::new(),
            free_var_parent_slots: Vec::new(),
            upvalue_parent_slots: Vec::new(),
            outer_ref_names: Vec::new(),
            free_var_writes: Vec::new(),
            free_var_container_writes: Vec::new(),
            named_sub_captures: Vec::new(),
            nested_routine_free_reads: Vec::new(),
            needs_cell_named_sub: Vec::new(),
            needs_cell_ref_capture_slots: Vec::new(),
            container_ref_capture_syms: Vec::new(),
            needs_cell_named_sub_free: Vec::new(),
            escaping_our_sub_captures: Vec::new(),
            needs_cell_escaping_our_sub: Vec::new(),
            needs_cell_escaping_our_sub_free: Vec::new(),
            captured_mutated_locals: Vec::new(),
            needs_cell_locals: Vec::new(),
            needs_cell_regex: Vec::new(),
            type_body_written_lexicals: Vec::new(),
            thread_escaping: false,
            authoritative_free_vars: Vec::new(),
            self_capture_decl_locals: Vec::new(),
            outer_code_var_names: std::collections::HashSet::new(),
            needs_cell_free_vars: Vec::new(),
            has_calls: false,
            upvalue_syms: Vec::new(),
            env_only_decls: Vec::new(),
            const_syms: std::sync::OnceLock::new(),
            local_attr_keys: std::sync::OnceLock::new(),
            free_var_sym_set: std::sync::OnceLock::new(),
            local_sym_set: std::sync::OnceLock::new(),
            stmt_pool_bodies: std::sync::OnceLock::new(),
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

    /// The shared body of the closure declaration at `stmt_pool[idx]`, built
    /// once per slot. `Stmt::SubDecl`/`Stmt::Block` bodies live in the pool and
    /// are never mutated through the `SubData` that carries them, so every
    /// closure created from this slot can share one `Arc` instead of deep-cloning
    /// the `Vec<Stmt>`. That clone was O(body size) on a path that runs once per
    /// `.map({...})` CALL: ~5.9us per creation for a 29-statement block.
    ///
    /// `extract` yields the body for the two closure-declaring statement kinds;
    /// any other pool entry (never asked for here) shares an empty body.
    pub(crate) fn closure_body_arc(&self, idx: usize) -> std::sync::Arc<Vec<Stmt>> {
        let extract = |i: usize| -> std::sync::Arc<Vec<Stmt>> {
            match self.stmt_pool.get(i) {
                Some(Stmt::SubDecl { body, .. }) => std::sync::Arc::new(body.clone()),
                Some(Stmt::Block(body)) => std::sync::Arc::new(body.clone()),
                _ => std::sync::Arc::new(Vec::new()),
            }
        };
        let slots = self.stmt_pool_bodies.get_or_init(|| {
            (0..self.stmt_pool.len())
                .map(|_| std::sync::OnceLock::new())
                .collect()
        });
        match slots.get(idx) {
            Some(slot) => slot.get_or_init(|| extract(idx)).clone(),
            // The pool grew after the side table was sized (a chunk still being
            // built): fall back to an uncached clone rather than mis-indexing.
            None => extract(idx),
        }
    }

    /// This chunk's free variables as a `Symbol` set, built once. The closure
    /// capture (`capture_closure_env`) tests membership per env key, and used to
    /// `collect()` this set afresh on every closure creation.
    pub(crate) fn capture_free_var_set(&self) -> &rustc_hash::FxHashSet<Symbol> {
        self.free_var_sym_set
            .get_or_init(|| self.free_var_syms.iter().copied().collect())
    }

    /// This chunk's own local/parameter names as a `Symbol` set, built once.
    /// The capture drops a same-named enclosing binding for each of these (a
    /// WhateverCode's `_` param must not inherit the creating frame's topic), and
    /// used to `collect()` a `HashSet<&str>` over `locals` per closure creation.
    /// `locals_sym` is the interned twin of `locals`, so `Symbol` membership is
    /// exactly string membership — without hashing the string.
    pub(crate) fn capture_local_set(&self) -> &rustc_hash::FxHashSet<Symbol> {
        self.local_sym_set.get_or_init(|| {
            // A hand-built chunk that never ran `compute_locals_sym` has an
            // empty `locals_sym` (see `local_sym`), so intern from `locals` in
            // that case rather than silently returning an empty set.
            if self.locals_sym.len() == self.locals.len() {
                self.locals_sym.iter().copied().collect()
            } else {
                self.locals.iter().map(|s| Symbol::intern(s)).collect()
            }
        })
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
        // Does the body touch the topic `$_`? Any read/write of `$_` interns the
        // name `"_"` into the constant pool (GetGlobal/SetGlobal name), so a
        // pool scan is a sound (never-miss) over-approximation — a stray string
        // literal `"_"` only costs a harmless extra topic-shadow write on that
        // routine's calls. A topic-free routine (e.g. `fib`) has no `"_"`
        // constant, so its hot-loop calls skip the shadow write entirely.
        self.reads_topic = self
            .constants
            .iter()
            .any(|c| matches!(c.view(), crate::value::ValueView::Str(s) if s.as_str() == "_"));
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
        let n = self.locals.len();
        self.needs_env_sync = vec![false; n];
        self.env_consumer_slots = EnvConsumerSlots::default();
        let mut has_for_loop = false;
        let mut has_block_scope = false;
        let mut has_block_local_scope = false;
        let mut has_gather = false;
        let mut has_whenever = false;
        let mut has_package_scope = false;
        let mut for_loop_slots = vec![false; n];
        let mut block_scope_slots = vec![false; n];
        let mut block_local_scope_slots = vec![false; n];
        let mut gather_slots = vec![false; n];
        let mut whenever_slots = vec![false; n];
        let mut package_scope_slots = vec![false; n];
        for (op_idx, op) in self.ops.iter().enumerate() {
            match op {
                OpCode::ForLoop(spec) => {
                    has_for_loop = true;
                    for slot in [
                        spec.param_local,
                        spec.topic_local,
                        spec.source_container_local,
                    ]
                    .into_iter()
                    .flatten()
                    .chain(spec.source_var_locals.iter().flatten().copied())
                    .chain(spec.single_array_source_local)
                    {
                        if let Some(needed) = for_loop_slots.get_mut(slot as usize) {
                            *needed = true;
                        }
                    }
                    self.mark_name_access_slots(
                        op_idx + 1,
                        spec.body_end as usize,
                        &mut for_loop_slots,
                    );
                }
                OpCode::BlockScope { end, .. } => {
                    has_block_scope = true;
                    self.mark_name_access_slots(op_idx + 1, *end as usize, &mut block_scope_slots);
                    self.mark_local_access_slots(op_idx + 1, *end as usize, &mut block_scope_slots);
                }
                OpCode::BlockLocalScope { body_end, .. } => {
                    has_block_local_scope = true;
                    self.mark_name_access_slots(
                        op_idx + 1,
                        *body_end as usize,
                        &mut block_local_scope_slots,
                    );
                    self.mark_local_access_slots(
                        op_idx + 1,
                        *body_end as usize,
                        &mut block_local_scope_slots,
                    );
                }
                OpCode::MakeGather(_, Some(cc_idx)) => {
                    has_gather = true;
                    self.mark_closure_parent_slots(*cc_idx, &mut gather_slots);
                }
                OpCode::MakeGather(_, None) => has_gather = true,
                OpCode::WheneverScope {
                    analysis_cc_idx, ..
                } => {
                    has_whenever = true;
                    self.mark_closure_parent_slots(*analysis_cc_idx, &mut whenever_slots);
                }
                OpCode::PackageScope { body_end, .. } => {
                    has_package_scope = true;
                    self.mark_name_access_slots(
                        op_idx + 1,
                        *body_end as usize,
                        &mut package_scope_slots,
                    );
                    self.mark_local_access_slots(
                        op_idx + 1,
                        *body_end as usize,
                        &mut package_scope_slots,
                    );
                }
                _ => {}
            }
        }
        // A block declaration's entry prelude may clear the previously visible
        // same-named outer slot. Block exit restores that exact peer from env,
        // so every simultaneously live peer is a dependency of this consumer —
        // still a precise duplicate-name subset, never a frame-wide blanket.
        if has_block_scope {
            self.mark_same_named_slot_peers(&mut block_scope_slots);
        }
        if has_block_local_scope {
            self.mark_same_named_slot_peers(&mut block_local_scope_slots);
        }
        // `exec_package_scope_op` restores the SAME "every simultaneously live
        // peer" way `BlockScope`/`BlockLocalScope` do (its `restored_env`
        // reconciliation reads/writes bare env keys, not slot indices, so a
        // same-named peer slot is indistinguishable from the one actually
        // referenced in the body).
        if has_package_scope {
            self.mark_same_named_slot_peers(&mut package_scope_slots);
        }
        if has_for_loop {
            self.env_consumer_slots.for_loop = for_loop_slots;
        }
        if has_block_scope {
            self.env_consumer_slots.block_scope = block_scope_slots;
        }
        if has_block_local_scope {
            self.env_consumer_slots.block_local_scope = block_local_scope_slots;
        }
        if has_gather {
            self.env_consumer_slots.gather = gather_slots;
        }
        if has_whenever {
            self.env_consumer_slots.whenever = whenever_slots;
        }
        if has_package_scope {
            self.env_consumer_slots.package_scope = package_scope_slots;
        }
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
        // ForLoop's iteration-local restore still journals shadowed bindings by
        // name. Publish only the duplicate-name slots that can participate in
        // that journal; ordinary loop slots are already covered by the baked
        // source/body bitmap above.
        if has_for_loop {
            for (slot, is_duplicate) in self.dup_named_locals.iter().copied().enumerate() {
                if is_duplicate {
                    self.env_consumer_slots.for_loop[slot] = true;
                }
            }
        }
        if n == 0 {
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
                | OpCode::TopicDotAssign(idx)
                | OpCode::IndexAssignExprNested { name_idx: idx, .. }
                | OpCode::IndexAssignDeepNested { name_idx: idx, .. }
                | OpCode::MultiDimIndexAssign { name_idx: idx, .. } => Some(*idx),
                OpCode::AtomicCompoundVar { name_idx, .. } => Some(*name_idx),
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
        // mirror must be current. Under the (B) per-store env-write a plain
        // lexical's store skips that mirror, leaving the decl-seed `Any`, so the
        // consumer reads a stale value. Fold every nested-closure free var that is
        // one of this frame's own locals back into `needs_env_sync` so its store
        // keeps mirroring. It never touches a hot-arithmetic loop local (those are
        // not closure free variables).
        {
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
            // A local that reaches an rw-arg-sink builtin (`cas` et al.) inside
            // a nested closure at ANY depth — `rw_arg_env_sync_syms`, already
            // bubbled transitively up to the owning frame by
            // `compute_free_vars` — needs the same treatment: the builtin
            // resolves and reconciles it by NAME, so its env/cross-thread
            // mirror must stay live even though it is otherwise unread by
            // name in this frame.
            for sym in &self.rw_arg_env_sync_syms {
                if let Some(&slot) = sym.with_str(|s| locals_map.get(s))
                    && let Some(b) = self.needs_env_sync.get_mut(slot)
                {
                    *b = true;
                }
            }
            // A bare call whose callee name collides with one of this frame's own
            // declared lexicals (`my $e; e()` — `$e` is stored under the bare name
            // `e`) reaches the function-call fallback, which reads `env[e]` to decide
            // whether the call is a bound-generic-type-parameter coercion
            // (`T()` -> `Int(Any)`). Under the gate a plain `my $e` keeps only its
            // decl-seed `Any` in env (its initializing store's mirror is skipped), so
            // the fallback sees `Package(Any)` and resolves `e()` to `Any(Any)`
            // instead of dying with X::Undeclared (roast S32-trig/e.t). Fold every
            // such colliding local back into `needs_env_sync` so its env mirror stays
            // live and the fallback reads the real value. Gate-ON only, so the
            // default build is byte-identical / perf-neutral.
            for op in &self.ops {
                if let Some(idx) = Self::op_callee_name_const_idx(op)
                    && let Some(ValueView::Str(name)) =
                        self.constants.get(idx as usize).map(Value::view)
                    && let Some(&slot) = locals_map.get(name.as_str())
                {
                    self.needs_env_sync[slot] = true;
                }
            }
            for nested in &self.closure_compiled_codes {
                for sym in &nested.free_var_syms {
                    let slot = sym.with_str(|s| {
                        locals_map.get(s).copied().or_else(|| {
                            // A `@$x` / `%$x` deref of a scalar records its free var
                            // as `@x` / `%x` (the array/hash-context spelling), but the
                            // underlying lexical is the sigil-less scalar `x`. Fall back
                            // to the stripped name so the deref keeps `x`'s env mirror
                            // live — the closure's `GetArrayVar`/`GetHashVar` reads it
                            // by name (roast S32-list/skip.t: `throws-like { @$s }` must
                            // see the consumed Seq bound to `$s`).
                            s.strip_prefix(['@', '%', '&'])
                                .and_then(|bare| locals_map.get(bare).copied())
                        })
                    });
                    if let Some(slot) = slot {
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
                    OpCode::RegisterDecl(_)
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
        // ADR-0018: each env-by-name consumer now publishes exactly the slots
        // its analysis selected. Block restore retains lexical slot identity,
        // so the presence of a block no longer forces a frame-wide blanket.
        for slot in 0..n {
            self.needs_env_sync[slot] |= self
                .env_consumer_slots
                .for_loop
                .get(slot)
                .copied()
                .unwrap_or(false)
                || self
                    .env_consumer_slots
                    .block_scope
                    .get(slot)
                    .copied()
                    .unwrap_or(false)
                || self
                    .env_consumer_slots
                    .block_local_scope
                    .get(slot)
                    .copied()
                    .unwrap_or(false)
                || self
                    .env_consumer_slots
                    .gather
                    .get(slot)
                    .copied()
                    .unwrap_or(false)
                || self
                    .env_consumer_slots
                    .whenever
                    .get(slot)
                    .copied()
                    .unwrap_or(false)
                || self
                    .env_consumer_slots
                    .package_scope
                    .get(slot)
                    .copied()
                    .unwrap_or(false);
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
                | OpCode::GetCallerOuterVar { .. }
                | OpCode::GetPseudoStash(_)
                | OpCode::SymbolicDeref { .. }
                | OpCode::SymbolicDerefStore(_)
                | OpCode::IndirectCodeLookup(_) => true,
                // `EVAL`/`EVALFILE` are reflective regardless of which call
                // shape the call site compiled to: a statement-position call
                // (`EVAL q[...];`, whose value is discarded) reaches
                // `ExecCall`/`ExecCallPairs`, not just the tail/expression
                // forms `CallFunc`/`CallFuncNamed`. Missing the statement
                // forms here left the READ side of EVAL's caller-lexical
                // visibility working only when an EVAL happened to also
                // appear in tail position somewhere in the same compiled
                // chunk (see `todo/tickets/repl-routine-unimplemented.md` /
                // `news/2026-08/eval-read-side-caller-lexicals.md`): with no
                // tail-form EVAL anywhere, this flag never latched, so a
                // plain lexical's `SetLocal` never mirrored into `env` and a
                // later `EVAL 'say $x'` -- which resolves `$x` by name
                // against `env`, having no compile-time knowledge of the
                // caller's local slots -- read the stale placeholder instead
                // of the live value.
                OpCode::CallFunc { name_idx, .. }
                | OpCode::CallFuncNamed { name_idx, .. }
                | OpCode::ExecCall { name_idx, .. }
                | OpCode::ExecCallPairs { name_idx, .. } => {
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
    ///
    /// `GetUpvalue` is included so that `compute_free_vars` is IDEMPOTENT under
    /// upvalue promotion. `compute_upvalues` rewrites a read-only free scalar's
    /// `GetGlobal(name)` into `GetUpvalue { index, name_idx }`; a *second*
    /// `compute_free_vars` over the rewritten ops (which happens whenever
    /// `compute_needs_env_sync` is re-run on already-promoted code — e.g.
    /// `Compiler::compile_method_body`'s explicit call after
    /// `compile_routine_closure_body` already promoted) would otherwise find no
    /// name-bearing op for that variable at all and silently RESET
    /// `free_var_syms` to empty, losing the capture record while
    /// `upvalue_syms` still names it.
    fn op_name_const_idx(op: &OpCode) -> Option<u32> {
        match op {
            OpCode::GetUpvalue { name_idx: idx, .. }
            | OpCode::GetGlobal(idx)
            | OpCode::GetScalarContainer { name_idx: idx, .. }
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
            | OpCode::AtomicCompoundVar { name_idx: idx, .. }
            | OpCode::IndexAssignExprNamed { name_idx: idx, .. }
            | OpCode::IndexAssignExprNested { name_idx: idx, .. }
            | OpCode::IndexAssignDeepNested { name_idx: idx, .. }
            | OpCode::IndexElemAutoviv { name_idx: idx, .. } => Some(*idx),
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

    fn op_name_write_const_idx(&self, op: &OpCode) -> Option<u32> {
        match op {
            OpCode::GetScalarContainer { name_idx: idx, .. }
            | OpCode::SetGlobal(idx)
            | OpCode::SetGlobalRaw(idx)
            | OpCode::PostIncrement(idx, _)
            | OpCode::PostDecrement(idx, _)
            | OpCode::PreIncrement(idx, _)
            | OpCode::PreDecrement(idx, _)
            | OpCode::AssignExpr(idx)
            | OpCode::TopicDotAssign(idx)
            | OpCode::AtomicCompoundVar { name_idx: idx, .. } => Some(*idx),
            // `$obj<key> = value` may copy-on-write a user-class instance,
            // so its scalar container is a real write target for closure
            // capture analysis. `@`/`%` element stores remain in the separate
            // in-place-container lane below.
            OpCode::IndexAssignExprNamed { name_idx: idx, .. }
            | OpCode::IndexAssignExprNested { name_idx: idx, .. }
            | OpCode::IndexAssignDeepNested { name_idx: idx, .. }
            | OpCode::IndexElemAutoviv { name_idx: idx, .. }
                if matches!(self.constants.get(*idx as usize).map(Value::view), Some(ValueView::Str(name)) if name.starts_with('$')) =>
            {
                Some(*idx)
            }
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
            OpCode::PostIncrementIndex(name_idx, _)
            | OpCode::PostDecrementIndex(name_idx, _)
            | OpCode::PreIncrementIndex(name_idx, _)
            | OpCode::PreDecrementIndex(name_idx, _) => Some(*name_idx),
            OpCode::DeleteIndexNamed(name_idx, _) => Some(*name_idx),
            OpCode::IndexAssignPseudoStashNamed { stash_name_idx, .. } => Some(*stash_name_idx),
            OpCode::IndexAssignPseudoStashKeyed { stash_name_idx } => Some(*stash_name_idx),
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
    pub(crate) fn regex_interpolated_var_names(pattern: &str) -> Vec<String> {
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

    /// Bare routine names referenced by this body or a nested closure body.
    ///
    /// Closure creation uses this to pin an existing lexical `&name` binding.
    /// In particular, a `use` inside `EVAL` installs exported routines in the
    /// EVAL scope, and a returned closure must retain only the imports its
    /// bytecode can actually call after that scope is restored.
    pub(crate) fn bare_callee_names(&self) -> std::collections::HashSet<Symbol> {
        let mut names = std::collections::HashSet::new();
        self.collect_bare_callee_names(&mut names);
        names
    }

    fn collect_bare_callee_names(&self, names: &mut std::collections::HashSet<Symbol>) {
        for op in &self.ops {
            if let Some(idx) = Self::op_callee_name_const_idx(op)
                && let Some(ValueView::Str(name)) =
                    self.constants.get(idx as usize).map(Value::view)
                && !name.contains("::")
            {
                names.insert(Symbol::intern(&name));
            }
        }
        for nested in &self.closure_compiled_codes {
            nested.collect_bare_callee_names(names);
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
            //
            // Gated on `outer_code_var_names` — the same "declared by this point
            // in source order" table the bare-call-name branch below already
            // requires — so a `&name` referenced BEFORE its own `my &name = ...`
            // declaration has compiled is deliberately left OUT of `free_var_syms`
            // (forward-captured-code-var-snapshot fix). CBOR::Simple's
            // mutually-ordered decoders rely on this: `my &decode-array = { ...
            // &decode... }; ...; my &decode = {...};` — `decode-array`'s closure
            // literal is created (and its free vars baked) before `&decode` is
            // even declared, so unconditionally capturing `&decode` here would
            // freeze its pre-declaration local slot (still Nil at that point)
            // into `decode-array`'s closure forever. Leaving a genuine forward
            // reference out of `free_var_syms` instead lets `GetCodeVar`'s
            // `resolve_code_var` fall through to the live env-chain lookup at
            // CALL time — by which point the enclosing `my &decode = ...` has
            // run — exactly like a bare call `decode(...)` in the same forward
            // position already resolves (that path was never gated on eager
            // capture in the first place). A capture that IS available at this
            // point (a `&`-sigiled parameter, or an already-declared `my &f`)
            // is unaffected: `outer_code_var_names` is populated from every
            // enclosing scope's `&`-locals up to and including its own
            // parameters (declared before the body starts compiling), so
            // man-or-boy.t's captured `&x1` parameter is still recognized here.
            if let Some(idx) = Self::op_code_var_read_const_idx(op)
                && let Some(ValueView::Str(name)) =
                    self.constants.get(idx as usize).map(Value::view)
                && !name.contains(':')
            {
                let key = format!("&{}", name.as_str());
                if crate::env::is_plain_user_lexical(&key)
                    && !own.contains(key.as_str())
                    && self.outer_code_var_names.contains(&key)
                {
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
            if let Some(idx) = self.op_name_write_const_idx(op)
                && let Some(ValueView::Str(name)) =
                    self.constants.get(idx as usize).map(Value::view)
            {
                if own.contains(name.as_str()) {
                    self_mutated.insert(Symbol::intern(&name));
                } else if self.expr_declared_syms.contains(&Symbol::intern(&name)) {
                    // An expression-position `my` THIS body declares
                    // (`if (my $d = ...)`) stores env-only under the bare name;
                    // that store is the declaration's own binding, not a write
                    // to an enclosing lexical. Recording it as a free-var write
                    // made the call-site writeback drain copy the callee's `$d`
                    // over a same-named CALLER lexical (Text::CSV's `csv()`
                    // clobbering the caller's `$file`).
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
            // `GetCallerOuterVar` (an immediate-block `CALLER::` that resolves
            // lexically) reads the same enclosing binding via `get_outer_var`, so
            // it must snapshot it into the closure env exactly like `GetOuterVar`.
            if let OpCode::GetOuterVar { name_idx, .. } | OpCode::GetCallerOuterVar { name_idx, .. } =
                op
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
        // An atomic op's target is written through a `__mutsu_*_var("name", …)`
        // call, which the op scan above cannot see as a write. Fold those names
        // in explicitly (see `atomic_target_syms`).
        for sym in &self.atomic_target_syms {
            if sym.with_str(|s| own.contains(s)) {
                self_mutated.insert(*sym);
            } else {
                free.insert(*sym);
                free_writes.insert(*sym);
            }
        }
        // Regex literals interpolate lexical variables at match time (`/<$r>/`,
        // `/$x/`, `/<&rule>/`). Those reads happen inside the regex engine, not via
        // a name-const op, so the op scan above misses them. A closure that stores
        // such a regex (e.g. `-> $r { * ~~ /<$r>/ }`) must still capture `$r`, so
        // scan every regex constant for sigil'd variable references and treat them
        // as free vars (unless this body declares them).
        //
        // A name that IS one of `own`'s locals is an own capture instead: the
        // regex literal (loaded via `OpCode::LoadRegexClosure`, see
        // `regex_literal_closure_captures`) closes over the name out of THIS
        // frame's own locals rather than a parent's. Track those separately
        // (`regex_captured_own`) so, once `self_mutated` is fully known below,
        // we can compute which own regex-captured names are also mutated after
        // the regex is constructed — those need a shared cell (bug 1 of
        // `todo/tickets/stored-regex-loses-its-defining-scope-lexicals.md`).
        let mut regex_captured_own: std::collections::HashSet<Symbol> =
            std::collections::HashSet::new();
        for c in &self.constants {
            let pattern = match c.view() {
                ValueView::Regex(s) => Some(s.clone()),
                ValueView::RegexWithAdverbs(a) => Some(a.pattern.clone()),
                _ => None,
            };
            if let Some(pattern) = pattern {
                for name in Self::regex_interpolated_var_names(&pattern) {
                    if own.contains(name.as_str()) {
                        regex_captured_own.insert(Symbol::intern(&name));
                    } else {
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
            // Bubble rw-arg-sink targets (`cas` et al.) up as a DEDICATED side
            // channel — deliberately NOT folded into `free`/`free_writes`
            // above, since that would change this closure's own
            // capture/cell-promotion classification for the name (see
            // `rw_arg_env_sync_syms`'s doc comment). A name that is one of
            // the nested closure's own locals stops here (it does not belong
            // to an outer frame); everything else continues bubbling until it
            // reaches the frame that owns the local.
            for sym in &nested.rw_arg_env_sync_syms {
                if !sym.with_str(|s| nested.locals.iter().any(|l| l == s)) {
                    self.rw_arg_env_sync_syms.insert(*sym);
                }
            }
        }
        // Fold directly-nested registered routines' (named subs, and class /
        // role method bodies) free variables into `free` the same way a nested
        // closure's `free_var_syms` was just folded above -- see
        // `nested_routine_free_reads`'s doc comment. Such a routine has no
        // runtime closure-creation op, so without this fold a variable
        // referenced ONLY from inside its body never lands in this code's own
        // capture set, and is silently absent from the closure env snapshotted
        // when this code is later invoked as a Callable value. Deliberately
        // does NOT touch `self_mutated`/`free_writes` here: nested-routine
        // write tracking (mutation -> shared-cell boxing) is already handled by
        // the separate `named_sub_captures` channel below, and by
        // `type_body_written_lexicals` for a method body.
        for syms in &self.nested_routine_free_reads {
            for sym in syms {
                if !sym.with_str(|s| own.contains(s)) {
                    free.insert(*sym);
                }
            }
        }
        // `self_mutated` is fully known now (nothing below this point adds to
        // it — the loops that follow only read it). An own local interpolated
        // into one of our own regex constants AND mutated after declaration
        // needs a shared cell so the stored regex observes later writes
        // (raku-verified: `my $x = 1; my $re = rx/ abc <?{ $x == 2 }> /; $x =
        // 2; "abc" ~~ $re` matches). Purely additive — does not touch `free`,
        // `free_var_writes`, `captured_mutated`, or `needs_cell_locals`.
        let needs_cell_regex: std::collections::HashSet<Symbol> = regex_captured_own
            .iter()
            .filter(|sym| self_mutated.contains(*sym))
            .copied()
            .collect();
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
                // A name that closure declares in EXPRESSION position is its own
                // binding, however the env-only store spells it, so it must not
                // earn OUR same-named local a shared cell — an unrelated later
                // `my Pair $p` then found the cell instead of its own fresh
                // binding (roast S02-types/pair.t #181). The name stays a free
                // var, but its store no longer writes through to any cell we
                // hand it: the two `SetGlobal` write-through sites
                // (`vm_exec_dispatch.rs`, `vm_env_helpers.rs`) consult this
                // same `expr_declared_syms` set at runtime and skip the
                // write-through for a fresh binding. The one deliberate
                // exception is the synthesized `WhateverCode` "promoted"
                // declaration, which is excluded from `expr_declared_syms` on
                // purpose because it lexically belongs to the enclosing block
                // and therefore MUST write through (roast
                // S02-types/whatever.t #45). See `expr_declared_syms`.
                if nested.expr_declared_syms.contains(sym) {
                    continue;
                }
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
        // A `my enum`'s type and variant names are this code's OWN lexical
        // bindings, but they get no local slot, so every bareword read of one
        // landed in `free` above. Left there, the free-var exemption in the
        // closure-exit writeback filters would push the block-private binding
        // back onto a same-named caller lexical. See `my_declared_enum_sym`.
        if !self.my_declared_enum_sym.is_empty() {
            free.retain(|sym| !self.my_declared_enum_sym.contains(sym));
        }
        // A plain-scalar for-loop parameter (`for @a -> $i {...}`) is this
        // code's OWN binding, never something to capture from an enclosing
        // scope. See `for_loop_param_syms`.
        if !self.for_loop_param_syms.is_empty() {
            free.retain(|sym| !self.for_loop_param_syms.contains(sym));
        }
        // ADR-0032 D1 populates `container_ref_capture_syms` at EMISSION time
        // inside `Compiler::emit_wrap_var_ref`, purely from "does `local_map`
        // (slot-addressed) own this name" -- which is also `false` for a
        // for-loop parameter or a `my enum`'s bareword bindings, since BOTH
        // are this code's own binding but deliberately never get a local
        // slot (see the two `free`-filtering blocks just above, whose
        // rationale applies identically here). Left unfiltered, a plain
        // `isa-ok($pair, Pair)` read of a for-loop's OWN `-> $pair {...}`
        // parameter was misclassified as an outer capture and bubbled a
        // bogus decl-site boxing request up to a same-named ANCESTOR `my`
        // that has nothing to do with the loop (`roast/S02-types/pair.t`:
        // `sub test2(%h) { for %h.pairs -> $pair { isa-ok($pair,Pair); ... } }`
        // corrupted a later, unrelated file-scope `my $pair`). Apply the same
        // two exclusions post-compile, mirroring `free_var_syms` exactly.
        if !self.container_ref_capture_syms.is_empty() {
            if !self.my_declared_enum_sym.is_empty() {
                self.container_ref_capture_syms
                    .retain(|sym| !self.my_declared_enum_sym.contains(sym));
            }
            if !self.for_loop_param_syms.is_empty() {
                self.container_ref_capture_syms
                    .retain(|sym| !self.for_loop_param_syms.contains(sym));
            }
        }
        self.free_var_syms = free.into_iter().collect();
        self.outer_ref_names = outer_ref_names;
        self.free_var_writes = free_writes.into_iter().collect();
        self.free_var_container_writes = free_container_writes.into_iter().collect();
        self.captured_mutated_locals = captured_mutated.into_iter().collect();
        self.needs_cell_locals = needs_cell.into_iter().collect();
        self.needs_cell_regex = needs_cell_regex.into_iter().collect();
        // A self-capturing declaration only matters when the local actually gets a
        // cell — otherwise there is no cell for the declaration to preserve.
        self.self_capture_decl_locals = self_capture_decl
            .into_iter()
            .filter(|sym| self.needs_cell_locals.contains(sym))
            .collect();
        self.needs_cell_free_vars = needs_cell_free.into_iter().collect();
        // Thread-escape is transitive through enclosing closures: a nested
        // `start { $c = ... }` inside `.map({ ... })` reaches the outer `$c`
        // only via this frame's capture, so the boxing decision at the OUTER
        // creation site (which consults `cc.thread_escaping` to relax the
        // typed-scalar skip) must see the nested thread hand-off. The cell
        // requirement itself already bubbles via `needs_cell_free_vars`; this
        // carries the thread bit alongside it.
        if !self.thread_escaping
            && self
                .closure_compiled_codes
                .iter()
                .any(|nested| nested.thread_escaping)
        {
            self.thread_escaping = true;
        }
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
    /// `free_var_writes`, `free_var_container_writes`).
    ///
    /// Conservative by design (Phase 1):
    /// - A variable is eligible only if it is a plain user lexical, is NEVER
    ///   written anywhere in the closure subtree (not in `free_var_writes` /
    ///   `free_var_container_writes`), and appears in this code's ops only through
    ///   pure-read ops. Read-only capture means the by-value-or-cell snapshot in
    ///   the upvalue array observes the creator's container correctly (a mutated
    ///   capture is boxed into a shared `ContainerRef` cell, which the snapshot
    ///   clones), so reads stay coherent without any write-back.
    pub(crate) fn compute_upvalues(&mut self, runtime_bound: &std::collections::HashSet<Symbol>) {
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
            // ADR-0032 D3b: a name whose raw container is captured by
            // `WrapVarRef` anywhere in this code (`container_ref_capture_syms`,
            // populated by D1 during THIS code's own compile, which precedes
            // this post-compile pass) must keep reading through `GetGlobal`
            // so `exec_wrap_var_ref_op`'s by-name env-cell recovery (D3a)
            // still applies. `GetUpvalue` reads `val.into_deref()` — an
            // unconditional strip that would defeat D1/D2 for the exact
            // shape they exist to fix.
            .filter(|sym| !self.container_ref_capture_syms.contains(sym))
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
        if !self.uses_callframe {
            match &op {
                // A direct `callframe(…)`/`callframes()` call observes the
                // caller frame; so do the CALLER:: pseudo-package variable ops.
                OpCode::CallFunc { name_idx, .. } | OpCode::CallFuncNamed { name_idx, .. } => {
                    if let Some(v) = self.constants.get(*name_idx as usize)
                        && let ValueView::Str(s) = v.view()
                        && matches!(s.as_str(), "callframe" | "callframes")
                    {
                        self.uses_callframe = true;
                    }
                }
                OpCode::GetCallerVar { .. }
                | OpCode::SetCallerVar { .. }
                | OpCode::BindCallerVar { .. }
                | OpCode::GetCallerOuterVar { .. } => {
                    self.uses_callframe = true;
                }
                _ => {}
            }
        }
        if !self.uses_dispatcher
            && let OpCode::CallFunc { name_idx, .. }
            | OpCode::CallFuncNamed { name_idx, .. }
            // `callsame`/`nextsame`/`callwith`/`nextwith` written with no
            // parens/args (the common case) compile as a bareword term read
            // (`GetBareWord`), not a call opcode — `exec_get_bare_word_op`
            // dispatches those four names specially at runtime.
            | OpCode::GetBareWord(name_idx) = &op
            && let Some(v) = self.constants.get(*name_idx as usize)
            && let ValueView::Str(s) = v.view()
            && matches!(
                s.as_str(),
                "callsame" | "nextsame" | "callwith" | "nextwith"
            )
        {
            self.uses_dispatcher = true;
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
                OpCode::GetScalarContainer { .. }
                    // A smartmatch updates the implicit match state (`$/`,
                    // numeric captures, and named captures) in the env. This
                    // matters even for a 0-local routine: its capture reset
                    // must execute in a scoped overlay rather than removing a
                    // caller's named-capture key directly.
                    | OpCode::SmartMatchExpr { .. }
                    | OpCode::SetGlobal(_)
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
                    | OpCode::IndexAssignPseudoStashKeyed { .. }
                    | OpCode::IndexElemAutoviv { .. }
                    | OpCode::PostIncrement(..)
                    | OpCode::PostDecrement(..)
                    | OpCode::PostIncrementIndex(..)
                    | OpCode::PostDecrementIndex(..)
                    | OpCode::PreIncrement(..)
                    | OpCode::PreDecrement(..)
                    | OpCode::PreIncrementIndex(..)
                    | OpCode::PreDecrementIndex(..)
                    | OpCode::MultiDimIndexAssign { .. }
                    | OpCode::MultiDimIndexAssignGeneric { .. }
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
                    | OpCode::RegisterDecl(_)
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
        // Same peephole for the METAOP_ASSIGN identity seed: `$i += 1` on a local
        // compiles to `GetLocal(slot); MetaAssignIdentity(Zero); ...`, and the two
        // are always emitted back-to-back by `compile_expr_unary`, so no jump can
        // target the second one.
        if let OpCode::MetaAssignIdentity(identity) = op
            && let Some(OpCode::GetLocal(slot)) = self.ops.last()
        {
            let slot = *slot;
            let idx = self.ops.len() - 1;
            self.ops[idx] = OpCode::GetLocalMetaAssign { slot, identity };
            return idx;
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
            OpCode::BlockLocalScope { body_end, .. } => *body_end = target,
            _ => panic!("patch_block_local_body_end on non-BlockLocalScope opcode"),
        }
    }

    pub(crate) fn patch_succeed_barrier_body_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::SucceedBarrier { body_end } => *body_end = target,
            _ => panic!("patch_succeed_barrier_body_end on non-SucceedBarrier opcode"),
        }
    }

    pub(crate) fn patch_reset_state_locals_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::ResetStateLocals { body_end } => *body_end = target,
            _ => panic!("patch_reset_state_locals_end on non-ResetStateLocals opcode"),
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
            OpCode::BeginOnceExpr { body_end, .. } => *body_end = target,
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

    pub(crate) fn patch_routine_scope_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::RoutineScope { body_end } => *body_end = target,
            _ => panic!("patch_routine_scope_end on non-RoutineScope opcode"),
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
        self.stmt_pool.push(stmt);
        idx
    }

    /// Record a sub declaration plan. `name_chunk` and `trait_arg_chunks` are the
    /// compiled declaration-time expressions the compiler lowered for this site
    /// (ADR-0019 C5); `trait_arg_chunks` is index-aligned with the declaration's
    /// `custom_traits`.
    pub(crate) fn add_sub_decl_plan(
        &mut self,
        stmt: &Stmt,
        name_chunk: Option<CompiledDeclExpr>,
        trait_args: Vec<Option<DeclTraitArg>>,
    ) -> u32 {
        let Stmt::SubDecl {
            name,
            name_expr,
            params,
            param_defs,
            return_type,
            associativity,
            signature_alternates,
            body,
            multi,
            is_rw,
            is_raw,
            is_export,
            export_tags,
            is_test_assertion,
            supersede,
            custom_traits,
            ..
        } = stmt
        else {
            panic!("add_sub_decl_plan expects SubDecl");
        };
        let fingerprint = name_expr.is_none().then(|| {
            crate::ast::sub_registration_fingerprint(
                params,
                param_defs,
                body,
                return_type.as_ref(),
                *multi,
                *is_rw,
                *is_raw,
            )
        });
        let routine_metadata = compiled_routine_metadata(params, param_defs, body, *is_rw, *is_raw);
        let alternate_metadata = signature_alternates
            .iter()
            .map(|(alt_params, alt_param_defs)| {
                compiled_routine_metadata(alt_params, alt_param_defs, body, *is_rw, *is_raw)
            })
            .collect();
        debug_assert_eq!(name_chunk.is_some(), name_expr.is_some());
        let plan_traits = zip_decl_trait_args(custom_traits, trait_args);
        let plan_idx = self.sub_decl_plans.len() as u32;
        self.sub_decl_plans.push(CompiledSubDeclPlan {
            name: *name,
            name_chunk,
            params: params.clone(),
            param_defs: param_defs.clone(),
            return_type: return_type.clone(),
            associativity: associativity.clone(),
            signature_alternates: signature_alternates.clone(),
            alternate_metadata,
            compiled_routine_keys: Vec::new(),
            multi: *multi,
            is_rw: *is_rw,
            is_raw: *is_raw,
            is_export: *is_export,
            export_tags: export_tags.clone(),
            is_test_assertion: *is_test_assertion,
            supersede: *supersede,
            custom_traits: plan_traits,
            fingerprint,
            routine_metadata,
        });
        let idx = self.decl_plans.len() as u32;
        self.decl_plans.push(CompiledDeclPlanRef::Sub(plan_idx));
        idx
    }

    /// Record a `proto sub`/`proto method` declaration plan (ADR-0019 C8).
    /// `compiled_routine_key` starts `None`; the caller compiles the
    /// `{*}`-rewritten body separately (mirroring `add_sub_decl_plan` +
    /// `set_sub_decl_compiled_routine_keys`) and attaches it with
    /// [`Self::set_proto_decl_compiled_routine_key`].
    pub(crate) fn add_proto_decl_plan(&mut self, stmt: &Stmt) -> u32 {
        let Stmt::ProtoDecl {
            name,
            params,
            param_defs,
            return_type,
            body,
            is_export,
            custom_traits,
            is_method,
            is_our,
        } = stmt
        else {
            panic!("add_proto_decl_plan expects ProtoDecl");
        };
        let plan_idx = self.proto_decl_plans.len() as u32;
        self.proto_decl_plans.push(CompiledProtoDeclPlan {
            name: *name,
            params: params.clone(),
            param_defs: param_defs.clone(),
            return_type: return_type.clone(),
            is_export: *is_export,
            custom_traits: custom_traits.clone(),
            is_method: *is_method,
            is_our: *is_our,
            legacy_body: body.clone(),
            compiled_routine_key: None,
        });
        let idx = self.decl_plans.len() as u32;
        self.decl_plans.push(CompiledDeclPlanRef::Proto(plan_idx));
        idx
    }

    /// Record a `proto token`/`proto rule` LTM marker (ADR-0019 C8). Unlike
    /// `add_proto_decl_plan`, there is no signature, body, or trait to lower —
    /// `Stmt::ProtoToken` carries only a name.
    pub(crate) fn add_proto_token_decl_plan(&mut self, name: Symbol) -> u32 {
        let idx = self.decl_plans.len() as u32;
        self.decl_plans.push(CompiledDeclPlanRef::ProtoToken(name));
        idx
    }

    /// Record a `token`/`rule` declaration plan (ADR-0019 F7). `raw_body` stays
    /// an opaque payload — a token/rule body is never bytecode-compiled, that
    /// stays interpreter-executed by ADR-0009's own design — mirroring
    /// `add_proto_decl_plan`'s `legacy_body` precedent for the same reason.
    /// `source_line` is the caller's `last_source_line` at the point this
    /// top-level declaration compiles (the `SetLine` marker the parser always
    /// emits right before it), feeding `Code.line`/`Code.file`.
    pub(crate) fn add_token_decl_plan(&mut self, stmt: &Stmt, source_line: Option<i64>) -> u32 {
        let plan_idx = self.token_decl_plans.len() as u32;
        self.token_decl_plans
            .push(build_token_decl_plan(stmt, source_line));
        let idx = self.decl_plans.len() as u32;
        self.decl_plans.push(CompiledDeclPlanRef::Token(plan_idx));
        idx
    }

    pub(crate) fn set_proto_decl_compiled_routine_key(
        &mut self,
        decl_idx: u32,
        key: Option<Symbol>,
    ) {
        let Some(CompiledDeclPlanRef::Proto(plan_idx)) = self.decl_plans.get(decl_idx as usize)
        else {
            panic!("declaration plan is not a proto");
        };
        self.proto_decl_plans[*plan_idx as usize].compiled_routine_key = key;
    }

    /// The declaration-site fingerprint recorded for a sub plan, absent when the
    /// declaration's name is resolved at runtime (`sub ::($name)`).
    pub(crate) fn sub_decl_plan_fingerprint(&self, decl_idx: u32) -> Option<u64> {
        let Some(CompiledDeclPlanRef::Sub(plan_idx)) = self.decl_plans.get(decl_idx as usize)
        else {
            return None;
        };
        self.sub_decl_plans[*plan_idx as usize].fingerprint
    }

    pub(crate) fn set_sub_decl_compiled_routine_keys(&mut self, decl_idx: u32, keys: Vec<Symbol>) {
        let Some(CompiledDeclPlanRef::Sub(plan_idx)) = self.decl_plans.get(decl_idx as usize)
        else {
            panic!("declaration plan is not a sub");
        };
        self.sub_decl_plans[*plan_idx as usize].compiled_routine_keys = keys;
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn add_class_decl_plan(
        &mut self,
        stmt: &Stmt,
        name_chunk: Option<CompiledDeclExpr>,
        trait_args: Vec<Option<DeclTraitArg>>,
        attr_decls: Vec<(Symbol, CompiledAttrDecl)>,
        method_name_chunks: Vec<Option<CompiledDeclExpr>>,
        parent_arg_chunks: Vec<(String, Vec<DeclTraitArg>)>,
        method_compiled_keys: Vec<Option<Symbol>>,
        method_outer_lexical_slots: Vec<(Symbol, u32)>,
        body_plan: Vec<ClassBodyOp>,
    ) -> u32 {
        let Stmt::ClassDecl {
            name,
            name_expr,
            parents,
            class_is_rw,
            is_hidden,
            is_lexical,
            hidden_parents,
            does_parents,
            repr,
            body,
            language_version,
            custom_traits,
            decl_id,
            ..
        } = stmt
        else {
            panic!("add_class_decl_plan expects ClassDecl");
        };
        debug_assert_eq!(name_chunk.is_some(), name_expr.is_some());
        let plan_traits = zip_decl_trait_args(custom_traits, trait_args);
        let is_stub = is_stub_routine_body(body);
        let trusts = body
            .iter()
            .filter_map(|s| match s {
                Stmt::TrustsDecl { name } => Some(*name),
                _ => None,
            })
            .collect();
        let own_attribute_names = class_own_attribute_names(body);
        let declared_static_names = class_declared_static_names(body);
        let mut method_decls = compile_method_decls(body);
        // ADR-0019 D3-8a: attach each method's precomputed main-pass
        // bytecode key, position-aligned by the same flattened walk
        // `compile_method_decls` used to build `method_decls`.
        debug_assert_eq!(method_decls.len(), method_compiled_keys.len());
        for (decl, key) in method_decls.iter_mut().zip(method_compiled_keys) {
            decl.compiled_routine_key = key;
        }
        let plan_idx = self.class_decl_plans.len() as u32;
        self.class_decl_plans.push(CompiledClassDeclPlan {
            name: *name,
            name_chunk,
            parents: parents.clone(),
            class_is_rw: *class_is_rw,
            is_hidden: *is_hidden,
            is_lexical: *is_lexical,
            hidden_parents: hidden_parents.clone(),
            does_parents: does_parents.clone(),
            repr: repr.clone(),
            language_version: language_version.clone(),
            custom_traits: plan_traits,
            decl_id: *decl_id,
            is_stub,
            trusts,
            own_attribute_names,
            attr_decls,
            method_name_chunks,
            method_decls,
            method_outer_lexical_slots,
            declared_static_names,
            parent_arg_chunks,
            body_plan,
        });
        let idx = self.decl_plans.len() as u32;
        self.decl_plans.push(CompiledDeclPlanRef::Class(plan_idx));
        idx
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn add_role_decl_plan(
        &mut self,
        stmt: &Stmt,
        trait_args: Vec<Option<DeclTraitArg>>,
        attr_decls: Vec<(Symbol, CompiledAttrDecl)>,
        method_name_chunks: Vec<Option<CompiledDeclExpr>>,
        parent_ops: Vec<RoleParentOp>,
        method_compiled_keys: Vec<Option<Symbol>>,
        method_outer_lexical_slots: Vec<(Symbol, u32)>,
        deferred_body_ops: Vec<DeferredBodyOp>,
    ) -> u32 {
        let Stmt::RoleDecl {
            name,
            type_params,
            type_param_defs,
            is_export,
            export_tags,
            body,
            is_rw,
            language_version,
            custom_traits,
        } = stmt
        else {
            panic!("add_role_decl_plan expects RoleDecl");
        };
        let (own_attribute_names, body_used_modules, body_declared_types) = role_body_prescan(body);
        let mut method_decls = compile_method_decls(body);
        // ADR-0019 D3-8a: see `add_class_decl_plan`'s identical comment.
        debug_assert_eq!(method_decls.len(), method_compiled_keys.len());
        for (decl, key) in method_decls.iter_mut().zip(method_compiled_keys) {
            decl.compiled_routine_key = key;
        }
        let is_stub = role_body_is_stub(body);
        let our_scope_violation = role_body_our_scope_violation(body);
        let body_plan = role_body_plan(body);
        let plan_idx = self.role_decl_plans.len() as u32;
        self.role_decl_plans.push(CompiledRoleDeclPlan {
            name: *name,
            type_params: type_params.clone(),
            type_param_defs: type_param_defs.clone(),
            is_export: *is_export,
            export_tags: export_tags.clone(),
            is_rw: *is_rw,
            language_version: language_version.clone(),
            custom_traits: zip_decl_trait_args(custom_traits, trait_args),
            own_attribute_names,
            body_used_modules,
            body_declared_types,
            attr_decls,
            method_outer_lexical_slots,
            method_name_chunks,
            method_decls,
            is_stub,
            our_scope_violation,
            parent_ops,
            body_plan,
            deferred_body_ops,
            role_id: crate::runtime::next_role_id(),
        });
        let idx = self.decl_plans.len() as u32;
        self.decl_plans.push(CompiledDeclPlanRef::Role(plan_idx));
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
pub(crate) type MemoCache = std::sync::Arc<std::sync::Mutex<Vec<(Vec<Value>, Value)>>>;

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
    /// When true, calls memoize successful results by argument values.
    pub(crate) is_cached: bool,
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
    /// top-level `RegisterDecl(Sub)` / `RegisterSubset` opcode (`my sub`, a bare
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
    /// Compiled functions this routine's body directly or transitively
    /// declares as nested `sub`s, keyed exactly as they were installed into
    /// the enclosing compile pass's functions table (post name-collision
    /// remap). `None` when the body declares no nested sub.
    ///
    /// A dispatch site that invokes this routine as a detached `Sub` VALUE —
    /// a map/grep block, an operator fallback, a `.wrap` target, `MAIN`, a
    /// reduce/hyper step — is not necessarily still inside the `CompiledFns`
    /// table this routine was originally compiled alongside, so it cannot
    /// resolve a nested `RegisterSub`'s `compiled_routine_keys` from its own
    /// calling context. Without this field such sites substituted
    /// `CompiledFns::default()`, which made a nested sub's declaration plan
    /// fail to resolve its own bytecode and fall back to registering with an
    /// executable AST body — the blocker recorded in
    /// `todo/deep/c6e-legacy-body-drop-blocked-by-gate-rejected-shapes.md`
    /// (ADR-0019 C6e-3c). Every such site should prefer this table over an
    /// empty one before falling back to whatever `CompiledFns` the caller
    /// happens to have in scope.
    pub(crate) compiled_fns: Option<std::sync::Arc<CompiledFns>>,
    pub(crate) memo_cache: MemoCache,
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
            // `:@l` / `:%h` / `:&c` are stored with their sigil; the caller's
            // Pair key is the bare name.
            let match_key = pd
                .name
                .strip_prefix('@')
                .or_else(|| pd.name.strip_prefix('%'))
                .or_else(|| pd.name.strip_prefix('&'))
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
                    // Only a LEAF of the rename chain names a body variable.
                    // In `:mil(:milli(:$millis))` the caller may pass `mil`,
                    // `milli` or `millis` (all become alias_keys above), but
                    // inside the body only `$millis` exists — the intermediate
                    // alias names (`mil`, `milli`, which themselves carry a
                    // sub-signature) must NOT be bound as body variables, else
                    // they shadow a same-named outer constant/lexical. A leaf
                    // with no further sub-signature is the real variable
                    // (`$millis`, or the `$a`/`$b` of a destructuring sig).
                    if sub_pd.sub_signature.is_none() {
                        alias_binds.push((sub_pd.name.clone(), slot_of(&sub_pd.name)));
                    }
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
                    OpCode::RegisterDecl(..)
                        | OpCode::RegisterSubset(..)
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
        self.declares_inner_routines = self.code.declares_inner_routines();
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
        if self.is_callee_local_sym_direct(sym) {
            return true;
        }
        // A callee's own typed-lexical metadata (`__mutsu_type::<local>`,
        // written env-scoped by a typed parameter bind or `SetVarTypeScoped`)
        // is frame state: merging it back into the caller env would re-create
        // the cross-frame constraint leak through the env store (see
        // `todo/deep/bare-name-type-constraint-store-is-scope-blind.md`).
        sym.with_str(|s| {
            s.strip_prefix("__mutsu_type::")
                .is_some_and(|base| self.is_callee_local_sym_direct(Symbol::intern(base)))
        })
    }

    /// [`Self::is_callee_local_sym`] without the `__mutsu_type::` metadata-key
    /// unwrapping — the bare membership test over params / `my` decls / `for`
    /// params.
    #[inline]
    fn is_callee_local_sym_direct(&self, sym: Symbol) -> bool {
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
