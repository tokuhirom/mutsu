use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

/// Default value for `IndexAssign.is_positional` when the field is missing
/// from a serialized AST. Most legacy IndexAssign nodes were created from
/// positional subscripts, so `true` is the safe default.
fn default_is_positional() -> bool {
    true
}

/// Marker argument appended to a `__mutsu_subscript_adverb` call when the
/// subscript was written with `[...]`. The value adverbs (`:kv` / `:p` / `:k` /
/// `:v`) need the bracket for the same reason `:exists` does: a target that is
/// not `Positional` is a one-element list holding itself under `[0]`, while
/// `<a>` on it stays a key lookup. Passed as a marker string alongside the
/// call's other tagged extras rather than as a fixed argument slot, so the
/// existing positional arguments keep their indices.
pub const SUBSCRIPT_POSITIONAL_MARKER: &str = "__subscript_positional__";

/// Marker argument appended to a `__mutsu_subscript_adverb` call when the
/// subscript was written with `{...}` or `<...>`. See
/// [`SUBSCRIPT_POSITIONAL_MARKER`].
pub const SUBSCRIPT_ASSOCIATIVE_MARKER: &str = "__subscript_associative__";

/// A process-global counter assigning each `my class`/lexical `ClassDecl`
/// declaration site a stable id at parse time. Two distinct source
/// declarations get distinct ids; a single declaration inside a loop keeps one
/// id across re-executions (the AST node, and thus its `decl_id` value, is
/// shared). Used to give same-named lexical classes in different scopes their
/// own type identity. See `Interpreter::exec_register_class_op`.
static CLASS_DECL_ID_COUNTER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1);

/// Allocate the next class-declaration site id (always non-zero; 0 means
/// "no stable site", e.g. a runtime-synthesized or deserialized node).
pub(crate) fn next_class_decl_id() -> u64 {
    // The unit-local analysis counter starts at 1 for the same reason this
    // global does: 0 is the "no stable site" sentinel and must not be mintable.
    crate::anon_names::next_id(crate::anon_names::AnonKind::DeclId, &CLASS_DECL_ID_COUNTER)
}

/// Specifies how delegation (`handles`) should forward methods.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub(crate) enum HandleSpec {
    /// Forward a method by name (same name on both sides).
    Name(String),
    /// Rename: expose `exposed` on the class, forwarding to `target` on the delegate.
    Rename { exposed: String, target: String },
    /// Forward all methods defined in the given type (class or role name).
    Type(String),
    /// Forward all methods whose name matches the regex pattern.
    Regex(String),
    /// Wildcard: forward all unknown methods.
    Wildcard,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub(crate) struct ParamDef {
    pub(crate) name: String,
    pub(crate) default: Option<Expr>,
    pub(crate) multi_invocant: bool,
    pub(crate) required: bool,
    pub(crate) named: bool,
    pub(crate) slurpy: bool,
    pub(crate) double_slurpy: bool,
    /// True for single-argument rule slurpy (`+@a`, `+%h`, etc.)
    pub(crate) onearg: bool,
    #[allow(dead_code)]
    pub(crate) sigilless: bool,
    pub(crate) type_constraint: Option<String>,
    pub(crate) literal_value: Option<Value>,
    #[allow(dead_code)]
    pub(crate) sub_signature: Option<Vec<ParamDef>>,
    #[allow(dead_code)]
    pub(crate) where_constraint: Option<Box<Expr>>,
    #[allow(dead_code)]
    pub(crate) traits: Vec<String>,
    pub(crate) optional_marker: bool,
    pub(crate) outer_sub_signature: Option<Vec<ParamDef>>,
    pub(crate) code_signature: Option<(Vec<ParamDef>, Option<String>)>,
    /// True when this parameter is the explicit invocant (e.g. `$self:` in a method signature).
    pub(crate) is_invocant: bool,
    /// Shape constraint for array parameters, e.g. `@a[3]`, `@a[4,4]`, `@a[*]`, `@a[$n]`.
    pub(crate) shape_constraints: Option<Vec<Expr>>,
    /// True when this parameter belongs to a block (pointy/bare), whose
    /// implicit nominal type is Mu, not Any. An unpassed untyped optional
    /// seeds Mu for blocks and Any for routines.
    #[serde(default)]
    pub(crate) block_param: bool,
}

/// Trait marker the parser records on an invocant `ParamDef` it *synthesized*
/// rather than one the user named: `method () { ... }`, `method (Foo:D:)`,
/// `method (::?CLASS:)`. Both forms are recorded under the name `self`, but only
/// a user-written `$self:` declares a `$self` lexical in the body — see
/// [`ParamDef::declares_self_lexical`] and ADR-0061.
pub(crate) const IMPLICIT_INVOCANT_TRAIT: &str = "implicit-invocant";

/// True when a *signature* declares a parameter the source spelled `$self`,
/// including one nested in a destructuring sub-signature (`sub f([$self, $x])`).
///
/// The single oracle both halves of ADR-0061 consult: the compiler's
/// `self_is_signature_param` flag and the runtime's binding-time mirror. Keeping
/// them on one function is what stops the two from disagreeing — a compiler that
/// thinks `$self` means the parameter while the binder thinks it means the
/// reserved lexical key is exactly the silent mis-binding the ADR set out to
/// avoid.
pub(crate) fn signature_declares_self_lexical(param_defs: &[ParamDef]) -> bool {
    param_defs.iter().any(|pd| {
        pd.declares_self_lexical()
            || pd
                .sub_signature
                .as_deref()
                .is_some_and(signature_declares_self_lexical)
    })
}

/// True when a bare parameter-NAME list declares a `$self` lexical.
///
/// The legacy binding path carries a single pointy-block parameter
/// (`-> $self { }`) as a bare name with no `ParamDef` at all, so the list has to
/// be consulted too. A `self` in a METHOD's parameter list is the *injected*
/// invocant rather than a lexical; `?CLASS` is injected alongside it and is the
/// existing marker for that shape (see `Compiler::lexically_in_method`).
pub(crate) fn param_names_declare_self_lexical(params: &[String]) -> bool {
    !params.iter().any(|p| p == "?CLASS") && params.iter().any(|p| p == "self")
}

/// Build the read expression for a `$`-sigiled scalar whose bare (sigil-less)
/// name is `name`, applying the reserved-`$self` rename: `self` is a *term*, so
/// a `$`-sigiled `self` is a user lexical and takes [`crate::env::LEX_SELF`]
/// rather than the invocant's key (ADR-0061).
pub(crate) fn scalar_var_expr(name: String) -> Expr {
    if name == "self" {
        Expr::Var(crate::env::LEX_SELF.to_string())
    } else {
        Expr::Var(name)
    }
}

impl ParamDef {
    /// True when the *source* declares a parameter spelled `$self` — an explicit
    /// invocant (`method m($self: $n)`, `method symbol(::?CLASS $self: ...)`) or
    /// an ordinary parameter (`sub ($self)`, `-> $self, $x`). A parser-synthesized
    /// anonymous invocant is excluded: it is named `self` only because that is the
    /// invocant's env key, and it declares no lexical (ADR-0061).
    pub(crate) fn declares_self_lexical(&self) -> bool {
        self.name == "self" && !self.traits.iter().any(|t| t == IMPLICIT_INVOCANT_TRAIT)
    }

    /// True when this parameter is a capture that carries a subsignature, i.e.
    /// `|c(...)` or the anonymous `|(...)` — both sigilless slurpies.  Such a
    /// parameter consumes all remaining arguments and delegates dispatch to its
    /// subsignature, so for arity counting it behaves like a slurpy capture.
    /// A plain destructuring parameter `($a, $b)` — also recorded under the
    /// synthetic `__subsig__` name but NOT slurpy — consumes exactly one
    /// positional argument and is deliberately excluded.
    /// True for every parameter that binds a *variable* number of arguments:
    /// `*@a` / `*%h` (`slurpy`), `**@a` (`double_slurpy`), and the
    /// single-argument-rule `+@a` / `+%h` (`onearg`).
    ///
    /// `+@a` is a slurpy in rakudo — it differs from `*@a` only in the
    /// single-argument rule — but mutsu's parser records it as a plain `@`
    /// parameter carrying `onearg`, so anything reasoning about arity has to ask
    /// for all three flags. Asking only about `slurpy` made multi dispatch treat
    /// `multi f($s, +@i)` as a fixed two-argument candidate, so `f("x", 1, 2)`
    /// found no candidate at all while the identical non-`multi` sub bound fine.
    pub(crate) fn is_variadic(&self) -> bool {
        self.slurpy || self.double_slurpy || self.onearg
    }

    /// True when this parameter binds the CALLER's container rather than a value
    /// copy: an explicit `is raw` / `is rw`, or a plain **sigilless** parameter
    /// (`\p`), which Raku defines as implicitly raw.
    ///
    /// mutsu used to spell this as a bare `traits` scan, which left `\p` out of
    /// every container-aliasing gate: the method fast path
    /// (`vm_method_dispatch.rs`'s `has_rw_params`) skipped the binder entirely
    /// for a `\p` method, and the binder's own shared-cell branch
    /// (`binding_signature.rs`'s `rw_shared_cell_key`) never ran. `\p` was left
    /// with only the by-name `__mutsu_sigilless_alias::p` bookkeeping, which
    /// reconciles the caller through a one-shot VALUE writeback at return — so
    /// any binding that OUTLIVES the call (`$!s := p` stored in an attribute, a
    /// closure over `p`, a relay into a further raw parameter) never reached the
    /// caller's variable. `value/signature.rs`'s introspection already reported
    /// a sigilless parameter as `raw`; this is the same rule for the binder.
    ///
    /// Only the plain scalar form is implicitly raw. `|c` captures and
    /// `+a` / `*@a` slurpies also carry `sigilless`, but they bind a freshly
    /// built aggregate, not the caller's container.
    pub(crate) fn binds_caller_container(&self) -> bool {
        self.traits.iter().any(|t| t == "rw" || t == "raw")
            || (self.sigilless
                && !self.is_variadic()
                && !self.named
                && !self.is_invocant
                && self.sub_signature.is_none())
    }

    pub(crate) fn is_capture_subsignature(&self) -> bool {
        self.sub_signature.is_some()
            && self.type_constraint.is_none()
            && self.literal_value.is_none()
            && self.slurpy
            && self.sigilless
    }

    /// Every external key a *named* parameter answers to, sigil- and
    /// colon-stripped. A named parameter may carry aliases, which the parser
    /// records as nested named entries in `sub_signature`: `:s(:$sort)` becomes
    /// `ParamDef { name: "s", named: true, sub_signature: [ParamDef { name:
    /// "sort", named: true }] }`, and the call may use either `:s(…)` or
    /// `:sort(…)`.
    ///
    /// Callers that match a named argument against a signature must consult all
    /// of them. Binding already did (`types/signature.rs`); multi-candidate
    /// matching did not, so `multi f($n, :s(:$sort) = False)` rejected
    /// `f(1, :sort(True))` with "No matching candidates" while the same
    /// signature on a plain `sub` accepted it (`Prime::Factor`'s `divisors`
    /// re-dispatches with `:sort($sort)`).
    ///
    /// Returns an empty vector for a non-named parameter.
    pub(crate) fn named_external_keys(&self) -> Vec<String> {
        if !self.named {
            return Vec::new();
        }
        let strip = |n: &str| {
            n.trim_start_matches(|c: char| "$@%&:".contains(c))
                .trim_start_matches(['!', '.'])
                .to_string()
        };
        let mut keys = vec![strip(&self.name)];
        if let Some(aliases) = &self.sub_signature {
            keys.extend(
                aliases
                    .iter()
                    .filter(|a| a.named && !a.slurpy)
                    .map(|a| strip(&a.name)),
            );
        }
        keys
    }

    /// Mark this param (and every nested sub-signature param) as belonging to
    /// a block, so an unpassed untyped optional seeds Mu instead of Any.
    pub(crate) fn mark_block_param(&mut self) {
        self.block_param = true;
        for nested in [&mut self.sub_signature, &mut self.outer_sub_signature]
            .into_iter()
            .flatten()
        {
            for pd in nested.iter_mut() {
                pd.mark_block_param();
            }
        }
        if let Some((code_defs, _)) = &mut self.code_signature {
            for pd in code_defs.iter_mut() {
                pd.mark_block_param();
            }
        }
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub(crate) struct FunctionDef {
    pub(crate) package: Symbol,
    pub(crate) name: Symbol,
    pub(crate) params: Vec<String>,
    pub(crate) param_defs: Vec<ParamDef>,
    pub(crate) body: Vec<Stmt>,
    pub(crate) is_test_assertion: bool,
    #[serde(default)]
    pub(crate) is_cached: bool,
    pub(crate) is_rw: bool,
    pub(crate) is_raw: bool,
    /// True when this routine represents an `our method` code reference.
    pub(crate) is_method: bool,
    /// When true, this sub has an explicit empty signature `()` and should reject any arguments.
    pub(crate) empty_sig: bool,
    /// Whether the declaration body is a yada stub (`...`, `!!!`, or `???`).
    /// Compiled declaration plans provide this without a registration-time AST scan.
    #[serde(default)]
    pub(crate) is_stub: bool,
    /// Return type annotation (e.g., "Str", "Str(Numeric:D)", "Foo:D()")
    pub(crate) return_type: Option<String>,
    /// `is default` trait — this candidate is preferred when multi dispatch ties.
    pub(crate) is_default: bool,
    /// `is DEPRECATED` trait message: None = not deprecated, Some(msg) = deprecated.
    /// Empty string means "something else", non-empty is the custom replacement text.
    pub(crate) deprecated_message: Option<String>,
    /// Source file this routine was declared in (None = the main script).
    /// Set at registration time from the interpreter's `?FILE` (which module
    /// loading scopes to the module path), so backtrace frames for module subs
    /// can report the module file (integration/error-reporting.t test 15).
    #[serde(default)]
    pub(crate) source_file: Option<String>,
    /// The declarator keyword's source line (`sub`/`method`/`token`/`rule`/...),
    /// mirroring `source_file` above. A `Sub`/`Method` already carries its line
    /// on its own compiled body (`CompiledCode::source_line`), so this field's
    /// primary consumer is a `token`/`rule` declaration, which has no compiled
    /// body at all by design (ADR-0009) and therefore nowhere else to keep it
    /// -- see `register_token_decl`. Also makes the sub/method path robust
    /// should `compiled` ever be `None`. `None` when the declaration site is
    /// not known (e.g. a synthetic/prelude definition).
    #[serde(default)]
    pub(crate) source_line: Option<i64>,
    /// Monotonic declaration/registration order, stamped by
    /// `runtime::resolution::next_decl_order()` at every registration site.
    /// Two tie-breaks read it, both matching Rakudo's "first declared wins":
    /// an equal-length Longest-Token-Match tie between proto `token`/`rule`
    /// candidates (`token pp:sym<**>` declared before `token pp:sym<m>`), and
    /// an equal-narrowness multi-dispatch tie (`multi f(:$a)` before
    /// `multi f(Str :$a)`). 0 only for defs built outside a registration path.
    #[serde(default)]
    pub(crate) decl_order: u64,
    /// Bytecode body selected by the declaration plan that installed this
    /// candidate. Temporary ADR-0019 adapter; skipped by the AST/precomp format.
    #[serde(skip)]
    pub(crate) compiled: Option<std::sync::Arc<crate::opcode::CompiledFunction>>,
    /// Memoized [`Self::body_fingerprint`]. Derived state, so it is neither
    /// serialized nor part of the declaration; a deserialized or cloned def
    /// simply recomputes it on first use.
    #[serde(skip)]
    pub(crate) body_fp_cache: std::sync::OnceLock<u64>,
    /// Memoized [`RoutineBodyFacts`], filled by
    /// `Interpreter::routine_body_facts`. Derived state, like `body_fp_cache`.
    #[serde(skip)]
    pub(crate) body_facts_cache: std::sync::OnceLock<RoutineBodyFacts>,
}

/// Properties of a routine body that the on-the-fly compilation gates ask about.
///
/// Each is a pure predicate over the body AST, and each used to be recomputed by
/// walking that AST at every gate evaluation. They are memoized together on the
/// def ([`FunctionDef::body_facts_cache`]): one walk more on first touch is
/// negligible next to the
/// compile the gates decide whether to perform.
#[derive(Debug, Clone, Copy)]
pub(crate) struct RoutineBodyFacts {
    /// The body contains a construct whose semantics the standalone-compiled
    /// form would not preserve (a type declaration, a `start` block, ...).
    pub(crate) needs_interpreter: bool,
    /// The body declares a `state` variable somewhere.
    pub(crate) declares_state: bool,
    /// The body contains an explicit `return-rw` call somewhere. Such a
    /// routine hands its caller a container even without the `is rw` trait
    /// (`sub f() { return-rw $v }; f() = 5` writes `$v` in Rakudo), so the
    /// lvalue-assignment machinery treats it as rw-capable (ADR-0059).
    pub(crate) uses_return_rw: bool,
    /// Line-insensitive identity of the declaration (params, param_defs, body
    /// with top-level `SetLine` markers stripped) — the redeclaration
    /// comparison keys on it. Carried here so a plan-derived def keeps its
    /// identity after `legacy_body` is dropped (ADR-0019 C6e-3).
    pub(crate) registration_identity: u64,
}

impl FunctionDef {
    /// Structural identity of this routine: the fingerprint of its signature and
    /// body. Multi-candidate identity, `state`-variable scoping, wrap chains,
    /// `MAIN` candidate dedup, and redeclaration checks all key on it.
    ///
    /// Computed once per def and cached inline. The underlying hash Debug-renders
    /// the whole body AST, which profiled as a large share of multi/method
    /// redispatch; two side caches (`func_def_fp_cache`, keyed on the def's `Arc`
    /// pointer) existed only to avoid that, and this field replaces them with
    /// state that cannot go stale or miss.
    pub(crate) fn body_fingerprint(&self) -> u64 {
        *self
            .body_fp_cache
            .get_or_init(|| function_body_fingerprint(&self.params, &self.param_defs, &self.body))
    }

    /// Drop the memoized fingerprint after the body has been rewritten in place
    /// (the `proto` dispatch rewrite is the only such mutation).
    pub(crate) fn invalidate_body_fingerprint(&mut self) {
        self.body_fp_cache = std::sync::OnceLock::new();
    }
}

/// A `fmt::Write` sink that streams formatted bytes straight into a `Hasher`,
/// so `write!(.., "{:?}", x)` hashes the Debug rendering without ever
/// allocating an intermediate `String`. `function_body_fingerprint` runs on the
/// per-dispatch hot path (candidate identity in multi/method dispatch), so the
/// three `format!` allocations it used to do showed up as a large share of the
/// allocator traffic in method-call / class benchmarks.
struct HashWrite<'a, H: Hasher>(&'a mut H);

impl<H: Hasher> std::fmt::Write for HashWrite<'_, H> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.0.write(s.as_bytes());
        Ok(())
    }
}

pub(crate) fn function_body_fingerprint(
    params: &[String],
    param_defs: &[ParamDef],
    body: &[Stmt],
) -> u64 {
    use std::fmt::Write as _;
    let mut hasher = DefaultHasher::new();
    let mut sink = HashWrite(&mut hasher);
    // Separators keep distinct fields from colliding when their renderings abut.
    let _ = write!(sink, "{params:?}\x00{param_defs:?}\x00{body:?}");
    hasher.finish()
}

/// Line-insensitive identity of a routine declaration for redeclaration
/// comparison: params, param_defs, and the body with top-level `SetLine`
/// markers stripped, streamed into a hasher. Identical redeclarations that
/// differ only in source line compare equal. Distinct from
/// [`function_body_fingerprint`], which hashes `SetLine` markers too (it is a
/// structural identity, not a redeclaration identity).
pub(crate) fn registration_identity_fingerprint(
    params: &[String],
    param_defs: &[ParamDef],
    body: &[Stmt],
) -> u64 {
    use std::fmt::Write as _;
    let mut hasher = DefaultHasher::new();
    let mut sink = HashWrite(&mut hasher);
    let _ = write!(sink, "{params:?}\x00{param_defs:?}\x00");
    for stmt in body.iter().filter(|s| !matches!(s, Stmt::SetLine(_))) {
        let _ = write!(sink, "{stmt:?}\x00");
    }
    hasher.finish()
}

/// Identity fingerprint of a sub *declaration* for idempotent re-registration.
///
/// Two executions of the same `RegisterSub` site install structurally identical
/// declarations; this fingerprint lets the registrar recognize that in O(1) and
/// skip re-deriving the `FunctionDef`. It extends `function_body_fingerprint`
/// with the return type and the flags that distinguish otherwise same-bodied
/// declarations (`multi`/`is rw`/`is raw`), so a genuine change is never mistaken
/// for a no-op. The name and package are *not* hashed: they are the registry key
/// the fingerprint is compared under, so they are already known to match.
pub(crate) fn sub_registration_fingerprint(
    params: &[String],
    param_defs: &[ParamDef],
    body: &[Stmt],
    return_type: Option<&String>,
    multi: bool,
    is_rw: bool,
    is_raw: bool,
) -> u64 {
    let mut hasher = DefaultHasher::new();
    function_body_fingerprint(params, param_defs, body).hash(&mut hasher);
    return_type.hash(&mut hasher);
    multi.hash(&mut hasher);
    is_rw.hash(&mut hasher);
    is_raw.hash(&mut hasher);
    hasher.finish()
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub(crate) enum PhaserKind {
    Begin,
    Check,
    Init,
    End,
    Enter,
    Leave,
    Keep,
    Undo,
    First,
    Next,
    Last,
    Pre,
    Post,
    Quit,
    Close,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[allow(clippy::enum_variant_names, dead_code)]
pub(crate) enum Expr {
    Literal(Value),
    /// A literal whose original source text differs from the canonical
    /// stringification of its value (e.g. `0xFF` → `Int(255)`, `1.5e0` → a
    /// rounded `Num`, `∞` → `Inf`). The compiler treats this as fully
    /// transparent — identical to `Literal(value)` — but the sink-context
    /// warning analysis uses `source` so the "Useless use of ..." message
    /// preserves the format the user actually wrote.
    LiteralSrc(Value, Box<str>),
    /// Marks a parenthesized expression so the compiler can distinguish
    /// `(1|2)|3` (grouped) from `1|2|3` (list-associative chain).
    /// The compiler treats this as transparent — it simply compiles the
    /// inner expression — but the chain-flattener stops at Grouped
    /// boundaries to prevent incorrect junction flattening.
    Grouped(Box<Expr>),
    Whatever,
    /// A `*` that participates in Whatever-priming (an "argument" `*`, in
    /// Rakudo's `WhateverCode::Argument` terminology), as opposed to a bare
    /// `Expr::Whatever` *value*. Not yet produced by the parser (ADR-0033
    /// Phase 1 is a behaviour-preserving deferral only); `should_wrap_whatevercode`
    /// /`contains_whatever` still decide priming the same way they always have.
    /// Phase 2/4 will start emitting this from the leaf-splitting rule in
    /// ADR-0033 §1 and give it real RakuAST/compiler semantics.
    WhateverArg,
    HyperWhatever,
    BareWord(String),
    /// A function call that the parser resolved to a user-declared or imported
    /// routine shadowing a container listop.  This parse-time resolution must
    /// survive until compilation because the parser's lexical scope stack no
    /// longer exists when the compiler runs.
    UserRoutineCall {
        name: Symbol,
        args: Vec<Expr>,
    },
    StringInterpolation(Vec<Expr>),
    /// Deferred heredoc interpolation: stores raw content to be interpolated
    /// at compile time in the scope where the AST node appears, not where
    /// the qq:to declaration was parsed. This is needed because Raku resolves
    /// heredoc body variables in the scope of the terminator, not the declaration.
    ///
    /// The second field is true when the source text remaining on the heredoc
    /// marker's own physical line (before its terminator's body is spliced in)
    /// contains a `}` — i.e. an enclosing block closes on that same line, before
    /// the heredoc's own terminator is reached. Only then can a `my` local
    /// declared inside that block be out of scope by the time Raku resolves the
    /// heredoc body (see `check_heredoc_scope_errors`); a heredoc whose marker
    /// line has no closing brace leaves every enclosing block open through the
    /// whole heredoc, so ordinary lexical scoping applies.
    HeredocInterpolation(String, bool),
    Var(String),
    CaptureVar(String),
    ArrayVar(String),
    HashVar(String),
    CodeVar(String),
    EnvIndex(String),
    /// m/pattern/ — match against $_ and return the result
    MatchRegex(Value),
    Subst {
        pattern: String,
        replacement: String,
        samecase: bool,
        sigspace: bool,
        samemark: bool,
        samespace: bool,
        global: bool,
        nth: Option<String>,
        /// Raw `:x` adverb argument spec: a count (`"3"`) or a range
        /// (`"1..3"`), parsed at substitution time. `None` when `:x` is absent.
        x: Option<String>,
        perl5: bool,
    },
    NonDestructiveSubst {
        pattern: String,
        replacement: String,
        samecase: bool,
        sigspace: bool,
        samemark: bool,
        samespace: bool,
        global: bool,
        nth: Option<String>,
        /// Raw `:x` adverb argument spec: a count (`"3"`) or a range
        /// (`"1..3"`), parsed at substitution time. `None` when `:x` is absent.
        x: Option<String>,
        perl5: bool,
    },
    Transliterate {
        from: String,
        to: String,
        delete: bool,
        complement: bool,
        squash: bool,
        non_destructive: bool,
    },
    MethodCall {
        target: Box<Expr>,
        name: Symbol,
        args: Vec<Expr>,
        modifier: Option<char>,
        /// True when the method name was quoted (e.g. `."DEFINITE"()`),
        /// which bypasses pseudo-method macros like .DEFINITE, .WHAT, etc.
        quoted: bool,
    },
    DynamicMethodCall {
        target: Box<Expr>,
        name_expr: Box<Expr>,
        args: Vec<Expr>,
        modifier: Option<char>,
        /// True for the string-name `\.""` form; false when the name value
        /// itself must be Callable (or a type object), as in `.$name`.
        quoted: bool,
    },
    HyperMethodCall {
        target: Box<Expr>,
        name: Symbol,
        args: Vec<Expr>,
        modifier: Option<char>,
        /// True when the method name was quoted in source.
        quoted: bool,
    },
    HyperMethodCallDynamic {
        target: Box<Expr>,
        name_expr: Box<Expr>,
        args: Vec<Expr>,
        modifier: Option<char>,
    },
    Exists {
        target: Box<Expr>,
        negated: bool,
        delete: bool,
        arg: Option<Box<Expr>>,
        adverb: ExistsAdverb,
    },
    /// Zen slice: `@a[]` — represents all indices of an array.
    ZenSlice(Box<Expr>),
    RoutineMagic,
    /// Phaser used as an rvalue expression: `my $x = INIT { 42 }`
    /// The body is evaluated once at the appropriate phaser time and its result
    /// is stored in a temporary variable for later retrieval.
    PhaserExpr {
        kind: PhaserKind,
        body: Vec<Stmt>,
    },
    Once {
        body: Vec<Stmt>,
    },
    BlockMagic,
    Block(Vec<Stmt>),
    AnonSub {
        body: Vec<Stmt>,
        is_rw: bool,
        /// true when this is a bare block `{ }`, false when it's `sub { }`.
        /// Bare blocks are NOT routine boundaries for `return`.
        is_block: bool,
    },
    AnonSubParams {
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        return_type: Option<String>,
        body: Vec<Stmt>,
        is_rw: bool,
        /// True when this closure was generated by Whatever-currying.
        is_whatever_code: bool,
        /// True when the source wrote the `sub` declarator (`sub ($x) { }`),
        /// false for every other multi-parameter closure that lands on this
        /// node — a pointy block (`-> $a, $b { }`), a placeholder block
        /// (`{ $^a }`), a `method (...) { }` literal, and the closures the
        /// compiler/runtime synthesize. raku models the two spellings with
        /// different nodes (`RakuAST::Sub` vs `RakuAST::PointyBlock`), so the
        /// RakuAST converter reads this flag instead of guessing. It has no
        /// effect on execution.
        is_sub: bool,
    },
    CallOn {
        target: Box<Expr>,
        args: Vec<Expr>,
    },
    Lambda {
        param: String,
        body: Vec<Stmt>,
        /// True when this closure was generated by Whatever-currying.
        is_whatever_code: bool,
        /// True when the single parameter is sigilless (`-> \x { }`). A
        /// sigilless binding shadows a same-named term constant (e.g. the
        /// imaginary unit `i`), so the body binder marks it accordingly.
        param_sigilless: bool,
    },
    /// Marks a maximal Whatever-priming scope (ADR-0033). Carries the
    /// un-curried body — `Expr::Whatever`/`Expr::WhateverArg` leaves are still
    /// in place. This is a marker only: it is not a closure and never reaches
    /// the VM as itself. The compiler expands it into the same `Lambda` /
    /// `AnonSubParams { is_whatever_code: true, .. }` that the parser used to
    /// build eagerly (`whatever_curry::build_closure`), so emitted bytecode is
    /// unchanged. `whatever_curry::plant` is the single authority for where
    /// these markers get inserted; in ADR-0033 Phase 1 that authority is still
    /// distributed across the parser's existing `wrap_whatevercode` call sites
    /// (now constructing this marker instead of the closure directly).
    WhateverCurry(Box<Expr>),
    ArrayLiteral(Vec<Expr>),
    /// A pair expression that was parenthesized, e.g. `(:a(3))`.
    /// At runtime this becomes a ValuePair so it is treated as a positional argument.
    PositionalPair(Box<Expr>),
    /// Array constructed with [...] (reports as "Array" type vs "List" for comma lists).
    /// The bool flag is `true` when a trailing comma was present (e.g. `[x,]`),
    /// which prevents single-element flattening.
    BracketArray(Vec<Expr>, bool),
    /// Capture literal: \(positional..., named...) — mixed exprs separated at compile time
    CaptureLiteral(Vec<Expr>),
    Index {
        target: Box<Expr>,
        index: Box<Expr>,
        /// true when this index was written with `[...]` (positional subscript);
        /// false when written with `{...}` or `<...>` (associative subscript).
        is_positional: bool,
    },
    /// Multi-dimensional indexing with semicolons: @a[$x;$y;$z]
    MultiDimIndex {
        target: Box<Expr>,
        dimensions: Vec<Expr>,
        /// true when the subscript was `[...]` (positional); false when
        /// `{...}` / `<...>` (associative). An associative multi-dim
        /// subscript is a chain of nested Hash keys, not a shape.
        #[serde(default = "default_is_positional")]
        is_positional: bool,
    },
    /// Multi-dimensional index assignment: @a[$x;$y;$z] = value
    MultiDimIndexAssign {
        target: Box<Expr>,
        dimensions: Vec<Expr>,
        value: Box<Expr>,
        /// See `MultiDimIndex::is_positional`.
        #[serde(default = "default_is_positional")]
        is_positional: bool,
    },
    IndexAssign {
        target: Box<Expr>,
        index: Box<Expr>,
        value: Box<Expr>,
        /// true when the assigned subscript was `[...]` (positional);
        /// false when `{...}` / `<...>` (associative). Used to choose
        /// the autovivification kind (Array vs Hash) for missing
        /// intermediate containers in nested writes like
        /// `%h<key>[42] = 17`.
        #[serde(default = "default_is_positional")]
        is_positional: bool,
    },
    Ternary {
        cond: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    AssignExpr {
        name: String,
        expr: Box<Expr>,
        /// True when parsed from `:=` (bind) rather than `=` (assign).
        /// When `true`, the expression should rebind the variable rather
        /// than write through any existing alias.
        is_bind: bool,
    },
    /// A compound assignment with its source-level operator preserved.
    ///
    /// The parser normally expands `x += y` into an ordinary assignment whose
    /// RHS is `x + y`, because that is the shape consumed by the existing
    /// compiler. RakuAST needs the original `+=` distinction, however: raku
    /// exposes it as `MetaInfix::Assign(Infix("+"))`. The expanded expression
    /// remains the execution representation; this marker is transparent to
    /// the compiler and exists so model-layer conversion can recover the
    /// source construct without guessing from the expansion.
    CompoundAssign {
        target: Box<Expr>,
        op: String,
        rhs: Box<Expr>,
        expanded: Box<Expr>,
    },
    Unary {
        op: TokenKind,
        expr: Box<Expr>,
    },
    PostfixOp {
        op: TokenKind,
        expr: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: TokenKind,
        right: Box<Expr>,
    },
    /// A chained comparison `a OP1 b OP2 c ...` (e.g. `1 < 2 < 3`,
    /// `a !before b before c`). `operands.len() == ops.len() + 1`; `ops[i]`
    /// (operator, negated) links `operands[i]` and `operands[i+1]`. This is a
    /// marker only, mirroring `Expr::WhateverCurry`: the compiler's
    /// `Expr::ChainedCompare` arm expands it into the runtime `&&`-conjunction
    /// shape (`crate::chain_compare::expand`) at compile time, evaluating each
    /// operand exactly once, so no operand is duplicated in the durable AST.
    /// Only actual chains (more than one comparison) use this node; a lone
    /// comparison stays a plain `Binary`/`Unary`, matching rakudo's own
    /// `ApplyInfix` rendering.
    ChainedCompare {
        operands: Vec<Expr>,
        ops: Vec<(TokenKind, bool)>,
    },
    Hash(Vec<(String, Option<Expr>)>),
    Call {
        name: Symbol,
        args: Vec<Expr>,
    },
    Try {
        body: Vec<Stmt>,
        catch: Option<Vec<Stmt>>,
    },
    Gather(Vec<Stmt>),
    Eager(Box<Expr>),
    /// Item context coercion: `$%hash` or `$@array` — wraps value in Scalar container
    /// so it won't be flattened in list context.
    Itemize(Box<Expr>),
    /// De-itemize the chunk element of a `for … -> @a` binding. Like `.list`
    /// (flattens a one-element itemized-array wrap into its elements), but
    /// preserves the source array's element type so `@a` keeps `array[int]`
    /// instead of collapsing to an untyped `Array`.
    DeitemizeForBind(Box<Expr>),
    Reduction {
        op: String,
        expr: Box<Expr>,
    },
    InfixFunc {
        name: String,
        left: Box<Expr>,
        right: Vec<Expr>,
        modifier: Option<String>,
    },
    HyperOp {
        op: String,
        left: Box<Expr>,
        right: Box<Expr>,
        dwim_left: bool,
        dwim_right: bool,
    },
    /// Hyper operator with a function reference: >>[&func]<<, <<[&func]>>, etc.
    HyperFuncOp {
        func_name: String,
        left: Box<Expr>,
        right: Box<Expr>,
        dwim_left: bool,
        dwim_right: bool,
    },
    MetaOp {
        meta: String, // "R", "X", "Z"
        op: String,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    /// Feed operator (`==>`, `<==`, `==>>`, `<<==`) — Sequencer precedence (the
    /// loosest infix). Kept as a deferred node (rather than folded into the sink
    /// call immediately) so that an assignment/declaration on the textually-left
    /// side can split it: `my @a = (1,2,3) ==> map {...}` parses with `=` binding
    /// tighter than `==>`, becoming `(my @a = (1,2,3)) ==> map {...}`. `source`
    /// flows into `sink`; `append` distinguishes `==>>`/`<<==` from `==>`/`<==`.
    /// `left_is_source` records whether the textually-left operand is the source
    /// (`==>`) or the sink (`<==`), so the split knows which side to wrap.
    Feed {
        source: Box<Expr>,
        sink: Box<Expr>,
        append: bool,
        left_is_source: bool,
    },
    DoBlock {
        body: Vec<Stmt>,
        label: Option<String>,
    },
    DoStmt(Box<Stmt>),
    ControlFlow {
        kind: ControlFlowKind,
        label: Option<String>,
    },
    IndirectTypeLookup(Box<Expr>),
    IndirectCodeLookup {
        package: Box<Expr>,
        name: String,
    },
    /// Symbolic variable dereference: $::("name"), @::("name"), %::("name")
    /// Resolves a variable by name at runtime. The sigil is "$", "@", or "%".
    SymbolicDeref {
        sigil: String,
        expr: Box<Expr>,
    },
    /// Symbolic variable dereference assignment: $::("name") = value
    SymbolicDerefAssign {
        sigil: String,
        expr: Box<Expr>,
        value: Box<Expr>,
    },
    /// Indirect type lookup assignment: ::('$name') = value
    IndirectTypeLookupAssign {
        expr: Box<Expr>,
        value: Box<Expr>,
    },
    PseudoStash(String),
    /// Hash hyperslice: %hash{**}:adverb
    HyperSlice {
        target: Box<Expr>,
        adverb: HyperSliceAdverb,
    },
}

/// Secondary adverb on :exists subscript adverb
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub(crate) enum ExistsAdverb {
    None,
    Kv,
    NotKv,
    P,
    NotP,
    NotV,
    /// Invalid combos that should die at runtime
    InvalidK,
    InvalidNotK,
    InvalidV,
}

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub(crate) enum HyperSliceAdverb {
    Kv,
    K,
    V,
    Tree,
    DeepK,
    DeepKv,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub(crate) enum ControlFlowKind {
    Last,
    Next,
    Redo,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub(crate) enum CallArg {
    Positional(Expr),
    Named {
        name: String,
        value: Option<Expr>,
    },
    /// Capture slip: `|c` — flatten a capture variable into the argument list
    Slip(Expr),
    /// Invocant colon: `foo($obj:)` — call sub `foo` as a method on `$obj`
    Invocant(Expr),
}

/// Execution mode for `for` loops.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub(crate) enum ForMode {
    Normal,
    Race,
    Hyper,
    /// `lazy for` — loop body executes lazily (not until Seq is consumed)
    Lazy,
}

/// The declarator keyword used for a `Stmt::Package`. Determines the
/// `package-kind` reported by X::Attribute::Package.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub(crate) enum PackageKind {
    Module,
    Package,
    Grammar,
}

impl PackageKind {
    pub(crate) fn as_str(self) -> &'static str {
        match self {
            PackageKind::Module => "module",
            PackageKind::Package => "package",
            PackageKind::Grammar => "grammar",
        }
    }
}

/// Why a name is in the interpreter's readonly set. Rakudo reports three
/// distinct exceptions for "you cannot assign to this", and which one it
/// picks is a property of the *lvalue*, not of the assignment site:
///
/// * a readonly **binding** that still owns a `Scalar` container (a non-`is rw`
///   sub/block parameter, a `for`-loop named alias) — `X::AdHoc`,
///   "Cannot assign to a readonly variable or a value";
/// * a **sigiled variable** that has no container at all because it was bound
///   straight to an immutable value (`my $x := 42`, `my constant $PI = 3.14`,
///   a topic aliased to a literal) — `X::AdHoc`,
///   "Cannot assign to an immutable value";
/// * a name that denotes the immutable **value** itself rather than a variable
///   (a sigilless `constant PI` / `\c` term, an `is List` array) — the
///   assignment reaches `infix:<=>` on the value, giving the specific
///   `X::Assignment::RO`, "Cannot modify an immutable TYPE (VALUE)".
///
/// Recording the kind where the readonly-ness is *decided* keeps the three
/// apart without any name-based guessing at the (single, shared) check site.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub(crate) enum ReadonlyKind {
    /// Readonly binding with a container behind it: parameters, `for` aliases.
    Alias,
    /// Sigiled variable bound directly to an immutable value (no container).
    Immutable,
    /// The name *is* an immutable value (sigilless term, immutable container).
    ImmutableValue,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub(crate) enum Stmt {
    VarDecl {
        name: String,
        expr: Expr,
        type_constraint: Option<String>,
        is_state: bool,
        is_our: bool,
        is_dynamic: bool,
        is_export: bool,
        export_tags: Vec<String>,
        /// Custom variable `is` traits as `(trait_name, optional_arg_expr)`.
        custom_traits: Vec<(String, Option<Expr>)>,
        /// Optional `where` constraint expression for inline subset typing
        where_constraint: Option<Box<Expr>>,
    },
    /// Mark a variable as readonly (used for `:=` binding desugaring).
    /// The [`ReadonlyKind`] records *why* it is readonly, which decides the
    /// exception Rakudo reports for an assignment through it.
    MarkReadonly(String, ReadonlyKind),
    /// Mark a container variable as `:=`-bound via `__mutsu_bound::NAME` env key.
    /// Distinguishes a bound container (writable as a whole, propagating to the
    /// bound source) from a genuinely readonly `constant` container — both end
    /// up in `readonly_vars`, so a separate marker is needed.
    MarkBoundContainer(String),
    /// Flag that the next VarDecl in this SyntheticBlock uses `:=` binding.
    MarkBind,
    /// Mark a sigilless variable as readonly via `__mutsu_sigilless_readonly::NAME` env key.
    MarkSigillessReadonly(String),
    /// Register a sigilless variable name in the compiler's `sigilless_locals`
    /// (so a bare-word read resolves to its local slot) WITHOUT marking it
    /// readonly. Used for a *typed* sigilless bind (`my Int \d := 7`), which keeps
    /// the container's mutability but must still read from the slot, not `env`.
    MarkSigilless(String),
    Assign {
        name: String,
        expr: Expr,
        op: AssignOp,
    },
    SubDecl {
        name: Symbol,
        name_expr: Option<Expr>,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        return_type: Option<String>,
        associativity: Option<String>,
        precedence_trait: Option<(String, String)>,
        signature_alternates: Vec<(Vec<String>, Vec<ParamDef>)>,
        body: Vec<Stmt>,
        multi: bool,
        is_rw: bool,
        is_raw: bool,
        is_export: bool,
        export_tags: Vec<String>,
        is_test_assertion: bool,
        supersede: bool,
        /// Custom `is` traits (non-builtin trait names like `me'd`) with optional argument expression
        custom_traits: Vec<(String, Option<Expr>)>,
    },
    TokenDecl {
        name: Symbol,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        body: Vec<Stmt>,
        multi: bool,
        /// `my token foo` — lexically scoped; a duplicate is X::Redeclaration.
        is_my: bool,
        /// `our token foo` — package scoped; a duplicate is X::Redeclaration.
        is_our: bool,
    },
    RuleDecl {
        name: Symbol,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        body: Vec<Stmt>,
        multi: bool,
    },
    #[allow(dead_code)]
    ProtoToken {
        name: Symbol,
    },
    Package {
        name: Symbol,
        body: Vec<Stmt>,
        /// The declarator keyword used (`module`, `package`, `grammar`), which
        /// determines `package-kind` in the X::Attribute::Package error raised
        /// when a `has` attribute is declared in this package's body.
        kind: PackageKind,
        /// True for `unit module Foo;` / `unit package Foo;` where the scope
        /// extends to the rest of the enclosing scope, false for brace-scoped
        /// `package Foo { ... }`.
        is_unit: bool,
        /// True when declared with `my package` (lexically scoped).
        is_my: bool,
    },
    Return(Expr),
    For {
        iterable: Expr,
        param: Option<String>,
        param_def: Box<Option<ParamDef>>,
        params: Vec<String>,
        /// Full ParamDef list for multi-param pointy blocks (`-> $a, $b = 7`),
        /// aligned 1:1 with `params`. Empty for single-param / non-pointy loops.
        /// Carries per-param optionality and default expressions so the compiler
        /// can emit an arity check and default-value binds.
        params_def: Vec<ParamDef>,
        body: Vec<Stmt>,
        label: Option<String>,
        mode: ForMode,
        /// True when `<->` is used, making all params rw.
        rw_block: bool,
        /// True when `-> {}` (empty pointy block) was used, meaning the block
        /// explicitly declares zero parameters. Passing any argument should throw.
        explicit_zero_params: bool,
        /// True when this loop came from the `EXPR for LIST` **statement
        /// modifier** form rather than the `for LIST { ... }` block form. A
        /// modifier body is not a block: it is evaluated in the enclosing
        /// scope, so a placeholder in it (`{ say $^b for 1, 2 }`) belongs to
        /// the *enclosing* block, not to the loop. The block form is its own
        /// placeholder scope (`for @a { $^x }` gives the loop the parameter).
        #[serde(default)]
        is_statement_modifier: bool,
        /// The block references `&?BLOCK` and therefore needs a callable value
        /// while its ordinary, inline `ForLoop` execution is in progress.
        #[serde(default)]
        uses_block_magic: bool,
    },
    Say(Vec<Expr>),
    Put(Vec<Expr>),
    Print(Vec<Expr>),
    Note(Vec<Expr>),
    Call {
        name: Symbol,
        args: Vec<CallArg>,
    },
    Use {
        module: String,
        arg: Option<Expr>,
        /// Import tags specified as colonpairs (e.g. `:ALL`, `:others`).
        /// Empty means default import (:DEFAULT).
        tags: Vec<String>,
        /// Condition from the `if` pragma's `:if(EXPR)` adverb
        /// (`use Foo:if($cond)`): the module is loaded only when `EXPR` is true,
        /// evaluated at runtime. `None` for an unconditional `use`.
        condition: Option<Box<Expr>>,
    },
    /// `no Module ...;` — disable pragma/module effects for current lexical scope.
    No {
        module: String,
        /// Positional argument (e.g. `no Module BareWord`), if any. Mirrors
        /// `Use { arg }`; used for undeclared-symbol detection.
        arg: Option<Expr>,
    },
    /// `need Module;` — load module without importing exports
    Need {
        module: String,
    },
    /// `import Module :tag;` — import exports from an already-declared/loaded module.
    Import {
        module: String,
        tags: Vec<String>,
    },
    Subtest {
        name: Expr,
        body: Vec<Stmt>,
    },
    Block(Vec<Stmt>),
    /// Non-lexical statement sequence used by parser desugarings.
    SyntheticBlock(Vec<Stmt>),
    If {
        cond: Expr,
        then_branch: Vec<Stmt>,
        else_branch: Vec<Stmt>,
        /// Optional binding variable: `if EXPR -> $var { }`
        binding_var: Option<String>,
        /// True when this `If` is the lowering of a postfix `if`/`unless`
        /// statement modifier rather than a source `if BLOCK`. A modifier
        /// introduces no block, so its "branch" is not a block literal the
        /// enclosing scope re-clones — a `state` in it belongs to the enclosing
        /// block and must NOT restart per execution
        /// (`sub f { state $n = 0 if 1; ++$n }` counts 1, 2, 3 across calls).
        /// Mirrors `Stmt::For` / `Stmt::Given`'s flag of the same name.
        is_statement_modifier: bool,
    },
    While {
        cond: Expr,
        body: Vec<Stmt>,
        label: Option<String>,
        /// True when this `While` is the lowering of a postfix `while`/`until`
        /// statement modifier rather than a source `while COND BLOCK`. A
        /// modifier introduces no block of its own, so (ADR-0048 D4) its
        /// "body" placeholders are the enclosing block's own parameters:
        /// `sub f { say "$^a" while $i++ < 2 }; f(7)` prints 7 twice, not the
        /// condition. Mirrors `Stmt::If` / `Stmt::For` / `Stmt::Given`'s flag
        /// of the same name.
        is_statement_modifier: bool,
        /// True when the source keyword was `until`, i.e. `cond` is the
        /// parser's synthetic `!` wrapper around the written condition.
        /// ADR-0048 D4 supplies the *written* condition's value to the body
        /// (`until False { $^c }` binds `False`, raku prints `False`), so the
        /// placeholder bind has to see through that wrapper — and only for a
        /// real `until`, never for a hand-written `while !$x`, whose supplied
        /// value really is the negation.
        is_until: bool,
    },
    Loop {
        init: Option<Box<Stmt>>,
        cond: Option<Expr>,
        step: Option<Expr>,
        body: Vec<Stmt>,
        repeat: bool,
        label: Option<String>,
        /// `repeat { ... } until COND`: as for [`Stmt::While::is_until`],
        /// `cond` holds the parser's synthetic `!` wrapper and ADR-0048 D4
        /// binds the written condition's value.
        is_until: bool,
    },
    React {
        body: Vec<Stmt>,
    },
    Whenever {
        supply: Expr,
        param: Option<String>,
        /// The pointy param's declared type constraint, if any
        /// (`whenever $s -> Int $x { }`). Enforced on the emitted value at
        /// call time via the callback's `ParamDef`, same as an ordinary
        /// typed block parameter.
        param_type: Option<String>,
        body: Vec<Stmt>,
    },
    Last(Option<String>),
    Next(Option<String>),
    Redo(Option<String>),
    Proceed,
    Succeed,
    /// `done` — terminate the innermost react event loop
    ReactDone,
    /// The `supply { ... }` desugar's own terminator for a bare `done`
    /// (`src/parser/primary/ident/supply.rs`): ends just the synchronous
    /// execution of the enclosing on-demand body/whenever closure, never a
    /// routine-level `return`. Kept distinct from both `Return` (so it can't
    /// be mistaken for a user `return` and mis-target an enclosing method,
    /// see `todo/tickets/supply-done-in-method-supply-block-escapes-as-cx-return.md`)
    /// and `ReactDone` (so it never terminates an *enclosing* react loop).
    SupplyBodyDone,
    Given {
        topic: Expr,
        body: Vec<Stmt>,
        /// True for postfix statement-modifier `STMT given EXPR`. Unlike the
        /// block form, a modifier does not introduce a lexical scope.
        is_statement_modifier: bool,
    },
    When {
        cond: Expr,
        body: Vec<Stmt>,
        /// True for the postfix `STMT when COND` spelling. Rakudo lowers that
        /// modifier to a plain conditional (`COND.ACCEPTS($_) ?? STMT !! Nil`),
        /// so it is NOT a `when` *clause*: it never abandons the enclosing
        /// block on a match, and — the observable difference this flag exists
        /// for — a `proceed` raised inside it is not consumed by it but keeps
        /// unwinding to the nearest real `when` clause. mutsu builds the
        /// modifier as a synthetic `Given { is_statement_modifier: true }`
        /// wrapping this `When` so the match's `succeed` still has a catcher;
        /// this flag stops the `When` itself from swallowing a `proceed`.
        is_statement_modifier: bool,
    },
    Default(Vec<Stmt>),
    Die(Expr),
    Fail(Expr),
    Catch(Vec<Stmt>),
    Control(Vec<Stmt>),
    /// `take` / `take-rw`. The bool is `is_rw`: a `take-rw` of an lvalue captures
    /// the source container (a shared `ContainerRef` cell) so the gathered value
    /// keeps container identity with the original (`=:=`), instead of a snapshot.
    Take(Expr, bool),
    Goto(Expr),
    Label {
        name: String,
        stmt: Box<Stmt>,
    },
    EnumDecl {
        name: Symbol,
        variants: Vec<(String, Option<Expr>)>,
        is_export: bool,
        /// Whether declared with an explicit `my` scope (lexical). A `my enum`
        /// is private to its enclosing scope and, unlike a default our-scoped
        /// enum, is allowed inside a role body.
        is_my: bool,
        /// Base type constraint (e.g., `my Str enum ...` has base_type = Some("Str"))
        base_type: Option<String>,
        /// Roles composed by a `does Role` clause on the declaration
        /// (`enum Flags does Weird (A => 1)`), in declaration order.
        roles: Vec<String>,
        /// Language version active when this enum was declared (e.g., "6.c", "6.d", "6.e")
        language_version: String,
    },
    ClassDecl {
        name: Symbol,
        name_expr: Option<Expr>,
        parents: Vec<String>,
        class_is_rw: bool,
        is_hidden: bool,
        is_lexical: bool,
        hidden_parents: Vec<String>,
        does_parents: Vec<String>,
        repr: Option<String>,
        body: Vec<Stmt>,
        /// Language version active when this class was declared (e.g., "6.c", "6.d", "6.e")
        language_version: String,
        /// Custom `is` traits with optional arguments, dispatched via `trait_mod:<is>`
        custom_traits: Vec<(String, Option<Expr>)>,
        /// Whether this class was declared with `unit class` (file-scoped body)
        is_unit: bool,
        /// Stable per-declaration-site id (parse-time assigned, non-zero) used to
        /// distinguish same-named lexical (`my`) classes in different scopes.
        /// 0 means "no stable site" (runtime-synthesized or deserialized node).
        #[serde(skip)]
        decl_id: u64,
        /// Parsed argument expressions for a bracketed `is`/`does`/`hides`
        /// parent (`is Parent[Args]`), keyed by the full concatenated parent
        /// string that also appears in `parents`/`does_parents`/
        /// `hidden_parents` (ADR-0019 D4-1). An entry is present only when
        /// the bracket content parsed cleanly as a comma-separated
        /// expression list; the concatenated string in the other fields
        /// remains the sole authoritative source for the parent name/
        /// registry key either way — this is a purely additive capture with
        /// no consumer yet (D4-2/D4-3).
        #[serde(default)]
        parent_args: Vec<(String, Vec<Expr>)>,
    },
    HasDecl {
        name: Symbol,
        is_public: bool,
        default: Option<Expr>,
        handles: Vec<HandleSpec>,
        #[allow(dead_code)]
        is_rw: bool,
        is_readonly: bool,
        type_constraint: Option<String>,
        /// Type smiley: "D", "U", or "_" (from `Int:D`, `Int:U`, `Int:_`)
        type_smiley: Option<String>,
        /// `is required` trait: None = not required, Some(None) = required,
        /// Some(Some(reason)) = required with reason string
        is_required: Option<Option<String>>,
        /// Sigil of the attribute: '$', '@', or '%'
        sigil: char,
        /// Optional `where` constraint expression
        where_constraint: Option<Box<Expr>>,
        /// `has $x` (no twigil) creates an alias: `$x` → `$!x` inside the class
        is_alias: bool,
        /// `our $.x` — package-scoped class attribute (shared across instances)
        is_our: bool,
        /// `my $.x` — lexically-scoped class attribute (shared across instances)
        is_my: bool,
        /// `is default(expr)` trait — the value to restore when Nil is assigned.
        /// When set, this value should be used both as the default for `.VAR.default`
        /// and as the restore value when Nil is assigned to the attribute.
        /// Distinct from `default` which may be an explicit `= expr` initializer.
        is_default: Option<Expr>,
        /// `is Type` trait — container type for `@`/`%` attributes (e.g. `is Buf`, `is BagHash`)
        is_type: Option<String>,
        /// `is DEPRECATED` message: None = not deprecated, Some("") = deprecated without message,
        /// Some(msg) = deprecated with custom message.
        deprecated_message: Option<String>,
        is_built: Option<bool>,
        /// Unknown traits: list of `(kind, name, arg)` tuples for unknown trait
        /// applications (e.g., `is bar` -> `("is", "bar", None)`, `is doc('x')` ->
        /// `("is", "doc", Some(<'x'>))`, `will bar` -> `("will", "bar", None)`).
        /// If a user-defined `trait_mod:<is>` can handle the trait it is dispatched
        /// to that sub at class registration; otherwise this causes an
        /// `X::Comp::Trait::Unknown` error.
        unknown_traits: Vec<(String, String, Option<Expr>)>,
    },
    MethodDecl {
        name: Symbol,
        name_expr: Option<Expr>,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        body: Vec<Stmt>,
        multi: bool,
        is_rw: bool,
        /// `is raw` trait. Together with `is_rw` and a `return-rw` in the body
        /// this forms the one rw-capability oracle a method is asked about
        /// (`Interpreter::method_is_rw_capable`, ADR-0067 slice 2) — the same
        /// rule `FunctionDef` already states for a `sub`.
        is_raw: bool,
        is_private: bool,
        is_our: bool,
        is_my: bool,
        /// True for `submethod` declarations (not inherited, but dispatched on own class).
        /// Distinct from `is_my` which means `my method` (lexical, not in method table).
        is_submethod: bool,
        /// True for `our &name = method name(...) { ... }` form.
        /// Unlike `our method name()`, this form keeps the method in the class
        /// method table in addition to registering it as a package function.
        our_variable_form: bool,
        return_type: Option<String>,
        /// `is default` trait for multi dispatch tie-breaking.
        is_default_candidate: bool,
        /// `is DEPRECATED` message (None = not deprecated)
        deprecated_message: Option<String>,
        /// `handles` specifications on this method: when set, this method acts
        /// as a delegator source. For each spec, a forwarder method is
        /// synthesized at class-registration time that calls
        /// `self.<this-method>.<exposed>(|args)`.
        handles: Vec<HandleSpec>,
        /// Custom `is` traits (non-builtin trait names) with optional argument expression
        custom_traits: Vec<(String, Option<Expr>)>,
        /// `is export` on the method: when a class/role is imported, exported
        /// methods are made available as their sub-form (e.g. operator subs).
        is_export: bool,
        export_tags: Vec<String>,
    },
    RoleDecl {
        name: Symbol,
        type_params: Vec<String>,
        type_param_defs: Vec<ParamDef>,
        is_export: bool,
        export_tags: Vec<String>,
        body: Vec<Stmt>,
        /// Whether this role was declared with `is rw` or `also is rw`
        is_rw: bool,
        /// Language version active when this role was declared (e.g., "6.c", "6.d", "6.e")
        language_version: String,
        /// Custom `is` traits with optional arguments, dispatched via `trait_mod:<is>`
        custom_traits: Vec<(String, Option<Expr>)>,
    },
    DoesDecl {
        name: Symbol,
        /// Parsed argument expressions for a bracketed role application
        /// (`does Role[Args]`), if the bracket content parsed cleanly as a
        /// comma-separated expression list (ADR-0019 D4-1). `name` (which
        /// carries the full `Role[Args]` string) remains the sole
        /// authoritative source for the role name/registry key either way —
        /// purely additive, no consumer yet (D4-2/D4-3/D7-3).
        #[serde(default)]
        args: Option<Vec<Expr>>,
    },
    TrustsDecl {
        name: Symbol,
    },
    AugmentClass {
        name: Symbol,
        body: Vec<Stmt>,
        /// Roles composed onto the augmented type via `does Role` on the augment
        /// declaration itself (`augment class Str does Rotate { }`). Their methods
        /// are mixed into the existing (builtin or user) class.
        does_roles: Vec<Symbol>,
        /// True when declared with `augment role ...` (roles are always closed,
        /// so augmenting one is illegal); false for `augment class ...`.
        is_role: bool,
    },
    SubsetDecl {
        name: Symbol,
        base: String,
        predicate: Option<Expr>,
        version: String,
        is_export: bool,
        export_tags: Vec<String>,
        /// `my subset F ...` — lexically scoped: NOT reachable (nor
        /// registered) under the enclosing package's qualified name.
        is_my: bool,
    },
    Phaser {
        kind: PhaserKind,
        body: Vec<Stmt>,
        /// Verbatim source text of a `PRE`/`POST` phaser's condition — the
        /// block including its braces (`{ $x ~~ Int }`), or the bare statement
        /// of the `PRE 0` form. `X::Phaser::PrePost.condition` is exactly this
        /// text, and its message quotes it ("Precondition '...' failed"), so it
        /// has to be captured while the source slice is still in hand. `None`
        /// for every other phaser kind, which has no condition.
        condition: Option<Symbol>,
    },
    ProtoDecl {
        name: Symbol,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        return_type: Option<String>,
        body: Vec<Stmt>,
        is_export: bool,
        custom_traits: Vec<String>,
        /// True when declared as `proto method`/`proto submethod` (inside a
        /// class/role body). Such a proto registers a method-level proto body
        /// whose `{*}` dispatches to the matching multi method candidate,
        /// rather than a package-level proto sub.
        is_method: bool,
        /// True for `our proto sub`. The proto is the one *visible* name of a
        /// multi (its candidates are lexical), so `our` on it makes the whole
        /// routine a package symbol: `module M { our proto sub f(|) {*} }` puts
        /// `&f` in `M::` and `::('M::&f')` resolves.
        is_our: bool,
    },
    Let {
        name: String,
        index: Option<Box<Expr>>,
        value: Option<Box<Expr>>,
        is_temp: bool,
        undefine_first: bool,
    },
    TempMethodAssign {
        var_name: String,
        method_name: String,
        method_args: Vec<Expr>,
        value: Expr,
    },
    /// Set the current source line number (for deprecation tracking, etc.).
    SetLine(i64),
    Expr(Expr),
}

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub(crate) enum AssignOp {
    Assign,
    Bind,
    #[allow(dead_code)]
    MatchAssign,
}

/// Scan a statement list for placeholder variables (names starting with `^`,
/// e.g. `$^a`, `$^b`) and return their sorted, deduplicated names.
///
/// This is used at parse time to detect implicit block parameters so that
/// constructs like `{ $^a + $^b }` automatically introduce parameters.
/// Collect the names a routine body binds *locally* — `for`/`while`/`loop`/`given`
/// pointy parameters and every nested `my` declaration — so the interpreter's
/// return env merge does not write them back over a same-named *caller* lexical.
/// Without this, a routine that recurses into a same-named `for` loop and
/// early-returns (e.g. Zef's `system-collapse`) leaks its inner loop parameter's
/// last value into the caller (the caller's loop variable / hash key is corrupted).
/// Scalar names only (matching the caller's scalar-writeback filter); `@`/`%`
/// binders are handled by the Array/Hash writeback path.
/// Collect every `my`-declared lexical name (scalars, arrays, hashes) that a
/// body introduces, recursing through the control-flow constructs whose bodies
/// run in the *same* env scope (for/while/loop/if/block/given/when/gather/...),
/// but NOT into nested `sub`/method/closure bodies (those are separate scopes).
///
/// Used to seed `CompiledCode::env_only_decls` so the method-dispatch return
/// merge treats a `my @x` declared inside a deferred body (e.g. a `gather` block,
/// stashed in `stmt_pool` and run by-name against the method env) as method-local
/// and does not leak it into a same-named caller lexical across (self-)recursion.
pub(crate) fn collect_all_my_decl_names(
    stmts: &[Stmt],
    out: &mut std::collections::HashSet<String>,
) {
    fn add(name: &str, out: &mut std::collections::HashSet<String>) {
        let bare = name.strip_prefix('\\').unwrap_or(name);
        if !bare.is_empty() {
            out.insert(bare.to_string());
        }
    }
    // A `my` declaration can hide inside a *condition* expression — e.g.
    // `next unless my @x = ...` parses to `If { cond: !DoStmt(VarDecl @x) }`,
    // and `while my $x = ...` puts the decl in the loop condition. Walk the
    // condition Expr for embedded statement bodies (DoStmt/Block/Gather) so those
    // env-only lexicals are collected too. Stops at closure boundaries (Lambda /
    // AnonSub) — those are separate scopes.
    fn add_from_expr(expr: &Expr, out: &mut std::collections::HashSet<String>) {
        match expr {
            Expr::DoStmt(s) => collect_all_my_decl_names(std::slice::from_ref(s), out),
            Expr::Block(stmts) | Expr::Gather(stmts) => collect_all_my_decl_names(stmts, out),
            Expr::DoBlock { body, .. } => collect_all_my_decl_names(body, out),
            Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => add_from_expr(expr, out),
            Expr::Binary { left, right, .. } => {
                add_from_expr(left, out);
                add_from_expr(right, out);
            }
            _ => {}
        }
    }
    for stmt in stmts {
        match stmt {
            Stmt::VarDecl { name, .. } => add(name, out),
            Stmt::For {
                param,
                params,
                body,
                ..
            } => {
                if let Some(p) = param {
                    add(p, out);
                }
                for p in params {
                    add(p, out);
                }
                collect_all_my_decl_names(body, out);
            }
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                binding_var,
                ..
            } => {
                add_from_expr(cond, out);
                if let Some(v) = binding_var {
                    add(v, out);
                }
                collect_all_my_decl_names(then_branch, out);
                collect_all_my_decl_names(else_branch, out);
            }
            Stmt::While { cond, body, .. } => {
                add_from_expr(cond, out);
                collect_all_my_decl_names(body, out);
            }
            Stmt::Loop { body, .. }
            | Stmt::React { body }
            | Stmt::Block(body)
            | Stmt::SyntheticBlock(body)
            | Stmt::Default(body)
            | Stmt::Catch(body)
            | Stmt::Control(body) => collect_all_my_decl_names(body, out),
            Stmt::Given { body, .. } | Stmt::When { body, .. } => {
                collect_all_my_decl_names(body, out)
            }
            Stmt::Whenever { body, .. } => collect_all_my_decl_names(body, out),
            Stmt::Label { stmt, .. } => collect_all_my_decl_names(std::slice::from_ref(stmt), out),
            _ => {}
        }
    }
}

pub(crate) fn collect_routine_body_local_names(
    stmts: &[Stmt],
    out: &mut std::collections::HashSet<String>,
) {
    fn add_scalar(name: &str, out: &mut std::collections::HashSet<String>) {
        let bare = name.strip_prefix('\\').unwrap_or(name);
        if !bare.is_empty() && !bare.starts_with('@') && !bare.starts_with('%') {
            out.insert(bare.to_string());
        }
    }
    // A `my` can hide inside a *condition* expression — `if (my $d = ...)` /
    // `while my $x = ...` parse the decl into the cond (as `DoStmt(VarDecl)`),
    // not the branch body. Without walking it, the callee's `$d` is not
    // recognized as routine-local and the return env merge writes it back over
    // a same-named caller lexical (found via Text::CSV's `csv()`, whose
    // `if (my $file = %args<file>:delete)` clobbered the caller's `$file`).
    // Mirrors `collect_all_my_decl_names::add_from_expr`; stops at closure
    // boundaries the same way.
    fn add_from_cond(expr: &Expr, out: &mut std::collections::HashSet<String>) {
        match expr {
            Expr::DoStmt(s) => collect_routine_body_local_names(std::slice::from_ref(s), out),
            Expr::Block(stmts) | Expr::Gather(stmts) => {
                collect_routine_body_local_names(stmts, out)
            }
            Expr::DoBlock { body, .. } => collect_routine_body_local_names(body, out),
            Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => add_from_cond(expr, out),
            Expr::Binary { left, right, .. } => {
                add_from_cond(left, out);
                add_from_cond(right, out);
            }
            _ => {}
        }
    }
    for stmt in stmts {
        match stmt {
            Stmt::VarDecl { name, .. } => add_scalar(name, out),
            Stmt::For {
                param,
                params,
                body,
                ..
            } => {
                if let Some(p) = param {
                    add_scalar(p, out);
                }
                for p in params {
                    add_scalar(p, out);
                }
                collect_routine_body_local_names(body, out);
            }
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                binding_var,
                ..
            } => {
                add_from_cond(cond, out);
                if let Some(v) = binding_var {
                    add_scalar(v, out);
                }
                collect_routine_body_local_names(then_branch, out);
                collect_routine_body_local_names(else_branch, out);
            }
            Stmt::While { cond, body, .. } => {
                add_from_cond(cond, out);
                collect_routine_body_local_names(body, out);
            }
            Stmt::Loop { body, .. }
            | Stmt::React { body }
            | Stmt::Block(body)
            | Stmt::SyntheticBlock(body)
            | Stmt::Default(body)
            | Stmt::Catch(body)
            | Stmt::Control(body) => collect_routine_body_local_names(body, out),
            Stmt::Given { body, .. } | Stmt::When { body, .. } => {
                collect_routine_body_local_names(body, out)
            }
            Stmt::Whenever { body, .. } => collect_routine_body_local_names(body, out),
            Stmt::Label { stmt, .. } => {
                collect_routine_body_local_names(std::slice::from_ref(stmt), out)
            }
            _ => {}
        }
    }
}

pub(crate) fn collect_placeholders(stmts: &[Stmt]) -> Vec<String> {
    let mut names = Vec::new();
    for stmt in stmts {
        collect_ph_stmt(stmt, &mut names);
    }
    // Sort by the name component (strip & and ^ prefixes) so that
    // $^a, @^a, %^a, &^a sort as a regardless of sigil.
    names.sort_by(|a, b| {
        let a_name = placeholder_sort_key(a);
        let b_name = placeholder_sort_key(b);
        a_name.cmp(b_name)
    });
    names.dedup();
    names
}

/// Collect placeholders without recursing into nested closures (AnonSubParams,
/// Lambda, AnonSub).  Used by Stmt::Block compilation and VM MakeAnonSub to
/// determine the block's own params without picking up placeholders from
/// nested closures assigned to variables.
pub(crate) fn collect_placeholders_shallow(stmts: &[Stmt]) -> Vec<String> {
    let mut names = Vec::new();
    for stmt in stmts {
        collect_ph_stmt_shallow(stmt, &mut names);
    }
    names.sort_by(|a, b| {
        let a_name = placeholder_sort_key(a);
        let b_name = placeholder_sort_key(b);
        a_name.cmp(b_name)
    });
    names.dedup();
    names
}

/// Collect placeholder variables that belong directly to the *current*
/// (non-signature) scope: the mainline, a `do {}` block, or a class/role/module
/// body. Descends through expressions and statement header positions but stops
/// at any nested `{}` block (control-flow bodies, bare/pointy blocks, closures,
/// do-blocks, gather/try/phasers) since those introduce their own placeholder
/// scope and capture the placeholders inside them.
///
/// Also recognizes the implicit slurpy placeholders `@_` / `%_`. Returns
/// display names like `$^x`, `@^a`, `@_`. False negatives are safe (we just
/// miss raising an error); a positive means a placeholder is genuinely used
/// where no signature can capture it.
pub(crate) fn collect_unattached_placeholders(stmts: &[Stmt]) -> Vec<String> {
    let mut names = Vec::new();
    for stmt in stmts {
        collect_unattached_ph_stmt(stmt, &mut names);
    }
    names
}

/// Collect placeholder names (`^name`, sigil-stripped) that appear as the
/// *target* of an assignment inside a `where`-block body — e.g. the `^epic` in
/// `where { $^epic = "fail" }`. The ordinary `collect_placeholders` walks only
/// expression positions, so an assign-only placeholder is otherwise invisible.
/// A `where`-block parameter is read-only, so the caller binds these and marks
/// them read-only to make `where { $^x = ... }` die.
pub(crate) fn collect_where_assign_placeholders(stmts: &[Stmt]) -> Vec<String> {
    let mut names = Vec::new();
    for stmt in stmts {
        collect_assign_ph_stmt(stmt, &mut names);
    }
    names.dedup();
    names
}

fn push_if_placeholder(name: &str, out: &mut Vec<String>) {
    let bare = name.trim_start_matches(|c: char| "$@%&".contains(c));
    if let Some(rest) = bare.strip_prefix('^') {
        let key = format!("^{rest}");
        if !out.contains(&key) {
            out.push(key);
        }
    }
}

fn collect_assign_ph_stmt(stmt: &Stmt, out: &mut Vec<String>) {
    match stmt {
        Stmt::Assign { name, expr, .. } | Stmt::VarDecl { name, expr, .. } => {
            push_if_placeholder(name, out);
            collect_assign_ph_expr(expr, out);
        }
        Stmt::Expr(e)
        | Stmt::Return(e)
        | Stmt::Die(e)
        | Stmt::Fail(e)
        | Stmt::Take(e, _)
        | Stmt::Goto(e) => collect_assign_ph_expr(e, out),
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            ..
        } => {
            collect_assign_ph_expr(cond, out);
            for s in then_branch.iter().chain(else_branch.iter()) {
                collect_assign_ph_stmt(s, out);
            }
        }
        _ => {}
    }
}

fn collect_assign_ph_expr(expr: &Expr, out: &mut Vec<String>) {
    match expr {
        Expr::AssignExpr { name, expr, .. } => {
            push_if_placeholder(name, out);
            collect_assign_ph_expr(expr, out);
        }
        Expr::Binary { left, right, .. } => {
            collect_assign_ph_expr(left, out);
            collect_assign_ph_expr(right, out);
        }
        Expr::Unary { expr, .. } | Expr::Grouped(expr) => collect_assign_ph_expr(expr, out),
        Expr::Block(body) | Expr::AnonSub { body, .. } => {
            for s in body {
                collect_assign_ph_stmt(s, out);
            }
        }
        _ => {}
    }
}

fn collect_unattached_ph_stmt(stmt: &Stmt, out: &mut Vec<String>) {
    match stmt {
        Stmt::Expr(e)
        | Stmt::Return(e)
        | Stmt::Die(e)
        | Stmt::Fail(e)
        | Stmt::Take(e, _)
        | Stmt::Goto(e) => collect_unattached_ph_expr(e, out),
        Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => {
            collect_unattached_ph_expr(expr, out)
        }
        Stmt::Say(es) | Stmt::Put(es) | Stmt::Print(es) | Stmt::Note(es) => {
            for e in es {
                collect_unattached_ph_expr(e, out);
            }
        }
        Stmt::Call { args, .. } => {
            for arg in args {
                match arg {
                    CallArg::Positional(e) | CallArg::Invocant(e) | CallArg::Slip(e) => {
                        collect_unattached_ph_expr(e, out)
                    }
                    CallArg::Named { value: Some(e), .. } => collect_unattached_ph_expr(e, out),
                    CallArg::Named { value: None, .. } => {}
                }
            }
        }
        // Control-flow headers are evaluated in the current scope, but their
        // bodies are signature-capable blocks -> do NOT descend into bodies.
        //
        // Unlike `collect_ph_stmt_shallow`, this is a deliberately NARROWER,
        // conservative walk (see the module-level doc above: "False
        // negatives are safe"): it never descends an `If`/`While`/`When`/
        // `Given` body at all, even for an `If`/`Given` statement modifier
        // where `collect_ph_stmt_shallow` (and `placeholder_body_kind`)
        // would say `Transparent`. Only `For`'s modifier form below is
        // oracle-driven, because it is the only construct where this walk
        // already had a body-descend decision to make; extending the same
        // treatment to `If`/`Given` (or to non-modifier While/When/Loop/
        // React/etc. bodies that `placeholder_body_kind` classifies
        // `Transparent`) would newly detect placeholders this function has
        // never looked for, which is an observable behaviour change
        // (ADR-0048 Phase 1 must not make one) — left for a later phase.
        Stmt::If { cond, .. } | Stmt::While { cond, .. } | Stmt::When { cond, .. } => {
            collect_unattached_ph_expr(cond, out)
        }
        Stmt::For { iterable, body, .. } => {
            collect_unattached_ph_expr(iterable, out);
            // A `for` statement modifier is not a block — its body runs in the
            // enclosing scope, so a placeholder there is unattached here too.
            if matches!(
                placeholder_body_kind(stmt),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    collect_unattached_ph_stmt(s, out);
                }
            }
        }
        Stmt::Given { topic, .. } => collect_unattached_ph_expr(topic, out),
        Stmt::Label { stmt, .. } => collect_unattached_ph_stmt(stmt, out),
        // Everything else (blocks, sub/class decls, ...) is a boundary.
        _ => {}
    }
}

fn collect_unattached_ph_expr(expr: &Expr, out: &mut Vec<String>) {
    let push = |name: String, out: &mut Vec<String>| {
        if !out.contains(&name) {
            out.push(name);
        }
    };
    match expr {
        Expr::Var(name) if name.starts_with('^') || name.starts_with(':') => {
            push(format!("${}", name), out)
        }
        Expr::CodeVar(name) if name.starts_with('^') => push(format!("&{}", name), out),
        Expr::ArrayVar(name) if name.starts_with('^') || name.starts_with(':') => {
            push(format!("@{}", name), out)
        }
        Expr::HashVar(name) if name.starts_with('^') || name.starts_with(':') => {
            push(format!("%{}", name), out)
        }
        // Implicit slurpy placeholders.
        Expr::ArrayVar(name) if name == "_" => push("@_".to_string(), out),
        Expr::HashVar(name) if name == "_" => push("%_".to_string(), out),
        Expr::Binary { left, right, .. } => {
            collect_unattached_ph_expr(left, out);
            collect_unattached_ph_expr(right, out);
        }
        Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => {
            collect_unattached_ph_expr(expr, out)
        }
        Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
            collect_unattached_ph_expr(target, out);
            for a in args {
                collect_unattached_ph_expr(a, out);
            }
        }
        Expr::Call { args, .. } | Expr::UserRoutineCall { args, .. } => {
            for a in args {
                collect_unattached_ph_expr(a, out);
            }
        }
        Expr::CallOn { target, args } => {
            collect_unattached_ph_expr(target, out);
            for a in args {
                collect_unattached_ph_expr(a, out);
            }
        }
        Expr::Index { target, index, .. } => {
            collect_unattached_ph_expr(target, out);
            collect_unattached_ph_expr(index, out);
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            collect_unattached_ph_expr(cond, out);
            collect_unattached_ph_expr(then_expr, out);
            collect_unattached_ph_expr(else_expr, out);
        }
        Expr::AssignExpr { expr, .. }
        | Expr::PositionalPair(expr)
        | Expr::ZenSlice(expr)
        | Expr::Grouped(expr) => collect_unattached_ph_expr(expr, out),
        Expr::ArrayLiteral(es)
        | Expr::BracketArray(es, _)
        | Expr::StringInterpolation(es)
        | Expr::CaptureLiteral(es) => {
            for e in es {
                collect_unattached_ph_expr(e, out);
            }
        }
        Expr::Hash(pairs) => {
            for (_, v) in pairs {
                if let Some(e) = v {
                    collect_unattached_ph_expr(e, out);
                }
            }
        }
        // Closures and nested blocks define their own placeholder scope: stop.
        _ => {}
    }
}

/// Find the first virtual accessor call (`$.attr` / `@.attr` / `%.attr`) used in
/// an attribute initializer expression. Such a call dereferences the
/// partially-constructed invocant and is X::Syntax::VirtualCall. Descends into
/// bare blocks (block initializers) but stops at sub/method/class boundaries,
/// which rebind the invocant.
pub(crate) fn first_virtual_call_in_expr(expr: &Expr) -> Option<String> {
    let mut found = None;
    collect_virtual_call_expr(expr, &mut found);
    found
}

fn collect_virtual_call_expr(expr: &Expr, out: &mut Option<String>) {
    if out.is_some() {
        return;
    }
    let hit = |sigil: char, name: &str, out: &mut Option<String>| {
        if out.is_none() && name.starts_with('.') {
            *out = Some(format!("{}{}", sigil, name));
        }
    };
    match expr {
        Expr::Var(name) => hit('$', name, out),
        Expr::ArrayVar(name) => hit('@', name, out),
        Expr::HashVar(name) => hit('%', name, out),
        Expr::Binary { left, right, .. } => {
            collect_virtual_call_expr(left, out);
            collect_virtual_call_expr(right, out);
        }
        Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => {
            collect_virtual_call_expr(expr, out)
        }
        Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
            collect_virtual_call_expr(target, out);
            for a in args {
                collect_virtual_call_expr(a, out);
            }
        }
        Expr::Call { args, .. } | Expr::UserRoutineCall { args, .. } => {
            for a in args {
                collect_virtual_call_expr(a, out);
            }
        }
        Expr::CallOn { target, args } => {
            collect_virtual_call_expr(target, out);
            for a in args {
                collect_virtual_call_expr(a, out);
            }
        }
        Expr::Index { target, index, .. } => {
            collect_virtual_call_expr(target, out);
            collect_virtual_call_expr(index, out);
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            collect_virtual_call_expr(cond, out);
            collect_virtual_call_expr(then_expr, out);
            collect_virtual_call_expr(else_expr, out);
        }
        Expr::AssignExpr { expr, .. } | Expr::PositionalPair(expr) | Expr::ZenSlice(expr) => {
            collect_virtual_call_expr(expr, out)
        }
        Expr::ArrayLiteral(es)
        | Expr::BracketArray(es, _)
        | Expr::StringInterpolation(es)
        | Expr::CaptureLiteral(es) => {
            for e in es {
                collect_virtual_call_expr(e, out);
            }
        }
        Expr::Hash(pairs) => {
            for (_, v) in pairs {
                if let Some(e) = v {
                    collect_virtual_call_expr(e, out);
                }
            }
        }
        // A bare block initializer (`has $.x = { $.y }`) is still evaluated on the
        // partially-constructed object, so descend into it.
        Expr::AnonSub {
            body,
            is_block: true,
            ..
        } => {
            for stmt in body {
                collect_virtual_call_stmt(stmt, out);
            }
        }
        // Real subs/methods/classes rebind the invocant: stop.
        _ => {}
    }
}

fn collect_virtual_call_stmt(stmt: &Stmt, out: &mut Option<String>) {
    if out.is_some() {
        return;
    }
    match stmt {
        Stmt::Expr(e)
        | Stmt::Return(e)
        | Stmt::Die(e)
        | Stmt::Fail(e)
        | Stmt::Take(e, _)
        | Stmt::Goto(e) => collect_virtual_call_expr(e, out),
        Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => {
            collect_virtual_call_expr(expr, out)
        }
        Stmt::Say(es) | Stmt::Put(es) | Stmt::Print(es) | Stmt::Note(es) => {
            for e in es {
                collect_virtual_call_expr(e, out);
            }
        }
        _ => {}
    }
}

fn placeholder_sort_key(name: &str) -> &str {
    let without_sigil = if let Some(first) = name.chars().next() {
        if matches!(first, '$' | '@' | '%' | '&') {
            &name[first.len_utf8()..]
        } else {
            name
        }
    } else {
        name
    };
    if let Some(stripped) = without_sigil.strip_prefix('^') {
        stripped
    } else if let Some(stripped) = without_sigil.strip_prefix(':') {
        stripped
    } else {
        without_sigil
    }
}

fn collect_ph_stmt(stmt: &Stmt, out: &mut Vec<String>) {
    match stmt {
        Stmt::Expr(e)
        | Stmt::Return(e)
        | Stmt::Die(e)
        | Stmt::Fail(e)
        | Stmt::Take(e, _)
        | Stmt::Goto(e) => {
            collect_ph_expr(e, out);
        }
        Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => collect_ph_expr(expr, out),
        Stmt::Call { args, .. } => {
            for arg in args {
                match arg {
                    CallArg::Positional(e) | CallArg::Invocant(e) => collect_ph_expr(e, out),
                    CallArg::Named { value: Some(e), .. } => collect_ph_expr(e, out),
                    CallArg::Named { value: None, .. } => {}
                    CallArg::Slip(e) => collect_ph_expr(e, out),
                }
            }
        }
        Stmt::Say(es) | Stmt::Put(es) | Stmt::Print(es) | Stmt::Note(es) => {
            for e in es {
                collect_ph_expr(e, out);
            }
        }
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            ..
        } => {
            collect_ph_expr(cond, out);
            for s in then_branch {
                collect_ph_stmt(s, out);
            }
            for s in else_branch {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::While { cond, body, .. } => {
            collect_ph_expr(cond, out);
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::For { iterable, body, .. } => {
            collect_ph_expr(iterable, out);
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::Loop { body, .. } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::React { body } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::Whenever { supply, body, .. } => {
            collect_ph_expr(supply, out);
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::Block(body)
        | Stmt::SyntheticBlock(body)
        | Stmt::Default(body)
        | Stmt::Catch(body)
        | Stmt::Control(body)
        | Stmt::RoleDecl { body, .. } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::Phaser { body, .. } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::Given { topic, body, .. } => {
            collect_ph_expr(topic, out);
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::When { cond, body, .. } => {
            collect_ph_expr(cond, out);
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::Let { value, index, .. } => {
            if let Some(e) = value {
                collect_ph_expr(e, out);
            }
            if let Some(e) = index {
                collect_ph_expr(e, out);
            }
        }
        Stmt::TempMethodAssign {
            method_args, value, ..
        } => {
            for e in method_args {
                collect_ph_expr(e, out);
            }
            collect_ph_expr(value, out);
        }
        Stmt::Label { stmt, .. } => {
            collect_ph_stmt(stmt, out);
        }
        Stmt::ProtoDecl { .. } => {}
        Stmt::DoesDecl { .. } => {}
        Stmt::TrustsDecl { .. } => {}
        Stmt::SubsetDecl {
            predicate: Some(predicate),
            ..
        } => {
            collect_ph_expr(predicate, out);
        }
        Stmt::SubsetDecl {
            predicate: None, ..
        } => {}
        _ => {}
    }
}

/// Scan a raw `s///`/`S///` pattern or replacement *string* for placeholder
/// variables (`$^a`, `@^a`, `%^a`, `&^a`). The substitution stores its pattern
/// and replacement as un-parsed strings, so the normal expression walk never
/// sees the placeholders inside `S/5/$^a/`; this recovers them in the same name
/// format the `Expr::Var`/`ArrayVar`/… arms produce (`$`→`^a`, `@`→`@^a`, …).
fn collect_placeholders_in_str(src: &str, out: &mut Vec<String>) {
    let bytes = src.as_bytes();
    let mut i = 0;
    while i + 2 < bytes.len() {
        let sigil = bytes[i];
        if matches!(sigil, b'$' | b'@' | b'%' | b'&')
            && bytes[i + 1] == b'^'
            && bytes[i + 2].is_ascii_alphabetic()
        {
            let start = i + 2;
            let mut j = start;
            while j < bytes.len() && (bytes[j].is_ascii_alphanumeric() || bytes[j] == b'_') {
                j += 1;
            }
            let name = &src[start..j];
            let entry = match sigil {
                b'$' => format!("^{}", name),
                b'@' => format!("@^{}", name),
                b'%' => format!("%^{}", name),
                _ => format!("&^{}", name),
            };
            if !out.contains(&entry) {
                out.push(entry);
            }
            i = j;
        } else {
            i += 1;
        }
    }
}

fn collect_ph_expr(expr: &Expr, out: &mut Vec<String>) {
    match expr {
        Expr::Subst {
            pattern,
            replacement,
            ..
        }
        | Expr::NonDestructiveSubst {
            pattern,
            replacement,
            ..
        } => {
            collect_placeholders_in_str(pattern, out);
            collect_placeholders_in_str(replacement, out);
        }
        Expr::Var(name) if name.starts_with('^') || name.starts_with(':') => {
            if !out.contains(name) {
                out.push(name.clone());
            }
        }
        Expr::CodeVar(name) if name.starts_with('^') => {
            let prefixed = format!("&{}", name);
            if !out.contains(&prefixed) {
                out.push(prefixed);
            }
        }
        Expr::ArrayVar(name) if name.starts_with('^') || name.starts_with(':') => {
            let prefixed = format!("@{}", name);
            if !out.contains(&prefixed) {
                out.push(prefixed);
            }
        }
        Expr::HashVar(name) if name.starts_with('^') || name.starts_with(':') => {
            let prefixed = format!("%{}", name);
            if !out.contains(&prefixed) {
                out.push(prefixed);
            }
        }
        Expr::Binary { left, right, .. } => {
            collect_ph_expr(left, out);
            collect_ph_expr(right, out);
        }
        // `todo/tickets/chained-compare-ast-node.md`: `{ $^a < $^b < $^c }`
        // must see every operand, same as a plain `Binary` comparison.
        Expr::ChainedCompare { operands, .. } => {
            for o in operands {
                collect_ph_expr(o, out);
            }
        }
        Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => collect_ph_expr(expr, out),
        Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
            collect_ph_expr(target, out);
            for a in args {
                collect_ph_expr(a, out);
            }
        }
        Expr::DynamicMethodCall {
            target,
            name_expr,
            args,
            ..
        }
        | Expr::HyperMethodCallDynamic {
            target,
            name_expr,
            args,
            ..
        } => {
            collect_ph_expr(target, out);
            collect_ph_expr(name_expr, out);
            for a in args {
                collect_ph_expr(a, out);
            }
        }
        Expr::Call { args, .. } | Expr::UserRoutineCall { args, .. } => {
            for a in args {
                collect_ph_expr(a, out);
            }
        }
        Expr::CallOn { target, args } => {
            collect_ph_expr(target, out);
            for a in args {
                collect_ph_expr(a, out);
            }
        }
        Expr::Index { target, index, .. } => {
            collect_ph_expr(target, out);
            collect_ph_expr(index, out);
        }
        // A placeholder can be the TARGET of an element assignment
        // (`{ $^x<a> = 3 }` — Text::CSV's on_in callbacks); without this arm
        // the block compiled with arity 0 and never bound its argument.
        Expr::IndexAssign {
            target,
            index,
            value,
            ..
        } => {
            collect_ph_expr(target, out);
            collect_ph_expr(index, out);
            collect_ph_expr(value, out);
        }
        Expr::MultiDimIndexAssign {
            target,
            dimensions,
            value, ..
        } => {
            collect_ph_expr(target, out);
            for d in dimensions {
                collect_ph_expr(d, out);
            }
            collect_ph_expr(value, out);
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            collect_ph_expr(cond, out);
            collect_ph_expr(then_expr, out);
            collect_ph_expr(else_expr, out);
        }
        Expr::AssignExpr { expr, .. } | Expr::PositionalPair(expr) | Expr::ZenSlice(expr) => {
            collect_ph_expr(expr, out)
        }
        Expr::CompoundAssign {
            target,
            rhs,
            expanded,
            ..
        } => {
            collect_ph_expr(target, out);
            collect_ph_expr(rhs, out);
            collect_ph_expr(expanded, out);
        }
        Expr::Exists { target, arg, .. } => {
            collect_ph_expr(target, out);
            if let Some(a) = arg {
                collect_ph_expr(a, out);
            }
        }
        Expr::ArrayLiteral(es)
        | Expr::BracketArray(es, _)
        | Expr::StringInterpolation(es)
        | Expr::CaptureLiteral(es) => {
            for e in es {
                collect_ph_expr(e, out);
            }
        }
        Expr::Block(stmts)
        | Expr::AnonSub { body: stmts, .. }
        | Expr::AnonSubParams { body: stmts, .. }
        | Expr::Gather(stmts) => {
            for s in stmts {
                collect_ph_stmt(s, out);
            }
        }
        Expr::DoBlock { body, .. } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Expr::DoStmt(stmt) => {
            collect_ph_stmt(stmt, out);
        }
        Expr::Try { body, catch } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
            if let Some(c) = catch {
                for s in c {
                    collect_ph_stmt(s, out);
                }
            }
        }
        Expr::PhaserExpr { body, .. } | Expr::Once { body } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Expr::CodeVar(_) => {}
        Expr::IndirectCodeLookup { package, .. } => collect_ph_expr(package, out),
        Expr::SymbolicDeref { expr, .. } => collect_ph_expr(expr, out),
        Expr::SymbolicDerefAssign { expr, value, .. } => {
            collect_ph_expr(expr, out);
            collect_ph_expr(value, out);
        }
        Expr::IndirectTypeLookupAssign { expr, value } => {
            collect_ph_expr(expr, out);
            collect_ph_expr(value, out);
        }
        Expr::Reduction { expr, .. }
        | Expr::Eager(expr)
        | Expr::Itemize(expr)
        | Expr::Grouped(expr)
        | Expr::DeitemizeForBind(expr)
        // ADR-0033: a not-yet-expanded WhateverCurry marker is transparent to
        // the deep collector, same as every other closure kind it recurses
        // into unconditionally above.
        | Expr::WhateverCurry(expr) => collect_ph_expr(expr, out),
        Expr::HyperOp { left, right, .. }
        | Expr::HyperFuncOp { left, right, .. }
        | Expr::MetaOp { left, right, .. } => {
            collect_ph_expr(left, out);
            collect_ph_expr(right, out);
        }
        Expr::InfixFunc { left, right, .. } => {
            collect_ph_expr(left, out);
            for e in right {
                collect_ph_expr(e, out);
            }
        }
        Expr::Hash(pairs) => {
            for (_, v) in pairs {
                if let Some(e) = v {
                    collect_ph_expr(e, out);
                }
            }
        }
        _ => {}
    }
}

/// ADR-0048 D2: how much of a construct's own argument supply a `$^name`
/// placeholder inside it can see.
///
/// `ArgSupply` is only meaningful when the construct is `Signature`-capable
/// (see [`PlaceholderBodyKind`]); it names *what value* the construct hands
/// its body when it invokes it. Not every variant is exercised yet — Phase 1
/// classified `Condition`, `Elements`, `Topic` and `CallerArgs`; Phase 3
/// (D3/D6) put `None` to work for the zero-argument bodies (`when`, the bare
/// `{}` statement). `ConditionAfterFirstPass` (`repeat {} while/until`'s `Mu`
/// first pass) is still reserved for D4/Phase 4.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ArgSupply {
    /// The enclosing block's own arguments (routine bodies, closure values).
    CallerArgs,
    /// One argument: the raw (un-boolified) condition value.
    Condition,
    /// One argument: `Mu` on the first pass, then the condition value.
    ConditionAfterFirstPass,
    /// One argument: the topic.
    Topic,
    /// N arguments per iteration, N = the body's own placeholder count.
    Elements,
    /// One `Mu` per declared placeholder: a `role` body, which raku runs once
    /// at composition (ADR-0048 D7). Never under-supplied, so it never raises
    /// an arity failure. (Rakudo actually leaves each parameter as an
    /// uninitialized `VMNull` register: it gists as `(Mu)` and `$^c === Mu` is
    /// `True`, but `$^c.^name` says `VMNull` and `$^c.defined` throws. mutsu
    /// does not supply the value at all yet — see
    /// `todo/deep/role-body-placeholder-mu-supply.md` — so this variant
    /// currently only records that a role body never under-supplies.)
    AllMu,
    /// Zero arguments.
    None,
}

/// ADR-0048 D2: classifies whether a construct's `{ ... }` body may carry a
/// placeholder-derived signature of its own, and if so what it is supplied
/// with. This is the single table consulted by every placeholder-scope walk
/// (`collect_ph_stmt_shallow`/`collect_ph_expr_shallow` below,
/// `order_check_stmt`/`order_check_expr`/`check_bare_var_stmt`/
/// `check_bare_var_expr` in `placeholder_order.rs`, and the `For`-modifier
/// case in `collect_unattached_ph_stmt`) instead of each independently
/// re-deriving the same descend-or-stop decision — see
/// `docs/adr/0048-placeholder-scope-is-a-block-invocation-contract.md`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PlaceholderBodyKind {
    /// No block of its own: descend, placeholders belong to the enclosing
    /// scope. Covers statement MODIFIERS (`if`/`for`/`given` with
    /// `is_statement_modifier: true`, which have no block at all — mirroring
    /// the `For`/`Given` modifier rule below), the parser's synthetic
    /// `SyntheticBlock` desugar wrapper (no `{ ... }` in the source at all),
    /// and also `while`/`RoleDecl`/WhateverCode closures, which mutsu
    /// currently (WRONGLY, per ADR-0048's raku audit) treats the same way:
    /// they leak their placeholders to the enclosing scope (or, for
    /// `RoleDecl`, over-reject) instead of matching raku's rule. Phase 1 was
    /// a pure refactor of the existing (partly wrong) behaviour into one
    /// table; Phase 2 corrected `loop`/`react`/`default`/`Catch`/`Control`/
    /// `Phaser` (moved to `NoSignature`, see below); Phase 3 corrected `when`
    /// and the bare `{}` statement (moved to `Signature(ArgSupply::None)`);
    /// the remaining wrong entries are corrected by ADR-0048's later phases
    /// (D4 for `while`/`repeat`, D7 for `RoleDecl`), not here.
    Transparent,
    /// A boundary that takes a signature; the construct supplies `ArgSupply`
    /// when it invokes its body. `if`/`elsif`/`unless`/`with`/`without`
    /// (non-modifier) supply the raw condition (`ArgSupply::Condition`);
    /// `for` (non-modifier) supplies one N-tuple of elements per iteration
    /// (`ArgSupply::Elements`); `given`/`with` (non-modifier) and `whenever`
    /// supply the topic/emitted value (`ArgSupply::Topic`); real closures
    /// (`sub {}`/`-> {}`/named subs) supply the caller's own arguments
    /// (`ArgSupply::CallerArgs`); `when` bodies and the bare `{}` statement
    /// are invoked with nothing (`ArgSupply::None`, ADR-0048 Phase 3/D6).
    /// The *value* half of the contract lives in the compiler's shared
    /// `Compiler::emit_inlined_body_placeholder_binds` (ADR-0048 D3), which
    /// binds as many of the body's placeholders as the construct supplies and
    /// raises raku's `Too few positionals passed` for the rest.
    Signature(ArgSupply),
    /// A boundary that may not take a signature at all: a placeholder used
    /// directly inside it is `X::Placeholder::Block`. `class`/`RoleDecl`
    /// bodies fall to this variant via the catch-all below (RoleDecl is a
    /// deliberate Phase-1 over-reject, corrected only in Phase 5/D7).
    /// ADR-0048 Phase 2 moved `loop`, `try`, `react`, `once`, `default`,
    /// `CATCH`/`CONTROL` (standalone, and `Stmt::Phaser`'s BEGIN/CHECK/INIT/
    /// ENTER/END/PRE/POST kinds), `gather`, and `module`/`package`/`grammar`
    /// into this variant too, each reusing the same
    /// `placeholder_scope_error("block", ph)` helper `do {}`'s existing
    /// (separately-implemented) rejection already used. The statement-prefix
    /// group that desugars its body into a real closure at PARSE time
    /// (`start`, `sink`, `supply`, `lazy`, `eager`) cannot be classified here
    /// at all — by the time this oracle runs the placeholder has already been
    /// consumed as that closure's own signature — so Phase 2 rejects those at
    /// their compiler call sites instead (see the `emit_block_placeholder_die`
    /// call sites in `src/compiler/expr_call.rs`/`expr.rs`/`supply.rs`), not
    /// via this table. `race { }` (the bare, non-`for` statement-prefix form)
    /// has no dedicated AST construct in mutsu at all yet — `race` parses as
    /// an ordinary bareword, so it is left unaddressed by Phase 2.
    ///
    /// `do {}` (`Expr::DoBlock`) is *not* classified `NoSignature` here even
    /// though it already rejects a stray placeholder at runtime: that
    /// rejection is implemented by a wholly separate, unconditional check in
    /// `compile_do_block_expr` (`collect_unattached_placeholders` on the
    /// do-block's own body), which exempts a placeholder already "attached"
    /// as the *enclosing* block's own parameter. That attachment is only
    /// possible because THIS shallow walk treats `DoBlock` as `Transparent`
    /// — the parser's chained-comparison desugar
    /// (`src/parser/expr/precedence/chain_cmp.rs`) wraps `0 <= $^p <= 5`'s
    /// placeholder in a synthetic `DoBlock`, so a `where`/`subset` predicate
    /// written that way relies on `$^p` leaking through it to become the
    /// enclosing block's own placeholder parameter (pinned by
    /// `t/subset-where-placeholder-chain.t`; broke Cro::Core's `Cro::Port`
    /// when tried). Reclassifying `DoBlock` as `NoSignature` here would stop
    /// that leak and make every such chained comparison in a placeholder
    /// block newly reject with `X::Placeholder::Block` — a real behaviour
    /// change Phase 1 must not make. Untangling this is left to whichever
    /// later phase gives `do {}` a real `NoSignature` classification.
    NoSignature,
}

/// ADR-0048 D2 oracle for `Stmt`. See [`PlaceholderBodyKind`] for the
/// per-variant rationale (moved here from the individual match arms below,
/// per the ADR: "move them, do not duplicate them").
pub(crate) fn placeholder_body_kind(stmt: &Stmt) -> PlaceholderBodyKind {
    match stmt {
        Stmt::Label { stmt, .. } => placeholder_body_kind(stmt),
        Stmt::If {
            is_statement_modifier: true,
            ..
        } => PlaceholderBodyKind::Transparent,
        Stmt::If { .. } => PlaceholderBodyKind::Signature(ArgSupply::Condition),
        // ADR-0048 D4/Phase 4: a `while`/`until` BLOCK is a real Block that
        // the loop invokes with the *raw* (un-boolified) condition value on
        // every pass — `while 42 { $^c }` prints 42, `until False { $^c }`
        // prints `False`, and `{ while 42 { $^c } }.arity` is 0 because the
        // name never reaches the enclosing block. A `while`/`until`
        // STATEMENT MODIFIER introduces no block at all, so its placeholders
        // are the enclosing block's own parameters
        // (`sub f { say "$^a" while $i++ < 2 }; f(7)` prints 7 twice) —
        // exactly the `if`/`for`/`given` modifier rule above.
        Stmt::While {
            is_statement_modifier: true,
            ..
        } => PlaceholderBodyKind::Transparent,
        Stmt::While { .. } => PlaceholderBodyKind::Signature(ArgSupply::Condition),
        Stmt::For {
            is_statement_modifier: true,
            ..
        } => PlaceholderBodyKind::Transparent,
        Stmt::For { .. } => PlaceholderBodyKind::Signature(ArgSupply::Elements),
        // ADR-0048 Phase 2: `loop {}` (headerless and C-style) does not take
        // a signature in raku — flip from the Phase-1 (wrong) `Transparent`
        // classification to `NoSignature` via the catch-all below. `repeat
        // {} while/until` (`repeat: true`) is a DIFFERENT construct that
        // stays `Transparent` here: per the ADR's evidence table it IS
        // signature-capable (`ArgSupply::ConditionAfterFirstPass` — `Mu` on
        // the first pass, then the condition value), so it belongs with D4
        // (Phase 4), not this rejecting set. Verified against `raku`:
        // `repeat while $b < 10 { $tracker = $^a; $b++ }` does NOT reject
        // `$^a` (pins `roast/S04-statements/repeat.t`'s "placeholders and
        // 'repeat while' mix" subtest, which would otherwise regress).
        //
        // ADR-0048 Phase 3 had to promote it from that placeholder
        // `Transparent` to its real `Signature` classification: once the bare
        // `{ ... }` STATEMENT became a zero-argument boundary (D6, below), a
        // `repeat` nested in one — exactly the shape of
        // `roast/S04-statements/repeat.t`'s subtest and of
        // `t/placeholder-scope-rejecting.t`'s accepting pin — leaked its
        // `$^a` out to the enclosing bare block, which then reported it as a
        // parameter nothing supplies. This is the *classification* half of
        // D4 only: the `ArgSupply::ConditionAfterFirstPass` bind itself (`Mu`
        // on the first pass, the raw condition afterwards) is still Phase 4's
        // work, so a placeholder in a `repeat` body is a parameter of that
        // body that nothing binds yet, rather than the enclosing block's.
        Stmt::Loop { repeat: true, .. } => {
            PlaceholderBodyKind::Signature(ArgSupply::ConditionAfterFirstPass)
        }
        // `loop {}` (`repeat: false`, both headerless and C-style) and
        // `react {}` fall through to the `NoSignature` catch-all below.
        // The `whenever` body is its own block scope, supplied the emitted
        // value (aliased as the topic) — but mutsu's shallow walks never
        // descend into it today (only the `supply` header is collected in
        // this scope), so this classification's practical effect in Phase 1
        // is identical to `NoSignature`: a boundary, body not visited here.
        Stmt::Whenever { .. } => PlaceholderBodyKind::Signature(ArgSupply::Topic),
        // `Default`/`Catch`/`Control`/`Phaser` do not take a signature in
        // raku either — ADR-0048 Phase 2 flips them to `NoSignature` via the
        // catch-all below.
        //
        // ADR-0048 Phase 3 (D3/D6): a bare `{ ... }` STATEMENT and a `when`
        // body are real Blocks that raku invokes with ZERO arguments, so a
        // placeholder in one is that block's own unsatisfied parameter, not
        // the enclosing block's — `{ $^c }` and `given 5 { when 5 { $^c } }`
        // both die with "Too few positionals passed; expected 1 argument but
        // got 0", and `{ when 5 { $^c } }.arity` is 0. Hence
        // `Signature(ArgSupply::None)`: a boundary the shallow walks stop at,
        // whose arity failure `emit_inlined_body_placeholder_binds` raises at
        // the body's own compile site.
        //
        // `SyntheticBlock` is NOT included: it is a parser desugar wrapper
        // (destructuring declarations, `has` attribute lowering, package
        // meta-statements, ...) with no `{ ... }` in the source at all, so a
        // placeholder inside one still belongs to the enclosing block.
        // `RoleDecl` stays `Transparent` too (a deliberate Phase-1
        // over-reject via its own `emit_block_placeholder_die` call site —
        // correcting it is Phase 5/D7, not here).
        Stmt::Block(_) | Stmt::When { .. } => PlaceholderBodyKind::Signature(ArgSupply::None),
        // ADR-0048 D7/Phase 5: a `role` body IS signature-capable in raku
        // (`role R { $^c }; class D does R {}` compiles and runs at
        // composition), unlike the `class`/`module`/`package`/`grammar`
        // bodies that fall to `NoSignature` below. Every placeholder it
        // declares is supplied the same value, so it never raises an arity
        // failure — see `ArgSupply::AllMu`. Only this SCOPE half is
        // implemented: the boundary stops `$^c` leaking onto the enclosing
        // block (`{ role R { $^c } }.arity` is 0, as in raku), but the
        // compiler still rejects a role body that actually uses a
        // placeholder, because the value cannot be supplied from the
        // `Stmt::RoleDecl` compile site — see the comment on that arm in
        // `src/compiler/stmt.rs` and
        // `todo/deep/role-body-placeholder-mu-supply.md`.
        Stmt::RoleDecl { .. } => PlaceholderBodyKind::Signature(ArgSupply::AllMu),
        Stmt::SyntheticBlock(_) => PlaceholderBodyKind::Transparent,
        Stmt::Given {
            is_statement_modifier: true,
            ..
        } => PlaceholderBodyKind::Transparent,
        Stmt::Given { .. } => PlaceholderBodyKind::Signature(ArgSupply::Topic),
        // Every other `Stmt` kind has no body visited by the shallow walks
        // today: real routine/method/class/package bodies are collected by
        // their own dedicated compile-time pass, never by this shallow one.
        _ => PlaceholderBodyKind::NoSignature,
    }
}

/// ADR-0048 D2 oracle for `Expr` (the sibling of [`placeholder_body_kind`]
/// for expression-position bodies: closures, `Try`, `Gather`, `DoBlock`,
/// phasers-as-expressions).
pub(crate) fn placeholder_body_kind_expr(expr: &Expr) -> PlaceholderBodyKind {
    match expr {
        // A WhateverCode (`*`-derived) closure owns only its `*`-derived
        // params, not `$^name` placeholders, which belong to the nearest
        // enclosing *explicit* block — so it is transparent here.
        Expr::AnonSubParams {
            is_whatever_code: true,
            ..
        }
        | Expr::Lambda {
            is_whatever_code: true,
            ..
        } => PlaceholderBodyKind::Transparent,
        // A real closure (`sub {}`/`-> {}`/an already-signatured block)
        // supplies the caller's own arguments; it is its own placeholder
        // scope already, so the shallow walks never need to look inside it.
        Expr::AnonSub { .. } | Expr::AnonSubParams { .. } | Expr::Lambda { .. } => {
            PlaceholderBodyKind::Signature(ArgSupply::CallerArgs)
        }
        // The bare `{}` TERM (an `Expr::Block` in value position) stays
        // `Transparent`: `compile_expr_block` turns a placeholder-bearing one
        // into a real closure with those placeholders as its signature
        // (`{ $^c }.arity` is 1), so it is not the zero-argument statement
        // Block that ADR-0048 Phase 3/D6 reclassified. `Gather`
        // (`gather {}`) does NOT — ADR-0048 Phase 2 flips it to `NoSignature`
        // via the catch-all below (raku: a placeholder inside `gather {}` is
        // `X::Placeholder::Block`, not the enclosing block's own param).
        Expr::Block(_) => PlaceholderBodyKind::Transparent,
        // Not `NoSignature` — see the long note on `PlaceholderBodyKind::NoSignature`
        // above: the chained-comparison desugar's synthetic `DoBlock` relies
        // on this leak to attach `$^p` to the enclosing block.
        Expr::DoBlock { .. } => PlaceholderBodyKind::Transparent,
        // `Try`/`PhaserExpr`/`Once` do not take a signature in raku — ADR-0048
        // Phase 2 flips them to `NoSignature` via the catch-all below.
        _ => PlaceholderBodyKind::NoSignature,
    }
}

/// Shallow version of collect_ph_stmt: skips AnonSub/AnonSubParams/Lambda
/// closures so their placeholders are not attributed to the outer block.
fn collect_ph_stmt_shallow(stmt: &Stmt, out: &mut Vec<String>) {
    match stmt {
        Stmt::Expr(e)
        | Stmt::Return(e)
        | Stmt::Die(e)
        | Stmt::Fail(e)
        | Stmt::Take(e, _)
        | Stmt::Goto(e) => {
            collect_ph_expr_shallow(e, out);
        }
        Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => {
            collect_ph_expr_shallow(expr, out)
        }
        Stmt::Call { args, .. } => {
            for arg in args {
                match arg {
                    CallArg::Positional(e) | CallArg::Invocant(e) => {
                        collect_ph_expr_shallow(e, out)
                    }
                    CallArg::Named { value: Some(e), .. } => collect_ph_expr_shallow(e, out),
                    CallArg::Named { value: None, .. } => {}
                    CallArg::Slip(e) => collect_ph_expr_shallow(e, out),
                }
            }
        }
        Stmt::Say(es) | Stmt::Put(es) | Stmt::Print(es) | Stmt::Note(es) => {
            for e in es {
                collect_ph_expr_shallow(e, out);
            }
        }
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            ..
        } => {
            // The header is always evaluated in THIS block's scope (see the
            // oracle's `Signature(Condition)` doc); the branches only join
            // this scope when the oracle says `Transparent` (statement
            // modifiers — see the oracle's `Transparent` doc).
            collect_ph_expr_shallow(cond, out);
            if matches!(
                placeholder_body_kind(stmt),
                PlaceholderBodyKind::Transparent
            ) {
                for s in then_branch.iter().chain(else_branch.iter()) {
                    collect_ph_stmt_shallow(s, out);
                }
            }
        }
        Stmt::While { cond, body, .. } => {
            collect_ph_expr_shallow(cond, out);
            if matches!(
                placeholder_body_kind(stmt),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    collect_ph_stmt_shallow(s, out);
                }
            }
        }
        Stmt::For { iterable, body, .. } => {
            collect_ph_expr_shallow(iterable, out);
            if matches!(
                placeholder_body_kind(stmt),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    collect_ph_stmt_shallow(s, out);
                }
            }
        }
        Stmt::Loop { body, .. } | Stmt::React { body } => {
            if matches!(
                placeholder_body_kind(stmt),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    collect_ph_stmt_shallow(s, out);
                }
            }
        }
        Stmt::Whenever { supply, .. } => {
            // Only the `supply` header is in this scope — see the oracle's
            // `Whenever` doc for why the body is never descended here.
            collect_ph_expr_shallow(supply, out);
        }
        Stmt::Block(body)
        | Stmt::SyntheticBlock(body)
        | Stmt::Default(body)
        | Stmt::Catch(body)
        | Stmt::Control(body)
        | Stmt::RoleDecl { body, .. } => {
            if matches!(
                placeholder_body_kind(stmt),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    collect_ph_stmt_shallow(s, out);
                }
            }
        }
        Stmt::Phaser { body, .. } => {
            if matches!(
                placeholder_body_kind(stmt),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    collect_ph_stmt_shallow(s, out);
                }
            }
        }
        Stmt::Given { topic, body, .. } => {
            collect_ph_expr_shallow(topic, out);
            if matches!(
                placeholder_body_kind(stmt),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    collect_ph_stmt_shallow(s, out);
                }
            }
        }
        Stmt::When { cond, body, .. } => {
            collect_ph_expr_shallow(cond, out);
            if matches!(
                placeholder_body_kind(stmt),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    collect_ph_stmt_shallow(s, out);
                }
            }
        }
        Stmt::Let { value, index, .. } => {
            if let Some(e) = value {
                collect_ph_expr_shallow(e, out);
            }
            if let Some(e) = index {
                collect_ph_expr_shallow(e, out);
            }
        }
        Stmt::TempMethodAssign {
            method_args, value, ..
        } => {
            for e in method_args {
                collect_ph_expr_shallow(e, out);
            }
            collect_ph_expr_shallow(value, out);
        }
        Stmt::Label { stmt, .. } => {
            collect_ph_stmt_shallow(stmt, out);
        }
        Stmt::SubsetDecl {
            predicate: Some(predicate),
            ..
        } => {
            collect_ph_expr_shallow(predicate, out);
        }
        _ => {}
    }
}

/// Shallow version of collect_ph_expr: stops at closure boundaries
/// (AnonSub, AnonSubParams, Lambda) since those closures define their
/// own placeholder scope.
fn collect_ph_expr_shallow(expr: &Expr, out: &mut Vec<String>) {
    match expr {
        Expr::Subst {
            pattern,
            replacement,
            ..
        }
        | Expr::NonDestructiveSubst {
            pattern,
            replacement,
            ..
        } => {
            collect_placeholders_in_str(pattern, out);
            collect_placeholders_in_str(replacement, out);
        }
        Expr::Var(name) if name.starts_with('^') || name.starts_with(':') => {
            if !out.contains(name) {
                out.push(name.clone());
            }
        }
        Expr::CodeVar(name) if name.starts_with('^') => {
            let prefixed = format!("&{}", name);
            if !out.contains(&prefixed) {
                out.push(prefixed);
            }
        }
        Expr::ArrayVar(name) if name.starts_with('^') || name.starts_with(':') => {
            let prefixed = format!("@{}", name);
            if !out.contains(&prefixed) {
                out.push(prefixed);
            }
        }
        Expr::HashVar(name) if name.starts_with('^') || name.starts_with(':') => {
            let prefixed = format!("%{}", name);
            if !out.contains(&prefixed) {
                out.push(prefixed);
            }
        }
        Expr::Binary { left, right, .. } => {
            collect_ph_expr_shallow(left, out);
            collect_ph_expr_shallow(right, out);
        }
        // `todo/tickets/chained-compare-ast-node.md`: `{ $^a < $^b < $^c }`
        // must see every operand, same as a plain `Binary` comparison.
        Expr::ChainedCompare { operands, .. } => {
            for o in operands {
                collect_ph_expr_shallow(o, out);
            }
        }
        Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => {
            collect_ph_expr_shallow(expr, out)
        }
        Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
            collect_ph_expr_shallow(target, out);
            for a in args {
                collect_ph_expr_shallow(a, out);
            }
        }
        Expr::DynamicMethodCall {
            target,
            name_expr,
            args,
            ..
        }
        | Expr::HyperMethodCallDynamic {
            target,
            name_expr,
            args,
            ..
        } => {
            collect_ph_expr_shallow(target, out);
            collect_ph_expr_shallow(name_expr, out);
            for a in args {
                collect_ph_expr_shallow(a, out);
            }
        }
        Expr::Call { args, .. } | Expr::UserRoutineCall { args, .. } => {
            for a in args {
                collect_ph_expr_shallow(a, out);
            }
        }
        Expr::CallOn { target, args } => {
            collect_ph_expr_shallow(target, out);
            for a in args {
                collect_ph_expr_shallow(a, out);
            }
        }
        Expr::Index { target, index, .. } => {
            collect_ph_expr_shallow(target, out);
            collect_ph_expr_shallow(index, out);
        }
        // Element-assignment TARGET placeholders (`{ $^x<a> = 3 }`) — see the
        // matching arm in `collect_ph_expr`.
        Expr::IndexAssign {
            target,
            index,
            value,
            ..
        } => {
            collect_ph_expr_shallow(target, out);
            collect_ph_expr_shallow(index, out);
            collect_ph_expr_shallow(value, out);
        }
        Expr::MultiDimIndexAssign {
            target,
            dimensions,
            value,
            ..
        } => {
            collect_ph_expr_shallow(target, out);
            for d in dimensions {
                collect_ph_expr_shallow(d, out);
            }
            collect_ph_expr_shallow(value, out);
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            collect_ph_expr_shallow(cond, out);
            collect_ph_expr_shallow(then_expr, out);
            collect_ph_expr_shallow(else_expr, out);
        }
        Expr::AssignExpr { expr, .. } | Expr::PositionalPair(expr) | Expr::ZenSlice(expr) => {
            collect_ph_expr_shallow(expr, out)
        }
        Expr::CompoundAssign {
            target,
            rhs,
            expanded,
            ..
        } => {
            collect_ph_expr_shallow(target, out);
            collect_ph_expr_shallow(rhs, out);
            collect_ph_expr_shallow(expanded, out);
        }
        Expr::Exists { target, arg, .. } => {
            collect_ph_expr_shallow(target, out);
            if let Some(a) = arg {
                collect_ph_expr_shallow(a, out);
            }
        }
        Expr::ArrayLiteral(es)
        | Expr::BracketArray(es, _)
        | Expr::StringInterpolation(es)
        | Expr::CaptureLiteral(es) => {
            for e in es {
                collect_ph_expr_shallow(e, out);
            }
        }
        // A WhateverCode (`is_whatever_code`) is synthesized from `*` and owns
        // only its `*`-derived params; it does NOT capture `$^name` placeholders,
        // which belong to the nearest enclosing *explicit* block. So descend
        // through it to attribute e.g. `$^namespace` in
        // `{ ... $^namespace ~ * => * }` to the outer block, matching Rakudo.
        //
        // ADR-0033 Phase 1: the parser no longer builds this closure eagerly —
        // at this (pre-compile) stage a curry is still an un-expanded
        // `WhateverCurry` marker, so descend into its body directly instead.
        Expr::WhateverCurry(inner) => collect_ph_expr_shallow(inner, out),
        Expr::AnonSubParams { body, .. } | Expr::Lambda { body, .. } => {
            if matches!(
                placeholder_body_kind_expr(expr),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    collect_ph_stmt_shallow(s, out);
                }
            }
        }
        // Stop at a real closure boundary: it defines its own placeholder scope.
        Expr::AnonSub { .. } => {}
        Expr::Block(stmts) | Expr::Gather(stmts) => {
            if matches!(
                placeholder_body_kind_expr(expr),
                PlaceholderBodyKind::Transparent
            ) {
                for s in stmts {
                    collect_ph_stmt_shallow(s, out);
                }
            }
        }
        Expr::DoBlock { body, .. } => {
            if matches!(
                placeholder_body_kind_expr(expr),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    collect_ph_stmt_shallow(s, out);
                }
            }
        }
        Expr::DoStmt(stmt) => {
            collect_ph_stmt_shallow(stmt, out);
        }
        Expr::Try { body, catch } => {
            if matches!(
                placeholder_body_kind_expr(expr),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    collect_ph_stmt_shallow(s, out);
                }
                if let Some(c) = catch {
                    for s in c {
                        collect_ph_stmt_shallow(s, out);
                    }
                }
            }
        }
        Expr::PhaserExpr { body, .. } | Expr::Once { body } => {
            if matches!(
                placeholder_body_kind_expr(expr),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    collect_ph_stmt_shallow(s, out);
                }
            }
        }
        Expr::Reduction { expr, .. }
        | Expr::Eager(expr)
        | Expr::Itemize(expr)
        | Expr::Grouped(expr)
        | Expr::DeitemizeForBind(expr) => collect_ph_expr_shallow(expr, out),
        Expr::HyperOp { left, right, .. }
        | Expr::HyperFuncOp { left, right, .. }
        | Expr::MetaOp { left, right, .. } => {
            collect_ph_expr_shallow(left, out);
            collect_ph_expr_shallow(right, out);
        }
        Expr::InfixFunc { left, right, .. } => {
            collect_ph_expr_shallow(left, out);
            for e in right {
                collect_ph_expr_shallow(e, out);
            }
        }
        Expr::Hash(pairs) => {
            for (_, v) in pairs {
                if let Some(e) = v {
                    collect_ph_expr_shallow(e, out);
                }
            }
        }
        _ => {}
    }
}

pub(crate) fn has_var_decl(stmts: &[Stmt], name: &str) -> bool {
    for stmt in stmts {
        match stmt {
            Stmt::VarDecl {
                name: decl_name, ..
            } if decl_name == name => return true,
            _ => {}
        }
    }
    false
}

/// Create an `Expr::AnonSub` or `Expr::AnonSubParams` depending on whether
/// the block body contains placeholder variables (`$^a`, `$^b`, etc.).
pub(crate) fn make_anon_sub(stmts: Vec<Stmt>) -> Expr {
    let placeholders = collect_placeholders_shallow(&stmts);
    if placeholders.is_empty() {
        // A signature-less block has an implicit `*@_` when it reads the
        // legacy argument array. Keep that distinction from an explicitly
        // empty `-> {}` signature, which still rejects positional arguments.
        let body_debug = format!("{stmts:?}");
        let uses_at_underscore = body_debug.contains("ArrayVar(\"_\")");
        if uses_at_underscore {
            let legacy_params = vec!["@_".to_string()];
            let param_defs = legacy_params
                .iter()
                .map(|name| ParamDef {
                    name: name.clone(),
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
                    block_param: true,
                })
                .collect();
            return Expr::AnonSubParams {
                params: legacy_params,
                param_defs,
                return_type: None,
                body: stmts,
                is_rw: false,
                is_whatever_code: false,
                is_sub: false,
            };
        }
        Expr::AnonSub {
            body: stmts,
            is_rw: false,
            is_block: true,
        }
    } else {
        Expr::AnonSubParams {
            params: placeholders.clone(),
            param_defs: placeholders
                .iter()
                .map(|name| {
                    // Named placeholders use `:` twigil: $:f, @:f, %:f
                    let is_named = name.contains(':');
                    ParamDef {
                        name: name.clone(),
                        default: None,
                        multi_invocant: true,
                        required: false,
                        named: is_named,
                        slurpy: false,
                        sigilless: false,
                        type_constraint: None,
                        literal_value: None,
                        sub_signature: None,
                        where_constraint: None,
                        traits: Vec::new(),
                        double_slurpy: false,
                        onearg: false,
                        optional_marker: false,
                        outer_sub_signature: None,
                        code_signature: None,
                        is_invocant: false,
                        shape_constraints: None,
                        block_param: false,
                    }
                })
                .collect(),
            return_type: None,
            body: stmts,
            is_rw: false,
            is_whatever_code: false,
            is_sub: false,
        }
    }
}

#[cfg(test)]
mod env_only_decl_tests {
    use super::*;

    fn vardecl(name: &str) -> Stmt {
        Stmt::VarDecl {
            name: name.to_string(),
            expr: Expr::Literal(crate::value::Value::NIL),
            type_constraint: None,
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: Vec::new(),
            where_constraint: None,
        }
    }

    // `my @needed` declared inside a `next unless my @needed = ...` condition
    // parses to `If { cond: !DoStmt(VarDecl @needed) }`. The declaration is in the
    // condition Expr, not the then/else body, so the collector must walk the
    // condition. Regression for the zef `!find-prereq-candidates` `@needed` leak.
    #[test]
    fn collects_my_decl_embedded_in_if_condition() {
        let inner_if = Stmt::If {
            cond: Expr::Unary {
                op: crate::token_kind::TokenKind::Bang,
                expr: Box::new(Expr::DoStmt(Box::new(vardecl("@needed")))),
            },
            then_branch: vec![Stmt::Next(None)],
            else_branch: vec![],
            binding_var: None,
            is_statement_modifier: false,
        };
        // Wrapped in a gather-shaped Block([While { body: [...] }]).
        let body = vec![Stmt::Block(vec![Stmt::While {
            cond: Expr::Literal(crate::value::Value::NIL),
            body: vec![inner_if],
            label: None,
            is_statement_modifier: false,
            is_until: false,
        }])];
        let mut out = std::collections::HashSet::new();
        collect_all_my_decl_names(&body, &mut out);
        assert!(
            out.contains("@needed"),
            "expected @needed to be collected from the If condition, got {out:?}"
        );
    }

    // Array/hash `my` names in a plain body must be collected (not just scalars).
    #[test]
    fn collects_array_and_hash_decls() {
        let body = vec![vardecl("@arr"), vardecl("%hash"), vardecl("$scalar")];
        let mut out = std::collections::HashSet::new();
        collect_all_my_decl_names(&body, &mut out);
        assert!(out.contains("@arr"));
        assert!(out.contains("%hash"));
        assert!(out.contains("$scalar"));
    }
}
