use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::{Value, ValueView};
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

/// Draw from the process-global declaration-site counter directly, bypassing
/// the analysis-only unit-local mode. For ids minted at compile or run time
/// that share a namespace with `decl_id` (a role's `role_id`, see
/// `crate::runtime::next_role_id`).
pub(crate) fn next_global_decl_id() -> u64 {
    CLASS_DECL_ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
}

/// Specifies how delegation (`handles`) should forward methods.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum HandleSpec {
    /// Forward a method by name (same name on both sides).
    Name(String),
    /// Expand the value of an expression into method names.
    ///
    /// Raku permits capture slips in a parenthesized `handles` list, such as
    /// `handles ('name', |SomeType.methods)`. The expression is evaluated when
    /// the declaration is composed, just like the rest of the declaration's
    /// traits.
    Expr(Box<Expr>),
    /// Rename: expose `exposed` on the class, forwarding to `target` on the delegate.
    Rename { exposed: String, target: String },
    /// Forward all methods defined in the given type (class or role name).
    Type(String),
    /// Forward all methods whose name matches the regex pattern.
    Regex(String),
    /// Wildcard: forward all unknown methods.
    Wildcard,
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct ParamDef {
    pub(crate) name: String,
    pub(crate) default: Option<Expr>,
    pub(crate) multi_invocant: bool,
    pub(crate) required: bool,
    pub(crate) named: bool,
    /// True when a named parameter uses the alias form (`:key($value)`).
    /// Named variable parameters with a following sub-signature (`:$key
    /// ($value)`) destructure instead; both forms otherwise share
    /// `sub_signature`.
    #[serde(default)]
    pub(crate) named_alias: bool,
    pub(crate) slurpy: bool,
    pub(crate) double_slurpy: bool,
    /// True for single-argument rule slurpy (`+@a`, `+%h`, etc.)
    pub(crate) onearg: bool,
    pub(crate) sigilless: bool,
    pub(crate) type_constraint: Option<String>,
    /// The name a `::T` type capture binds, with NO `::` prefix and no type
    /// smiley — `None` on a parameter that captures nothing.
    ///
    /// This is deliberately a field of its own rather than a `"::T"` spelling
    /// inside [`ParamDef::type_constraint`]: a parameter can carry a capture
    /// *and* a nominal type at the same time (`method m(::T Foo:D: $x)`, #7984),
    /// which a single `Option<String>` cannot express. `type_constraint` is
    /// therefore only ever the nominal half.
    #[serde(default)]
    pub(crate) type_capture: Option<String>,
    pub(crate) literal_value: Option<Value>,
    pub(crate) sub_signature: Option<Vec<ParamDef>>,
    pub(crate) where_constraint: Option<Box<Expr>>,
    pub(crate) traits: Vec<String>,
    /// The captured argument for a *custom* (non-builtin) parameter trait that
    /// carries one, e.g. the `<!>` in `is option<!>` (Getopt::Long, #8560) or
    /// the `('utf8')` in a hypothetical `is myencoding('utf8')`. Sparse: only
    /// traits with an argument appear here, keyed by name so a trait without
    /// one still dispatches with the plain `True` `check_param_custom_traits`
    /// already passed. A builtin trait's own argument (`is encoded('utf8')`)
    /// is handled natively and never reaches this field.
    #[serde(default)]
    pub(crate) trait_args: Vec<(String, Expr)>,
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
    /// The precompiled chunks of this parameter's `where` clause, default and
    /// shape dimensions (ADR-0133). Shared by every clone of this parse node;
    /// filled by the compiler when it compiles the routine owning the
    /// signature. Code that rewrites one of those expressions must reset it.
    #[serde(skip)]
    pub(crate) code: ParamCode,
}

/// The bytecode for a parameter's signature-time expressions, compiled once in
/// the declaring scope by `Compiler::attach_param_chunks` (ADR-0133).
///
/// Each chunk is a standalone unit with no local slots: every variable it names
/// resolves through the env the binder has set up (earlier parameters, `$_`,
/// the routine's captures), exactly as the `eval_block_value` compile it
/// replaces resolved them.
#[derive(Debug)]
pub(crate) struct ParamChunks {
    /// The `where` clause: the block's statements for `where { ... }`, the
    /// expression itself otherwise.
    pub(crate) where_chunk: Option<crate::opcode::CompiledDeclExpr>,
    /// True when `where_chunk` is the BODY of a one-argument WhateverCode
    /// (`where * < 100`) rather than the expression building it: the binder
    /// has already bound `$_` to the value under test, so the chunk's result
    /// is the predicate's answer, with no closure built or called per check.
    pub(crate) where_inline_predicate: bool,
    /// A default expression that is not an immutable scalar literal.
    pub(crate) default_chunk: Option<crate::opcode::CompiledDeclExpr>,
    /// One entry per `shape_constraints` element; `None` for a `*` or literal
    /// dimension the binder reads without evaluating.
    pub(crate) shape_chunks: Vec<Option<crate::opcode::CompiledDeclExpr>>,
}

/// The slot a [`ParamDef`] carries for its [`ParamChunks`]. `Clone` shares the
/// slot, so every copy of the node (plans, the `stmt_pool` entry, a closure's
/// shared signature, `CompiledFunction::param_defs`) sees one fill. `Default`
/// makes a fresh, empty slot; a slot the compiler never filled makes the binder
/// fall back to evaluating the AST.
#[derive(Clone, Default)]
pub(crate) struct ParamCode(std::sync::Arc<std::sync::OnceLock<ParamChunks>>);

impl ParamCode {
    #[inline]
    pub(crate) fn get(&self) -> Option<&ParamChunks> {
        self.0.get()
    }

    /// Fill the slot; a slot already filled (the same parse node reached by a
    /// second compile of its routine) keeps its first chunks.
    pub(crate) fn fill(&self, make: impl FnOnce() -> ParamChunks) {
        self.0.get_or_init(make);
    }

    #[inline]
    pub(crate) fn is_filled(&self) -> bool {
        self.0.get().is_some()
    }
}

impl std::fmt::Debug for ParamCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(if self.is_filled() {
            "ParamCode(compiled)"
        } else {
            "ParamCode(empty)"
        })
    }
}

/// A signature's identity is its source: the compiled chunks are derived from
/// the expressions already hashed, so they stay out of routine fingerprints.
impl Hash for ParamCode {
    fn hash<H: Hasher>(&self, _state: &mut H) {}
}

/// The external argument key a named parameter's spelling denotes: the name with
/// its sigil, `:` marker and twigil stripped. `:@hi` answers to the key `hi`
/// exactly as `:$hi` does, so every site that matches a caller's key against a
/// signature has to strip the same way — a second, sigil-blind copy of this rule
/// is what made `sub h(:h(:@hi))` reject `h(hi => …)` as an unexpected named
/// argument while `:h(:$hi)` accepted it.
pub(crate) fn named_param_external_key(name: &str) -> &str {
    name.trim_start_matches(|c: char| "$@%&:".contains(c))
        .trim_start_matches(['!', '.'])
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

/// True when `param_defs` declares a parameter whose *lexical* name is `name`.
///
/// A routine's flat `params` list (the `ParamDef::name`s) is NOT the set of
/// lexicals its signature binds. Two spellings bind a name that never appears
/// there:
///
///   * the named-alias form — `:d(:$directed)` binds `$directed`, while the
///     outer `ParamDef::name` is the external key `d` and the real name sits in
///     `sub_signature`;
///   * a destructuring sub-signature — `sub f([$a, $b])` binds `$a` and `$b`.
///
/// So any site asking "is this bare name one of *this frame's own* parameters?"
/// has to walk the sub-signatures. Answering from the flat list alone made
/// `reconcile_attrs` mistake an alias-bound parameter for a `:=` attribute
/// binding and write it into the receiver's attribute cell (#9007).
pub(crate) fn param_defs_declare_lexical(param_defs: &[ParamDef], name: &str) -> bool {
    param_defs.iter().any(|pd| {
        pd.name == name
            || pd
                .sub_signature
                .as_deref()
                .is_some_and(|sub| param_defs_declare_lexical(sub, name))
            || pd
                .outer_sub_signature
                .as_deref()
                .is_some_and(|sub| param_defs_declare_lexical(sub, name))
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
    /// The type constraint to register in the **assignment-time** lane when this
    /// parameter binds (`Interpreter::bind_param_type_constraint`).
    ///
    /// `None` for a SIGILLESS parameter (`Associative \container`), which is a
    /// raw alias rather than a container of its own: Rakudo checks its declared
    /// type once, when the argument binds, and every later write through the
    /// alias goes straight into the CALLER's container and is checked against
    /// *that* container's constraint. Registering the declared type here invents
    /// a constraint Rakudo does not have — `sub h(Associative \c) { c = Empty }`
    /// died with "Type check failed in assignment to $container; expected
    /// Associative but got Slip" where Rakudo stores the Slip into the caller's
    /// untyped scalar.
    ///
    /// The decision has to be made from `sigilless`, not from the name: a scalar
    /// parameter's env key drops its `$`, so `$p` and `\p` reach the binder
    /// spelled identically.
    ///
    /// A `$`-sigiled `is rw` / `is raw` parameter is the same kind of alias
    /// ([`Self::binds_caller_container`]): `sub g(Str:D $s is rw) { $s = 5 }`
    /// checks `Str:D` when `$z` binds, then stores the Int into the caller's
    /// untyped `$z` (#10146). A typed caller container (`my Str $t`) still
    /// rejects the write: the binder carries the SOURCE's constraint over, and
    /// the positional-light path registers it on the alias cell.
    ///
    /// Borrowed from the `ParamDef`, which outlives every binder use of it:
    /// the binder only ever reads the text and hands it to the env, so copying
    /// it made a `String` per typed parameter bind — the single largest source
    /// of `String::clone` in a `JSON::Fast` decode (#8898).
    pub(crate) fn assignment_type_constraint(&self) -> Option<&str> {
        if self.sigilless
            || (self.binds_caller_container() && !self.name.starts_with(['@', '%', '&']))
        {
            return None;
        }
        self.type_constraint.as_deref()
    }

    /// True when the *source* declares a parameter spelled `$self` — an explicit
    /// invocant (`method m($self: $n)`, `method symbol(::?CLASS $self: ...)`) or
    /// an ordinary parameter (`sub ($self)`, `-> $self, $x`). A parser-synthesized
    /// anonymous invocant is excluded: it is named `self` only because that is the
    /// invocant's env key, and it declares no lexical (ADR-0061).
    pub(crate) fn declares_self_lexical(&self) -> bool {
        self.name == "self" && !self.traits.iter().any(|t| t == IMPLICIT_INVOCANT_TRAIT)
    }

    /// The name a `::T` type capture on this parameter binds, if any.
    ///
    /// The single oracle for "does this parameter capture a type, and under what
    /// name". Prefer it over reading [`ParamDef::type_constraint`] and stripping
    /// a `::` prefix: that spelling cannot hold a capture and a nominal type at
    /// once, which is exactly what `method m(::T Foo:D: $x)` needs (#7984).
    ///
    /// The fallback covers the two constraint spellings the parser still keeps
    /// in `type_constraint` with their `::` prefix intact, because dispatch also
    /// consumes them as nominal constraints: the pseudo-types `::?CLASS` /
    /// `::?ROLE` (with an optional smiley) and the indirect form `::(expr)`.
    /// Those are not ident captures, and their long-standing behavior — binding
    /// a capture under the post-`::` spelling, which makes the constraint a
    /// no-op type check — is preserved verbatim.
    pub(crate) fn captured_type_name(&self) -> Option<&str> {
        if let Some(name) = self.type_capture.as_deref() {
            return Some(name);
        }
        self.type_constraint
            .as_deref()
            .and_then(|tc| tc.strip_prefix("::"))
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
    /// `:sort(…)`. Aliases nest arbitrarily deep (`:leaves(:rays(:$n))` is two
    /// levels: `leaves` aliasing `rays` aliasing `n`), so each alias's own
    /// `sub_signature` is walked in turn rather than stopping after one level
    /// — a `Graph::Star.new(n => 5, ...)` naming only the innermost alias
    /// otherwise never matched this parameter at all.
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
        let strip = |n: &str| named_param_external_key(n).to_string();
        let mut keys = vec![strip(&self.name)];
        if self.named_alias
            && let Some(aliases) = &self.sub_signature
        {
            for alias in aliases.iter().filter(|a| a.named && !a.slurpy) {
                keys.extend(alias.named_external_keys());
            }
        }
        keys
    }

    /// The spelling that carries this parameter's sigil.
    ///
    /// For an aliased named parameter (`:c(:&cb)` or `:c(&cb)`) the outer
    /// parameter is named for its external key (`c`, sigil-less) and the
    /// sigil lives on the alias, so a sigil-based check has to read the alias
    /// instead. Every other parameter answers with its own name.
    pub(crate) fn sigil_carrying_name(&self) -> &str {
        if self.named_alias
            && !self.name.starts_with(['$', '@', '%', '&'])
            && let Some(alias) = self
                .sub_signature
                .as_ref()
                .and_then(|aliases| aliases.iter().find(|a| !a.slurpy))
        {
            return &alias.name;
        }
        &self.name
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
    /// `is implementation-detail` -- read back via `Code.is-implementation-detail`
    /// (`dispatch_sub_method`'s `"line" | "file"` neighbor arm). `false` for
    /// anything with no declaration to carry the trait (a builtin like `&say`),
    /// matching real Raku.
    #[serde(default)]
    pub(crate) is_implementation_detail: bool,
    #[serde(default)]
    pub(crate) is_cached: bool,
    pub(crate) is_rw: bool,
    pub(crate) is_raw: bool,
    /// Which declarator this routine was written with. `Method` / `Submethod`
    /// make the `&name` code reference report that type instead of `Sub`; it is
    /// how a `my method foo` keeps its declarator, and how an `our method` code
    /// reference has always reported one. Never `Block` -- a block is not a
    /// registered routine.
    ///
    /// (This was `is_method: bool`, which could not tell `submethod` from
    /// `method`, so every named `my submethod` registered as a plain `Sub`.)
    #[serde(default = "declarator_sub")]
    pub(crate) declarator: RoutineDeclarator,
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
    /// Computed once per def and cached inline. The underlying hash walks the
    /// whole body AST, which profiled as a large share of multi/method
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

#[cfg(test)]
mod fingerprint_tests;

/// Structural identity of a routine declaration: its parameter names,
/// parameter definitions and body, hashed through the derived [`Hash`] impls
/// on the AST.
///
/// # Contract
///
/// This is an *identity* fingerprint — "were these two declarations parsed from
/// the same source shape" — not a value hash. Two properties it must keep, and
/// which the derived impls give for free:
///
/// - **Structure only.** No addresses and no per-object ids, so two separately
///   parsed copies of one source fingerprint equal. (`Symbol` hashes its
///   interning id, which is a pure function of the symbol's text within a
///   process; fingerprints are only ever compared in-process — none of them is
///   serialized, `FunctionDef::body_fp_cache` included.)
/// - **Line sensitivity.** This one hashes `Stmt::SetLine` markers like any
///   other statement; [`registration_identity_fingerprint`] deliberately does
///   not.
///
/// This used to stream a `Debug` rendering of the whole AST into the hasher,
/// which made `core::fmt` (`DebugStruct::field`, `DebugSet::entry`,
/// `format_inner`) the dominant cost of every routine the compiler touched —
/// 78.6% of one Cro HTTP/2 DATA frame at its worst. Hashing structurally pays
/// none of that machinery.
pub(crate) fn function_body_fingerprint(
    params: &[String],
    param_defs: &[ParamDef],
    body: &[Stmt],
) -> u64 {
    let mut hasher = DefaultHasher::new();
    // Slice hashing writes a length prefix, which is what keeps the three
    // fields from colliding into each other the way the old `\x00` separators
    // guarded against.
    params.hash(&mut hasher);
    param_defs.hash(&mut hasher);
    body.hash(&mut hasher);
    hasher.finish()
}

/// Line-insensitive identity of a routine declaration for redeclaration
/// comparison: params, param_defs, and the body with top-level `SetLine`
/// markers stripped, hashed structurally. Identical redeclarations that
/// differ only in source line compare equal. Distinct from
/// [`function_body_fingerprint`], which hashes `SetLine` markers too (it is a
/// structural identity, not a redeclaration identity).
pub(crate) fn registration_identity_fingerprint(
    params: &[String],
    param_defs: &[ParamDef],
    body: &[Stmt],
) -> u64 {
    let mut hasher = DefaultHasher::new();
    params.hash(&mut hasher);
    param_defs.hash(&mut hasher);
    let kept = || body.iter().filter(|s| !matches!(s, Stmt::SetLine(_)));
    // Stand in for the length prefix a slice hash would have written, so a
    // body cannot collide with a longer one whose extra statements hash empty.
    kept().count().hash(&mut hasher);
    for stmt in kept() {
        stmt.hash(&mut hasher);
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

/// Hands out the source-order index stored in [`Stmt::Phaser::end_index`].
///
/// The parser is a strictly left-to-right recursive descent, so calling this
/// as each `END` node is built numbers a compunit's ENDs in exactly the order
/// rakudo's compiler would install them — including the ones nested inside a
/// block, a sub, or a method, which is what a source-*line* key could not
/// distinguish when several shared one physical line.
///
/// The counter is process-global and never reset: only the relative order of
/// one compunit's indices matters, and never reusing a value means an index
/// identifies one parsed `END` node uniquely, so a module's or an `EVAL`'s
/// node can never be mistaken for a main-compunit slot.
pub(crate) fn next_end_phaser_index() -> u32 {
    static NEXT: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(0);
    NEXT.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
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

/// Which routine declarator a multi-parameter closure literal was written
/// with, as recorded by the parser on [`Expr::AnonSubParams`].
///
/// raku models these with different nodes (`RakuAST::PointyBlock`,
/// `RakuAST::Sub`, `RakuAST::Method`, `RakuAST::Submethod`) and gives them
/// different runtime types, so the spelling has to survive parsing rather than
/// being guessed later. It is not cosmetic: `Block` is not a `return` boundary
/// and types its parameters `Mu`, while all three routine spellings are and
/// type them `Any`. A new construction site must record what the source
/// actually wrote.
fn declarator_sub() -> RoutineDeclarator {
    RoutineDeclarator::Sub
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum RoutineDeclarator {
    /// No routine declarator: a pointy block (`-> $a, $b { }`), a placeholder
    /// block (`{ $^a }`), and the closures the compiler/runtime synthesize.
    Block,
    /// `sub ($x) { }`, `anon Str sub { }`, one candidate of an anonymous
    /// `multi sub`.
    Sub,
    /// A `method (...) { }` literal, including its `anon` and `my` spellings.
    Method,
    /// A `submethod (...) { }` literal, including its `anon` and `my`
    /// spellings.
    Submethod,
}

impl RoutineDeclarator {
    /// True for every spelling that declares a `Routine` — the compile path
    /// that makes `return` a boundary and gives parameters an `Any` nominal
    /// type.
    pub(crate) fn is_routine(self) -> bool {
        !matches!(self, RoutineDeclarator::Block)
    }

    /// The `custom_traits` marker this declarator leaves on the declaration
    /// the compiler pools, so the closure-building opcode knows which
    /// `__mutsu_callable_type` to install in the captured environment.
    /// `Block` and `Sub` need none: each is what its compile path already
    /// produces.
    pub(crate) fn literal_marker(self) -> Option<&'static str> {
        match self {
            RoutineDeclarator::Method => Some(METHOD_LITERAL_MARKER),
            RoutineDeclarator::Submethod => Some(SUBMETHOD_LITERAL_MARKER),
            RoutineDeclarator::Block | RoutineDeclarator::Sub => None,
        }
    }

    /// Recover the declarator from the markers [`RoutineDeclarator::literal_marker`]
    /// left on a pooled declaration's `custom_traits`. `Sub` when no marker is
    /// present, which is what an unmarked routine declaration is.
    ///
    /// Both routine paths read the markers through this: the closure-building
    /// opcode for a `method (...) { }` literal, and `register_sub` for a named
    /// `my method foo` / `my submethod foo` declaration.
    pub(crate) fn from_markers<'a>(markers: impl IntoIterator<Item = &'a str>) -> Self {
        for marker in markers {
            match marker {
                METHOD_LITERAL_MARKER => return RoutineDeclarator::Method,
                SUBMETHOD_LITERAL_MARKER => return RoutineDeclarator::Submethod,
                _ => {}
            }
        }
        RoutineDeclarator::Sub
    }

    /// The `__mutsu_callable_type` this declarator installs, which is what
    /// `.^name` / `.WHAT` report (see `value::types_isa`). `None` for the two
    /// spellings whose compile path already produces the right type.
    pub(crate) fn callable_type(self) -> Option<&'static str> {
        match self {
            RoutineDeclarator::Method => Some("Method"),
            RoutineDeclarator::Submethod => Some("Submethod"),
            RoutineDeclarator::Block | RoutineDeclarator::Sub => None,
        }
    }
}

/// See [`RoutineDeclarator::literal_marker`]. Spelled out rather than built
/// from the type name: these are declaration markers, not the `__mutsu_<ns>::`
/// environment keys `MetaNs` owns.
pub(crate) const METHOD_LITERAL_MARKER: &str = "__method_literal";
/// See [`RoutineDeclarator::literal_marker`].
pub(crate) const SUBMETHOD_LITERAL_MARKER: &str = "__submethod_literal";

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
#[allow(clippy::enum_variant_names, dead_code)]
pub(crate) enum Expr {
    Literal(Value),
    /// A CORE term keyword (`True`, `False`, `Nil`, `Empty`, `Any`) parsed in a
    /// compunit that `use`d a module whose exports are computed by a run-time
    /// `sub EXPORT` hook, so the keyword's binding may be shadowed by whatever
    /// that hook installs (#9047).
    ///
    /// In Raku these are ordinary CORE-scope lexicals, not syntax, and an
    /// import that brings in a same-named symbol legitimately shadows them for
    /// the rest of the importing file — `Logic::Ternary` replaces all three of
    /// `True`/`Unknown`/`False` with three-valued-logic objects that way. mutsu
    /// folds them to `Literal` at parse time, which no run-time import can
    /// reach, so in a tainted compunit the parser emits this instead: the
    /// compiler keeps the folded `value` as the fallback and the VM prefers an
    /// `EXPORT`-installed binding of `name` when one exists. Outside such a
    /// compunit — everywhere else in the language — the constant folding is
    /// untouched.
    ShadowableTermKeyword {
        name: crate::symbol::Symbol,
        value: Value,
    },
    /// A bare, paren-less, argument-less use of an imported routine's name
    /// (`t;`, `t.hi`) parsed in a compunit that `use`d a module exporting
    /// through a run-time `sub EXPORT` hook (#9339).
    ///
    /// The static module scan learns `t` is a routine from a tag-exported
    /// `sub t is export(:t)` and so parses `t` as a zero-arg call, but the
    /// hook may install a sigilless TERM of the same name — and in Raku a
    /// bare `t` names that term (only `t()` calls the routine). Which names
    /// the hook installs is not knowable at parse time, so the choice is
    /// deferred to the VM: an `EXPORT`-installed `name` wins, otherwise `call`
    /// runs. Never emitted outside such a compunit.
    ExportTermOrCall {
        name: crate::symbol::Symbol,
        call: Box<Expr>,
    },
    /// A parser-created static regex with its source-level tree retained for
    /// RakuAST conversion. Execution still consumes `value` until the shared
    /// tree's lowering covers the whole regex grammar (ADR-0088).
    RegexLiteral {
        value: Value,
        tree: crate::regex_tree::RegexTree,
    },
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
    /// A parser-created static m/pattern/ with its source-level tree retained
    /// for RakuAST conversion. Execution remains the existing match opcode.
    MatchRegexTree {
        value: Value,
        tree: crate::regex_tree::RegexTree,
    },
    /// `m:pos(EXPR)/pattern/` / `m:continue(EXPR)/pattern/` (and the `:p`/`:c`
    /// spellings) whose adverb argument is not a compile-time-literal offset
    /// (e.g. `m:p($!pos)/.../`). Unlike `MatchRegex`, the base `value` is
    /// patched with a freshly-evaluated `pos`/`continue` position at every
    /// match, since a plain `Value` constant cannot carry a live expression.
    MatchRegexDynamicAdverbs {
        value: Value,
        pos_expr: Option<Box<Expr>>,
        continue_expr: Option<Box<Expr>>,
    },
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
        /// The RHS of an assignment-form substitution (`s[pat] = EXPR`,
        /// `S[pat] = EXPR`), parsed in the enclosing scope. It is a thunk, not
        /// a Block: it is evaluated per match with `$/` bound to that match, a
        /// placeholder or an anonymous `state` (`$++`) in it belongs to the
        /// enclosing block, and `replacement` is empty. `None` for the quote
        /// forms (`s/pat/repl/`), whose `replacement` is a `qq` source.
        replacement_thunk: Option<Box<Expr>>,
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
        /// The RHS of an assignment-form substitution (`s[pat] = EXPR`,
        /// `S[pat] = EXPR`), parsed in the enclosing scope. It is a thunk, not
        /// a Block: it is evaluated per match with `$/` bound to that match, a
        /// placeholder or an anonymous `state` (`$++`) in it belongs to the
        /// enclosing block, and `replacement` is empty. `None` for the quote
        /// forms (`s/pat/repl/`), whose `replacement` is a `qq` source.
        replacement_thunk: Option<Box<Expr>>,
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
        /// `is raw` trait. Read together with `is_rw` by the rw-capability
        /// oracle (`is_rw || is_raw || a return-rw in the body`), the same rule
        /// `FunctionDef` states for a named `sub`. Without its own field the
        /// trait was parsed and then dropped, so `my $f = sub () is raw { ... }`
        /// refused an lvalue assignment that rakudo accepts.
        is_raw: bool,
        /// true when this is a bare block `{ }`, false when it's `sub { }`.
        /// Bare blocks are NOT routine boundaries for `return`.
        is_block: bool,
        /// Declarator documentation the parser attached (`my $b = #| doc
        /// {; ... }`); the compiler carries it to the code object.
        #[serde(default)]
        doc: crate::decl_doc::DocSlot,
    },
    AnonSubParams {
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        return_type: Option<String>,
        body: Vec<Stmt>,
        is_rw: bool,
        /// `is raw` trait — see the note on [`Expr::AnonSub::is_raw`].
        is_raw: bool,
        /// Custom routine traits are applied when the anonymous sub is built.
        /// Boxed: see [`AnonSubTraits`].
        custom_traits: AnonSubTraits,
        /// True when this closure was generated by Whatever-currying.
        is_whatever_code: bool,
        /// Which routine declarator the source actually wrote. See
        /// [`RoutineDeclarator`] — it selects the compile path (a `Block` is
        /// not a `return` boundary and types its parameters `Mu`; every
        /// routine spelling is and types them `Any`), the runtime type the
        /// closure reports (`Sub` / `Method` / `Submethod`), and the node the
        /// RakuAST converter emits.
        declarator: RoutineDeclarator,
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
    /// Hyper operator with a function reference: `>>[&func]<<`, `<<[&func]>>`, etc.
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
    /// Run `body` and yield its last value.
    ///
    /// This node is overloaded: it is *both* the AST for a genuine source
    /// `do { ... }` block *and* the generic "run these statements, yield a
    /// value" vehicle that around forty parser/compiler desugars build (the
    /// chained-comparison temp-var lowering, `cas`, compound assignment,
    /// `.=` writeback, item context `$( ... )`, ...). Only the first kind is a
    /// Raku block, and `origin` is what tells them apart — see
    /// [`DoBlockOrigin`]. Build a desugar's node with
    /// [`Expr::desugar_block`] rather than writing the origin out by hand.
    DoBlock {
        body: Vec<Stmt>,
        /// The block's own label, or the `__mutsu_check_phaser__` sentinel a
        /// lifted CHECK phaser body carries. Whether the node is a source-level
        /// block is `origin`'s job, not this field's.
        label: Option<String>,
        origin: DoBlockOrigin,
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

/// What a [`Expr::DoBlock`] node actually is.
///
/// The node has two unrelated jobs, and telling them apart matters wherever a
/// question is really being asked about *Raku block scope* rather than about
/// "some statements that yield a value". The motivating one is `let`/`temp`:
/// those save the previous value and resolve it — restore on failure, commit
/// on success — **at the end of the enclosing block**. A synthesized wrapper
/// is not that block, so resolving a save at one would resolve it far too
/// early (GH-7635).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum DoBlockOrigin {
    /// Real `{ ... }` braces the user wrote, which *are* a Raku block: `do {
    /// ... }`, a labelled `L: { ... }`, and the statement prefixes whose block
    /// runs inline in the current frame (`lazy`/`sink`/`quietly`).
    SourceBlock,
    /// A parser or compiler desugar using the node as a generic sequencing
    /// vehicle. It introduces no scope of its own, so a `let` inside one still
    /// belongs to whatever real block encloses it.
    ///
    /// Item context `$( ... )` is deliberately here: `{ $seen = $(let $a = 23;
    /// $a); Mu }` restores `$a` when the *outer* block fails, an idiom roast
    /// leans on throughout (`S04-blocks-and-statements/let.t`). String
    /// interpolation `"{ ... }"` is here too — measured against Rakudo, its
    /// block does not resolve a save either.
    Desugar,
}

/// Secondary adverb on :exists subscript adverb
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
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

#[derive(Debug, Clone, Copy, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum HyperSliceAdverb {
    Kv,
    K,
    V,
    Tree,
    DeepK,
    DeepKv,
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum ControlFlowKind {
    Last,
    Next,
    Redo,
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum ForMode {
    Normal,
    Race,
    Hyper,
    /// `lazy for` — loop body executes lazily (not until Seq is consumed)
    Lazy,
}

/// The declaration a [`Stmt::PackageRuntimeBody`] belongs to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum PackageRuntimeDecl {
    /// A `class`/`grammar` declaration, with its lexical flag and site id.
    Class { is_lexical: bool, decl_id: u64 },
    /// A brace-scoped `package`/`module`.
    Package,
}

/// The declarator keyword used for a `Stmt::Package`. Determines the
/// `package-kind` reported by X::Attribute::Package.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum PackageKind {
    Module,
    Package,
    Grammar,
}

/// The source spelling of an enum's variant body, retained for RakuAST.
///
/// Runtime enum registration only needs the normalized `(name, value)` pairs,
/// but Rakudo's AST preserves whether the source used a word quote or a
/// parenthesized pair list.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum EnumVariantForm {
    Words,
    QuoteWords,
    PairList,
    Computed,
}

fn default_enum_variant_form() -> EnumVariantForm {
    EnumVariantForm::Words
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

/// Why a name is in the interpreter's readonly set. Rakudo reports several
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
///   `X::Assignment::RO`, "Cannot modify an immutable TYPE (VALUE)";
/// * a **sigiled variable** bound straight to a TYPE OBJECT (`$s := IB`) —
///   `X::AdHoc`, "assign requires a concrete object (got a IB type object
///   instead)" ([`ReadonlyKind::TypeObject`]).
///
/// Recording the kind where the readonly-ness is *decided* keeps these apart
/// without any name-based guessing at the (single, shared) check site.
///
/// [`ReadonlyKind::ImmutableDeep`] is a fourth, narrower kind layered on top
/// of the `Immutable` case: not a fresh exception class, but an extra fact
/// the same binding carries (see its own doc comment).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum ReadonlyKind {
    /// Readonly binding with a container behind it: parameters, `for` aliases.
    Alias,
    /// Sigiled variable bound directly to an immutable value (no container).
    Immutable,
    /// The name *is* an immutable value (sigilless term, immutable container).
    ImmutableValue,
    /// Like [`Self::Immutable`] (same "Cannot assign to an immutable value"
    /// on `$_ = ...`), plus a second refusal `Immutable` does not carry:
    /// method-based mutation through the binding is blocked too (`.value =
    /// ...` on a `Pair`/`Mix`/`Set`/`Bag` item). Used for the implicit `for`
    /// topic over an immutable `QuantHash` (ADR-0097 §5 slice 4's
    /// `deep_readonly`, formerly a `__mutsu_deep_readonly::<name>` env
    /// marker written and probed independently of the readonly-set mark it
    /// always accompanied): `for $b.values { $_ = 1 }` and `for $b.values {
    /// .value = 1 }` are refused for the same underlying reason, so they now
    /// share one mark instead of two side-by-side ones that could drift out
    /// of sync (see `Interpreter::restore_topic_readonly`, which used to
    /// restore only the `Alias`/`Immutable`/`ImmutableValue` half and always
    /// clear the deep half regardless of what the enclosing scope needed).
    ImmutableDeep,
    /// Sigiled variable bound directly to a TYPE OBJECT (`$s := IB`, `$s :=
    /// Int`), no container at all — like [`Self::Immutable`], but rakudo's
    /// wording for this shape names the type instead of the generic
    /// "immutable value": `X::AdHoc`, "assign requires a concrete object
    /// (got a IB type object instead)" (#9730).
    TypeObject,
}

/// What role a [`Stmt::Given`] plays in a `with`-family desugar.
///
/// The desugar is lossy on its own: `STMT with X` becomes
/// `given X { if $_.defined { STMT } }`, which a hand-written
/// `(STMT if $_.defined) given X` also produces. `Stmt::Given`'s `with_kind`
/// carries the distinction so the RakuAST converter can render
/// `StatementModifier::With` / `::Without` instead of guessing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum GivenWithKind {
    /// `STMT with EXPR` -- run `STMT` when the topic is defined.
    With,
    /// `STMT without EXPR` -- run `STMT` when the topic is NOT defined.
    Without,
    /// Not a keyword: the topicalizing `given` the `with`-family BLOCK forms
    /// wrap a `{ ... }` body in, so `$_` is established by the `given` opcode.
    /// raku does not model it as a `given` at all -- it is the
    /// `implicit-topic` `Block` of `Statement::With` / `::Without` /
    /// `::Orwith`.
    BlockTopic,
    /// The same scaffold, for a body written with an explicit signature
    /// (`with X -> $a { }`), which additionally binds the parameter inside the
    /// `given`. raku spells that as a `PointyBlock`, a shape the converter does
    /// not build yet -- so it is tagged distinctly, to report the boundary
    /// rather than render an implicit-topic block that has swallowed the
    /// binding.
    BlockTopicPointy,
}

/// Which `with`-family BLOCK keyword the parser desugared into a [`Stmt::If`].
///
/// Like [`GivenWithKind`] this exists because the desugar cannot be read back
/// off the resulting shape -- see `Stmt::If`'s `with_kind`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum WithBlockKind {
    /// `with EXPR { ... }` -- run the block, topicalized, when `EXPR` is defined.
    With,
    /// `without EXPR { ... }` -- run the block when `EXPR` is NOT defined.
    Without,
    /// `orwith EXPR { ... }` -- a `with` continuation clause, nested in the
    /// preceding conditional's else branch.
    Orwith,
}

/// One entry of [`Stmt::NestedTypeShells`].
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct NestedTypeShell {
    /// The packages enclosing the declaration inside its top-level statement,
    /// outermost first, as written (a `GLOBAL::` prefix makes one absolute).
    /// Empty = the package the top-level statement itself is in.
    pub(crate) packages: Vec<String>,
    /// The class or role declaration, as written.
    pub(crate) decl: Stmt,
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
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
    /// The site of a `method` declared in a nested block of a package body
    /// (`class C { do { sub helper { }; method m { helper() } } }`).
    ///
    /// A `method` declarator is has-scoped: it installs the method in the
    /// package wherever it lexically sits, while its body closes over the
    /// block's lexicals. The parser therefore hoists the declaration itself to
    /// the package body (carrying the `__nested_block_method` trait with the
    /// same `index`) and leaves this marker in the block. When the block runs,
    /// `closure` -- an anonymous method with the declaration's signature and
    /// body -- is built only for the lexical capture the ordinary closure
    /// machinery computes; the class-body walk then gives that capture to the
    /// hoisted method with the same `index`. `routines` names the `sub`s and
    /// `proto`s the enclosing blocks declare: they live in the routine
    /// registry, not in the closure env, and the registry forgets them when
    /// the block exits, so they are resolved into the capture as `&name`
    /// while it is live. See `parser::stmt::nested_block_methods`.
    NestedMethodCapture {
        index: u32,
        closure: Box<Expr>,
        routines: Vec<Symbol>,
    },
    /// The compile-time installation and composition of the `our` classes
    /// and roles declared inside the code of one top-level statement
    /// (#10470, #10494): a routine, block or loop body. Rakudo composes such
    /// a type when it compiles the declaration, so the composed roles' bodies
    /// run then, whether or not the enclosing code ever runs. The BEGIN
    /// prologue (ADR-0134) puts this marker where that statement's
    /// BEGIN-time effects go, so the shells register after the unit's earlier
    /// declarations and see its lexicals in their static state; the compiler
    /// emits a declaration-only shell registration for each entry
    /// (`Compiler::emit_type_decl_shell`). The declarations themselves stay in
    /// place, and an analysis walking the tree sees them there, not here.
    NestedTypeShells(Vec<NestedTypeShell>),
    /// Flag that the next slice assignment is a HYPER one (`%h<a b c> »=» 7`).
    ///
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
        /// True when the source assignment target was a sigilless term
        /// (`name = value`), rather than a sigiled scalar (`$name = value`).
        /// The two spellings share an environment key but are distinct lexical
        /// namespaces, so the compiler must retain this bit when a sigilless
        /// parameter shadows an enclosing scalar of the same name.
        #[serde(default)]
        target_is_sigilless: bool,
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
        /// Source-level regex tree, when this declaration was parsed from a
        /// static body. The body remains normalized for execution.
        #[serde(default)]
        source_regex: Option<crate::regex_tree::RegexTree>,
        /// `regex` shares the legacy TokenDecl execution path but has a
        /// distinct RakuAST declaration node.
        #[serde(default)]
        regex_kind: crate::regex_tree::RegexDeclKind,
        multi: bool,
        /// `my token foo` — lexically scoped; a duplicate is X::Redeclaration.
        is_my: bool,
        /// `our token foo` — package scoped; a duplicate is X::Redeclaration.
        is_our: bool,
        /// `token foo is export` — the Regex is importable under `&foo`.
        is_export: bool,
        /// Tags named by `is export(:TAG)`; `["DEFAULT"]` for a bare `is export`.
        export_tags: Vec<String>,
    },
    RuleDecl {
        name: Symbol,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        body: Vec<Stmt>,
        /// Source-level regex tree, when this declaration was parsed from a
        /// static body. The body remains normalized for execution.
        #[serde(default)]
        source_regex: Option<crate::regex_tree::RegexTree>,
        multi: bool,
        /// `rule foo is export` — the Regex is importable under `&foo`.
        is_export: bool,
        /// Tags named by `is export(:TAG)`; `["DEFAULT"]` for a bare `is export`.
        export_tags: Vec<String>,
    },
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
    /// The run-time part of a class or package body whose declaration the
    /// BEGIN prologue moved ahead (ADR-0134 §7, slice 1 residue). It re-enters
    /// the declared package at the declaration's source position and runs the
    /// body's bare statements and variable initializers there. The prologue
    /// keeps the declaration with only its BEGIN-time part.
    PackageRuntimeBody {
        name: Symbol,
        /// The run-time statements, in source order.
        body: Vec<Stmt>,
        /// The `my` lexicals the declaration's body declares, which `body`
        /// reads and writes through the package's static store.
        lexicals: Vec<String>,
        /// Which declaration the body belongs to, so the package name is
        /// qualified the way that declaration's own registration qualifies it.
        decl: PackageRuntimeDecl,
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
        /// (`use Foo:if($cond)`): the module is loaded only when `EXPR` is true.
        /// At a unit's top level it is evaluated in the BEGIN prologue
        /// (ADR-0134 §2.1.6). `None` for an unconditional `use`.
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
    Block(Vec<Stmt>),
    /// Non-lexical statement sequence used by parser desugarings.
    SyntheticBlock(Vec<Stmt>),
    /// Opens the region of a loop iteration whose early exit runs the loop's
    /// exit phasers; closed by the matching [`Stmt::LoopExitGuardEnd`] later in
    /// the same statement list. Emitted only by `expand_loop_phasers`.
    ///
    /// The guarded statements stay flat siblings between the two markers (not
    /// nested in this variant) so every analysis that walks the loop body sees
    /// them exactly as before. When a `next`/`last`/`redo`/`return` signal
    /// unwinds out of the region -- raised directly in the body, inside a
    /// `try`, or by a closure the body called -- the VM runs `next_ph` (only for
    /// a `next` that targets this loop, per `label`) and then `exit_ph`, and
    /// re-raises the signal. Both lists are copies of phaser bodies that also
    /// stay in the tree on the loop's normal-completion path.
    LoopExitGuard {
        label: Option<String>,
        next_ph: Vec<Stmt>,
        exit_ph: Vec<Stmt>,
    },
    /// Closes the innermost open [`Stmt::LoopExitGuard`] region.
    LoopExitGuardEnd,
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
        /// True when the source keyword was `unless`, i.e. `cond` holds the
        /// parser's synthetic `!` wrapper around the written condition and
        /// `else_branch` is necessarily empty (rakudo rejects `unless`/`else`
        /// at compile time). Carries no execution meaning — `unless X` and
        /// `if !X` run identically — but raku models them as different nodes
        /// (`RakuAST::Statement::Unless` vs `::If`), so the RakuAST converter
        /// needs the source keyword back. Mirrors `Stmt::While::is_until`.
        is_unless: bool,
        /// Set when this `If` is the lowering of a `with` / `without` /
        /// `orwith` BLOCK form rather than a source `if`. The desugar is lossy:
        /// `with X { BODY }` becomes
        /// `if (my $tmp = X).defined { given X { BODY } }`, which a
        /// hand-written conditional of the same shape also produces, and the
        /// synthetic temp's *name* is not a sound discriminator (a program may
        /// declare one). Carries no execution meaning; only the RakuAST
        /// converter reads it, to render `Statement::With` / `::Without` /
        /// `::Orwith`. Sibling of `is_unless`.
        #[serde(default)]
        with_kind: Option<WithBlockKind>,
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
        /// The pointy block's parameter names (`whenever $s -> $x { }`), in
        /// the same shape as `Expr::AnonSubParams::params`; empty for a bare
        /// block (whose first placeholder, if any, becomes the parameter).
        params: Vec<String>,
        /// The pointy block's full signature, as parsed by the ordinary
        /// pointy-block parser (types, sub-signatures, optional params).
        /// Empty for a single untyped `-> $x`, exactly like `Expr::Lambda`.
        param_defs: Vec<ParamDef>,
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
        /// Which source keyword produced this `Given`, when it was not `given`
        /// itself. `STMT with X` and `STMT without X` desugar to
        /// `given X { if $_.defined { STMT } }` (negated for `without`), which
        /// is exactly the shape a hand-written `(STMT if $_.defined) given X`
        /// produces -- so without this marker the source keyword is
        /// unrecoverable. Only the RakuAST converter reads it; execution
        /// treats every `Given` alike. Mirrors `Stmt::If`'s `is_unless` and
        /// `Stmt::While`'s `is_until`.
        with_kind: Option<GivenWithKind>,
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
    /// `DOC <phaser>` (e.g. `DOC INIT { ... }`): the phaser runs only under
    /// `--doc` and is a no-op in an ordinary run, as in rakudo.
    DocPhaser(Box<Stmt>),
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
        /// The original variant-body spelling for the RakuAST boundary.
        #[serde(default = "default_enum_variant_form")]
        variant_form: EnumVariantForm,
        is_export: bool,
        /// Export tags declared by `is export`, or empty for an untagged enum.
        #[serde(default)]
        export_tags: Vec<String>,
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
        /// Whether the trailing `Grammar` entry in `parents` was supplied
        /// implicitly, because a `grammar` declarator carried no `is` clause.
        /// A later `also is Parent` in the body replaces it rather than adding a
        /// second parent: Rakudo linearizes `grammar G { also is Base }` exactly
        /// like `grammar G is Base { }` (`G, Base, ...`), and keeping both would
        /// make the C3 merge inconsistent whenever `Base` itself is a grammar.
        #[serde(default)]
        implicit_grammar_parent: bool,
        /// True when the source used the `grammar` declarator. It must not be
        /// inferred from the synthesized `Grammar` parent.
        #[serde(default)]
        is_grammar: bool,
        /// Stable per-declaration-site id (parse-time assigned, non-zero) used to
        /// distinguish same-named lexical (`my`) classes in different scopes.
        /// 0 means "no stable site" (a runtime-synthesized node).
        ///
        /// Not serialized -- an id is only unique within the process that
        /// minted it -- but a node read back from the precompilation cache
        /// mints a fresh one, exactly as re-parsing the source would. It used
        /// to come back as 0, so a module's `my class` was registered under
        /// its mangled storage name when the module was parsed and under the
        /// bare name on every cache hit: the warm/cold divergence
        /// `crate::precomp`'s module docs warn about (#9733).
        #[serde(skip, default = "crate::ast::next_class_decl_id")]
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
        /// Parent names contributed by an `also is Parent` statement in the
        /// class *body* rather than by the declaration header. Rakudo applies
        /// `also is` at its position in the body, so such a parent can become
        /// resolvable only once the body has run -- it may be brought in by a
        /// `use` inside the body, or be a class the body itself declares.
        /// These names also appear in `parents`; this vector marks which of
        /// them may be deferred past the body instead of raising
        /// X::Inheritance::UnknownParent before the body has had a chance to
        /// introduce them.
        #[serde(default)]
        body_parents: Vec<String>,
    },
    HasDecl {
        name: Symbol,
        is_public: bool,
        default: Option<Expr>,
        handles: Vec<HandleSpec>,
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
        /// `HAS Type $.x` — NativeCall's *embedded* attribute declarator. The
        /// member's storage is inlined into the enclosing `is repr('CStruct')`
        /// / `'CPPStruct'` class instead of being held as a pointer to it.
        /// `false` for an ordinary `has`.
        is_embedded: bool,
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
        /// `default` was written with `:=`, not `=`. Only a CLASS-LEVEL
        /// attribute (`our @.x := @c` / `my @.x := @c`) can be bound —
        /// `has @.x := ...` is "Cannot use := to initialize an attribute" in
        /// rakudo — and the two spellings mean different things: a bind makes
        /// the accessor hand back the very container on the right, while `=`
        /// is an assignment and must copy it (#8150).
        #[serde(default)]
        default_is_bind: bool,
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
        /// Stable per-declaration-site id, exactly as `ClassDecl::decl_id`: a
        /// `my role` is stored under `Name\u{0}<decl_id>` so two same-named
        /// lexical roles in different scopes keep their own identity
        /// (ADR-0047 P1, #9894). 0 means "no stable site".
        #[serde(skip, default = "crate::ast::next_class_decl_id")]
        decl_id: u64,
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
        /// This synthetic statement came from an `is Parent` clause on a role
        /// header rather than from `does Parent`. Raku decides `is Foo` from
        /// whether `Foo` names a known type, never from its capitalisation, so
        /// an unknown `is` name is a custom `trait_mod:<is>` trait; an unknown
        /// `does` name is a typo. The two spellings are otherwise folded into
        /// the same statement, so without this flag the role path cannot tell
        /// them apart (#8100).
        #[serde(default)]
        from_is: bool,
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
        /// Whether the source wrote the base type (`subset S of Int`, `my Int
        /// subset S`) or left it out, in which case `base` holds the implied
        /// `Any`. Semantically the two are the same, but they are *different
        /// declarations* to raku's own model layer: `.AST` renders an explicit
        /// base as a `Trait::Of` entry and an implied one as no `traits` field
        /// at all, so collapsing them here would make the RakuAST converter
        /// invent a trait the source never wrote.
        #[serde(default)]
        base_is_explicit: bool,
        predicate: Option<Expr>,
        version: String,
        is_export: bool,
        export_tags: Vec<String>,
        /// `my subset F ...` — lexically scoped: NOT reachable (nor
        /// registered) under the enclosing package's qualified name.
        is_my: bool,
        /// Stable per-declaration-site id, exactly as `ClassDecl::decl_id`: a
        /// `my subset` is stored under `Name\u{0}<decl_id>` so two same-named
        /// lexical subsets in different scopes keep their own identity
        /// (ADR-0047 P1). 0 means "no stable site" (a synthesized node).
        #[serde(skip, default = "crate::ast::next_class_decl_id")]
        decl_id: u64,
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
        /// Source-order index of an `END` phaser, handed out by
        /// [`next_end_phaser_index`] as the parser walks past it. rakudo
        /// *installs* every END when its compunit is compiled, in source
        /// order, and runs them in reverse, so this index — not the order in
        /// which execution happens to reach the phaser — is what decides the
        /// exit-time run order (see `runtime::end_order`). It is also the key
        /// that lets the pre-registration pass (`runtime::end_phasers`)
        /// recognise, at run time, which already-installed phaser this node
        /// is. `None` for every non-`END` phaser and for the `END` nodes the
        /// runtime synthesises rather than parses.
        ///
        /// Not serialized: the precompilation cache would otherwise replay one
        /// process's index into another, where it names a completely different
        /// declaration. A module's ENDs are ordered by load order anyway, never
        /// by a main-compunit source index, so dropping it on a cache hit loses
        /// nothing.
        #[serde(skip)]
        end_index: Option<u32>,
    },
    ProtoDecl {
        name: Symbol,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        return_type: Option<String>,
        body: Vec<Stmt>,
        is_export: bool,
        /// Tags on `is export(:TAG1, :TAG2)`; empty means the untagged
        /// `is export` (DEFAULT). Without this, `import_module` could never
        /// see a proto's real tags and treated every exported proto as
        /// DEFAULT-only, so `use Mod :some-tag` silently dropped a whole
        /// multi family whose proto was `is export(:some-tag, :ALL)`.
        export_tags: Vec<String>,
        custom_traits: Vec<String>,
        /// The same traits as `custom_traits` with their argument
        /// expressions (`is also<a b>`), index-aligned. A `proto method`'s
        /// traits dispatch to a user `trait_mod:<is>` exactly as a `method`'s
        /// do, and that needs the argument.
        #[serde(default)]
        trait_args: Vec<(String, Option<Expr>)>,
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
        /// A *multi-level* element `temp` (`temp $s[1]<k>[1] = v`): `value` is
        /// then the whole element assignment (an `Expr::IndexAssign`), and the
        /// element its target names is what gets saved and restored -- `name`
        /// is only the base variable.
        nested_lvalue: bool,
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

#[derive(Debug, Clone, Copy, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum AssignOp {
    Assign,
    Bind,
    MatchAssign,
}

mod body_local_names;
mod chains;
mod placeholder_kind;
pub(crate) mod placeholders;
mod virtual_call;

pub(crate) use body_local_names::{collect_all_my_decl_names, collect_routine_body_local_names};
pub(crate) use placeholder_kind::ArgSupply;
pub(crate) use placeholders::{
    collect_placeholders, collect_placeholders_shallow, collect_unattached_placeholders,
    collect_where_assign_placeholders,
};
pub(crate) use virtual_call::first_virtual_call_in_expr;

impl Expr {
    /// Whether this expression is one of the syntactic empty import lists
    /// accepted by `use Module Empty` and `use Module ()`.
    ///
    /// These forms still load the module, but do not run its export hook or
    /// import any of its exported symbols. Keep this deliberately narrow:
    /// an arbitrary expression that happens to evaluate to an empty list may
    /// still be meaningful input to a module's `EXPORT` routine.
    pub(crate) fn is_empty_import_list(&self) -> bool {
        match self {
            Expr::Literal(value) => {
                matches!(value.view(), ValueView::Slip(items) if items.is_empty())
            }
            Expr::Grouped(inner) => {
                matches!(inner.as_ref(), Expr::ArrayLiteral(items) if items.is_empty())
            }
            _ => false,
        }
    }

    /// A [`DoBlockOrigin::Desugar`] [`Expr::DoBlock`]: run `body`, yield its
    /// last value, introduce no scope.
    ///
    /// This is the constructor every parser/compiler desugar wants. A genuine
    /// source `do { ... }` writes the struct literal out instead, with
    /// [`DoBlockOrigin::SourceBlock`] — there are only a handful of those, and
    /// spelling them the verbose way keeps them greppable.
    pub(crate) fn desugar_block(body: Vec<Stmt>) -> Expr {
        Expr::DoBlock {
            body,
            label: None,
            origin: DoBlockOrigin::Desugar,
        }
    }

    /// Look through the parenthesization markers the parser records, returning
    /// the expression the source actually wrote inside the parentheses.
    ///
    /// The parser marks *every* `(...)` (see
    /// `parser::primary::container::paren::mark_parenthesized`), so a consumer
    /// that pattern-matches a shape must ask for the shape through this, not
    /// match `Expr` directly, unless it genuinely cares whether parentheses
    /// were written (junction chain flattening, list assignment, the Whatever
    /// freeze).
    pub fn peel_parens(&self) -> &Expr {
        let mut expr = self;
        while let Expr::Grouped(inner) = expr {
            expr = inner;
        }
        expr
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

/// Whether `stmts` reads the legacy argument array `@_` anywhere.
///
/// This is the ONE thing that makes a routine accept more positional arguments
/// than its signature names: rakudo refuses a surplus for `sub f { $^x }` but
/// allows it for `sub f { $^x; @_.elems }`, where the leftovers flow into `@_`.
/// Measured 2026-09-08 — and note that a `%_` read does NOT buy the same
/// leniency (`sub f { $^x; %_.elems }` called with three positionals dies),
/// because `%_` is about *named* arguments and has no bearing on positional
/// arity.
///
/// The debug-format probe is the same one `make_anon_sub` has always used for
/// the signature-less-block case; it is deliberately conservative, since a
/// false positive only makes a call more permissive than rakudo and a false
/// negative only leaves the pre-existing behaviour.
///
/// A literal value statement is skipped: it reads no variable, and a
/// `token`/`rule` body is one whose regex value can carry its captured scope
/// (closures included, whose formatted form is unbounded).
pub(crate) fn body_reads_args_array(stmts: &[Stmt]) -> bool {
    stmts
        .iter()
        .filter(|stmt| !matches!(stmt, Stmt::Expr(Expr::Literal(_))))
        .any(|stmt| format!("{stmt:?}").contains("ArrayVar(\"_\")"))
}

/// Whether `stmts` reads the legacy named-argument hash `%_` anywhere.
///
/// Unlike `@_`, a `%_` read does not make a signature-less routine accept
/// surplus positional arguments. It does, however, require the implicit
/// named slurpy so that the block can observe named arguments passed to it.
/// Keep this separate from [`body_reads_args_array`] because the two legacy
/// aggregates have different call-arity semantics.
pub(crate) fn body_reads_args_hash(stmts: &[Stmt]) -> bool {
    format!("{stmts:?}").contains("HashVar(\"_\")")
}

/// Create an `Expr::AnonSub` or `Expr::AnonSubParams` depending on whether
/// the block body contains placeholder variables (`$^a`, `$^b`, etc.).
pub(crate) fn make_anon_sub(stmts: Vec<Stmt>) -> Expr {
    let placeholders = collect_placeholders_shallow(&stmts);
    if placeholders.is_empty() {
        // A signature-less block has an implicit `*@_`/`*%_` when it reads
        // the corresponding legacy argument aggregate. Keep that distinction
        // from an explicitly empty `-> {}` signature, which still rejects
        // positional arguments.
        let uses_at_underscore = body_reads_args_array(&stmts);
        let uses_hash_underscore = body_reads_args_hash(&stmts);
        if uses_at_underscore || uses_hash_underscore {
            let legacy_params = [
                uses_at_underscore.then_some("@_"),
                uses_hash_underscore.then_some("%_"),
            ]
            .into_iter()
            .flatten()
            .map(str::to_string)
            .collect::<Vec<_>>();
            let param_defs = legacy_params
                .iter()
                .map(|name| ParamDef {
                    type_capture: None,
                    name: name.clone(),
                    default: None,
                    multi_invocant: true,
                    required: false,
                    named: false,
                    named_alias: false,
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
                    code: Default::default(),
                    trait_args: Vec::new(),
                })
                .collect();
            return Expr::AnonSubParams {
                params: legacy_params,
                param_defs,
                return_type: None,
                body: stmts,
                is_rw: false,
                is_raw: false,
                custom_traits: Default::default(),
                is_whatever_code: false,
                declarator: crate::ast::RoutineDeclarator::Block,
            };
        }
        Expr::AnonSub {
            body: stmts,
            is_rw: false,
            is_raw: false,
            is_block: true,
            doc: Default::default(),
        }
    } else {
        let uses_hash_underscore = body_reads_args_hash(&stmts);
        let mut params = placeholders.clone();
        let mut param_defs: Vec<ParamDef> = placeholders
            .iter()
            .map(|name| {
                // Named placeholders use `:` twigil: $:f, @:f, %:f
                let is_named = name.contains(':');
                ParamDef {
                    type_capture: None,
                    name: name.clone(),
                    default: None,
                    multi_invocant: true,
                    required: is_named,
                    named: is_named,
                    named_alias: false,
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
                    code: Default::default(),
                    trait_args: Vec::new(),
                }
            })
            .collect();
        if uses_hash_underscore {
            params.push("%_".to_string());
            param_defs.push(ParamDef {
                type_capture: None,
                name: "%_".to_string(),
                default: None,
                multi_invocant: true,
                required: false,
                named: false,
                named_alias: false,
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
                code: Default::default(),
                trait_args: Vec::new(),
            });
        }
        Expr::AnonSubParams {
            params,
            param_defs,
            return_type: None,
            body: stmts,
            is_rw: false,
            is_raw: false,
            custom_traits: Default::default(),
            is_whatever_code: false,
            declarator: crate::ast::RoutineDeclarator::Block,
        }
    }
}

/// Build the execution closure for a RakuAST `Block` whose body contains an
/// implicit legacy placeholder.  RakuAST has already told us that `%_` is the
/// block's own named-slurpy declaration, so keep it distinct from the parser's
/// ordinary signature-less block.  The latter may capture an enclosing
/// method's `%_` instead of introducing a shadowing hash.
pub(crate) fn make_rakuast_anon_sub(stmts: Vec<Stmt>) -> Expr {
    let expr = make_anon_sub(stmts);
    if let Expr::AnonSubParams {
        params, param_defs, ..
    } = &expr
        && params.len() == 1
        && params[0] == "%_"
        && param_defs.len() == 1
        && param_defs[0].name == "%_"
    {
        let Expr::AnonSubParams {
            params,
            mut param_defs,
            return_type,
            body,
            is_rw,
            is_raw,
            custom_traits,
            is_whatever_code,
            declarator,
        } = expr
        else {
            unreachable!("the RakuAST legacy placeholder shape was checked above")
        };
        param_defs[0].block_param = false;
        return Expr::AnonSubParams {
            params,
            param_defs,
            return_type,
            body,
            is_rw,
            is_raw,
            custom_traits,
            is_whatever_code,
            declarator,
        };
    }
    expr
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
            is_unless: false,
            with_kind: None,
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

/// The custom routine traits of an anonymous sub (`sub () is foo(1) { }`),
/// together with its declarator documentation (`my $f = #| doc\n sub () { }`),
/// stored behind one optional box. Almost every anonymous sub has neither, and
/// an inline `Vec` made `AnonSubParams` the widest variant, taking every `Expr`
/// from 120 to 128 bytes. The parser and compiler recurse on `Expr` values, so that width
/// is paid in every stack frame; it was enough to overflow the 2 MiB test
/// thread stack in a debug build (`expr_size_guard` pins the size).
#[derive(Debug, Clone, Default, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct AnonSubTraits(Option<Box<AnonSubExtras>>);

/// The boxed payload of [`AnonSubTraits`].
#[derive(Debug, Clone, Default, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct AnonSubExtras {
    traits: Vec<AnonSubTrait>,
    doc: crate::decl_doc::DocSlot,
}

/// One custom routine trait: its name and optional argument.
pub(crate) type AnonSubTrait = (String, Option<Expr>);

impl AnonSubTraits {
    pub(crate) fn as_slice(&self) -> &[AnonSubTrait] {
        self.0.as_deref().map_or(&[], |extras| &extras.traits)
    }

    /// The traits, for a rewriting pass ([`crate::ast_visit::VisitMut`]).
    pub(crate) fn as_mut_slice(&mut self) -> &mut [AnonSubTrait] {
        self.0
            .as_deref_mut()
            .map_or(&mut [], |extras| &mut extras.traits)
    }

    /// The declarator documentation the parser attached to this sub.
    pub(crate) fn doc(&self) -> Option<&crate::decl_doc::DeclDoc> {
        self.0.as_deref().and_then(|extras| extras.doc.get())
    }

    /// Give this sub a documentation slot for the parser to fill.
    pub(crate) fn set_doc_slot(&mut self, slot: crate::decl_doc::DocSlot) {
        self.0.get_or_insert_with(Box::default).doc = slot;
    }
}

impl From<Vec<AnonSubTrait>> for AnonSubTraits {
    fn from(v: Vec<AnonSubTrait>) -> Self {
        Self((!v.is_empty()).then(|| {
            Box::new(AnonSubExtras {
                traits: v,
                doc: Default::default(),
            })
        }))
    }
}

impl std::ops::Deref for AnonSubTraits {
    type Target = [AnonSubTrait];
    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

#[cfg(test)]
mod expr_size_guard {
    #[test]
    fn expr_stays_small() {
        // The parser and compiler recurse on `Expr`, so its width is paid in
        // every frame of that recursion. A 24-byte `Vec` added inline to one
        // variant took it from 120 to 128 bytes and overflowed a debug test
        // thread's stack. Box a new rarely-used payload instead of widening
        // the enum.
        let sz = std::mem::size_of::<super::Expr>();
        assert!(sz <= 120, "size_of::<Expr>() = {sz}, expected <= 120");
    }
}
