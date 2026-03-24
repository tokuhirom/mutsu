use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

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
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub(crate) struct FunctionDef {
    pub(crate) package: Symbol,
    pub(crate) name: Symbol,
    pub(crate) params: Vec<String>,
    pub(crate) param_defs: Vec<ParamDef>,
    pub(crate) body: Vec<Stmt>,
    pub(crate) is_test_assertion: bool,
    pub(crate) is_rw: bool,
    pub(crate) is_raw: bool,
    /// True when this routine represents an `our method` code reference.
    pub(crate) is_method: bool,
    /// When true, this sub has an explicit empty signature `()` and should reject any arguments.
    pub(crate) empty_sig: bool,
    /// Return type annotation (e.g., "Str", "Str(Numeric:D)", "Foo:D()")
    pub(crate) return_type: Option<String>,
}

pub(crate) fn function_body_fingerprint(
    params: &[String],
    param_defs: &[ParamDef],
    body: &[Stmt],
) -> u64 {
    let mut hasher = DefaultHasher::new();
    format!("{:?}", params).hash(&mut hasher);
    format!("{:?}", param_defs).hash(&mut hasher);
    format!("{:?}", body).hash(&mut hasher);
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
    Whatever,
    HyperWhatever,
    BareWord(String),
    StringInterpolation(Vec<Expr>),
    /// Deferred heredoc interpolation: stores raw content to be interpolated
    /// at compile time in the scope where the AST node appears, not where
    /// the qq:to declaration was parsed. This is needed because Raku resolves
    /// heredoc body variables in the scope of the terminator, not the declaration.
    HeredocInterpolation(String),
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
        x: Option<usize>,
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
        x: Option<usize>,
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
    },
    AnonSubParams {
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        return_type: Option<String>,
        body: Vec<Stmt>,
        is_rw: bool,
    },
    CallOn {
        target: Box<Expr>,
        args: Vec<Expr>,
    },
    Lambda {
        param: String,
        body: Vec<Stmt>,
    },
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
    },
    /// Multi-dimensional indexing with semicolons: @a[$x;$y;$z]
    MultiDimIndex {
        target: Box<Expr>,
        dimensions: Vec<Expr>,
    },
    /// Multi-dimensional index assignment: @a[$x;$y;$z] = value
    MultiDimIndexAssign {
        target: Box<Expr>,
        dimensions: Vec<Expr>,
        value: Box<Expr>,
    },
    IndexAssign {
        target: Box<Expr>,
        index: Box<Expr>,
        value: Box<Expr>,
    },
    Ternary {
        cond: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    AssignExpr {
        name: String,
        expr: Box<Expr>,
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
    /// Hash hyperindex: %hash{||@keys}
    HyperIndex {
        target: Box<Expr>,
        keys: Box<Expr>,
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
    MarkReadonly(String),
    /// Flag that the next VarDecl in this SyntheticBlock uses `:=` binding.
    MarkBind,
    /// Mark a sigilless variable as readonly via `__mutsu_sigilless_readonly::NAME` env key.
    MarkSigillessReadonly(String),
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
        signature_alternates: Vec<(Vec<String>, Vec<ParamDef>)>,
        body: Vec<Stmt>,
        multi: bool,
        is_rw: bool,
        is_raw: bool,
        is_export: bool,
        export_tags: Vec<String>,
        is_test_assertion: bool,
        supersede: bool,
        /// Custom `is` traits (non-builtin trait names like `me'd`)
        custom_traits: Vec<String>,
    },
    TokenDecl {
        name: Symbol,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        body: Vec<Stmt>,
        multi: bool,
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
        /// True for `unit module Foo;` / `unit package Foo;` where the scope
        /// extends to the rest of the enclosing scope, false for brace-scoped
        /// `package Foo { ... }`.
        is_unit: bool,
    },
    Return(Expr),
    For {
        iterable: Expr,
        param: Option<String>,
        param_def: Box<Option<ParamDef>>,
        params: Vec<String>,
        body: Vec<Stmt>,
        label: Option<String>,
        mode: ForMode,
        /// True when `<->` is used, making all params rw.
        rw_block: bool,
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
    },
    /// `no Module ...;` — disable pragma/module effects for current lexical scope.
    No {
        module: String,
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
    },
    While {
        cond: Expr,
        body: Vec<Stmt>,
        label: Option<String>,
    },
    Loop {
        init: Option<Box<Stmt>>,
        cond: Option<Expr>,
        step: Option<Expr>,
        body: Vec<Stmt>,
        repeat: bool,
        label: Option<String>,
    },
    React {
        body: Vec<Stmt>,
    },
    Whenever {
        supply: Expr,
        param: Option<String>,
        body: Vec<Stmt>,
    },
    Last(Option<String>),
    Next(Option<String>),
    Redo(Option<String>),
    Proceed,
    Succeed,
    /// `done` — terminate the innermost react event loop
    ReactDone,
    Given {
        topic: Expr,
        body: Vec<Stmt>,
    },
    When {
        cond: Expr,
        body: Vec<Stmt>,
    },
    Default(Vec<Stmt>),
    Die(Expr),
    Fail(Expr),
    Catch(Vec<Stmt>),
    Control(Vec<Stmt>),
    Take(Expr),
    Goto(Expr),
    Label {
        name: String,
        stmt: Box<Stmt>,
    },
    EnumDecl {
        name: Symbol,
        variants: Vec<(String, Option<Expr>)>,
        is_export: bool,
        /// Base type constraint (e.g., `my Str enum ...` has base_type = Some("Str"))
        base_type: Option<String>,
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
    },
    MethodDecl {
        name: Symbol,
        name_expr: Option<Expr>,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        body: Vec<Stmt>,
        multi: bool,
        is_rw: bool,
        is_private: bool,
        is_our: bool,
        is_my: bool,
        return_type: Option<String>,
    },
    RoleDecl {
        name: Symbol,
        type_params: Vec<String>,
        type_param_defs: Vec<ParamDef>,
        is_export: bool,
        export_tags: Vec<String>,
        body: Vec<Stmt>,
        /// Language version active when this role was declared (e.g., "6.c", "6.d", "6.e")
        language_version: String,
    },
    DoesDecl {
        name: Symbol,
    },
    TrustsDecl {
        name: Symbol,
    },
    AugmentClass {
        name: Symbol,
        body: Vec<Stmt>,
    },
    SubsetDecl {
        name: Symbol,
        base: String,
        predicate: Option<Expr>,
        version: String,
    },
    Phaser {
        kind: PhaserKind,
        body: Vec<Stmt>,
    },
    ProtoDecl {
        name: Symbol,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        body: Vec<Stmt>,
        is_export: bool,
        custom_traits: Vec<String>,
    },
    Let {
        name: String,
        index: Option<Box<Expr>>,
        value: Option<Box<Expr>>,
        is_temp: bool,
    },
    TempMethodAssign {
        var_name: String,
        method_name: String,
        method_args: Vec<Expr>,
        value: Expr,
    },
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
        | Stmt::Take(e)
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
        Stmt::Given { topic, body } => {
            collect_ph_expr(topic, out);
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::When { cond, body } => {
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

fn collect_ph_expr(expr: &Expr, out: &mut Vec<String>) {
    match expr {
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
        Expr::Call { args, .. } => {
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
        Expr::Index { target, index } => {
            collect_ph_expr(target, out);
            collect_ph_expr(index, out);
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
        Expr::Lambda { body, .. } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
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
        Expr::Reduction { expr, .. } | Expr::Eager(expr) => collect_ph_expr(expr, out),
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

/// Check if a bare variable reference ($name or $name = ...) appears before
/// the corresponding placeholder variable ($^name) in statement order.
/// Used to detect X::Undeclared and X::Placeholder::NonPlaceholder.
pub(crate) fn bare_precedes_placeholder(stmts: &[Stmt], bare_name: &str) -> bool {
    let ph_name = format!("^{}", bare_name);
    let mut ph_seen = false;
    for stmt in stmts {
        if stmt_contains_var_named(stmt, &ph_name) {
            ph_seen = true;
        }
        if !ph_seen && stmt_references_bare(stmt, bare_name) {
            return true;
        }
    }
    false
}

/// Check if a statement contains a variable reference Var(name).
fn stmt_contains_var_named(stmt: &Stmt, var_name: &str) -> bool {
    let mut found = false;
    check_bare_var_stmt(stmt, var_name, &mut found);
    found
}

/// Check if a statement references a bare variable (Var(name) or Assign{name}).
fn stmt_references_bare(stmt: &Stmt, bare_name: &str) -> bool {
    // Check assignment target name
    if let Stmt::Assign { name, .. } = stmt
        && name == bare_name
    {
        return true;
    }
    let mut found = false;
    check_bare_var_stmt(stmt, bare_name, &mut found);
    found
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

fn check_bare_var_stmt(stmt: &Stmt, bare_name: &str, found: &mut bool) {
    match stmt {
        Stmt::Expr(e) | Stmt::Return(e) | Stmt::Die(e) | Stmt::Fail(e) | Stmt::Take(e) => {
            check_bare_var_expr(e, bare_name, found);
        }
        Stmt::Assign { expr, .. } => check_bare_var_expr(expr, bare_name, found),
        Stmt::Say(es) | Stmt::Put(es) | Stmt::Print(es) | Stmt::Note(es) => {
            for e in es {
                check_bare_var_expr(e, bare_name, found);
            }
        }
        Stmt::Call { args, .. } => {
            for arg in args {
                match arg {
                    CallArg::Positional(e) | CallArg::Invocant(e) => {
                        check_bare_var_expr(e, bare_name, found)
                    }
                    CallArg::Named { value: Some(e), .. } => {
                        check_bare_var_expr(e, bare_name, found)
                    }
                    _ => {}
                }
            }
        }
        _ => {}
    }
}

fn check_bare_var_expr(expr: &Expr, bare_name: &str, found: &mut bool) {
    match expr {
        Expr::Var(name) if name == bare_name => *found = true,
        Expr::Binary { left, right, .. } => {
            check_bare_var_expr(left, bare_name, found);
            check_bare_var_expr(right, bare_name, found);
        }
        Expr::Unary { expr, .. } => check_bare_var_expr(expr, bare_name, found),
        Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
            check_bare_var_expr(target, bare_name, found);
            for a in args {
                check_bare_var_expr(a, bare_name, found);
            }
        }
        Expr::DynamicMethodCall {
            target,
            name_expr,
            args,
        }
        | Expr::HyperMethodCallDynamic {
            target,
            name_expr,
            args,
            ..
        } => {
            check_bare_var_expr(target, bare_name, found);
            check_bare_var_expr(name_expr, bare_name, found);
            for a in args {
                check_bare_var_expr(a, bare_name, found);
            }
        }
        Expr::Call { args, .. } => {
            for a in args {
                check_bare_var_expr(a, bare_name, found);
            }
        }
        Expr::StringInterpolation(parts) => {
            for p in parts {
                check_bare_var_expr(p, bare_name, found);
            }
        }
        Expr::AssignExpr { expr, .. } => check_bare_var_expr(expr, bare_name, found),
        _ => {}
    }
}

/// Create an `Expr::AnonSub` or `Expr::AnonSubParams` depending on whether
/// the block body contains placeholder variables (`$^a`, `$^b`, etc.).
pub(crate) fn make_anon_sub(stmts: Vec<Stmt>) -> Expr {
    let placeholders = collect_placeholders(&stmts);
    if placeholders.is_empty() {
        Expr::AnonSub {
            body: stmts,
            is_rw: false,
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
                        optional_marker: false,
                        outer_sub_signature: None,
                        code_signature: None,
                        is_invocant: false,
                        shape_constraints: None,
                    }
                })
                .collect(),
            return_type: None,
            body: stmts,
            is_rw: false,
        }
    }
}
