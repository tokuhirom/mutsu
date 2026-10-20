//! Compile attribute source fields once into the descriptor shared by consumers.

use super::DeclTraitArg;
use crate::ast::{Expr, Stmt};
use crate::value::ValueView;

/// Precompiled replacements for `CompiledAttrDecl::from_stmt`'s AST-only
/// declaration-time expressions (defaults, constraints and custom traits),
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
    pub(crate) unknown_traits: Option<Vec<(String, String, Option<DeclTraitArg>)>>,
}

/// A typed mirror of `Stmt::HasDecl` (ADR-0019 D2b), built once by
/// [`CompiledAttrDecl::from_stmt`] instead of being re-destructured with an
/// 18-field pattern at each of the class-body, role-body, augment, and
/// mainline/EVAL `has`-registration sites. `name` is the resolved (twigil-free)
/// attribute name and `is_default` is unboxed, matching what every consumer
/// actually reads; everything else mirrors the AST field for field.
/// Defaults, constraints and custom trait arguments are `DeclTraitArg` rather than a raw `Expr`
/// (ADR-0019 D2c-4, matching `ClassAttributeDef`'s own D2c-2 field type) —
/// every reader runs them through `Interpreter::eval_decl_trait_arg`/
/// `.literal()` instead of matching `Expr::Literal` directly.
#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
#[bincode(decode_context = "crate::precomp_codec::DecodeCtx")]
pub(crate) struct CompiledAttrDecl {
    pub(crate) name: String,
    pub(crate) is_public: bool,
    pub(crate) default: Option<DeclTraitArg>,
    #[bincode(with_serde)]
    pub(crate) handles: Vec<crate::ast::HandleSpec>,
    pub(crate) is_rw: bool,
    pub(crate) is_readonly: bool,
    pub(crate) type_constraint: Option<String>,
    pub(crate) type_smiley: Option<String>,
    pub(crate) is_required: Option<Option<String>>,
    pub(crate) sigil: char,
    pub(crate) where_constraint: Option<DeclTraitArg>,
    pub(crate) is_alias: bool,
    /// `HAS Type $.x` — NativeCall's embedded attribute declarator (the member
    /// is stored by value inside the enclosing CStruct).
    pub(crate) is_embedded: bool,
    pub(crate) is_our: bool,
    pub(crate) is_my: bool,
    /// The `is default(...)` trait argument (ADR-0019 D2c). `Ast` unless a
    /// caller with compiler access at plan-lowering time supplied a
    /// precompiled `Literal`/`Compiled` replacement — see [`Self::from_stmt`].
    pub(crate) is_default: Option<DeclTraitArg>,
    pub(crate) is_type: Option<String>,
    pub(crate) deprecated_message: Option<String>,
    pub(crate) is_built: Option<bool>,
    pub(crate) unknown_traits: Vec<(String, String, Option<DeclTraitArg>)>,
    /// Declared shape dimensions for an `@`-sigil attribute (`has @.a[2]`),
    /// extracted once from the raw `default` expression's compiler-generated
    /// `Array.new(:shape(...))` pattern (ADR-0019 D2c-4, the D2a precompute
    /// pattern) — `default` above no longer carries a raw `Expr` a consumer
    /// could re-inspect at construction time.
    pub(crate) declared_shape: Option<Vec<usize>>,
    /// True when `default` matches the `Array.new(:shape(...))` pattern but
    /// at least one dimension is not a literal integer (`has @.a[N]` with a
    /// named `constant N`), so `declared_shape` above came back `None` even
    /// though this genuinely is a shaped-array declaration (#8032). A
    /// registration site with `&mut self` available can still resolve it —
    /// `default` is exactly `Array.new(:shape(N))`, and evaluating N through
    /// the same `constant`/enum lookup a normal read would use is safe,
    /// unlike guessing. See `Interpreter::resolve_dynamic_attr_shape`.
    pub(crate) dynamic_shape: bool,
    /// `default` was written with `:=` rather than `=` — only reachable for a
    /// class-level attribute (`our @.x := @c`), and the reason the class-level
    /// registration path stores the right-hand container itself instead of a
    /// copy. See `Stmt::HasDecl::default_is_bind`.
    pub(crate) default_is_bind: bool,
    /// `default` is the parser's synthesized seed, not a written initializer.
    /// See `Stmt::HasDecl::default_is_seed`.
    pub(crate) default_is_seed: bool,
    /// The line the `has` keyword sits on, filled in by the class-body walk
    /// (`Compiler::compile_class_attr_decls`) that tracks `Stmt::SetLine`
    /// markers as it visits each declaration -- `from_stmt` itself has no
    /// line history to draw on, so this starts `None` and is set by the
    /// caller afterward, the same two-step `decl_line` already used for
    /// `compile_method_body_keys`.
    pub(crate) decl_line: Option<i64>,
}

impl CompiledAttrDecl {
    /// Executable attribute expressions share capture and environment analysis.
    // Cost: O(t), t = custom traits.
    pub(crate) fn declaration_args(&self) -> impl Iterator<Item = &DeclTraitArg> {
        [
            self.default.as_ref(),
            self.where_constraint.as_ref(),
            self.is_default.as_ref(),
        ]
        .into_iter()
        .flatten()
        .chain(
            self.unknown_traits
                .iter()
                .filter_map(|(_, _, argument)| argument.as_ref()),
        )
    }

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
            handles_terms: _,
            is_rw,
            is_readonly,
            type_constraint,
            hash_key_type,
            hash_key_type_expr: _,
            type_smiley,
            is_required,
            sigil,
            where_constraint,
            is_alias,
            is_embedded,
            is_our,
            is_my,
            is_default,
            is_type,
            deprecated_message,
            is_built,
            unknown_traits,
            default_is_bind,
            default_is_seed,
            default_is_trait: _,
            trait_order: _,
        } = stmt
        else {
            unreachable!("CompiledAttrDecl::from_stmt called on a non-HasDecl statement");
        };
        let (declared_shape, has_shape_pattern) = attr_declared_shape(default.as_ref());
        let dynamic_shape = has_shape_pattern && declared_shape.is_none();
        CompiledAttrDecl {
            name: name.resolve(),
            is_public: *is_public,
            default: chunks
                .default
                .or_else(|| default.clone().map(|e| DeclTraitArg::Ast(Box::new(e)))),
            handles: handles.clone(),
            is_rw: *is_rw,
            is_readonly: *is_readonly,
            type_constraint: hash_key_type.as_deref().map_or_else(
                || type_constraint.clone(),
                |key| {
                    Some(crate::ast::keyed_hash::join(
                        type_constraint.as_deref(),
                        key,
                    ))
                },
            ),
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
            is_embedded: *is_embedded,
            is_our: *is_our,
            is_my: *is_my,
            is_default: chunks
                .is_default
                .or_else(|| is_default.clone().map(|e| DeclTraitArg::Ast(Box::new(e)))),
            is_type: is_type.clone(),
            deprecated_message: deprecated_message.clone(),
            is_built: *is_built,
            unknown_traits: chunks.unknown_traits.unwrap_or_else(|| {
                // Existing AST-only registration adapters have no compiler plan.
                // TODO: migrate those adapters to immutable declaration plans.
                unknown_traits
                    .iter()
                    .map(|(kind, name, argument)| {
                        (
                            kind.clone(),
                            name.clone(),
                            argument
                                .clone()
                                .map(|expr| DeclTraitArg::Ast(Box::new(expr))),
                        )
                    })
                    .collect()
            }),
            declared_shape,
            dynamic_shape,
            default_is_bind: *default_is_bind,
            default_is_seed: *default_is_seed,
            decl_line: None,
        }
    }
}

/// Extract shape dimensions from a default expression that matches the
/// pattern `Array.new(:shape(N))` or `Array.new(:shape(N, M, ...))`, as
/// generated for `has @.a[2]` or `has @.a[2;3]` declarations. A pure
/// syntactic fact about the raw `default` expression (ADR-0019 D2c-4,
/// following the D2a precompute pattern), so it is derived once here —
/// before `default` is lowered to a `DeclTraitArg` and this pattern-match
/// becomes unreachable — instead of on every instance construction via
/// `DeclTraitArg::as_expr()` (which panics on a `Compiled` chunk).
/// Returns `(literal dims if every dimension is a literal integer, whether
/// `default` matches the shaped-array pattern at all)`. The second element
/// lets a caller distinguish "not a shaped declaration" from "a shaped
/// declaration whose dimensions need a runtime lookup" (`dynamic_shape`
/// above) — both look like `None` if only the first is kept.
fn attr_declared_shape(default: Option<&Expr>) -> (Option<Vec<usize>>, bool) {
    let Some(expr) = default else {
        return (None, false);
    };
    // Match Array.new(:shape(...)) or Array.new(:shape(...), :data(...))
    let Expr::MethodCall {
        target, name, args, ..
    } = expr
    else {
        return (None, false);
    };
    if name.resolve() != "new" {
        return (None, false);
    }
    if !matches!(target.as_ref(), Expr::BareWord(s) if s == "Array") {
        return (None, false);
    }
    // Find the :shape(...) pair in args
    for arg in args {
        if let Expr::Binary {
            left,
            op: crate::token_kind::TokenKind::FatArrow,
            right,
            ..
        } = arg
            && let Expr::Literal(lit) = left.as_ref()
            && let ValueView::Str(key) = lit.view()
            && key.as_str() == "shape"
        {
            return (attr_shape_dims_from_expr(right), true);
        }
    }
    (None, false)
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
