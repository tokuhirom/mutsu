//! The head statements that bind a pointy block's parameter to the value a
//! control statement tested or topicalized: `given EXPR -> PARAM`, `with` /
//! `if COND -> PARAM`. One implementation for the parser's expansion and for
//! `rakuast::lower` (a hand-built `Statement::Given` over a `PointyBlock`).

use crate::ast::var_marks::TermMark;
use crate::ast::{AssignOp, Expr, ParamDef, ReadonlyKind, Stmt};
use crate::symbol::Symbol;

/// Build the head statement that binds a pointy-block parameter to the topic.
///
/// A plain (aliasing) parameter uses `:=` so the compiler/VM writes the
/// parameter's final value back to the topic source — `given @a -> @p { @p.push }`
/// aliases `@a`, matching Raku. `is copy` uses `=` (a fresh copy) so nothing is
/// written back. The declaration reads its value through `Expr::GivenPointyTopic`,
/// independently of dynamic `$_`.
pub(crate) fn pointy_topic_bind(pd: &ParamDef) -> Stmt {
    let mut binding = pointy_topic_bind_inner(pd);
    // A pointy `given`/`with` parameter reads the new topic exactly once, when
    // its synthetic declaration initializes. After that binding, the block's
    // `$_` continues to refer to the enclosing topic. Mark the declaration so
    // the compiler emits the matching `ExitPointyTopic` immediately after it, inside
    // any `BlockLocalScope` that wraps the body.
    if pd.name != "_" {
        if !matches!(binding, Stmt::SyntheticBlock(_)) {
            binding = Stmt::SyntheticBlock(vec![binding]);
        }
        let Stmt::SyntheticBlock(stmts) = &mut binding else {
            unreachable!()
        };
        if let Some(Stmt::VarDecl { marks, .. }) = stmts
            .iter_mut()
            .rev()
            .find(|stmt| matches!(stmt, Stmt::VarDecl { .. }))
        {
            marks.restore_pointy_given_topic = true;
        }
    }
    binding
}

fn pointy_topic_bind_inner(pd: &ParamDef) -> Stmt {
    if let Some(sub_params) = &pd.sub_signature {
        let topic = Expr::Var("_".to_string());
        // A destructuring parameter (`given $obj -> (:@list, |) { ... }`,
        // ASTQuery::Match) binds nothing under its own name: it unpacks the
        // topic into the sub-signature's lexicals. That unpack is the same
        // operation `for` performs, so it is the shared lowering that does it
        // here — the topic is declared under the parameter's synthetic name
        // first, exactly as `for` declares `__for_unpack`, and the binds read
        // from that name. Dropping the sub-signature (which is what this
        // function did before) left `@list` unbound and silently empty.
        let target = if pd.name.is_empty() {
            "__subsig__"
        } else {
            pd.name.as_str()
        };
        let mut stmts = vec![Stmt::VarDecl {
            marks: Default::default(),
            anonymous: false,
            name: target.to_string(),
            expr: topic,
            type_constraint: None,
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: Vec::new(),
            has_initializer: true,
            constant: None,
            bind: None,
            where_constraint: None,
        }];
        // An aggregate-sigil pattern (`-> % (:$k is rw)`) BINDS the topic, so its
        // elements alias the source rather than a copy.
        if target.starts_with(['@', '%']) {
            stmts = vec![Stmt::SyntheticBlock(vec![
                crate::ast::var_marks::with_bound_mark(stmts.remove(0)),
            ])];
        }
        crate::param_destructure::destructure_declaring(target, sub_params, &mut stmts);
        return Stmt::SyntheticBlock(stmts);
    }
    let topic = Expr::GivenPointyTopic;
    if pd.traits.iter().any(|t| t == "copy") {
        // `is copy` is a fresh, writable copy with no link to the source. Use a
        // plain assignment (not `:=`, so the compiler does not treat it as a
        // writeback alias). For `@`/`%` params flatten the topic first
        // (`@p = $_.list` / `%p = $_.hash`) so a copy of an array/hash topic
        // does not nest inside a single element (`$_` is an itemized container).
        let expr = if pd.name.starts_with('@') {
            method_call(topic, "list")
        } else if pd.name.starts_with('%') {
            method_call(topic, "hash")
        } else {
            topic
        };
        Stmt::VarDecl {
            marks: crate::ast::var_marks::VarMarks {
                pointy_copy: true,
                ..Default::default()
            },
            anonymous: false,
            name: pd.name.clone(),
            expr,
            type_constraint: pd.type_constraint.clone(),
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: Vec::new(),
            has_initializer: true,
            constant: None,
            bind: None,
            where_constraint: None,
        }
    } else if pd
        .type_constraint
        .as_deref()
        .is_some_and(|ty| ty.starts_with(|c: char| c.is_ascii_lowercase()))
    {
        // Native-typed lexicals cannot participate in `:=` binding. A native
        // pointy parameter receives a checked/unboxed value in its own lexical
        // container, matching ordinary native routine parameters. It still
        // needs `VarMarks::pointy_native_param` so the compiler recognizes it as the
        // given/with pointy parameter (see the matching detection in
        // `compiler/stmt.rs`'s `pointy_param_name`) — without it, `is rw`
        // writeback never fires because `Given`'s `pointy_param_idx` stays
        // `None`, the same way the general branch below needs a bound declaration.
        let decl = Stmt::VarDecl {
            marks: crate::ast::var_marks::VarMarks {
                pointy_native_param: true,
                ..Default::default()
            },
            anonymous: false,
            name: pd.name.clone(),
            expr: topic,
            type_constraint: pd.type_constraint.clone(),
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: Vec::new(),
            has_initializer: true,
            constant: None,
            bind: None,
            where_constraint: None,
        };
        if pd.traits.iter().any(|t| t == "rw") {
            decl
        } else {
            Stmt::SyntheticBlock(vec![
                decl,
                Stmt::MarkReadonly(pd.name.clone(), ReadonlyKind::Alias),
            ])
        }
    } else if pd.name.starts_with('&') {
        // A `&`-sigil parameter (`given $code -> &to-run { … }`) declares a
        // fresh lexical code alias. It must NOT go out as a bare `&f := $_`:
        // that is `Code items cannot be rebound` in Raku, and mutsu's compiler
        // rejects it as such (`OpCode::AssignReadOnly`). A code alias has no
        // writeback semantics to lose, so a declaration is the exact form.
        Stmt::VarDecl {
            marks: Default::default(),
            anonymous: false,
            name: pd.name.clone(),
            expr: Expr::Var("_".to_string()),
            type_constraint: None,
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: Vec::new(),
            has_initializer: false,
            constant: None,
            bind: None,
            where_constraint: None,
        }
    } else {
        // A pointy parameter is a declaration, not an assignment to an
        // enclosing same-named lexical. Use the same bound declaration as
        // `my $x := EXPR`; this both preserves aliasing and lets block-scope
        // tracking restore the outer binding when the given body exits.
        let mut stmts = vec![Stmt::VarDecl {
            marks: crate::ast::var_marks::VarMarks {
                bound: true,
                // A sigilless parameter (`given $x -> \ex`) is a term: the body spells
                // it as a bare word, so, as for `with`/`if` (`simple_pointy_bind`), the
                // declaration says the name reads from its slot and is not a
                // `$`-scalar (#11898).
                term: if pd.sigilless {
                    TermMark::Readonly
                } else {
                    TermMark::No
                },
                ..Default::default()
            },
            anonymous: false,
            name: pd.name.clone(),
            expr: topic,
            type_constraint: pd.type_constraint.clone(),
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: Vec::new(),
            has_initializer: true,
            constant: None,
            bind: None,
            where_constraint: None,
        }];
        // A `@`/`%` pointy param is always a fully-writable alias regardless of
        // any trait (verified against raku: `given @a -> @p { @p[0] = 99 }`
        // mutates `@a` with no `is rw` needed). Only a plain scalar (`$`,
        // stored here WITHOUT a sigil prefix — see `parse_pointy_param`'s
        // `param_name` construction) pointy param defaults to readonly,
        // lifted with `is rw`. Sigilless (`\a`) bindings are excluded too.
        if !pd.name.starts_with('@')
            && !pd.name.starts_with('%')
            && !pd.sigilless
            && !pd.traits.iter().any(|t| t == "rw")
        {
            stmts.push(Stmt::MarkReadonly(pd.name.clone(), ReadonlyKind::Alias));
        }
        Stmt::SyntheticBlock(stmts)
    }
}

/// Bind a control statement's pointy parameter to the (once-evaluated)
/// condition value.
///
/// A **sigilless** parameter (`with $x -> \\v { ... }`) is a term binding, not an
/// ordinary lexical: the body spells it as a bare word, so the declaration has
/// to carry [`TermMark::Readonly`], which tells the compiler this name reads
/// from its local slot. Without it `with 1 -> \\v { say v }` declared
/// `v` and then read the bare word as a package lookup, printing `(Any)` where
/// rakudo prints `1` (#7954). The marker also makes the binding read-only,
/// which is what rakudo gives a `\\name` parameter.
pub(crate) fn simple_pointy_bind(name: &str, source: &Expr, sigilless: bool) -> Stmt {
    Stmt::VarDecl {
        marks: crate::ast::var_marks::VarMarks {
            term: sigilless_term(sigilless),
            ..Default::default()
        },
        anonymous: false,
        name: name.to_string(),
        expr: source.clone(),
        type_constraint: None,
        is_state: false,
        is_our: false,
        is_dynamic: false,
        is_export: false,
        export_tags: Vec::new(),
        custom_traits: Vec::new(),
        has_initializer: false,
        constant: None,
        bind: None,
        where_constraint: None,
    }
}

/// The term mark of a pointy parameter: a sigilless one is a readonly term.
fn sigilless_term(sigilless: bool) -> TermMark {
    if sigilless {
        TermMark::Readonly
    } else {
        TermMark::No
    }
}

/// Build the declaration used for a pointy parameter on a non-lvalue topic.
///
/// This is the copy-style counterpart of [`pointy_topic_bind`].  The older
/// helper accepted only the parameter name and sigilless flag, which dropped a
/// coercion/type constraint when `with`/`if` lowered `-> Int() $value` to a
/// synthetic `VarDecl`.  Keep the full parameter metadata here so the normal
/// declaration binder performs the same conversion and checks as an ordinary
/// lexical initialization.
pub(crate) fn pointy_param_bind_decl(pd: &ParamDef, source: &Expr) -> Stmt {
    Stmt::VarDecl {
        marks: crate::ast::var_marks::VarMarks {
            term: sigilless_term(pd.sigilless),
            ..Default::default()
        },
        anonymous: false,
        name: pd.name.clone(),
        expr: source.clone(),
        type_constraint: pd.type_constraint.clone(),
        is_state: false,
        is_our: false,
        is_dynamic: false,
        is_export: false,
        export_tags: Vec::new(),
        custom_traits: Vec::new(),
        has_initializer: true,
        constant: None,
        bind: None,
        where_constraint: pd.where_constraint.clone(),
    }
}

/// Build a zero-argument method call expression (`$expr.NAME`).
fn method_call(target: Expr, name: &str) -> Expr {
    Expr::MethodCall {
        target: Box::new(target),
        name: Symbol::intern(name),
        args: Vec::new(),
        modifier: None,
        quoted: false,
        sugar: false,
    }
}

/// Build a `$_ = <expr>` assignment statement for topicalization.
pub(crate) fn topicalize(expr: &Expr) -> Stmt {
    Stmt::Assign {
        name: "_".to_string(),
        op: AssignOp::Assign,
        expr: expr.clone(),
        target_is_sigilless: false,
    }
}

/// The body of `given EXPR -> PARAM { BODY }` after the parser's expansion: the
/// parameter bind (or the autothreading call) around the written body.
/// `rakuast::lower` calls it too, so a hand-built `Statement::Given`
/// over a `PointyBlock` expands the same way.
// Cost: O(n), n = size of the body.
pub(crate) fn given_pointy_body(pd: crate::ast::ParamDef, body: Vec<Stmt>) -> Vec<Stmt> {
    let mut body = body;
    if pointy_param_autothreads(&pd) {
        // `given %r.all.value -> Any $_ { ... }`: the block is CALLED with
        // the topic, so a Junction autothreads over a parameter whose type
        // rejects it (Benchmark's statistics test) instead of failing the
        // bind. Same lowering as `if COND -> sig { ... }`.
        body = vec![Stmt::Expr(Expr::CallOn {
            target: Box::new(Expr::AnonSubParams {
                params: vec![pd.name.clone()],
                param_defs: vec![pd],
                return_type: None,
                body,
                is_rw: false,
                is_raw: false,
                custom_traits: Default::default(),
                is_whatever_code: false,
                declarator: crate::ast::RoutineDeclarator::PointyBlock,
            }),
            args: vec![Expr::Var("_".to_string())],
        })];
    } else {
        body.insert(0, pointy_topic_bind(&pd));
    }
    body
}

/// Whether a `given ... -> PARAM` binds by calling the block: a plain scalar
/// parameter whose declared type does not itself accept a Junction, so the call
/// autothreads. Every other shape (untyped, `Mu`/`Junction`, sub-signatures,
/// `is rw`/`is copy`/`is raw`, `@`/`%`/`&` sigils) keeps the inline bind.
fn pointy_param_autothreads(pd: &crate::ast::ParamDef) -> bool {
    let Some(ty) = pd.type_constraint.as_deref() else {
        return false;
    };
    let base = ty
        .trim_end_matches(":D")
        .trim_end_matches(":U")
        .trim_end_matches(":_");
    !matches!(base, "Mu" | "Junction")
        && pd.sub_signature.is_none()
        && pd.traits.is_empty()
        && !pd.name.starts_with(['@', '%', '&'])
        && !pd.sigilless
        && !pd.named
        && !pd.is_variadic()
}
