use super::conditionals::{ElseClause, IfChainClause};
use super::*;

static IF_BIND_TMP_COUNTER: AtomicUsize = AtomicUsize::new(0);

pub(super) fn parse_if_binding_params(input: &str) -> PResult<'_, Option<Vec<ParamDef>>> {
    let Some(rest) = input.strip_prefix("->") else {
        return Ok((input, None));
    };
    let (rest, _) = ws(rest)?;
    // Zero-parameter pointy block: `if EXPR -> { ... }`
    if rest.starts_with('{') {
        return Ok((rest, Some(Vec::new())));
    }

    let (rest, params) = if let Some(rest) = rest.strip_prefix('(') {
        let (rest, _) = ws(rest)?;
        let (rest, params) = super::super::parse_param_list_pub(rest)?;
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        // A pointy block has no parenthesised parameter list: `-> (...)` is one
        // parameter with a destructuring sub-signature, the same shape `for` and
        // a bare `-> (...)` lambda record. Reading the parens away turned
        // `-> (:key($k))` into a top-level NAMED parameter and `-> ($a, $b)`
        // into two positionals.
        (
            rest,
            crate::parser::stmt::sub_param::fold_parenthesised_pointy_params(params),
        )
    } else {
        super::super::parse_param_list_pub(rest)?
    };
    let (rest, _) = ws(rest)?;
    let (rest, _) = if let Some(after_arrow) = rest.strip_prefix("-->") {
        let (rest, _) = super::super::parse_return_type_annotation_pub(after_arrow)?;
        let (rest, _) = ws(rest)?;
        (rest, ())
    } else {
        (rest, ())
    };
    Ok((rest, Some(params)))
}

fn is_simple_if_binding(param: &ParamDef) -> bool {
    param.traits.is_empty()
        && param.shape_constraints.is_none()
        && !param.named
        && !param.slurpy
        && !param.double_slurpy
        // `+@a` (the single-argument rule) is a slurpy too: it must reach the
        // real signature binder rather than the simple `my @a := COND`
        // desugar, which cannot apply the one-arg rule.
        && !param.onearg
        && param.default.is_none()
        && !param.optional_marker
        && param.type_constraint.is_none()
        && param.sub_signature.is_none()
        && param.outer_sub_signature.is_none()
        && param.code_signature.is_none()
}

fn next_if_bind_tmp_name() -> String {
    let tmp_idx = IF_BIND_TMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("$__mutsu_if_bind_{tmp_idx}")
}

pub(super) fn lower_if_clause_binding(
    binding_params: Option<Vec<ParamDef>>,
    then_branch: Vec<Stmt>,
) -> (Option<String>, Vec<Stmt>) {
    let Some(param_defs) = binding_params else {
        return (None, then_branch);
    };
    if_pointy_clause(param_defs, then_branch)
}

/// The expansion of `if COND -> PARAMS { BODY }`: the `binding_var` the
/// compiler binds the condition to, and the then-branch, which opens with a
/// [`crate::ast::SourceForm::IfPointy`] record of the parameters and body as
/// written. `rakuast::lower` calls it too, so a hand-built `Statement::If`
/// over a `PointyBlock` expands the same way.
// Cost: O(n), n = size of the body.
pub(crate) fn if_pointy_clause(
    param_defs: Vec<ParamDef>,
    then_branch: Vec<Stmt>,
) -> (Option<String>, Vec<Stmt>) {
    if param_defs.is_empty() {
        return (None, then_branch);
    }
    let record = Stmt::SourceForm(Box::new(crate::ast::SourceForm::IfPointy {
        param_defs: param_defs.clone(),
        body: then_branch.clone(),
    }));
    let (binding, mut stmts) = expand_if_pointy(param_defs, then_branch);
    stmts.insert(0, record);
    (binding, stmts)
}

fn expand_if_pointy(
    param_defs: Vec<ParamDef>,
    then_branch: Vec<Stmt>,
) -> (Option<String>, Vec<Stmt>) {
    if param_defs.len() == 1 && is_simple_if_binding(&param_defs[0]) {
        // A sigilless pointy (`if EXPR -> \r { }`) is marked with a leading
        // `\` so the compiler binds the value itself (no scalar itemization)
        // and resolves the name as a bare word — see
        // `Compiler::compile_if_binding_decl`.
        let p = &param_defs[0];
        if !p.sigilless && p.name.trim_start_matches('$') == "_" {
            // `if COND -> $_ { BODY }` binds a FRESH topic for the block, not an
            // ordinary lexical: declaring it as one (`my $_ = COND`) writes the
            // enclosing scope's topic slot and leaves the bound value behind
            // after the branch. Evaluate the condition once into a hidden temp
            // and topicalize the body through `given`, whose own topic opcodes
            // establish and restore the scope — the same reason the `orwith`
            // arm below lowers through `given`.
            let source_binding = next_if_bind_tmp_name();
            let source_expr = Expr::Var(source_binding.trim_start_matches('$').to_string());
            return (
                Some(source_binding),
                vec![Stmt::Given {
                    topic: source_expr,
                    body: then_branch,
                    is_statement_modifier: false,
                    with_kind: None,
                }],
            );
        }
        let name = if p.sigilless {
            format!("\\{}", p.name)
        } else {
            p.name.clone()
        };
        return (Some(name), then_branch);
    }

    let source_binding = next_if_bind_tmp_name();
    let source_expr = Expr::Var(source_binding.trim_start_matches('$').to_string());
    // The condition is ONE argument. `if (1, 2) -> $a, $b` is "expected 2
    // arguments but got 1" in rakudo, not a two-way bind -- the clause receives
    // the condition value itself, and it is the *signature* that decides what
    // to do with it. Slipping it (`|$tmp`) made a list condition bind several
    // parameters, which no source ever asked for.
    //
    // The slurpy spellings in `roast/S04-statements/if.t` all follow from this
    // one rule rather than needing their own: `*@a` flattens the single list
    // argument, `**@a` keeps it whole, `+@a` applies the one-argument rule to
    // it. `**@a` used to be special-cased here for exactly that reason.
    let args = vec![source_expr];
    let call_expr = Expr::CallOn {
        target: Box::new(Expr::AnonSubParams {
            params: param_defs.iter().map(|p| p.name.clone()).collect(),
            param_defs,
            return_type: None,
            body: then_branch,
            is_rw: false,
            is_raw: false,
            custom_traits: Default::default(),
            is_whatever_code: false,
            declarator: crate::ast::RoutineDeclarator::Block,
        }),
        args,
    };
    (Some(source_binding), vec![Stmt::Expr(call_expr)])
}

pub(super) fn ensure_last_clause_binding_var(clauses: &mut [IfChainClause]) -> Option<String> {
    let last_clause = clauses.last_mut()?;
    Some(if let Some(existing) = &last_clause.binding_var {
        existing.clone()
    } else {
        let generated = next_if_bind_tmp_name();
        last_clause.binding_var = Some(generated.clone());
        generated
    })
}

/// Read the value the last clause's binding variable holds.
///
/// `binding_var` keeps the declaration's own spelling, and a sigilless
/// `if COND -> \\a { }` is recorded as `\\a` — so the `\\` has to come off before
/// the name is read, and the read itself is the bare word a sigilless binding
/// is spelled as. Stripping only `$` left `Expr::Var("\\a")`, a name nothing
/// declares, so `if 0 -> \\a { } else -> $x { }` handed the else clause Nil
/// instead of the condition value.
fn binding_var_read(source_binding: &str) -> Expr {
    match source_binding.strip_prefix('\\') {
        Some(bare) => Expr::BareWord(bare.to_string()),
        None => Expr::Var(source_binding.trim_start_matches('$').to_string()),
    }
}

pub(super) fn lower_else_binding(source_binding: &str, else_clause: ElseClause) -> Vec<Stmt> {
    let record = else_clause.binding_params.as_ref().map(|params| {
        Stmt::SourceForm(Box::new(crate::ast::SourceForm::IfPointy {
            param_defs: params.clone(),
            body: else_clause.body.clone(),
        }))
    });
    let mut body = expand_else_binding(source_binding, else_clause);
    if let Some(record) = record {
        body.insert(0, record);
    }
    body
}

/// Bind an else signature to the last condition's saved value.
// Cost: O(n), n = size of the signature and body.
pub(crate) fn else_pointy_clause(
    binding: &mut Option<String>,
    params: Vec<ParamDef>,
    body: Vec<Stmt>,
) -> Vec<Stmt> {
    let source = binding.get_or_insert_with(next_if_bind_tmp_name);
    lower_else_binding(
        source,
        ElseClause {
            binding_params: Some(params),
            body,
        },
    )
}

fn expand_else_binding(source_binding: &str, else_clause: ElseClause) -> Vec<Stmt> {
    let Some(param_defs) = else_clause.binding_params else {
        return else_clause.body;
    };
    if param_defs.is_empty() {
        return else_clause.body;
    }
    if param_defs.len() == 1 && is_simple_if_binding(&param_defs[0]) {
        if param_defs[0].name.trim_start_matches('$') == "_" && !param_defs[0].sigilless {
            // `else -> $_ { }` topicalizes through `given` for the same reason
            // as the then-branch form — see `lower_if_clause_binding`.
            return vec![Stmt::Given {
                topic: binding_var_read(source_binding),
                body: else_clause.body,
                is_statement_modifier: false,
                with_kind: None,
            }];
        }
        let mut body = Vec::with_capacity(else_clause.body.len() + 1);
        body.push(simple_pointy_bind(
            &param_defs[0].name,
            &binding_var_read(source_binding),
            param_defs[0].sigilless,
        ));
        body.extend(else_clause.body);
        return body;
    }

    let call_expr = Expr::CallOn {
        target: Box::new(Expr::AnonSubParams {
            params: param_defs.iter().map(|p| p.name.clone()).collect(),
            param_defs,
            return_type: None,
            body: else_clause.body,
            is_rw: false,
            is_raw: false,
            custom_traits: Default::default(),
            is_whatever_code: false,
            declarator: crate::ast::RoutineDeclarator::Block,
        }),
        // One argument, as in `lower_if_clause_binding` -- `else -> ...` shares
        // the rule.
        args: vec![binding_var_read(source_binding)],
    };
    vec![Stmt::Expr(call_expr)]
}
