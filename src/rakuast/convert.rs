//! Internal AST (`Stmt`/`Expr`) → RakuAST node tree (read direction, ADR-0011).
//!
//! Covered so far: the literal + say-call cluster (Phase 1); and variables,
//! plain `my` declarations, infix/prefix/postfix operators, `=` assignment, and
//! method calls (Phase 2). Constructs outside that set produce an explicit
//! `RuntimeError` (the documented coverage boundary) rather than a
//! silently-wrong node.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{AssignOp, Expr, ForMode, ParamDef, Stmt};
use crate::compiler::helpers_ops::token_kind_to_op_name;
use crate::value::{RuntimeError, Value, ValueView};

fn unsupported(what: &str) -> RuntimeError {
    RuntimeError::new(format!(
        "RakuAST: `.AST` does not yet support this construct: {what}"
    ))
}

fn node_field(name: Option<&'static str>, node: RakuAstNode) -> RakuAstField {
    RakuAstField {
        name,
        value: RakuAstFieldValue::Node(Value::rakuast(Box::new(node))),
    }
}

fn leaf_field(name: Option<&'static str>, value: Value) -> RakuAstField {
    RakuAstField {
        name,
        value: RakuAstFieldValue::Node(value),
    }
}

/// Top-level: a parsed program becomes a `RakuAST::StatementList`.
pub(super) fn statement_list(stmts: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = Vec::new();
    for stmt in stmts {
        if let Some(node) = convert_stmt(stmt)? {
            fields.push(node_field(None, node));
        }
    }
    Ok(RakuAstNode {
        class: RakuAstClass::StatementList,
        fields,
    })
}

/// Convert one statement. Returns `Ok(None)` for non-semantic bookkeeping
/// statements (e.g. `SetLine`) that carry no RakuAST representation.
fn convert_stmt(stmt: &Stmt) -> Result<Option<RakuAstNode>, RuntimeError> {
    match stmt {
        Stmt::SetLine(_) => Ok(None),
        Stmt::Expr(e) => Ok(Some(statement_expression(convert_expr(e)?))),
        // `say 42` / `put`/`print`/`note` as listops (no parens) parse to a
        // dedicated statement; raku models them as a call in WithoutParentheses
        // form.
        Stmt::Say(args) => Ok(Some(statement_expression(listop_call("say", args)?))),
        Stmt::Put(args) => Ok(Some(statement_expression(listop_call("put", args)?))),
        Stmt::Print(args) => Ok(Some(statement_expression(listop_call("print", args)?))),
        Stmt::Note(args) => Ok(Some(statement_expression(listop_call("note", args)?))),
        Stmt::VarDecl {
            name,
            expr,
            type_constraint,
            is_state,
            is_our,
            is_dynamic,
            custom_traits,
            where_constraint,
            ..
        } => {
            // `my`/`our`/`state` with an optional simple type. Dynamic (`$*x`),
            // `where` constraints, parameterised/definite/coercion types, and
            // real `is`/`does` traits carry richer shape, deferred.
            if *is_dynamic || where_constraint.is_some() {
                return Err(unsupported("dynamic / where-constrained declaration"));
            }
            if custom_traits.iter().any(|(n, _)| n != "__has_initializer") {
                return Err(unsupported("declaration with traits"));
            }
            // build_type_node validates simple/definite and defers the rest.
            let type_name = type_constraint.as_deref();
            let scope = if *is_our {
                Some("our")
            } else if *is_state {
                Some("state")
            } else {
                None
            };
            let has_initializer = custom_traits
                .iter()
                .any(|(name, _)| name == "__has_initializer");
            let init = has_initializer.then_some(expr);
            Ok(Some(statement_expression(var_declaration(
                name, init, scope, type_name, None, None,
            )?)))
        }
        // A bare `{ ... }` block at statement level -> Statement::Expression(Block).
        Stmt::Block(body) => Ok(Some(statement_expression(block_node(body)?))),
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            binding_var,
        } => {
            // mutsu desugars `unless X` to `if !X`, so an `unless` renders as a
            // `Statement::If` whose condition is `ApplyPrefix(!)` — a documented
            // divergence (raku keeps `Statement::Unless` with the bare condition).
            if binding_var.is_some() {
                return Err(unsupported("`if EXPR -> $var` topic binding"));
            }
            let mut fields = vec![
                node_field(Some("condition"), convert_expr(cond)?),
                node_field(Some("then"), block_node(then_branch)?),
            ];
            // mutsu nests each `elsif` as a single `if` inside the else-branch.
            // Flatten that chain into raku's `elsifs` list; whatever remains
            // after the last `elsif` is the final `else` block.
            let mut elsifs: Vec<Value> = Vec::new();
            let mut tail: &[Stmt] = else_branch;
            while let Some(Stmt::If {
                cond,
                then_branch,
                else_branch,
                binding_var,
            }) = single_if_stmt(tail)
            {
                if binding_var.is_some() {
                    return Err(unsupported("`elsif EXPR -> $var` topic binding"));
                }
                elsifs.push(Value::rakuast(Box::new(elsif_node(cond, then_branch)?)));
                tail = else_branch;
            }
            if !elsifs.is_empty() {
                fields.push(RakuAstField {
                    name: Some("elsifs"),
                    value: RakuAstFieldValue::List(elsifs),
                });
            }
            if tail.iter().any(|s| !matches!(s, Stmt::SetLine(_))) {
                fields.push(node_field(Some("else"), block_node(tail)?));
            }
            Ok(Some(RakuAstNode {
                class: RakuAstClass::StatementIf,
                fields,
            }))
        }
        Stmt::While { cond, body, label } => {
            // mutsu desugars `until X` to `while !X` (same as unless/if above).
            let mut fields = label_fields(label);
            fields.push(node_field(Some("condition"), convert_expr(cond)?));
            fields.push(node_field(Some("body"), block_node(body)?));
            Ok(Some(RakuAstNode {
                class: RakuAstClass::StatementLoopWhile,
                fields,
            }))
        }
        Stmt::Loop {
            init,
            cond,
            step,
            body,
            repeat,
            label,
        } => {
            if *repeat {
                // `repeat { } while X`. mutsu desugars `repeat { } until X` to a
                // negated `while` condition (same collapse as while/until), so
                // this always renders as `Loop::RepeatWhile`.
                let cond = cond
                    .as_ref()
                    .ok_or_else(|| unsupported("repeat loop without condition"))?;
                let mut fields = label_fields(label);
                fields.push(node_field(Some("body"), block_node(body)?));
                fields.push(node_field(Some("condition"), convert_expr(cond)?));
                return Ok(Some(RakuAstNode {
                    class: RakuAstClass::StatementLoopRepeatWhile,
                    fields,
                }));
            }
            if init.is_none() && cond.is_none() && step.is_none() {
                // Bare `loop { }`.
                let mut fields = label_fields(label);
                fields.push(node_field(Some("body"), block_node(body)?));
                return Ok(Some(RakuAstNode {
                    class: RakuAstClass::StatementLoop,
                    fields,
                }));
            }
            // C-style `loop (init; cond; step) { }`. Each clause is optional and
            // present only when written.
            let mut fields = label_fields(label);
            if let Some(init) = init.as_deref() {
                fields.push(node_field(Some("setup"), loop_setup_node(init)?));
            }
            if let Some(cond) = cond {
                fields.push(node_field(Some("condition"), convert_expr(cond)?));
            }
            if let Some(step) = step {
                fields.push(node_field(Some("increment"), convert_expr(step)?));
            }
            fields.push(node_field(Some("body"), block_node(body)?));
            Ok(Some(RakuAstNode {
                class: RakuAstClass::StatementLoop,
                fields,
            }))
        }
        Stmt::For {
            iterable,
            param,
            param_def,
            params,
            params_def,
            body,
            label,
            mode,
            rw_block,
            explicit_zero_params,
        } => {
            // Implicit-topic (`for SRC { ... $_ }`, slice 6) and explicit-signature
            // (`for @a -> $x`, slice 12) forms. Hyper/race/lazy modes, `<->` rw
            // blocks, and labels carry extra RakuAST shape, deferred.
            // The explicit param names live in `param_def` / `params_def`; the
            // sigil-stripped `param` / `params` string lists are unused here.
            let _ = (param, params);
            if *rw_block || *explicit_zero_params || !matches!(mode, ForMode::Normal) {
                return Err(unsupported("for loop with mode / rw"));
            }
            // A single explicit param lives in `param_def`, multiple in
            // `params_def`. With none, the body is an implicit-topic Block; with
            // an explicit signature, it is a PointyBlock (matching raku).
            let single = (**param_def).as_ref();
            let explicit_defs: &[ParamDef] = match single {
                Some(pd) => std::slice::from_ref(pd),
                None => params_def,
            };
            let body_node = if explicit_defs.is_empty() {
                topic_block_node(body)?
            } else {
                pointy_block(explicit_defs, body)?
            };
            // Field order matches raku: labels, mode, source, body.
            let mut fields = label_fields(label);
            fields.push(leaf_field(Some("mode"), Value::str("serial".to_string())));
            fields.push(node_field(Some("source"), convert_expr(iterable)?));
            fields.push(node_field(Some("body"), body_node));
            Ok(Some(RakuAstNode {
                class: RakuAstClass::StatementFor,
                fields,
            }))
        }
        // `given X { ... }` -> Statement::Given(source, body => topic Block).
        Stmt::Given { topic, body } => Ok(Some(RakuAstNode {
            class: RakuAstClass::StatementGiven,
            fields: vec![
                node_field(Some("source"), convert_expr(topic)?),
                node_field(Some("body"), topic_block_node(body)?),
            ],
        })),
        // `when Y { ... }` -> Statement::When(condition, body => plain Block).
        Stmt::When { cond, body } => Ok(Some(RakuAstNode {
            class: RakuAstClass::StatementWhen,
            fields: vec![
                node_field(Some("condition"), convert_expr(cond)?),
                node_field(Some("body"), block_node(body)?),
            ],
        })),
        // `default { ... }` -> Statement::Default(body => plain Block).
        Stmt::Default(body) => Ok(Some(RakuAstNode {
            class: RakuAstClass::StatementDefault,
            fields: vec![node_field(Some("body"), block_node(body)?)],
        })),
        Stmt::SubDecl {
            name,
            name_expr,
            param_defs,
            return_type,
            associativity,
            precedence_trait,
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
        } => {
            // Phase 2 slice 7 covers the plain named `sub NAME (params) { body }`
            // form. Traits, multi, export, return type, operator subs, and
            // alternate signatures carry extra RakuAST shape, deferred.
            if name_expr.is_some()
                || return_type.is_some()
                || associativity.is_some()
                || precedence_trait.is_some()
                || !signature_alternates.is_empty()
                || *multi
                || *is_rw
                || *is_raw
                || *is_export
                || !export_tags.is_empty()
                || *is_test_assertion
                || *supersede
                || !custom_traits.is_empty()
            {
                return Err(unsupported(
                    "sub with traits / multi / export / return type",
                ));
            }
            Ok(Some(statement_expression(routine_node(
                RakuAstClass::Sub,
                &name.resolve(),
                param_defs,
                body,
            )?)))
        }
        Stmt::MethodDecl {
            name,
            name_expr,
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
            ..
        } => {
            // Plain `method NAME (params) { body }`. Private/submethod/multi/our/
            // my forms, traits, delegation, and return types carry extra shape.
            if name_expr.is_some()
                || *multi
                || *is_rw
                || *is_private
                || *is_our
                || *is_my
                || *is_submethod
                || *our_variable_form
                || return_type.is_some()
                || *is_default_candidate
                || deprecated_message.is_some()
                || !handles.is_empty()
                || !custom_traits.is_empty()
            {
                return Err(unsupported(
                    "method with traits / private / multi / submethod",
                ));
            }
            Ok(Some(statement_expression(routine_node(
                RakuAstClass::Method,
                &name.resolve(),
                param_defs,
                body,
            )?)))
        }
        Stmt::ClassDecl {
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
            custom_traits,
            is_unit,
            ..
        } => {
            // Plain `class NAME { body }`. Inheritance (`is`/`does`), `my`/unit
            // scope, reprs, `rw`, and traits carry extra RakuAST shape, deferred.
            if name_expr.is_some()
                || !parents.is_empty()
                || *class_is_rw
                || *is_hidden
                || *is_lexical
                || !hidden_parents.is_empty()
                || !does_parents.is_empty()
                || repr.is_some()
                || !custom_traits.is_empty()
                || *is_unit
            {
                return Err(unsupported(
                    "class with inheritance / scope / repr / traits",
                ));
            }
            Ok(Some(statement_expression(RakuAstNode {
                class: RakuAstClass::Class,
                fields: vec![
                    node_field(Some("name"), name_from_identifier(&name.resolve())),
                    node_field(Some("body"), block_node(body)?),
                ],
            })))
        }
        Stmt::RoleDecl {
            name,
            type_params,
            is_export,
            export_tags,
            body,
            is_rw,
            custom_traits,
            ..
        } => {
            // Plain `role NAME { body }`. Parameterised roles (`role R[::T]`),
            // export, `rw`, and traits carry extra RakuAST shape, deferred. The
            // role body is a `RoleBody` (not a plain `Block`).
            if !type_params.is_empty()
                || *is_export
                || !export_tags.is_empty()
                || *is_rw
                || !custom_traits.is_empty()
            {
                return Err(unsupported("role with type params / export / traits"));
            }
            let role_body = RakuAstNode {
                class: RakuAstClass::RoleBody,
                fields: vec![node_field(Some("body"), blockoid(body)?)],
            };
            Ok(Some(statement_expression(RakuAstNode {
                class: RakuAstClass::Role,
                fields: vec![
                    node_field(Some("name"), name_from_identifier(&name.resolve())),
                    node_field(Some("body"), role_body),
                ],
            })))
        }
        Stmt::HasDecl {
            name,
            is_public,
            default,
            handles,
            is_rw,
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
            unknown_traits,
            ..
        } => {
            // A plain `has [Type] $.x` attribute -> a `VarDeclaration::Simple`
            // with `scope => "has"` and a `twigil` (`.` public accessor / `!`
            // private). An *explicit* attribute default (`has $.x = 5`) becomes a
            // `Trait::WillBuild` in raku (not an `initializer`), so it is
            // deferred; but a typed attribute (`has Int $.z`) carries an
            // *implicit* `BareWord(<TypeName>)` default that is NOT a will-build
            // and must be ignored. Traits, type smileys, `required`, `where`,
            // aliases, and `my`/`our` attributes are also deferred.
            // A typed attribute (`has Int $.z`) carries an *implicit*
            // `BareWord(<TypeName>)` default that is NOT a real default; an
            // *explicit* `= EXPR` default (slice 27) is a `Trait::WillBuild` and
            // an `initializer`.
            let explicit_default = match default {
                None => None,
                Some(Expr::BareWord(w)) if type_constraint.as_deref() == Some(w.as_str()) => None,
                Some(e) => Some(e),
            };
            if !handles.is_empty()
                || *is_rw
                || type_smiley.is_some()
                || is_required.is_some()
                || where_constraint.is_some()
                || *is_alias
                || *is_our
                || *is_my
                || is_default.is_some()
                || is_type.is_some()
                || deprecated_message.is_some()
                || !unknown_traits.is_empty()
            {
                return Err(unsupported("attribute with traits / smiley / scope"));
            }
            // Definite attributes carry `type_smiley` (guarded above), so the
            // type_constraint here is a bare type; build_type_node handles it.
            let type_name = type_constraint.as_deref();
            let twigil = if *is_public { "." } else { "!" };
            let full_name = if *sigil == '$' {
                name.resolve()
            } else {
                format!("{}{}", sigil, name.resolve())
            };
            Ok(Some(statement_expression(var_declaration(
                &full_name,
                explicit_default,
                Some("has"),
                type_name,
                Some(twigil),
                explicit_default,
            )?)))
        }
        Stmt::Assign { name, expr, op } => match op {
            // `$x = EXPR` — the special `Assignment` infix (slice 2). Note mutsu
            // desugars `$x += 3` to `$x = $x + 3` (op stays `Assign`), so a
            // compound assignment renders as a plain `=` over a binop rather than
            // raku's `MetaInfix::Assign` — a documented divergence.
            AssignOp::Assign => Ok(Some(statement_expression(assignment_infix(name, expr)?))),
            // `$x := EXPR` — a plain `:=` infix (slice 9).
            AssignOp::Bind => Ok(Some(statement_expression(bind_infix(name, expr)?))),
            AssignOp::MatchAssign => Err(unsupported("`~~` match-assignment")),
        },
        // A `LABEL: STMT` wrapper (mutsu uses this for labelled `repeat`/C-style
        // loops, where the inner loop's own `label` field is None). Convert the
        // inner statement and prepend its `labels` field — raku renders labels
        // first, matching the inline-label loops of slice 17.
        Stmt::Label { name, stmt } => {
            let mut node =
                convert_stmt(stmt)?.ok_or_else(|| unsupported("labelled empty statement"))?;
            let mut fields = label_fields(&Some(name.clone()));
            fields.append(&mut node.fields);
            node.fields = fields;
            Ok(Some(node))
        }
        other => Err(unsupported(&format!("{other:?}"))),
    }
}

/// `$x = EXPR` -> `ApplyInfix(left => Var::Lexical, infix => Assignment, right)`.
/// The `Assignment` node carries `:item` for scalar (`$`) targets; the list form
/// (`@`/`%`) has no adverb.
fn assignment_infix(name: &str, rhs: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let (sigil, desigil) = split_sigil(name);
    let assignment = RakuAstNode {
        class: RakuAstClass::Assignment,
        fields: if sigil == "$" {
            vec![RakuAstField {
                name: None,
                value: RakuAstFieldValue::Adverb("item"),
            }]
        } else {
            vec![]
        },
    };
    Ok(RakuAstNode {
        class: RakuAstClass::ApplyInfix,
        fields: vec![
            node_field(Some("left"), var_lexical(sigil, desigil)),
            node_field(Some("infix"), assignment),
            node_field(Some("right"), convert_expr(rhs)?),
        ],
    })
}

/// `$x := EXPR` -> `ApplyInfix(left => Var::Lexical, infix => Infix(":="), right)`.
/// Unlike `=`, binding uses a plain `Infix`, not the special `Assignment` node.
fn bind_infix(name: &str, rhs: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let (sigil, desigil) = split_sigil(name);
    Ok(RakuAstNode {
        class: RakuAstClass::ApplyInfix,
        fields: vec![
            node_field(Some("left"), var_lexical(sigil, desigil)),
            node_field(Some("infix"), plain_infix(":=")),
            node_field(Some("right"), convert_expr(rhs)?),
        ],
    })
}

/// A plain `Infix.new("<op>")` node from a literal operator string.
fn plain_infix(op: &str) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::Infix,
        fields: vec![leaf_field(None, Value::str(op.to_string()))],
    }
}

/// `my $x` / `my @a` / `my $x = EXPR` -> `VarDeclaration::Simple`. The sigil is
/// implicit (`$`) when mutsu already stripped it from the name; otherwise the
/// name carries its `@`/`%`/`&` sigil.
fn var_declaration(
    name: &str,
    init: Option<&Expr>,
    scope: Option<&'static str>,
    type_name: Option<&str>,
    twigil: Option<&str>,
    will_build: Option<&Expr>,
) -> Result<RakuAstNode, RuntimeError> {
    let (sigil, desigil) = split_sigil(name);
    // Field order matches raku: scope, type, sigil, twigil, desigilname, traits,
    // initializer — each omitted when absent (scope defaults to `my`; twigil and
    // traits appear only on attributes).
    let mut fields = Vec::new();
    if let Some(s) = scope {
        fields.push(leaf_field(Some("scope"), Value::str(s.to_string())));
    }
    if let Some(t) = type_name {
        fields.push(node_field(Some("type"), build_type_node(t)?));
    }
    fields.push(leaf_field(Some("sigil"), Value::str(sigil.to_string())));
    if let Some(tw) = twigil {
        fields.push(leaf_field(Some("twigil"), Value::str(tw.to_string())));
    }
    fields.push(node_field(
        Some("desigilname"),
        name_from_identifier(desigil),
    ));
    if let Some(wb) = will_build {
        // An attribute default (`has $.x = 5`) is a `Trait::WillBuild` (and also
        // an `initializer`, emitted below).
        let trait_node = RakuAstNode {
            class: RakuAstClass::TraitWillBuild,
            fields: vec![node_field(None, convert_expr(wb)?)],
        };
        fields.push(RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(trait_node))]),
        });
    }
    if let Some(init_expr) = init {
        let assign = RakuAstNode {
            class: RakuAstClass::InitializerAssign,
            fields: vec![node_field(None, convert_expr(init_expr)?)],
        };
        fields.push(node_field(Some("initializer"), assign));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::VarDeclarationSimple,
        fields,
    })
}

/// A `Name.from-identifier("<s>")` node.
fn name_from_identifier(s: &str) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::Name,
        fields: vec![leaf_field(None, Value::str(s.to_string()))],
    }
}

/// True when a type constraint is a plain (possibly `::`-qualified) identifier
/// (`Int`, `My::Type`) that maps to `Type::Simple`. Parameterised (`Array[Int]`)
/// and coercion (`Str()`) types carry richer RakuAST shape, deferred — so each
/// `::`-separated segment must be a bare identifier.
fn is_simple_type(t: &str) -> bool {
    !t.is_empty()
        && t.split("::").all(|seg| {
            !seg.is_empty() && seg.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
        })
}

/// A bare simple type `Int` -> `Type::Simple(Name.from-identifier("Int"))`.
fn simple_type_node(t: &str) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::TypeSimple,
        fields: vec![node_field(None, name_from_identifier(t))],
    }
}

/// Build the `type => ...` RakuAST node for a mutsu type-constraint string.
/// A plain identifier -> `Type::Simple`; a `:D`/`:U` definiteness smiley ->
/// `Type::Definedness`; a `Base[Arg, ...]` -> `Type::Parameterized`. Coercion
/// (`Str()`) and `:_` types defer.
fn build_type_node(t: &str) -> Result<RakuAstNode, RuntimeError> {
    if let Some(base) = t.strip_suffix(":D").or_else(|| t.strip_suffix(":U")) {
        if !is_simple_type(base) {
            return Err(unsupported("definite type over a non-simple base"));
        }
        let definite = t.ends_with(":D");
        return Ok(RakuAstNode {
            class: RakuAstClass::TypeDefinedness,
            fields: vec![
                node_field(Some("base-type"), simple_type_node(base)),
                RakuAstField {
                    name: Some("definite"),
                    value: RakuAstFieldValue::Node(Value::truth(definite)),
                },
            ],
        });
    }
    // `Array[Int]` / `Hash[Str, Int]` -> Type::Parameterized(base-type, args).
    if let Some(open) = t.find('[') {
        let inner = t
            .strip_suffix(']')
            .ok_or_else(|| unsupported("malformed parameterised type"))?;
        let base = &t[..open];
        let args_str = &inner[open + 1..];
        if !is_simple_type(base) {
            return Err(unsupported("parameterised type over a non-simple base"));
        }
        let mut args = Vec::new();
        for a in args_str.split(',') {
            args.push(node_field(None, build_type_node(a.trim())?));
        }
        let arglist = RakuAstNode {
            class: RakuAstClass::ArgList,
            fields: args,
        };
        return Ok(RakuAstNode {
            class: RakuAstClass::TypeParameterized,
            fields: vec![
                node_field(Some("base-type"), simple_type_node(base)),
                node_field(Some("args"), arglist),
            ],
        });
    }
    if is_simple_type(t) {
        return Ok(simple_type_node(t));
    }
    Err(unsupported("coercion type"))
}

/// Split a declaration name into `(sigil, desigilname)`. mutsu keeps the sigil
/// on `@`/`%`/`&` declarations but strips it from `$` ones.
fn split_sigil(name: &str) -> (&str, &str) {
    match name.as_bytes().first() {
        Some(b'@') => ("@", &name[1..]),
        Some(b'%') => ("%", &name[1..]),
        Some(b'&') => ("&", &name[1..]),
        _ => ("$", name),
    }
}

fn statement_expression(expr: RakuAstNode) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::StatementExpression,
        fields: vec![node_field(Some("expression"), expr)],
    }
}

fn convert_expr(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    match expr {
        Expr::Literal(v) | Expr::LiteralSrc(v, _) => convert_literal(v),
        Expr::Call { name, args } => Ok(call_name(name.as_str(), args, false)?),
        Expr::Var(name) => Ok(var_lexical("$", name)),
        Expr::ArrayVar(name) => Ok(var_lexical("@", name)),
        Expr::HashVar(name) => Ok(var_lexical("%", name)),
        Expr::CodeVar(name) => Ok(var_lexical("&", name)),
        // List-associative infixes (`andthen`/`orelse`/`notandthen`) render as a
        // single flat `ApplyListInfix` in raku; mutsu nests them left-associatively,
        // so flatten a same-operator left chain into one operand list.
        Expr::Binary { left, op, right } if is_list_infix(op) => {
            let mut operands = Vec::new();
            flatten_list_infix(op, left, right, &mut operands);
            let mut nodes = Vec::with_capacity(operands.len());
            for e in operands {
                nodes.push(Value::rakuast(Box::new(convert_expr(e)?)));
            }
            Ok(RakuAstNode {
                class: RakuAstClass::ApplyListInfix,
                fields: vec![
                    node_field(Some("infix"), operator_node(RakuAstClass::Infix, op)),
                    RakuAstField {
                        name: Some("operands"),
                        value: RakuAstFieldValue::List(nodes),
                    },
                ],
            })
        }
        Expr::Binary { left, op, right } => Ok(RakuAstNode {
            class: RakuAstClass::ApplyInfix,
            fields: vec![
                node_field(Some("left"), convert_expr(left)?),
                node_field(Some("infix"), operator_node(RakuAstClass::Infix, op)),
                node_field(Some("right"), convert_expr(right)?),
            ],
        }),
        Expr::Unary { op, expr } => Ok(RakuAstNode {
            class: RakuAstClass::ApplyPrefix,
            fields: vec![
                node_field(Some("prefix"), operator_node(RakuAstClass::Prefix, op)),
                node_field(Some("operand"), convert_expr(expr)?),
            ],
        }),
        Expr::PostfixOp { op, expr } => Ok(RakuAstNode {
            class: RakuAstClass::ApplyPostfix,
            fields: vec![
                node_field(Some("operand"), convert_expr(expr)?),
                node_field(Some("postfix"), postfix_node(op)),
            ],
        }),
        // `COND ?? THEN !! ELSE` -> Ternary(condition, then, else). Note raku
        // constant-folds a literal-condition ternary (`1 ?? 2 !! 3` -> IntLiteral(2));
        // mutsu does not, a documented divergence (the const-fold open question).
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => Ok(RakuAstNode {
            class: RakuAstClass::Ternary,
            fields: vec![
                node_field(Some("condition"), convert_expr(cond)?),
                node_field(Some("then"), convert_expr(then_expr)?),
                node_field(Some("else"), convert_expr(else_expr)?),
            ],
        }),
        Expr::MethodCall {
            target,
            name,
            args,
            modifier,
            quoted,
        } => {
            let postfix = method_call_postfix(name.as_str(), args, *modifier, *quoted)?;
            Ok(RakuAstNode {
                class: RakuAstClass::ApplyPostfix,
                fields: vec![
                    node_field(Some("operand"), convert_expr(target)?),
                    node_field(Some("postfix"), postfix),
                ],
            })
        }
        // Hyper method call `@a>>.abs` -> ApplyPostfix(operand,
        // postfix => MetaPostfix::Hyper(Call::Method(...))).
        Expr::HyperMethodCall {
            target,
            name,
            args,
            modifier,
            quoted,
        } => {
            let inner = method_call_postfix(name.as_str(), args, *modifier, *quoted)?;
            let hyper = RakuAstNode {
                class: RakuAstClass::MetaPostfixHyper,
                fields: vec![node_field(None, inner)],
            };
            Ok(RakuAstNode {
                class: RakuAstClass::ApplyPostfix,
                fields: vec![
                    node_field(Some("operand"), convert_expr(target)?),
                    node_field(Some("postfix"), hyper),
                ],
            })
        }
        // A bare comma list `1, 2, 3` -> ApplyListInfix(infix => ",", operands).
        Expr::ArrayLiteral(items) => {
            let mut operands = Vec::with_capacity(items.len());
            for it in items {
                operands.push(Value::rakuast(Box::new(convert_expr(it)?)));
            }
            Ok(RakuAstNode {
                class: RakuAstClass::ApplyListInfix,
                fields: vec![
                    node_field(Some("infix"), plain_infix(",")),
                    RakuAstField {
                        name: Some("operands"),
                        value: RakuAstFieldValue::List(operands),
                    },
                ],
            })
        }
        // Positional subscript `@x[EXPR]` -> ApplyPostfix(operand,
        // postfix => Postcircumfix::ArrayIndex(index => SemiList(...))).
        // Associative subscripts (`%h{...}` / `%h<...>`) are deferred: mutsu
        // cannot distinguish `<k>` (LiteralHashIndex) from `{"k"}` (HashIndex).
        // Reduction metaop `[+] @a` / triangle `[\+] @a` -> Term::Reduce.
        Expr::Reduction { op, expr } => {
            let (triangle, infix_op) = match op.strip_prefix('\\') {
                Some(stripped) => (true, stripped),
                None => (false, op.as_str()),
            };
            let arglist = RakuAstNode {
                class: RakuAstClass::ArgList,
                fields: vec![node_field(None, convert_expr(expr)?)],
            };
            Ok(RakuAstNode {
                class: RakuAstClass::TermReduce,
                fields: vec![
                    RakuAstField {
                        name: Some("triangle"),
                        value: RakuAstFieldValue::Node(Value::truth(triangle)),
                    },
                    node_field(Some("infix"), plain_infix(infix_op)),
                    node_field(Some("args"), arglist),
                ],
            })
        }
        Expr::Index {
            target,
            index,
            is_positional,
        } => {
            if !is_positional {
                return Err(unsupported("associative subscript"));
            }
            let semilist = RakuAstNode {
                class: RakuAstClass::SemiList,
                fields: vec![node_field(None, statement_expression(convert_expr(index)?))],
            };
            let array_index = RakuAstNode {
                class: RakuAstClass::PostcircumfixArrayIndex,
                fields: vec![node_field(Some("index"), semilist)],
            };
            Ok(RakuAstNode {
                class: RakuAstClass::ApplyPostfix,
                fields: vec![
                    node_field(Some("operand"), convert_expr(target)?),
                    node_field(Some("postfix"), array_index),
                ],
            })
        }
        // A bare `{ ... }` block in expression position.
        Expr::Block(body) => block_node(body),
        Expr::AnonSub {
            body,
            is_rw,
            is_block,
        } => {
            if *is_rw {
                return Err(unsupported("`is rw` block"));
            }
            if *is_block {
                // A bare `{ ... }` block.
                block_node(body)
            } else {
                // An anonymous, parameter-less `sub { ... }`. (`sub ($x) { }`
                // parses to `AnonSubParams`, which mutsu cannot distinguish from
                // a multi-param pointy block, so those still render as
                // `PointyBlock` — a documented divergence.)
                Ok(RakuAstNode {
                    class: RakuAstClass::Sub,
                    fields: vec![node_field(Some("body"), blockoid(body)?)],
                })
            }
        }
        Expr::Lambda {
            param,
            body,
            is_whatever_code,
        } => {
            if *is_whatever_code {
                return Err(unsupported("Whatever-code closure"));
            }
            pointy_block_from_lambda(param, body)
        }
        Expr::AnonSubParams {
            param_defs,
            body,
            is_rw,
            is_whatever_code,
            return_type,
            ..
        } => {
            if *is_rw || *is_whatever_code || return_type.is_some() {
                return Err(unsupported("`is rw` / Whatever / typed pointy block"));
            }
            pointy_block(param_defs, body)
        }
        other => Err(unsupported(&format!("{other:?}"))),
    }
}

/// If `stmts` (ignoring `SetLine` bookkeeping) is exactly one `if` statement,
/// return it — this is how mutsu nests an `elsif`. Anything else (a real
/// `else` block, multiple statements, empty) returns `None`.
fn single_if_stmt(stmts: &[Stmt]) -> Option<&Stmt> {
    let mut real = stmts.iter().filter(|s| !matches!(s, Stmt::SetLine(_)));
    let first = real.next()?;
    if real.next().is_some() {
        return None;
    }
    matches!(first, Stmt::If { .. }).then_some(first)
}

/// One `elsif` clause -> `Statement::Elsif(condition, then => Block)`.
fn elsif_node(cond: &Expr, then_branch: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        class: RakuAstClass::StatementElsif,
        fields: vec![
            node_field(Some("condition"), convert_expr(cond)?),
            node_field(Some("then"), block_node(then_branch)?),
        ],
    })
}

/// A `{ ... }` block body wraps its `StatementList` in a `Blockoid`.
fn blockoid(body: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        class: RakuAstClass::Blockoid,
        fields: vec![node_field(None, statement_list(body)?)],
    })
}

/// A bare `{ ... }` block -> `Block(body => Blockoid)`.
fn block_node(body: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        class: RakuAstClass::Block,
        fields: vec![node_field(Some("body"), blockoid(body)?)],
    })
}

/// The leading `labels => (Label(name => "..."),)` field for a labelled loop,
/// or an empty vec when unlabelled. raku always renders labels first.
fn label_fields(label: &Option<String>) -> Vec<RakuAstField> {
    match label {
        None => Vec::new(),
        Some(name) => {
            let label_node = RakuAstNode {
                class: RakuAstClass::Label,
                fields: vec![leaf_field(Some("name"), Value::str(name.clone()))],
            };
            vec![RakuAstField {
                name: Some("labels"),
                value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(label_node))]),
            }]
        }
    }
}

/// A C-style loop `setup` clause renders its node unwrapped — raku shows the
/// `VarDeclaration::Simple` / `ApplyInfix` directly, not inside a
/// `Statement::Expression`. mutsu's init is a full statement.
fn loop_setup_node(stmt: &Stmt) -> Result<RakuAstNode, RuntimeError> {
    // A `my $i = 0` init is a special case: the loop-init parse path does NOT
    // set the `__has_initializer` trait the top-level VarDecl handler relies on,
    // so detect the initializer from a non-Nil `expr` instead.
    if let Stmt::VarDecl {
        name,
        expr,
        type_constraint,
        is_state,
        is_our,
        is_dynamic,
        where_constraint,
        ..
    } = stmt
    {
        if type_constraint.is_some()
            || *is_state
            || *is_our
            || *is_dynamic
            || where_constraint.is_some()
        {
            return Err(unsupported("scoped/typed variable declaration"));
        }
        let init = (!expr_is_nil(expr)).then_some(expr);
        return var_declaration(name, init, None, None, None, None);
    }
    // Assignment / expression setups convert normally, then get unwrapped.
    let node = convert_stmt(stmt)?.ok_or_else(|| unsupported("empty loop setup clause"))?;
    if node.class != RakuAstClass::StatementExpression {
        return Ok(node);
    }
    if let Some(RakuAstField {
        value: RakuAstFieldValue::Node(v),
        ..
    }) = node.fields.into_iter().next()
        && let ValueView::RakuAst(inner) = v.view()
    {
        return Ok(inner.clone());
    }
    Err(unsupported("loop setup clause"))
}

/// True when `expr` is a `Nil` literal (mutsu's placeholder for a `my $x` with
/// no initializer in a loop-setup clause).
fn expr_is_nil(expr: &Expr) -> bool {
    matches!(expr, Expr::Literal(v) | Expr::LiteralSrc(v, _) if v.is_nil())
}

/// A topic-taking block body (the `{ ... }` of an implicit-topic `for`), which
/// raku marks with `implicit-topic => True` and `required-topic => 1` before
/// the `body` field.
fn topic_block_node(body: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        class: RakuAstClass::Block,
        fields: vec![
            RakuAstField {
                name: Some("implicit-topic"),
                value: RakuAstFieldValue::Node(Value::truth(true)),
            },
            RakuAstField {
                name: Some("required-topic"),
                value: RakuAstFieldValue::Node(Value::int(1)),
            },
            node_field(Some("body"), blockoid(body)?),
        ],
    })
}

/// A multi/zero-parameter pointy block (`-> $a, $b { }`, `-> { }`). An empty
/// parameter list omits the `signature` field entirely (matching raku).
fn pointy_block(param_defs: &[ParamDef], body: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = Vec::new();
    if !param_defs.is_empty() {
        fields.push(node_field(Some("signature"), signature(param_defs, false)?));
    }
    fields.push(node_field(Some("body"), blockoid(body)?));
    Ok(RakuAstNode {
        class: RakuAstClass::PointyBlock,
        fields,
    })
}

/// A single-parameter pointy block (`-> $x { }`). mutsu's `Lambda` node strips
/// the sigil from its single param and does NOT preserve `@`/`%` for a single
/// non-scalar param (`-> @a` becomes `param: "a"`), so we assume `$` — a
/// documented divergence from raku, which shows the real sigil.
fn pointy_block_from_lambda(param: &str, body: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    let sig = RakuAstNode {
        class: RakuAstClass::Signature,
        fields: vec![RakuAstField {
            name: Some("parameters"),
            value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(simple_parameter(
                "$", param, None, false,
            )?))]),
        }],
    };
    Ok(RakuAstNode {
        class: RakuAstClass::PointyBlock,
        fields: vec![
            node_field(Some("signature"), sig),
            node_field(Some("body"), blockoid(body)?),
        ],
    })
}

/// A named routine — `Sub` or `Method` — with an optional signature and a
/// body. A parameter-less routine omits the `signature` field; parameters
/// carry the implicit `type => Type::Setting(Any)` (`type_setting = true`).
fn routine_node(
    class: RakuAstClass,
    name: &str,
    param_defs: &[ParamDef],
    body: &[Stmt],
) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = vec![node_field(Some("name"), name_from_identifier(name))];
    if !param_defs.is_empty() {
        fields.push(node_field(Some("signature"), signature(param_defs, true)?));
    }
    fields.push(node_field(Some("body"), blockoid(body)?));
    Ok(RakuAstNode { class, fields })
}

/// `Signature(parameters => (Parameter, ...))`. `type_setting` prepends the
/// implicit `type => Type::Setting(Any)` on each parameter — present in
/// sub/method signatures, absent in pointy-block signatures.
fn signature(param_defs: &[ParamDef], type_setting: bool) -> Result<RakuAstNode, RuntimeError> {
    let mut params = Vec::with_capacity(param_defs.len());
    for pd in param_defs {
        params.push(Value::rakuast(Box::new(parameter(pd, type_setting)?)));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::Signature,
        fields: vec![RakuAstField {
            name: Some("parameters"),
            value: RakuAstFieldValue::List(params),
        }],
    })
}

/// One `Parameter`. Only plain positional params (name + optional default) are
/// modelled; anything richer (typed, named, slurpy, `where`, sub-signature,
/// traits, optional-marker, invocant, shaped) is the coverage boundary.
fn parameter(pd: &ParamDef, type_setting: bool) -> Result<RakuAstNode, RuntimeError> {
    if pd.named
        || pd.slurpy
        || pd.double_slurpy
        || pd.onearg
        || pd.type_constraint.is_some()
        || pd.literal_value.is_some()
        || pd.sub_signature.is_some()
        || pd.where_constraint.is_some()
        || !pd.traits.is_empty()
        || pd.optional_marker
        || pd.is_invocant
        || pd.shape_constraints.is_some()
        || pd.code_signature.is_some()
        || pd.outer_sub_signature.is_some()
    {
        return Err(unsupported("non-trivial signature parameter"));
    }
    let (sigil, desigil) = split_sigil(&pd.name);
    simple_parameter(sigil, desigil, pd.default.as_ref(), type_setting)
}

/// `Type::Setting.new(Name.from-identifier("Any"))` — the implicit default type
/// carried by every sub/method-signature parameter.
fn type_setting_any() -> RakuAstNode {
    let name = RakuAstNode {
        class: RakuAstClass::Name,
        fields: vec![leaf_field(None, Value::str("Any".to_string()))],
    };
    RakuAstNode {
        class: RakuAstClass::TypeSetting,
        fields: vec![node_field(None, name)],
    }
}

/// Build a `Parameter(target => ParameterTarget::Var, ...)`. A param with a
/// default renders `default => <expr>`; a required one renders `optional => False`.
/// `type_setting` prepends `type => Type::Setting(Any)` (sub/method params).
fn simple_parameter(
    sigil: &str,
    desigil: &str,
    default: Option<&Expr>,
    type_setting: bool,
) -> Result<RakuAstNode, RuntimeError> {
    let target = RakuAstNode {
        class: RakuAstClass::ParameterTargetVar,
        fields: vec![leaf_field(
            Some("name"),
            Value::str(format!("{sigil}{desigil}")),
        )],
    };
    let mut fields = Vec::new();
    if type_setting {
        fields.push(node_field(Some("type"), type_setting_any()));
    }
    fields.push(node_field(Some("target"), target));
    match default {
        Some(d) => fields.push(node_field(Some("default"), convert_expr(d)?)),
        None => fields.push(RakuAstField {
            name: Some("optional"),
            value: RakuAstFieldValue::Node(Value::truth(false)),
        }),
    }
    Ok(RakuAstNode {
        class: RakuAstClass::Parameter,
        fields,
    })
}

/// The postfix `Call::Method` / `Call::QuotedMethod` node shared by plain and
/// hyper method calls: a quoted name -> `Call::QuotedMethod` (no modifier), an
/// unquoted name -> `Call::Method` (with an optional `.?`/`.+`/`.*` dispatch).
fn method_call_postfix(
    name: &str,
    args: &[Expr],
    modifier: Option<char>,
    quoted: bool,
) -> Result<RakuAstNode, RuntimeError> {
    if quoted {
        if modifier.is_some() {
            return Err(unsupported("quoted method name with a modifier"));
        }
        call_quoted_method(name, args)
    } else {
        call_method(name, args, modifier)
    }
}

/// `.method` / `.method(args)` -> `Call::Method(name => Name, [args => ArgList])`.
fn call_method(
    name: &str,
    args: &[Expr],
    modifier: Option<char>,
) -> Result<RakuAstNode, RuntimeError> {
    let name_node = RakuAstNode {
        class: RakuAstClass::Name,
        fields: vec![leaf_field(None, Value::str(name.to_string()))],
    };
    // Field order matches raku: name, args, dispatch.
    let mut fields = vec![node_field(Some("name"), name_node)];
    if !args.is_empty() {
        fields.push(node_field(Some("args"), arg_list(args)?));
    }
    if let Some(m) = modifier {
        // `.?` / `.+` / `.*` become a `dispatch` string.
        fields.push(leaf_field(Some("dispatch"), Value::str(format!(".{m}"))));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::CallMethod,
        fields,
    })
}

/// `."foo"` / `."foo"(args)` -> `Call::QuotedMethod(name => QuotedString,
/// [args => ArgList])`. Unlike `Call::Method`, the name is a QuotedString
/// (a string literal) rather than a `Name.from-identifier`.
fn call_quoted_method(name: &str, args: &[Expr]) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = vec![node_field(
        Some("name"),
        quoted_string(Value::str(name.to_string())),
    )];
    if !args.is_empty() {
        fields.push(node_field(Some("args"), arg_list(args)?));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::CallQuotedMethod,
        fields,
    })
}

/// `$x` / `@a` / `%h` / `&f` usage -> `Var::Lexical("<sigil><name>")`.
fn var_lexical(sigil: &str, name: &str) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::VarLexical,
        fields: vec![leaf_field(None, Value::str(format!("{sigil}{name}")))],
    }
}

/// `Infix`/`Prefix` — a single positional operator string (e.g. `Infix.new("+")`).
fn operator_node(class: RakuAstClass, op: &crate::token_kind::TokenKind) -> RakuAstNode {
    RakuAstNode {
        class,
        fields: vec![leaf_field(None, Value::str(token_kind_to_op_name(op)))],
    }
}

/// True for the list-associative infixes raku renders as `ApplyListInfix`
/// (`andthen` / `orelse` / `notandthen`).
fn is_list_infix(op: &crate::token_kind::TokenKind) -> bool {
    use crate::token_kind::TokenKind;
    matches!(
        op,
        TokenKind::AndThen | TokenKind::OrElse | TokenKind::NotAndThen
    )
}

/// Flatten a left-nested same-operator chain (`a op b op c` parsed as
/// `(a op b) op c`) into a single operand list `[a, b, c]`.
fn flatten_list_infix<'a>(
    op: &crate::token_kind::TokenKind,
    left: &'a Expr,
    right: &'a Expr,
    out: &mut Vec<&'a Expr>,
) {
    if let Expr::Binary {
        left: ll,
        op: lop,
        right: lr,
    } = left
    {
        if lop == op {
            flatten_list_infix(op, ll, lr, out);
        } else {
            out.push(left);
        }
    } else {
        out.push(left);
    }
    out.push(right);
}

/// `Postfix` — a single NAMED `operator` string (e.g. `Postfix.new(operator => "++")`).
fn postfix_node(op: &crate::token_kind::TokenKind) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::Postfix,
        fields: vec![leaf_field(
            Some("operator"),
            Value::str(token_kind_to_op_name(op)),
        )],
    }
}

fn convert_literal(v: &Value) -> Result<RakuAstNode, RuntimeError> {
    match v.view() {
        ValueView::Int(_) | ValueView::BigInt(_) => Ok(RakuAstNode {
            class: RakuAstClass::IntLiteral,
            fields: vec![leaf_field(None, v.clone())],
        }),
        ValueView::Rat(..) | ValueView::FatRat(..) | ValueView::BigRat(..) => Ok(RakuAstNode {
            class: RakuAstClass::RatLiteral,
            fields: vec![leaf_field(None, v.clone())],
        }),
        ValueView::Str(_) => Ok(quoted_string(v.clone())),
        other => Err(unsupported(&format!("literal {other:?}"))),
    }
}

/// A string literal renders as `QuotedString.new(segments => (StrLiteral,))`.
fn quoted_string(str_value: Value) -> RakuAstNode {
    let seg = RakuAstNode {
        class: RakuAstClass::StrLiteral,
        fields: vec![leaf_field(None, str_value)],
    };
    RakuAstNode {
        class: RakuAstClass::QuotedString,
        fields: vec![RakuAstField {
            name: Some("segments"),
            value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(seg))]),
        }],
    }
}

/// A listop `say EXPR` — `Call::Name::WithoutParentheses`.
fn listop_call(name: &'static str, args: &[Expr]) -> Result<RakuAstNode, RuntimeError> {
    call_name(name, args, true)
}

fn call_name(
    name: &str,
    args: &[Expr],
    without_parentheses: bool,
) -> Result<RakuAstNode, RuntimeError> {
    let name_node = RakuAstNode {
        class: RakuAstClass::Name,
        fields: vec![leaf_field(None, Value::str(name.to_string()))],
    };
    let arg_list = arg_list(args)?;
    let class = if without_parentheses {
        RakuAstClass::CallNameWithoutParentheses
    } else {
        RakuAstClass::CallName
    };
    Ok(RakuAstNode {
        class,
        fields: vec![
            node_field(Some("name"), name_node),
            node_field(Some("args"), arg_list),
        ],
    })
}

fn arg_list(args: &[Expr]) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = Vec::with_capacity(args.len());
    for a in args {
        fields.push(node_field(None, convert_expr(a)?));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::ArgList,
        fields,
    })
}
