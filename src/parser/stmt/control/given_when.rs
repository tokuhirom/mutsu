use super::*;

/// Parse `given`/`when`/`default`.
pub(crate) fn given_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("given", input).ok_or_else(|| PError::expected("given statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, topic) = expression(rest)?;
    let (rest, _) = ws(rest)?;
    // Check for pointy block: given EXPR -> $param { ... }
    let (rest, pointy_param) = if let Some(r) = rest.strip_prefix("->") {
        let (r, _) = ws(r)?;
        let (r, pd) = parse_pointy_param(r)?;
        let (r, _) = ws(r)?;
        (r, Some(pd))
    } else {
        (rest, None)
    };
    let (rest, mut body) = block(rest)?;
    if let Some(pd) = pointy_param {
        body.insert(0, pointy_topic_bind(&pd));
    }
    Ok((
        rest,
        Stmt::Given {
            topic,
            body,
            is_statement_modifier: false,
        },
    ))
}

pub(crate) fn when_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("when", input).ok_or_else(|| PError::expected("when statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, cond) = condition_expr(rest)?;
    let (rest, _) = ws(rest)?;
    // An undeclared bareword in term position immediately followed by a block is
    // treated by raku as a function call that gobbles the block (e.g.
    // `when X::Y {}` parses as `when X::Y({...})`), which then leaves the `when`
    // without its required block. This produces an X::Syntax::BlockGobbled sorrow
    // plus an X::Syntax::Missing(block) panic, bundled in an X::Comp::Group.
    //
    // raku diagnoses this for *any* undeclared name because its parser has
    // already executed every `use` at BEGIN time — declaration order matters
    // there too (`when Foo {}; class Foo {}` is the same error). mutsu's
    // parse-time symbol table is a close approximation: `register_module_exports`
    // scans each `use`d module's source for the types it declares, transitively
    // and memoized, so imported names are known here as well. The one thing that
    // approximation cannot cover is a module the parser could not resolve to a
    // file (an `inst#` installed repository, a runtime `require`, a module
    // missing from this environment) — `type_index_is_complete` reports that,
    // and while it is false the diagnosis stays restricted to the reserved
    // `X::`/`CX::` exception namespaces, whose members are either known builtin
    // exceptions or genuinely undeclared.
    if rest.starts_with('{')
        && let Expr::BareWord(name) = &cond
        && (crate::parser::stmt::simple::type_index_is_complete()
            || name.starts_with("X::")
            || name.starts_with("CX::"))
        && !bareword_names_known_term(name)
    {
        return Err(gobbled_block_error(name, rest.len()));
    }
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::When { cond, body }))
}

/// Whether a bareword `when` matcher names something the parser already knows,
/// and so cannot be a routine call that gobbled the block.
fn bareword_names_known_term(name: &str) -> bool {
    use crate::parser::stmt::simple;
    use crate::runtime::utils;
    // A type smiley (`when Map:D { }`, `when Channel:U { }`) can only attach to
    // a type name, so the name is never a routine call whatever the base is —
    // an undeclared base is a different diagnostic ("Type ... is not declared").
    if name.ends_with(":D") || name.ends_with(":U") || name.ends_with(":_") {
        return true;
    }
    if utils::is_known_type_constraint(name)
        || utils::is_known_compound_type(name)
        || utils::is_builtin_enum_value(name)
        || utils::is_builtin_constant_term(name)
        || simple::is_user_declared_type(name)
        || simple::is_user_declared_value_term(name)
        || simple::is_user_declared_enum_value(name)
        // A `constant FOO is export` in a `use`d module: a complete nullary
        // term, harvested by the module scan.
        || simple::is_imported_value_term(name)
        // A declared routine used as a matcher (`when foo { }`) really does
        // gobble the block in raku, but the message differs and the construct is
        // vanishingly rare; leaving it alone keeps this check to the case the
        // ticket is about.
        || simple::is_user_declared_sub(name)
    {
        return true;
    }
    // A package-qualified enum value: `when Day::Mon { }`,
    // `when HTTP::HPACK::Indexing::Indexed { }`. The head names the enum type
    // (or, for a constant, the declaring package) and the last segment one of
    // its values.
    if let Some((head, last)) = name.rsplit_once("::") {
        let head_is_type = simple::is_user_declared_type(head)
            || utils::is_known_type_constraint(head)
            || utils::is_known_compound_type(head);
        let last_is_value = simple::is_user_declared_enum_value(last)
            || utils::is_builtin_enum_value(last)
            || simple::is_imported_value_term(last);
        if head_is_type && last_is_value {
            return true;
        }
    }
    false
}

/// Build the `X::Comp::Group` raised when an undeclared bareword gobbles the
/// block that a surrounding construct (e.g. `when`) required.
///
/// `remaining_len` is the length of the still-unparsed input at the offending
/// `when`, so the CLI can report a line/column. Both matter for diagnosability:
/// the message used to be the bare `X::Comp::Group: Missing block` with no name
/// and no location, which in a 1300-line module (`Raku::Pod::Render`'s
/// `ProcessedPod`, whose `when X::LibCurl { … }` is undeclared because its
/// `LibCurl::Easy` dependency is absent) said nothing about where to look. raku
/// names the routine and the line; now so does mutsu.
fn gobbled_block_error(name: &str, remaining_len: usize) -> PError {
    let sorrow = Value::make_exception(
        "X::Syntax::BlockGobbled",
        &[
            ("what", Value::str(name.to_string())),
            (
                "message",
                Value::str(format!(
                    "Function '{name}' needs parens to avoid gobbling block \
                     (or perhaps it's a class that's not declared or available in this scope?)"
                )),
            ),
        ],
    );
    let panic = Value::make_exception(
        "X::Syntax::Missing",
        &[
            ("what", Value::str("block".to_string())),
            (
                "message",
                Value::str(format!("Missing block (apparently claimed by '{name}')")),
            ),
        ],
    );
    let group = Value::make_comp_group(
        format!(
            "Function '{name}' needs parens to avoid gobbling block \
             (or perhaps it's a class that's not declared or available in this scope?)\n\
             Missing block (apparently claimed by '{name}')"
        ),
        Some(panic),
        vec![sorrow],
        vec![],
    );
    let mut err = PError::fatal_with_exception(
        format!(
            "X::Comp::Group: Function '{name}' needs parens to avoid gobbling block \
             (or perhaps it's a class that's not declared or available in this scope?)\n\
             Missing block (apparently claimed by '{name}')"
        ),
        Box::new(group),
    );
    err.remaining_len = Some(remaining_len);
    err
}

pub(crate) fn default_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("default", input).ok_or_else(|| PError::expected("default statement"))?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::Default(body)))
}
