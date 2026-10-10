use super::export_trait::parse_export_trait_tags;
use super::trait_type::parse_trait_type_name;
use super::*;

/// A `returns`/`of` trait whose type-name expression fails to parse at all
/// (`sub foo() returns !!!wtf??? { }`) is rakudo's `X::Syntax::Malformed:
/// Malformed trait` — not the generic "Confused" the unconverted parse error
/// would otherwise surface as. Mirrors `malformed_initializer`'s contract
/// (`stmt/decl/my_decl_assign.rs`): only convert an error that failed
/// immediately (no partial parse to prefer) and isn't already a fatal or
/// structured-exception error.
fn malformed_trait(err: PError, type_input: &str) -> PError {
    let failed_immediately = err.remaining_len.is_none_or(|len| len >= type_input.len());
    if err.exception.is_some() || err.is_fatal() || !failed_immediately {
        return err;
    }
    PError::malformed("trait")
}

/// The name after `is`: a Raku identifier, optionally package-qualified
/// (`Foo::Bar`). Each `::` must be followed by another identifier segment, so
/// a trailing `::` is left unconsumed.
fn parse_qualified_trait_name(input: &str) -> PResult<'_, &str> {
    let (mut rest, _) = parse_raku_ident(input)?;
    while let Some(after) = rest.strip_prefix("::") {
        match parse_raku_ident(after) {
            Ok((r, _)) => rest = r,
            Err(_) => break,
        }
    }
    Ok((rest, &input[..input.len() - rest.len()]))
}

/// Result of parsing sub traits.
pub(crate) struct SubTraits {
    pub source_traits: Vec<crate::ast::routine_trait::RoutineTrait>,
    pub is_export: bool,
    pub export_tags: Vec<String>,
    pub is_test_assertion: bool,
    pub is_rw: bool,
    pub is_raw: bool,
    pub return_type: Option<String>,
    pub associativity: Option<String>,
    /// Non-builtin trait names (e.g. `me'd`) for custom `trait_mod:<is>` dispatch,
    /// with optional argument expression.
    pub custom_traits: Vec<(String, Option<crate::ast::Expr>)>,
    /// Precedence trait: (trait_name, reference_operator).
    /// trait_name is one of "tighter", "looser", "equiv".
    /// reference_operator is the operator symbol or full name (e.g. `*`, `+`, `infix:<+>`, `prefix:<foo>`).
    pub precedence_trait: Option<(String, String)>,
    /// `handles` specifications on a method declaration, e.g.
    /// `method Str() handles 'uc' { ... }`.
    pub handles: Vec<crate::ast::HandleSpec>,
}

/// Parse sub/method traits like `is test-assertion`, `is export`, `returns Str`, `of Num`, etc.
/// Returns `SubTraits` indicating which traits were found.
pub(crate) fn parse_sub_traits(mut input: &str) -> PResult<'_, SubTraits> {
    use crate::ast::routine_trait::{RoutineTrait, TraitArgument};
    let keeping = crate::ast::spelled::keeping();
    let mut source_traits = Vec::new();
    let mut is_export = false;
    let mut export_tags: Vec<String> = Vec::new();
    let mut is_test_assertion = false;
    let mut is_rw = false;
    let mut is_raw = false;
    let mut return_type = None;
    let mut associativity = None;
    let mut custom_traits: Vec<(String, Option<crate::ast::Expr>)> = Vec::new();
    let mut seen_traits: Vec<String> = Vec::new();
    let mut precedence_trait: Option<(String, String)> = None;
    let mut handles: Vec<crate::ast::HandleSpec> = Vec::new();
    loop {
        let (r, _) = ws(input)?;
        if r.starts_with('{') || r.is_empty() {
            return Ok((
                r,
                SubTraits {
                    source_traits,
                    is_export,
                    export_tags,
                    is_test_assertion,
                    is_rw,
                    is_raw,
                    return_type,
                    associativity,
                    custom_traits: custom_traits.clone(),
                    precedence_trait,
                    handles: handles.clone(),
                },
            ));
        }
        if let Some(r_after) = keyword("handles", r) {
            // No space before the angle-word list is legal — `method build
            // handles<token node at-rule> { ... }` is how CSS::Grammar::Actions
            // (and Raku's own documentation) spells it — so only OPTIONAL
            // whitespace may be required here. `keyword` already guards the word
            // boundary, so `handlesfoo` still never matches. The attribute form
            // in `has_decl.rs` has always used `ws` for the same reason.
            let (r_after, _) = ws(r_after)?;
            let mut rest_out = r_after;
            super::super::decl::parse_handle_specs(r_after, &mut handles, &mut rest_out)?;
            input = rest_out;
            continue;
        }
        if let Some(r) = keyword("is", r) {
            let (r, _) = ws(r)?;
            // Parse the trait name (Raku identifier: may include hyphens and
            // apostrophes). A package-qualified type name (`is Path::Map(...)`)
            // is a trait too: it dispatches `trait_mod:<is>` with the type
            // object as a positional, exactly like an unqualified one.
            let (r, trait_name) = parse_qualified_trait_name(r)?;
            let mut source_argument = None;
            let mut source_argument_required = false;
            if seen_traits.contains(&trait_name.to_string()) {
                add_parse_warning(
                    format!(
                        "Potential difficulties:\n    Duplicate 'is {}' trait",
                        trait_name
                    ),
                    crate::parser::primary::current_line_number(input),
                );
            }
            seen_traits.push(trait_name.to_string());
            if trait_name == "hidden-from-backtrace" {
                if crate::ast::spelled::keeping() {
                    source_traits.push(RoutineTrait::Is {
                        name: trait_name.to_string(),
                        argument: None,
                    });
                }
                // Keep this as an internal marker so method declarations can
                // carry the trait through the AST without exposing it to a
                // user `trait_mod:<is>` candidate.
                custom_traits.push(("__hidden_from_backtrace".to_string(), None));
                input = r;
                continue;
            }
            if trait_name == "export" {
                is_export = true;
                let (r2, tags) = parse_export_trait_tags(r)?;
                if crate::ast::spelled::keeping() {
                    source_traits.push(RoutineTrait::Is {
                        name: trait_name.to_string(),
                        argument: (!tags.is_empty())
                            .then(|| TraitArgument::ExportTags(tags.clone())),
                    });
                }
                if tags.is_empty() {
                    if !export_tags.iter().any(|t| t == "DEFAULT") {
                        export_tags.push("DEFAULT".to_string());
                    }
                } else {
                    for tag in tags {
                        if !export_tags.iter().any(|t| t == &tag) {
                            export_tags.push(tag);
                        }
                    }
                }
                input = r2;
                continue;
            } else if trait_name == "test-assertion" {
                is_test_assertion = true;
                // Also queue it as a custom trait so a user-defined
                // `trait_mod:<is>(Routine:D, :$test-assertion!)` (e.g. the real
                // `Test.rakumod`) gets a chance to run, in addition to mutsu's
                // own builtin handling of the flag above.
                custom_traits.push((trait_name.to_string(), None));
            } else if trait_name == "rw" {
                is_rw = true;
            } else if trait_name == "raw" {
                is_raw = true;
            } else if trait_name == "looser" || trait_name == "tighter" || trait_name == "equiv" {
                // A precedence trait leaves its name as a placeholder, which
                // the prefix-`looser` parse check reads. It must not clobber
                // an explicit `is assoc<...>` written before it:
                // `is assoc<list> is equiv(&[~])` stays list-associative, as
                // in rakudo (OneSeq's `[>>>] @a, @b`).
                if associativity
                    .as_deref()
                    .is_none_or(|a| matches!(a, "looser" | "tighter" | "equiv"))
                {
                    associativity = Some(trait_name.to_string());
                }
            } else if trait_name == "DEPRECATED" {
                // Will capture parenthesized arg below
                custom_traits.push(("DEPRECATED".to_string(), None));
            } else if trait_name != "assoc"
                && trait_name != "equiv"
                && trait_name != "tighter"
                && trait_name != "looser"
                && trait_name != "readonly"
                && trait_name != "hidden-from-backtrace"
                && trait_name != "nodal"
                && trait_name != "pure"
            {
                // Placeholder — will be updated with arg below if present
                custom_traits.push((trait_name.to_string(), None));
            }
            let (mut r, _) = ws(r)?;
            if r.starts_with('<') {
                let (r2, arg) = parse_trait_angle_arg(r)?;
                if keeping {
                    source_argument = Some(TraitArgument::Words(arg.clone()));
                }
                if trait_name == "assoc" {
                    associativity = Some(arg);
                } else if trait_name == "tighter" || trait_name == "looser" || trait_name == "equiv"
                {
                    precedence_trait = Some((trait_name.to_string(), arg));
                } else if trait_name == "DEPRECATED" {
                    // `is DEPRECATED<msg>` — set the deprecation message
                    if let Some(pos) = custom_traits.iter().position(|(t, _)| t == "DEPRECATED") {
                        custom_traits[pos] = (format!("DEPRECATED:{}", arg), None);
                    }
                } else if let Some(pos) = custom_traits.iter().rposition(|(t, _)| t == trait_name) {
                    // Angle-bracket arguments are literal strings.  Keep them
                    // in the same custom-trait slot as parenthesized
                    // arguments so traits such as `is symbol<localtime>` can
                    // reach NativeCall registration.
                    // A multi-word `<a b>` is a word list, exactly as the
                    // expression `<a b>` is (`is also<a b>`).
                    let words: Vec<&str> = arg.split_whitespace().collect();
                    custom_traits[pos].1 = Some(if words.len() > 1 {
                        crate::ast::Expr::ArrayLiteral(
                            words
                                .into_iter()
                                .map(|w| crate::ast::Expr::Literal(Value::str(w.to_string())))
                                .collect(),
                        )
                    } else {
                        crate::ast::Expr::Literal(Value::str(arg))
                    });
                }
                r = r2;
            }
            // Parse optional parenthesized trait args: is export(:DEFAULT), is equiv(&prefix:<+>)
            if r.starts_with('(') {
                source_argument_required = true;
                let before_parens = r;
                r = skip_balanced_parens(r);
                if trait_name == "DEPRECATED" {
                    // Extract the deprecation message from parenthesized form
                    let paren_content = &before_parens[1..before_parens.len() - r.len() - 1];
                    if keeping
                        && let Ok((after, expr)) = expression(paren_content)
                        && after.trim().is_empty()
                    {
                        source_argument = Some(TraitArgument::Parentheses(expr));
                    }
                    let msg = paren_content.trim();
                    // Strip surrounding quotes from the message
                    let msg = if (msg.starts_with('"') && msg.ends_with('"'))
                        || (msg.starts_with('\'') && msg.ends_with('\''))
                    {
                        &msg[1..msg.len() - 1]
                    } else {
                        msg
                    };
                    // Replace the plain "DEPRECATED" with "DEPRECATED:msg"
                    if let Some(pos) = custom_traits.iter().position(|(t, _)| t == "DEPRECATED") {
                        custom_traits[pos] = (format!("DEPRECATED:{}", msg), None);
                    }
                } else if trait_name == "tighter" || trait_name == "looser" || trait_name == "equiv"
                {
                    // Extract the reference operator from parenthesized form.
                    // Traits apply in order, so a later precedence trait
                    // replaces an earlier one, as in rakudo.
                    let paren_content = &before_parens[1..before_parens.len() - r.len() - 1];
                    let ref_op = paren_content.trim().to_string();
                    if keeping {
                        source_argument = Some(TraitArgument::Operator(ref_op.clone()));
                    }
                    precedence_trait = Some((trait_name.to_string(), ref_op));
                } else if trait_name == "assoc" {
                    // `is assoc('non')` / `is assoc("left")` — parenthesized string form
                    let paren_content = &before_parens[1..before_parens.len() - r.len() - 1];
                    let value = paren_content.trim();
                    let value = if (value.starts_with('\'') && value.ends_with('\''))
                        || (value.starts_with('"') && value.ends_with('"'))
                    {
                        &value[1..value.len() - 1]
                    } else {
                        value
                    };
                    associativity = Some(value.to_string());
                    if keeping {
                        source_argument = Some(TraitArgument::Parentheses(
                            crate::ast::Expr::Literal(Value::str(value.to_string())),
                        ));
                    }
                } else {
                    // For custom traits, parse the parenthesized content as an expression
                    let paren_content = &before_parens[1..before_parens.len() - r.len() - 1];
                    let paren_content = paren_content.trim();
                    if !paren_content.is_empty()
                        && let Ok((after, expr)) = expression(paren_content)
                    {
                        // A trait argument may be a comma-separated expression
                        // list (`is memoized(%cache, &keyer)` or the documented
                        // `is native('foo', v1)` form). `expression` parses only
                        // the first item, so re-parse the whole content as a
                        // parenthesized list whenever another item follows.
                        let expr = if after.trim_start().starts_with(',') {
                            let wrapped = format!("({paren_content})");
                            match expression(&wrapped) {
                                Ok((_, list_expr)) => list_expr,
                                Err(_) => expr,
                            }
                        } else {
                            expr
                        };
                        if keeping
                            && (after.trim().is_empty() || after.trim_start().starts_with(','))
                        {
                            source_argument = Some(TraitArgument::Parentheses(expr.clone()));
                        }
                        // Update the last custom trait entry with the parsed argument
                        if let Some(pos) = custom_traits.iter().rposition(|(t, _)| t == trait_name)
                        {
                            custom_traits[pos].1 = Some(expr);
                        }
                    }
                }
            }
            input = r;
            if crate::ast::spelled::keeping() {
                if source_argument_required && source_argument.is_none() {
                    source_traits.push(RoutineTrait::Unsupported(format!(
                        "routine trait `{trait_name}` argument"
                    )));
                    continue;
                }
                source_traits.push(RoutineTrait::Is {
                    name: trait_name.to_string(),
                    argument: source_argument,
                });
            }
            continue;
        }
        if let Some(r) = keyword("returns", r) {
            let (r, _) = ws(r)?;
            let (r, type_name) = parse_trait_type_name(r).map_err(|e| malformed_trait(e, r))?;
            if crate::ast::spelled::keeping() {
                source_traits.push(RoutineTrait::Returns(type_name.clone()));
            }
            return_type = Some(type_name);
            // Mark that the return type came from a `returns`/`of` trait (not a
            // `-->` signature arrow): an undeclared one is X::InvalidType, while
            // an undeclared `-->` type is X::Undeclared.
            if !custom_traits.iter().any(|(t, _)| t == "__return_via_trait") {
                custom_traits.push(("__return_via_trait".to_string(), None));
            }
            input = r;
            continue;
        }
        if let Some(r) = keyword("of", r) {
            let (r, _) = ws(r)?;
            let (r, type_name) = parse_trait_type_name(r).map_err(|e| malformed_trait(e, r))?;
            if crate::ast::spelled::keeping() {
                match source_traits.last_mut() {
                    Some(RoutineTrait::Returns(base) | RoutineTrait::Of(base))
                        if !base.contains('[') =>
                    {
                        *base = format!("{base}[{type_name}]");
                    }
                    _ => source_traits.push(RoutineTrait::Of(type_name.clone())),
                }
            }
            // `of` parameterizes a preceding `returns`/role type, e.g.
            // `returns Positional of Int` means return type `Positional[Int]`.
            return_type = Some(match return_type {
                Some(base) if !base.contains('[') => format!("{base}[{type_name}]"),
                _ => type_name,
            });
            // `of` is a distinct trait from `returns` in RakuAST
            // (`Trait::Of` vs `Trait::Returns`), so it gets its own marker;
            // both mean "the return type came from a trait, not `-->`".
            if !custom_traits.iter().any(|(t, _)| t == "__return_via_of") {
                custom_traits.push(("__return_via_of".to_string(), None));
            }
            input = r;
            continue;
        }
        if let Some(r) = r.strip_prefix("-->") {
            let (r, _) = ws(r)?;
            let (r, type_name) = parse_trait_type_name(r)?;
            return_type = Some(type_name);
            input = r;
            continue;
        }
        return Ok((
            r,
            SubTraits {
                source_traits,
                is_export,
                export_tags,
                is_test_assertion,
                is_rw,
                is_raw,
                return_type,
                associativity,
                custom_traits: custom_traits.clone(),
                precedence_trait,
                handles: handles.clone(),
            },
        ));
    }
}

/// Reject invocant markers (':') in non-method signatures (sub, pointy block).
/// Reject attribute twigil parameters ($!x, $.x, @!a, @.a, %!h, %.h) in sub signatures
/// UNLESS `self` is lexically available — a plain `sub` nested directly in a
/// method/submethod body (with no intervening class/role/grammar/package body)
/// closes over the method's invocant, so it may bind an attributive parameter
/// exactly as the method itself can (#8452).
pub(crate) fn reject_attr_params_in_sub(params: &[ParamDef]) -> Result<(), PError> {
    if super::super::simple::self_available() {
        return Ok(());
    }
    for p in params {
        // $! is the error variable, not an attribute; only reject $!name (attribute twigil)
        if (p.name.starts_with('!') && p.name != "!") || p.name.starts_with('.') {
            let variable = format!("${}", p.name);
            let msg = format!(
                "X::Syntax::NoSelf: Variable {} used where no 'self' is available",
                variable
            );
            let mut attrs = std::collections::HashMap::new();
            attrs.insert(
                "message".to_string(),
                Value::str(format!(
                    "Variable {} used where no 'self' is available",
                    variable
                )),
            );
            attrs.insert("variable".to_string(), Value::str(variable));
            let ex = Value::make_instance(Symbol::intern("X::Syntax::NoSelf"), attrs);
            return Err(PError::fatal_with_exception(msg, Box::new(ex)));
        }
    }
    Ok(())
}

pub(crate) fn reject_invocant_in_sub(params: &[ParamDef]) -> Result<(), PError> {
    if params
        .iter()
        .any(|p| p.is_invocant || p.traits.iter().any(|t| t == "invocant"))
    {
        return Err(invocant_not_allowed_error());
    }
    Ok(())
}

/// A `:` invocant marker outside a method signature (a plain `sub`, or a
/// pointy block / lambda — `-> $a: { ... }`): only a method may declare an
/// invocant. Rakudo uses the same wording regardless of which non-method
/// context it was found in.
pub(crate) fn invocant_not_allowed_error() -> PError {
    let text = "Can only use the : invocant marker in the signature for a method";
    let msg = format!("X::Syntax::Signature::InvocantNotAllowed: {}", text);
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(text.to_string()));
    let ex = Value::make_instance(
        Symbol::intern("X::Syntax::Signature::InvocantNotAllowed"),
        attrs,
    );
    PError::fatal_with_exception(msg, Box::new(ex))
}
