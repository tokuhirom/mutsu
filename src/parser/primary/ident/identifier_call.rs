use crate::ast::{Expr, Stmt, make_anon_sub};
use crate::parser::expr::{
    expression, expression_no_sequence, parse_fat_arrow_value, should_wrap_whatevercode, term_expr,
    wrap_whatevercode,
};
use crate::parser::helpers::{
    consume_unspace, is_loop_label_name, is_raku_identifier_start, normalize_raku_identifier, ws,
    ws1,
};
use crate::parser::parse_result::{PError, PResult, merge_expected_messages, parse_char};
use crate::parser::primary::current_line_number;
use crate::parser::primary::ident::anon_sub::{
    make_anon_method, parse_anon_method_with_params, parse_anon_sub_with_params, set_anon_sub_rw,
};
use crate::parser::primary::ident::circumfix::parse_raw_braced_regex_body;
use crate::parser::primary::ident::listop::{
    TEST_CALLSITE_LINE_KEY, make_call_expr, make_call_expr_from_listop_args,
    parse_expr_listop_args, parse_listop_arg, try_parse_no_paren_invocant_colon_call,
};
use crate::parser::primary::ident::predicates::{
    balanced_paren_text, is_expr_listop, is_infix_word_op, is_keyword, is_listop,
    is_require_terminator, is_stmt_modifier_ahead, is_unspace_before_postfix,
    keyword_as_function_error, looks_like_binding, parens_then_block, starts_with_term_keyword,
    try_extend_colon_name,
};
use crate::parser::primary::ident::supply::supply_method_call;
use crate::parser::primary::misc::{
    anon_class_expr, anon_grammar_expr, anon_role_expr, parse_block_body,
};
use crate::parser::stmt::keyword;
use crate::symbol::Symbol;
use crate::value::Value;

/// When `do STMT` parses its inner statement via the full statement parser, a
/// statement modifier (`do $_ for @list`) or assignment consumes the trailing
/// `;`. In expression context the `;` belongs to the *outer* statement, so if it
/// were swallowed the surrounding expression parser would keep going and treat a
/// following listop (`my @a = do $_ for 2..4; say @a`) as an infix operator on
/// the `do` result. Give the terminator back by returning a remaining slice that
/// starts at the consumed `;`.
fn restore_do_stmt_terminator<'a>(orig: &'a str, after: &'a str) -> &'a str {
    let consumed = orig.len().saturating_sub(after.len());
    if consumed > 0 && orig.as_bytes()[consumed - 1] == b';' {
        &orig[consumed - 1..]
    } else {
        after
    }
}

fn parse_require_expr<'a>(input: &'a str, rest: &'a str) -> PResult<'a, Expr> {
    let (mut rest, _) = ws1(rest)?;
    let (r_target, target_raw) =
        if let Ok((r_mod, mod_name)) = crate::parser::stmt::parse_sub_name_pub(rest) {
            if r_mod.starts_with(":file(") {
                (
                    r_mod,
                    Expr::Literal(Value::Package(Symbol::intern(&normalize_raku_identifier(
                        &mod_name,
                    )))),
                )
            } else {
                term_expr(rest)?
            }
        } else {
            term_expr(rest)?
        };
    let module_name_for_parse = match &target_raw {
        Expr::BareWord(name) => Some(name.clone()),
        Expr::Literal(Value::Package(name)) => Some(name.resolve()),
        Expr::Literal(Value::Str(name)) => Some(name.to_string()),
        _ => None,
    };
    let target = if let Expr::BareWord(name) = target_raw {
        Expr::Literal(Value::Package(Symbol::intern(&name)))
    } else {
        target_raw
    };
    rest = r_target;

    let mut args = vec![target];

    if let Some(module_name) = module_name_for_parse {
        let is_path_like = module_name.ends_with(".rakumod")
            || module_name.ends_with(".pm6")
            || module_name.contains('/')
            || module_name.contains('\\');
        if !is_path_like {
            crate::parser::stmt::simple::register_module_exports(&module_name);
        }
    }

    let (r_ws, _) = ws(rest)?;
    rest = r_ws;
    if let Some(after_file) = rest.strip_prefix(":file(") {
        let (r, _) = ws(after_file)?;
        let (r, file_expr) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        args.push(Expr::Binary {
            left: Box::new(Expr::Literal(Value::str(
                "__mutsu_require_file".to_string(),
            ))),
            op: crate::token_kind::TokenKind::FatArrow,
            right: Box::new(file_expr),
        });
        rest = r;
    }

    loop {
        let ws_start = rest;
        let (r, _) = ws(rest)?;
        let consumed = &ws_start[..ws_start.len().saturating_sub(r.len())];
        if consumed.contains('\n') {
            rest = r;
            break;
        }
        rest = r;
        if is_require_terminator(rest) {
            break;
        }
        if rest.starts_with('<') {
            let (r, import_expr) = crate::parser::primary::primary(rest)?;
            args.push(import_expr);
            rest = r;
            continue;
        }
        let (r, import_expr) = parse_listop_arg(rest)?;
        args.push(import_expr);
        rest = r;
    }

    Ok((rest, make_call_expr("require".to_string(), input, args)))
}

pub(crate) fn identifier_or_call(input: &str) -> PResult<'_, Expr> {
    let (rest, name) = crate::parser::stmt::parse_raku_ident(input)?;
    let name = normalize_raku_identifier(name);

    // C++ constructor syntax: `new TypeName` (indirect object notation) is
    // unsupported in Raku — `Foo.new` must be used instead. Detect `new`
    // followed by whitespace and an uppercase-initial bareword type name and
    // reject with X::Obsolete (matching rakudo). Restricting to an uppercase
    // initial keeps operator/keyword words (all lowercase: `new and`, `new if`)
    // and `new(...)` / `newFoo` from triggering.
    if name == "new" && rest.starts_with([' ', '\t']) {
        let after = rest.trim_start_matches([' ', '\t']);
        if after.chars().next().is_some_and(char::is_uppercase)
            && let Ok((tail, _typename)) = crate::parser::stmt::parse_raku_ident(after)
            // `new Foo: args` is the valid colon-invocant method call (== Foo.new(args));
            // only the bare `new Foo` / `new Foo(...)` indirect form is C++ syntax.
            && (!tail.starts_with(':') || tail.starts_with("::"))
        {
            return Err(PError::fatal(
                "X::Obsolete: Unsupported use of C++ constructor syntax.  \
                 In Raku please use: method call syntax."
                    .to_string(),
            ));
        }
    }

    // A statement-control keyword immediately followed by `(...) { ... }` (no
    // whitespace before the parens) is a `keyword()`-as-function mistake
    // (`if() {}`, `with() {}`). Raku rejects this with an X::Comp::Group whose
    // first sorrow is X::Syntax::KeywordAsFunction. Without a trailing block
    // (`loop(5)`) Raku instead reports an undeclared routine, so we only flag
    // the block form here.
    if rest.starts_with('(')
        && matches!(
            name.as_str(),
            "if" | "unless"
                | "while"
                | "until"
                | "for"
                | "given"
                | "when"
                | "with"
                | "without"
                | "loop"
        )
        && parens_then_block(rest)
    {
        return Err(keyword_as_function_error(&name));
    }

    // If the identifier is followed by `=>` (fat arrow), treat it as a pair
    // key regardless of whether it would otherwise be a declarator keyword.
    // e.g., `(my => 1)`, `(sub => 1)`, `(class => 1)` are all valid pairs.
    {
        let after_ws = rest.trim_start();
        if after_ws.starts_with("=>") && !after_ws.starts_with("==>") {
            let (r, _) = ws(rest)?;
            let r2 = &r[2..];
            let (r2, _) = ws(r2)?;
            let (r2, mut value) = parse_fat_arrow_value(r2)?;
            // Wrap WhateverCode in the pair value (e.g. `foo => |*` should have
            // a WhateverCode as the value, not slip(Whatever)).
            if should_wrap_whatevercode(&value) {
                value = wrap_whatevercode(&value);
            }
            return Ok((
                r2,
                Expr::Binary {
                    left: Box::new(Expr::Literal(Value::str(name))),
                    op: crate::token_kind::TokenKind::FatArrow,
                    right: Box::new(value),
                },
            ));
        }
    }

    // Try to extend the identifier with colon-pair adverbs (e.g. meow:foo<bar>)
    // if the extended name matches a known user-defined sub.
    {
        let (extended_rest, extended_name) = try_extend_colon_name(rest, &name);
        if extended_name != name
            && crate::parser::stmt::simple::is_user_declared_sub(&extended_name)
        {
            let (r, _) = ws(extended_rest)?;
            if let Some(r) = r.strip_prefix('(') {
                let (r, _) = ws(r)?;
                let full_name = Symbol::intern(&extended_name);
                if let Some(r) = r.strip_prefix(')') {
                    return Ok((
                        r,
                        Expr::Call {
                            name: full_name,
                            args: vec![],
                        },
                    ));
                }
                let (r, args) = crate::parser::primary::parse_call_arg_list(r)?;
                let (r, _) = ws(r)?;
                let Some(r) = r.strip_prefix(')') else {
                    return Err(PError::expected("')'"));
                };
                return Ok((
                    r,
                    Expr::Call {
                        name: full_name,
                        args,
                    },
                ));
            }
            // No parens — bare reference
            return Ok((extended_rest, Expr::BareWord(extended_name)));
        }
    }

    // `undef` as a value is obsolete Perl 5 syntax. A following `(` makes it a
    // routine call instead (e.g. `sub undef {}; undef()`), so only flag the
    // bare-term use.
    if name == "undef" && !rest.starts_with('(') {
        return Err(crate::parser::stmt::control::make_obsolete_error(
            "undef",
            None,
            "X::Obsolete: Unsupported use of undef as a value. \
             In Raku please use: an appropriate type object such as Any.",
        ));
    }

    // Handle special expression keywords before qualified name resolution
    match name.as_str() {
        "infix" | "prefix" | "postfix" | "circumfix" | "postcircumfix" | "trait_mod" => {
            // `infix:(...)` adverbs the operator declarator with a signature
            // literal, which is illegal -> X::Syntax::Adverb ("You can't adverb
            // :(...)"). Operator names use `:<...>`/`:sym<...>`, never `:(...)`.
            if rest.starts_with(":(") {
                let adverb_text =
                    balanced_paren_text(&rest[1..]).unwrap_or_else(|| "(...)".to_string());
                return Err(PError::fatal(format!(
                    "X::Syntax::Adverb: You can't adverb :{}",
                    adverb_text
                )));
            }
            // infix:<OP>(args) — operator reference
            if rest.starts_with(":<") || rest.starts_with(":<<") {
                let r = &rest[1..]; // skip ':'
                let (delim_start, delim_end) = if r.starts_with("<<") {
                    ("<<", ">>")
                } else {
                    ("<", ">")
                };
                let r = &r[delim_start.len()..];
                if let Some(end_pos) = r.find(delim_end) {
                    let op_name = &r[..end_pos];
                    let r = &r[end_pos + delim_end.len()..];
                    let full_name = Symbol::intern(&format!("{}:<{}>", name, op_name));
                    // Check if followed by (args)
                    let (r, _) = ws(r)?;
                    if let Some(r) = r.strip_prefix('(') {
                        let (r, _) = ws(r)?;
                        if let Some(r) = r.strip_prefix(')') {
                            return Ok((
                                r,
                                Expr::Call {
                                    name: full_name,
                                    args: vec![],
                                },
                            ));
                        }
                        let (r, args) = crate::parser::primary::parse_call_arg_list(r)?;
                        let (r, _) = ws(r)?;
                        let Some(r) = r.strip_prefix(')') else {
                            return Err(PError::expected("')'"));
                        };
                        return Ok((
                            r,
                            Expr::Call {
                                name: full_name,
                                args,
                            },
                        ));
                    }
                    return Ok((r, Expr::BareWord(full_name.resolve())));
                }
            }
        }
        "try" => {
            let (r, _) = ws(rest)?;
            if r.starts_with('{') {
                let (r, body) = parse_block_body(r)?;
                return Ok((r, Expr::Try { body, catch: None }));
            }
            // try STMT — wrap the following statement in try.
            // Check if it's a binding statement ($a := expr) which the
            // expression parser can't handle. Only try assign_stmt when
            // the input looks like a binding (contains := before ;).
            if looks_like_binding(r)
                && let Ok((r, stmt)) = crate::parser::stmt::assign_stmt(r)
            {
                return Ok((
                    r,
                    Expr::Try {
                        body: vec![stmt],
                        catch: None,
                    },
                ));
            }
            // Statement modifiers (for, if, etc.) bind outside try,
            // so we parse an expression, not a full statement.
            let (r, expr) = expression(r)?;
            return Ok((
                r,
                Expr::Try {
                    body: vec![Stmt::Expr(expr)],
                    catch: None,
                },
            ));
        }
        "do" => {
            let (r, _) = ws(rest)?;
            if r.starts_with('{') {
                let (r, body) = parse_block_body(r)?;
                let (r_ws, _) = ws(r)?;
                for kw in &["while", "until", "for", "given"] {
                    if keyword(kw, r_ws).is_some() {
                        return Err(PError::raw(
                            format!(
                                "X::Obsolete: Unsupported use of do...{kw}. In Raku please use: repeat...while or repeat...until."
                            ),
                            Some(r_ws.len()),
                        ));
                    }
                }
                return Ok((r, Expr::DoBlock { body, label: None }));
            }
            // do if/unless/given/for/while — wrap the control flow statement
            {
                let is_ctrl = |s: &str| {
                    for kw in &["if", "unless", "given", "for", "while", "until"] {
                        if s.starts_with(kw)
                            && !s.as_bytes().get(kw.len()).is_some_and(|&c| {
                                c.is_ascii_alphanumeric() || c == b'_' || c == b'-'
                            })
                        {
                            return true;
                        }
                    }
                    false
                };
                if is_ctrl(r)
                    && let Ok((r_after, stmt)) = crate::parser::stmt::statement_pub(r)
                {
                    let r_after = restore_do_stmt_terminator(r, r_after);
                    return Ok((r_after, Expr::DoStmt(Box::new(stmt))));
                }
            }
            // do STMT — wrap an assignment or other statement
            if let Ok((r_after, stmt)) = crate::parser::stmt::statement_pub(r) {
                let r_after = restore_do_stmt_terminator(r, r_after);
                return Ok((r_after, Expr::DoStmt(Box::new(stmt))));
            }
            // do EXPR — just evaluate the expression
            let (r, expr) = expression(r)?;
            return Ok((r, expr));
        }
        "if" => {
            // One-pass parsing rule: `if` is a control keyword only when followed by whitespace.
            // This allows user-defined `sub if` to be called as `if()` / `if;`.
            if let Ok((r, _)) = ws1(rest) {
                let _ = r;
                let (r, stmt) = crate::parser::stmt::if_stmt_pub(input)?;
                return Ok((r, Expr::DoStmt(Box::new(stmt))));
            }
        }
        "unless" => {
            if let Ok((r, _)) = ws1(rest) {
                let (r, cond) = expression(r)?;
                let (r, _) = ws(r)?;
                let (r, body) = parse_block_body(r)?;
                return Ok((
                    r,
                    Expr::DoStmt(Box::new(crate::ast::Stmt::If {
                        cond: Expr::Unary {
                            op: crate::token_kind::TokenKind::Bang,
                            expr: Box::new(cond),
                        },
                        then_branch: body,
                        else_branch: Vec::new(),
                        binding_var: None,
                    })),
                ));
            }
        }
        "for" => {
            if let Ok((r, stmt)) = crate::parser::stmt::for_stmt_pub(input) {
                return Ok((r, Expr::DoStmt(Box::new(stmt))));
            }
        }
        "while" => {
            if let Ok((r, stmt)) = crate::parser::stmt::while_stmt_pub(input) {
                return Ok((r, Expr::DoStmt(Box::new(stmt))));
            }
        }
        "until" => {
            if let Ok((r, stmt)) = crate::parser::stmt::until_stmt_pub(input) {
                return Ok((r, Expr::DoStmt(Box::new(stmt))));
            }
        }
        "loop" => {
            if let Ok((r, stmt)) = crate::parser::stmt::loop_stmt_pub(input) {
                return Ok((r, Expr::DoStmt(Box::new(stmt))));
            }
        }
        "my" | "our" | "state" => {
            // my/our/state declaration in expression context
            // e.g., (my $x = 5) or (state $x = 3)
            match crate::parser::stmt::my_decl_expr_pub(input) {
                Ok((r, stmt)) => return Ok((r, Expr::DoStmt(Box::new(stmt)))),
                Err(err) if err.is_fatal() => return Err(err),
                Err(_) => {
                    // `my grammar { ... }` / `my class { ... }` / `my role { ... }`:
                    // an *anonymous* lexical package declarator used as an
                    // expression (e.g. `my grammar { ... }.parse: $s`). The named
                    // declarator parser above can't parse the no-name form, so
                    // delegate to the anonymous-package expression parsers on the
                    // text after the `my`/`our`/`state` keyword.
                    if let Some(after_kw) = keyword("my", input)
                        .or_else(|| keyword("our", input))
                        .or_else(|| keyword("state", input))
                        && let Ok((after_kw, _)) = ws1(after_kw)
                        && let Ok(res) = anon_grammar_expr(after_kw)
                            .or_else(|_| anon_class_expr(after_kw))
                            .or_else(|_| anon_role_expr(after_kw))
                    {
                        return Ok(res);
                    }
                }
            }
        }
        "constant" => {
            // constant declaration in expression context
            if let Ok((r, stmt)) = crate::parser::stmt::constant_decl_pub(input) {
                return Ok((r, Expr::DoStmt(Box::new(stmt))));
            }
        }
        "enum" => {
            // enum in expression context: enum < ... > or enum :: < ... >
            if let Ok((r, stmt)) = crate::parser::stmt::decl::enum_decl(input) {
                return Ok((r, Expr::DoStmt(Box::new(stmt))));
            }
        }
        "anon" => {
            // anon enum < ... > in expression context
            if let Ok((r, stmt)) = crate::parser::stmt::decl::anon_enum_decl(input) {
                return Ok((r, Expr::DoStmt(Box::new(stmt))));
            }
            // anon method (...) { ... } in expression context
            let (r_ws, _) = ws(rest)?;
            if let Some(after_method) = keyword("method", r_ws) {
                let (r, _) = ws(after_method)?;
                if r.starts_with('(') {
                    let (r, params_body) = parse_anon_sub_with_params(r).map_err(|err| PError {
                        messages: merge_expected_messages(
                            "expected anonymous method parameter list/body",
                            &err.messages,
                        ),
                        remaining_len: err.remaining_len.or(Some(r.len())),
                        exception: None,
                    })?;
                    return Ok((r, params_body));
                }
                // anon method name (...) { ... } or anon method name { ... }
                if let Ok((r, _name)) = crate::parser::parse_result::take_while1(r, |c: char| {
                    c.is_alphanumeric() || c == '_' || c == '-'
                }) {
                    let (r, _) = ws(r)?;
                    if r.starts_with('(') {
                        let (r, params_body) =
                            parse_anon_sub_with_params(r).map_err(|err| PError {
                                messages: merge_expected_messages(
                                    "expected anonymous method parameter list/body",
                                    &err.messages,
                                ),
                                remaining_len: err.remaining_len.or(Some(r.len())),
                                exception: None,
                            })?;
                        return Ok((r, params_body));
                    }
                    if r.starts_with('{') {
                        let (r, body) = parse_block_body(r)?;
                        return Ok((r, make_anon_sub(body)));
                    }
                }
                if after_method.starts_with('{') || after_method.trim_start().starts_with('{') {
                    let (r, _) = ws(after_method)?;
                    let (r, body) = parse_block_body(r)?;
                    return Ok((r, make_anon_sub(body)));
                }
            }
            // anon [Type] sub { ... } — anonymous sub with return type
            if let Some(after_sub) = keyword("sub", r_ws) {
                // `anon sub { }` — no return type
                let (r_sub, _) = ws(after_sub)?;
                if r_sub.starts_with('{') {
                    let (r, body) = parse_block_body(r_sub)?;
                    return Ok((
                        r,
                        Expr::AnonSub {
                            body,
                            is_rw: false,
                            is_block: false,
                        },
                    ));
                }
                if r_sub.starts_with('(') {
                    let (r, params_body) =
                        parse_anon_sub_with_params(r_sub).map_err(|err| PError {
                            messages: merge_expected_messages(
                                "expected anonymous sub parameter list/body",
                                &err.messages,
                            ),
                            remaining_len: err.remaining_len.or(Some(r_sub.len())),
                            exception: None,
                        })?;
                    return Ok((r, params_body));
                }
            } else if let Ok((r_type, type_name)) =
                crate::parser::parse_result::take_while1(r_ws, |c: char| {
                    c.is_alphanumeric() || c == ':' || c == '_'
                })
            {
                // anon Str sub { ... } — with return type
                let (r_type, _) = ws(r_type)?;
                if let Some(after_sub) = keyword("sub", r_type) {
                    let (r_sub, _) = ws(after_sub)?;
                    if r_sub.starts_with('{') {
                        let (r, body) = parse_block_body(r_sub)?;
                        return Ok((
                            r,
                            Expr::AnonSubParams {
                                params: Vec::new(),
                                param_defs: Vec::new(),
                                return_type: Some(type_name.to_string()),
                                body,
                                is_rw: false,
                                is_whatever_code: false,
                            },
                        ));
                    }
                    if r_sub.starts_with('(') {
                        let (r, mut params_body) =
                            parse_anon_sub_with_params(r_sub).map_err(|err| PError {
                                messages: merge_expected_messages(
                                    "expected anonymous sub parameter list/body",
                                    &err.messages,
                                ),
                                remaining_len: err.remaining_len.or(Some(r_sub.len())),
                                exception: None,
                            })?;
                        // Wrap with return type info
                        if let Expr::AnonSubParams {
                            ref mut return_type,
                            ..
                        } = params_body
                            && return_type.is_none()
                        {
                            *return_type = Some(type_name.to_string());
                        }
                        return Ok((r, params_body));
                    }
                }
            }
        }
        "sub" => {
            let (r, _) = ws(rest)?;
            if r.starts_with('{') {
                let (r, body) = parse_block_body(r)?;
                // `sub { }` is a routine boundary (unlike bare blocks)
                return Ok((
                    r,
                    Expr::AnonSub {
                        body,
                        is_rw: false,
                        is_block: false,
                    },
                ));
            }
            if let Ok((r_named, _name)) = crate::parser::stmt::parse_sub_name_pub(r) {
                let (r_named, _) = ws(r_named)?;
                if r_named.starts_with("is ")
                    || r_named.starts_with("returns ")
                    || r_named.starts_with("of ")
                {
                    let (r_named, traits) = crate::parser::stmt::parse_sub_traits_pub(r_named)?;
                    let (r_named, body) = parse_block_body(r_named)?;
                    let mut expr = Expr::AnonSub {
                        body,
                        is_rw: traits.is_rw,
                        is_block: false,
                    };
                    if traits.is_rw {
                        expr = set_anon_sub_rw(expr, true);
                    }
                    return Ok((r_named, expr));
                }
                if r_named.starts_with('{') {
                    let (r_named, body) = parse_block_body(r_named)?;
                    return Ok((
                        r_named,
                        Expr::AnonSub {
                            body,
                            is_rw: false,
                            is_block: false,
                        },
                    ));
                }
                if r_named.starts_with('(') {
                    let (r_named, params_body) =
                        parse_anon_sub_with_params(r_named).map_err(|err| PError {
                            messages: merge_expected_messages(
                                "expected anonymous sub parameter list/body",
                                &err.messages,
                            ),
                            remaining_len: err.remaining_len.or(Some(r_named.len())),
                            exception: None,
                        })?;
                    return Ok((r_named, params_body));
                }
            }
            if r.starts_with("is ") || r.starts_with("returns ") || r.starts_with("of ") {
                let (r, traits) = crate::parser::stmt::parse_sub_traits_pub(r)?;
                let (r, body) = parse_block_body(r)?;
                let mut expr = Expr::AnonSub {
                    body,
                    is_rw: traits.is_rw,
                    is_block: false,
                };
                if traits.is_rw {
                    expr = set_anon_sub_rw(expr, true);
                }
                return Ok((r, expr));
            }
            // sub with params: sub ($x, $y) { ... }
            if r.starts_with('(') {
                match parse_anon_sub_with_params(r) {
                    Ok((r2, params_body)) => return Ok((r2, params_body)),
                    Err(err) => {
                        // `sub(` without whitespace can be a call to a routine named `sub`.
                        // Keep anonymous-sub diagnostics for `sub (...)` forms, but fall through
                        // for tight-call syntax so generic call parsing can handle it.
                        if !rest.starts_with('(') {
                            return Err(PError {
                                messages: merge_expected_messages(
                                    "expected anonymous sub parameter list/body",
                                    &err.messages,
                                ),
                                remaining_len: err.remaining_len.or(Some(r.len())),
                                exception: None,
                            });
                        }
                    }
                }
            }
            // sub not followed by { or ( in expression context — not valid.
            if rest.starts_with('(') {
                // Let generic identifier/call parsing handle `sub(...)` call syntax.
            } else if crate::parser::stmt::parse_sub_name_pub(r).is_err() {
                // A bare anonymous `sub` with neither a signature nor a block is a
                // missing block (`for () { sub }` -> X::Syntax::Missing, what =>
                // 'block'). A *named* forward sub without a body (`sub foo;`) is a
                // separate X::UnitScope::Invalid handled by the statement path, so
                // only fire here for the anonymous case.
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("what".to_string(), Value::str("block".to_string()));
                attrs.insert(
                    "message".to_string(),
                    Value::str("Missing block".to_string()),
                );
                let ex = Value::make_instance(Symbol::intern("X::Syntax::Missing"), attrs);
                return Err(PError::fatal_with_exception(
                    "X::Syntax::Missing: Missing block".to_string(),
                    Box::new(ex),
                ));
            } else {
                return Err(PError::expected_at("anonymous sub body '{' or '('", r));
            }
        }
        "multi" => {
            // multi sub name(...) { ... } in expression context:
            // Parse as a sub declaration and wrap it so the current candidate
            // is returned as a value (just that candidate, not the full multi).
            let (r, _) = ws(rest)?;
            // Only handle `multi sub` in expression context.
            // Use sub_decl_with_semicolon_mode directly to avoid infinite recursion
            // (statement_pub -> expr_stmt -> expression -> "multi" -> statement_pub).
            if keyword("sub", r).is_some() {
                if let Ok((r2, stmt)) =
                    crate::parser::stmt::sub_decl_with_semicolon_mode_pub(input, false)
                    && let Stmt::SubDecl {
                        ref params,
                        ref param_defs,
                        ref body,
                        is_rw,
                        ..
                    } = stmt
                {
                    // Create an anonymous sub expression representing just this candidate
                    let anon_sub = Expr::AnonSubParams {
                        params: params.clone(),
                        param_defs: param_defs.clone(),
                        return_type: None,
                        body: body.clone(),
                        is_rw,
                        is_whatever_code: false,
                    };
                    return Ok((
                        r2,
                        Expr::DoBlock {
                            body: vec![stmt, Stmt::Expr(anon_sub)],
                            label: None,
                        },
                    ));
                }
                // Check for anonymous multi sub
                let after_sub = keyword("sub", r).unwrap();
                let after_sub_ws = ws(after_sub).map(|(r2, _)| r2).unwrap_or(after_sub);
                if after_sub_ws.starts_with('{') || after_sub_ws.starts_with('(') {
                    return Err(PError::fatal(
                        "FATAL:X::Anon::Multi: An anonymous routine may not take a multi declarator"
                            .to_string(),
                    ));
                }
            }
            // multi { } — anonymous multi with block, always invalid
            if r.starts_with('{') {
                return Err(PError::fatal(
                    "FATAL:X::Anon::Multi: An anonymous routine may not take a multi declarator"
                        .to_string(),
                ));
            }
            // multi(...) could be a function call if `sub multi` was defined;
            // don't emit fatal — fall through to normal call parsing.
        }
        "method" => {
            // Anonymous method in expression context: method () { ... } or method { ... }
            let (r, _) = ws(rest)?;
            if r.starts_with('(') {
                // Try parsing as anonymous method. If it fails (e.g. no block
                // after params), fall through to treat `method` as a regular
                // function call (for cases like `sub method {}; method();`).
                if let Ok((r, params_body)) = parse_anon_method_with_params(r) {
                    return Ok((r, params_body));
                }
            } else if r.starts_with('{') {
                let (r, body) = parse_block_body(r)?;
                return Ok((r, make_anon_method(body)));
            }
        }
        "token" | "regex" | "rule" => {
            // token/rule term literal: token { ... } / rule { ... }
            let (r, _) = ws(rest)?;
            if r.starts_with('{') {
                let (r, pat) = parse_raw_braced_regex_body(r)?;
                return Ok((r, Expr::Literal(Value::regex(pat))));
            }
        }
        "gather" => {
            let (r, _) = ws(rest)?;
            if r.starts_with('{') {
                let (r, body) = parse_block_body(r)?;
                return Ok((r, Expr::Gather(body)));
            }
            // gather <statement> (e.g. `gather for ^5 { ... }`)
            if let Ok((r, stmt)) = crate::parser::stmt::statement_pub(r) {
                return Ok((r, Expr::Gather(vec![stmt])));
            }
        }
        "die" | "fail" => {
            let (r, _) = ws(rest)?;
            let sym_name = Symbol::intern(&name);
            // die/fail with no argument
            if r.starts_with(';') || r.is_empty() || r.starts_with('}') || r.starts_with(')') {
                return Ok((
                    r,
                    Expr::Call {
                        name: sym_name,
                        args: vec![],
                    },
                ));
            }
            let (r, arg) = expression(r)?;
            return Ok((
                r,
                Expr::Call {
                    name: sym_name,
                    args: vec![arg],
                },
            ));
        }
        "quietly" => {
            let (r, _) = ws(rest)?;
            // `quietly EXPR` — pass the raw expression so the compiler can run it
            // INLINE in the current scope under a warning-suppression frame
            // (so `quietly my $x = ...` leaks `$x`, matching Raku). A block form
            // `quietly { ... }` arrives as an AnonSub and is handled there too.
            let (r, expr) = expression(r)?;
            return Ok((
                r,
                Expr::Call {
                    name: Symbol::intern(&name),
                    args: vec![expr],
                },
            ));
        }
        "sink" => {
            let (r, _) = ws(rest)?;
            // sink expr — evaluate expression and discard result
            let (r, expr) = expression(r)?;
            return Ok((
                r,
                Expr::Call {
                    name: Symbol::intern(&name),
                    args: vec![expr],
                },
            ));
        }
        "list" | "cache" if !rest.starts_with('(') => {
            let (r, _) = ws(rest)?;
            // list/cache take a comma expression as a single argument
            // e.g. `list 1,2,4...16` → `list((1,2,4)...16)`
            let (r, expr) = crate::parser::stmt::assign::parse_comma_or_expr(r)?;
            let args = match expr {
                Expr::ArrayLiteral(items) => items,
                other => vec![other],
            };
            return Ok((
                r,
                Expr::Call {
                    name: Symbol::intern(&name),
                    args,
                },
            ));
        }
        "start" => {
            let (r, _) = ws(rest)?;
            if r.starts_with('{') {
                let (r, body) = parse_block_body(r)?;
                return Ok((
                    r,
                    Expr::Call {
                        name: Symbol::intern("start"),
                        args: vec![make_anon_sub(body)],
                    },
                ));
            }
            // start <statement> — wrap the next statement in a block
            if let Ok((r, stmt)) = crate::parser::stmt::statement_pub(r) {
                return Ok((
                    r,
                    Expr::Call {
                        name: Symbol::intern("start"),
                        args: vec![make_anon_sub(vec![stmt])],
                    },
                ));
            }
        }
        "require" => {
            return parse_require_expr(input, rest);
        }
        "last" => {
            if rest.trim_start().starts_with("=>") {
                return Ok((rest, Expr::BareWord(name)));
            }
            return Ok((
                rest,
                Expr::ControlFlow {
                    kind: crate::ast::ControlFlowKind::Last,
                    label: None,
                },
            ));
        }
        "next" => {
            if rest.trim_start().starts_with("=>") {
                return Ok((rest, Expr::BareWord(name)));
            }
            return Ok((
                rest,
                Expr::ControlFlow {
                    kind: crate::ast::ControlFlowKind::Next,
                    label: None,
                },
            ));
        }
        "redo" => {
            if rest.trim_start().starts_with("=>") {
                return Ok((rest, Expr::BareWord(name)));
            }
            return Ok((
                rest,
                Expr::ControlFlow {
                    kind: crate::ast::ControlFlowKind::Redo,
                    label: None,
                },
            ));
        }
        _ => {}
    }

    if matches!(name.as_str(), "R" | "S") && rest.starts_with("??") {
        return Err(PError::expected("expression"));
    }

    // Labeled loop in expression context, e.g. `MEOW: for ^10 { ... }`
    if is_loop_label_name(&name) {
        let (r_ws, _) = ws(rest)?;
        if r_ws.starts_with(':')
            && let Ok((r, stmt)) = crate::parser::stmt::labeled_loop_stmt_pub(input)
        {
            return Ok((r, Expr::DoStmt(Box::new(stmt))));
        }
    }

    // Check for :: qualified name (e.g. Foo::Bar, CORE::<&run>)
    let (rest, name) = {
        let mut full_name = name;
        let mut r = rest;
        while r.starts_with("::") {
            let after = &r[2..];
            if let Some(after_brace) = after.strip_prefix('{') {
                let (r2, _) = ws(after_brace)?;
                let (r2, key_expr) = expression(r2)?;
                let (r2, _) = ws(r2)?;
                let (r2, _) = parse_char(r2, '}')?;
                let stash_name = format!("{full_name}::");
                return Ok((
                    r2,
                    Expr::Index {
                        target: Box::new(Expr::PseudoStash(stash_name)),
                        index: Box::new(key_expr),
                        is_positional: false,
                    },
                ));
            }
            // Handle ::<SYMBOL> subscript syntax (e.g., CORE::<&run>)
            if let Some(after_bracket) = after.strip_prefix('<')
                && let Some(end) = after_bracket.find('>')
            {
                let symbol = &after_bracket[..end];
                if matches!(symbol.chars().next(), Some('$' | '@' | '%' | '&')) {
                    let stash_name = format!("{full_name}::");
                    return Ok((
                        &after_bracket[end + 1..],
                        Expr::Index {
                            target: Box::new(Expr::PseudoStash(stash_name)),
                            index: Box::new(Expr::Literal(Value::str(symbol.to_string()))),
                            is_positional: false,
                        },
                    ));
                }
                full_name.push_str("::");
                full_name.push_str(symbol);
                r = &after_bracket[end + 1..];
                continue;
            }
            if let Ok((rest2, part)) = crate::parser::stmt::parse_sub_name_pub(after) {
                full_name.push_str("::");
                full_name.push_str(&part);
                r = rest2;
            } else if after.starts_with('.')
                || after.is_empty()
                || after.starts_with(';')
                || after.starts_with(')')
                || after.starts_with(',')
                || after.starts_with(' ')
                || after.starts_with('\n')
                || after.starts_with('\r')
                || after.starts_with('\t')
                || after.starts_with('}')
            {
                // Trailing `::` stash lookup form (e.g. `A::`, `MY::`).
                // Pseudo packages remain supported, and ordinary package stashes are accepted
                // for parse compatibility.
                let mut stash_name = full_name.clone();
                stash_name.push_str("::");
                return Ok((after, Expr::PseudoStash(stash_name)));
            } else {
                return Err(PError::expected_at("identifier after '::'", after));
            }
        }
        // Type smileys: TypeName:U, TypeName:D, TypeName:_
        if (r.starts_with(":D") || r.starts_with(":U") || r.starts_with(":_"))
            && !r[2..].starts_with('<')
            && full_name
                .chars()
                .next()
                .is_some_and(|c| c.is_ascii_uppercase())
        {
            let smiley = &r[..2];
            full_name.push_str(smiley);
            r = &r[2..];
        }
        (r, full_name)
    };

    if matches!(name.as_str(), "qx" | "qqx")
        && rest.starts_with('=')
        && let Ok(parsed) = crate::parser::primary::string::qx_string(input)
    {
        return Ok(parsed);
    }

    // Check if followed by `(` for function call (including degenerate unspace: `foo\(42)`)
    let rest_unspaced = consume_unspace(rest);
    if rest_unspaced.starts_with('(') {
        let (rest, _) = parse_char(rest_unspaced, '(')?;
        let (rest, _) = ws(rest)?;
        // Try invocant colon syntax: foo($obj:) or foo($obj: $a, $b)
        // Parse first arg, then check for ':'
        if let Ok((r_first, first_arg)) = expression(rest) {
            let (r_ws, _) = ws(r_first)?;
            // Check for invocant colon: foo($obj:) or foo($obj: args)
            // Must NOT confuse with adjacent colonpairs like foo(:a :b :c)
            // where `:b` is a colonpair, not invocant syntax.
            let is_invocant_colon =
                r_ws.starts_with(':') && !r_ws.starts_with("::") && r_ws.len() > 1 && {
                    let after = r_ws.as_bytes()[1];
                    // Colonpair starts: identifier char, !, $, @, %, &, digit
                    !(after.is_ascii_alphabetic()
                        || after == b'_'
                        || after == b'!'
                        || after == b'$'
                        || after == b'@'
                        || after == b'%'
                        || after == b'&'
                        || after.is_ascii_digit())
                };
            if is_invocant_colon {
                let after_colon = &r_ws[1..];
                let (after_ws, _) = ws(after_colon)?;
                if after_ws.starts_with(')') {
                    // foo($obj:) → $obj.foo()
                    let (rest, _) = parse_char(after_ws, ')')?;
                    let sym_name = Symbol::intern(&name);
                    return Ok((
                        rest,
                        Expr::MethodCall {
                            target: Box::new(first_arg),
                            name: sym_name,
                            args: vec![],
                            modifier: None,
                            quoted: false,
                        },
                    ));
                }
                // foo($obj: $a, $b) → $obj.foo($a, $b)
                if let Some(after_comma) = after_ws.strip_prefix(',') {
                    let (after_ws2, _) = ws(after_comma)?;
                    let (rest, method_args) =
                        crate::parser::primary::parse_call_arg_list(after_ws2)?;
                    let (rest, _) = ws(rest)?;
                    let (rest, _) = parse_char(rest, ')')?;
                    return Ok((
                        rest,
                        Expr::MethodCall {
                            target: Box::new(first_arg),
                            name: Symbol::intern(&name),
                            args: method_args,
                            modifier: None,
                            quoted: false,
                        },
                    ));
                }
                // Invocant colon followed by more args (without comma separator)
                let (rest, method_args) = crate::parser::primary::parse_call_arg_list(after_ws)?;
                let (rest, _) = ws(rest)?;
                let (rest, _) = parse_char(rest, ')')?;
                return Ok((
                    rest,
                    Expr::MethodCall {
                        target: Box::new(first_arg),
                        name: Symbol::intern(&name),
                        args: method_args,
                        modifier: None,
                        quoted: false,
                    },
                ));
            }
        }
        let (rest, args) = crate::parser::primary::parse_call_arg_list(rest)?;
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        let mut args = args;
        if args.is_empty() {
            args.push(Expr::Binary {
                left: Box::new(Expr::Literal(Value::str(
                    TEST_CALLSITE_LINE_KEY.to_string(),
                ))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(Expr::Literal(Value::Int(current_line_number(input)))),
            });
        }
        return Ok((rest, make_call_expr(name, input, args)));
    }

    // Identifier immediately followed by a quote character (no whitespace) is a syntax
    // error in Raku: "Two terms in a row".  e.g., `foo'bar'` or `foo"bar"`.
    // The only valid forms are `foo(...)` (handled above) or `foo 'bar'` (with space).
    if rest.starts_with('\'') || rest.starts_with('"') {
        return Err(PError::fatal(
            "X::Syntax::Confused: Two terms in a row".to_string(),
        ));
    }

    // Identifier immediately followed by single `:` and alphabetic char (no space):
    // `foo:bar` is a package-qualified long name, not a function call with adverb.
    // Raku treats this as an undeclared symbol.
    if rest.starts_with(':')
        && !rest.starts_with("::")
        && rest.len() > 1
        && rest.as_bytes()[1].is_ascii_alphabetic()
    {
        // Consume the `:name` part to form the full long name
        let adverb_rest = &rest[1..];
        let end = adverb_rest
            .find(|c: char| !c.is_ascii_alphanumeric() && c != '-' && c != '_')
            .unwrap_or(adverb_rest.len());
        let long_name = format!("{}:{}", name, &adverb_rest[..end]);
        return Err(PError::fatal(format!(
            "X::Undeclared::Symbols: Undeclared routine:\n    {} used at line 1",
            long_name
        )));
    }

    // Bareword followed by => — Pair constructor (higher precedence than operators)
    // e.g., b => "foo" should parse as Pair even in `%a ~~ b => "foo"`
    let (r, _) = ws(rest)?;
    // If ws() consumed unspace (e.g., `foo\ .lc` → r = ".lc"), record this so
    // we can prevent parsing `.lc` as a listop argument below.
    let ws_consumed_unspace = rest.starts_with('\\') && !std::ptr::eq(r, rest);
    // Unspace is not allowed within an identifier: `fo\ o` is an error.
    // If unspace was consumed and the next character starts an identifier,
    // that means someone wrote `ident\ ident` which is forbidden.
    if ws_consumed_unspace && r.chars().next().is_some_and(is_raku_identifier_start) {
        return Err(PError::fatal(
            "X::Comp: Unspace is not allowed in the middle of an identifier".to_string(),
        ));
    }
    if r.starts_with("=>") && !r.starts_with("==>") {
        let r2 = &r[2..];
        let (r2, _) = ws(r2)?;
        // Use parse_fat_arrow_value for right-associative chaining:
        // a => b => c parses as a => (b => c)
        let (r2, value) = parse_fat_arrow_value(r2)?;
        return Ok((
            r2,
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::str(name))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(value),
            },
        ));
    }

    // Bareword followed by block { ... } and comma/colon — function call with block arg
    // e.g., map { $_ * 2 }, @arr  or  map { $_ * 2 }: @arr
    if r.starts_with('{')
        && !is_keyword(&name)
        && let Ok((r2, block_body)) = parse_block_body(r)
    {
        if name == "BEGIN" {
            return Ok((
                r2,
                make_call_expr(name, input, vec![make_anon_sub(block_body)]),
            ));
        }
        let (r3, _) = ws(r2)?;
        // Also consume unspace before separator: `{ block }\ : args`
        let r3_unspaced = consume_unspace(r3);
        let is_comma = r3_unspaced.starts_with(',');
        let is_colon = r3_unspaced.starts_with(':') && !r3_unspaced.starts_with("::");
        if is_comma || is_colon {
            let r3 = &r3_unspaced[1..];
            let (r3, _) = ws(r3)?;
            let block_expr = make_anon_sub(block_body);
            let mut method_args = Vec::new();
            let (mut r3, first_arg) = expression(r3)?;
            method_args.push(first_arg);
            loop {
                let (r4, _) = ws(r3)?;
                if !r4.starts_with(',') {
                    break;
                }
                let r4 = &r4[1..];
                let (r4, _) = ws(r4)?;
                if r4.starts_with(';') || r4.is_empty() || r4.starts_with('}') {
                    r3 = r4;
                    break;
                }
                let (r4, next_arg) = expression(r4)?;
                method_args.push(next_arg);
                r3 = r4;
            }
            if is_colon {
                // Invocant colon: `name { block }: args` → `{ block }.name(args)`
                let sym_name = Symbol::intern(&name);
                return Ok((
                    r3,
                    Expr::MethodCall {
                        target: Box::new(block_expr),
                        name: sym_name,
                        args: method_args,
                        modifier: None,
                        quoted: false,
                    },
                ));
            }
            // Comma separator: `name { block }, args` → `name(block, args)`
            let mut args = vec![block_expr];
            args.extend(method_args);
            return Ok((r3, make_call_expr(name, input, args)));
        }
        // Block without trailing comma — return as separate expressions
        // Fall through to BareWord
    }

    // BEGIN/CHECK/INIT as statement prefix without braces in expression context:
    // e.g., `my $x = BEGIN uc 'moin'` should parse as `my $x = BEGIN(uc('moin'))`
    if matches!(name.as_str(), "BEGIN" | "CHECK" | "INIT") && !r.starts_with('{') {
        let (r2, expr) = expression(r)?;
        return Ok((r2, make_call_expr(name, input, vec![expr])));
    }

    // `supply { ... }` expression: lower to `Supply.on-demand(-> $emitter { ... })`.
    if name == "supply"
        && r.starts_with('{')
        && let Ok((r2, block_body)) = parse_block_body(r)
    {
        return Ok((r2, supply_method_call(block_body)));
    }

    // set/bag/mix followed immediately by < (no whitespace) is a parse error
    if matches!(name.as_str(), "set" | "bag" | "mix")
        && rest.starts_with('<')
        && std::ptr::eq(rest, r)
    {
        let msg = format!(
            "Use of non-subscript brackets after \"{}\" where postfix is expected; \
             please use whitespace before any arguments",
            name
        );
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        let exception = Value::make_instance(Symbol::intern("X::Syntax::Confused"), attrs);
        return Err(PError::fatal_with_exception(msg, Box::new(exception)));
    }

    // Check for listop: bareword followed by space and argument (but not statement modifier)
    // e.g., shift @a, push @a, 42, etc.
    // Skip when directly followed by '.' — `func.method` is `(func()).method`,
    // but `func .method` is a listop call with topic-method-call argument.
    if is_listop(&name)
        && !r.is_empty()
        && !r.starts_with(';')
        && !r.starts_with('}')
        && !r.starts_with(')')
        && !r.starts_with(']')
        && !r.starts_with(',')
        && !rest.starts_with('.')
        && !rest.starts_with('[')
        && !(ws_consumed_unspace && r.starts_with('.') && !r.starts_with(".."))
        && !is_unspace_before_postfix(r)
    {
        // Check if next token is a statement modifier keyword
        if !is_stmt_modifier_ahead(r) {
            // Expression listops (ok, is, diag, etc.) parse full expressions as args
            if is_expr_listop(&name) {
                return parse_expr_listop_args(r, name);
            }
            // Try to parse an argument.
            // List operators (grep, map, sort, etc.) parse full expressions as args
            // (up to comma/feed precedence), matching Raku's "list prefix" semantics.
            // e.g. `grep $_ == 1, 1, 2, 3` → `grep(($_ == 1), 1, 2, 3)`.
            let parse_arg = |input| {
                if is_stmt_modifier_ahead(input) {
                    return Err(PError::expected("listop argument"));
                }
                parse_listop_arg(input)
            };
            let (r2, arg) = parse_arg(r).map_err(|err| PError {
                messages: merge_expected_messages(
                    "expected listop argument expression",
                    &err.messages,
                ),
                remaining_len: err.remaining_len.or(Some(r.len())),
                exception: None,
            })?;
            let (r2, invocant_colon_call) =
                try_parse_no_paren_invocant_colon_call(&name, arg.clone(), r2)?;
            if let Some(method_call) = invocant_colon_call {
                return Ok((r2, method_call));
            }
            let mut args = vec![arg];
            let mut rest_after = r2;
            loop {
                let (r3, _) = ws(rest_after)?;
                if !r3.starts_with(',') {
                    break;
                }
                let r3 = &r3[1..];
                let (r3, _) = ws(r3)?;
                if r3.starts_with(';')
                    || r3.starts_with('}')
                    || r3.starts_with(')')
                    || r3.is_empty()
                {
                    break;
                }
                let (r3, rest_arg) = parse_arg(r3)?;
                args.push(rest_arg);
                rest_after = r3;
            }
            return Ok((rest_after, make_call_expr(name, input, args)));
        }
    }

    // User-defined function call in listop style: bareword followed by space and a term
    // e.g., `äöü 17` or `my-func $x`
    // Exclude single uppercase letters (like Z, X, R) which are infix meta-operators.
    // Raku requires whitespace between an identifier and listop arguments.
    // `foo'bar'` is a syntax error (two terms in a row), not `foo('bar')`.
    // `foo:bar` is a package-qualified name, not `foo(:bar)`.
    if !is_keyword(&name)
        && !is_infix_word_op(&name)
        && !r.is_empty()
        && !r.starts_with(';')
        && !r.starts_with('}')
        && !r.starts_with(')')
        && !r.starts_with(']')
        && !r.starts_with(',')
        && !rest.starts_with('.')
        && !(ws_consumed_unspace && r.starts_with('.') && !r.starts_with(".."))
        && !is_stmt_modifier_ahead(r)
        && !std::ptr::eq(rest, r)
    {
        let is_user_sub = crate::parser::stmt::simple::is_user_declared_sub(&name);
        let is_user_prefix_sub = crate::parser::stmt::simple::is_user_declared_prefix_sub(&name);
        // Raku removed prefix/listop `int`; only `int(...)` should parse as the builtin.
        if name == "int" && !is_user_sub && !is_user_prefix_sub {
            return Ok((rest, Expr::BareWord(name)));
        }
        let is_imported_sub = crate::parser::stmt::simple::is_imported_function(&name);
        let call_name = if is_user_prefix_sub {
            format!("prefix:<{}>", name)
        } else {
            name.clone()
        };
        let next = r.chars().next().unwrap();
        let hyphen_forward_call = !is_user_sub && name.contains('-');
        if is_user_prefix_sub {
            if let Ok((r2, arg)) = expression_no_sequence(r) {
                return Ok((r2, make_call_expr(call_name.clone(), input, vec![arg])));
            }
        } else if is_user_sub
            && let Ok((r2, expr)) = make_call_expr_from_listop_args(r, input, call_name.clone())
        {
            return Ok((r2, expr));
        } else if is_imported_sub
            && let Ok((r2, expr)) = make_call_expr_from_listop_args(r, input, call_name.clone())
        {
            return Ok((r2, expr));
        }
        if hyphen_forward_call
            && let Ok((r2, expr)) = make_call_expr_from_listop_args(r, input, name.clone())
        {
            return Ok((r2, expr));
        }
        // Only trigger if next token starts a term (not an operator)
        if (next == '$'
            || next == '@'
            || next == '%'
            || next == '&'
            || next == ':'
            || next == '\''
            || next == '"'
            || next == '\u{2018}'
            || next == '\u{2019}'
            || next == '\u{201A}'
            || next == '\u{201C}'
            || next == '\u{201D}'
            || next == '\u{201E}'
            || next == '\u{FF62}'
            || next == '('
            || (next == '\\' && r.starts_with("\\("))
            || next.is_ascii_digit()
            || starts_with_term_keyword(r)
            || crate::parser::stmt::simple::match_user_declared_term_symbol(r).is_some()
            || hyphen_forward_call
            || is_user_sub
            || is_imported_sub
            || crate::parser::stmt::simple::is_known_call(&name))
            && let Ok((r2, arg)) = parse_listop_arg(r)
        {
            let (r2, invocant_colon_call) =
                try_parse_no_paren_invocant_colon_call(&name, arg.clone(), r2)?;
            if let Some(method_call) = invocant_colon_call {
                return Ok((r2, method_call));
            }
            // For user subs, collect comma-separated args
            if is_user_prefix_sub {
                let (r2, arg) = expression_no_sequence(r)?;
                return Ok((r2, make_call_expr(call_name, input, vec![arg])));
            }
            if is_user_sub || is_imported_sub || hyphen_forward_call {
                return make_call_expr_from_listop_args(r, input, call_name);
            }
            // Known builtin calls like `defined`, `elems` take a single arg
            // in expression context (no comma-separated multi-arg collection).
            if !is_imported_sub
                && !hyphen_forward_call
                && crate::parser::stmt::simple::is_known_call(&name)
            {
                return Ok((r2, make_call_expr(call_name, input, vec![arg])));
            }
            let mut args = vec![arg];
            let mut rest_after = r2;
            loop {
                let (r3, _) = ws(rest_after)?;
                if !r3.starts_with(',') {
                    break;
                }
                let r3 = &r3[1..];
                let (r3, _) = ws(r3)?;
                if r3.starts_with(';')
                    || r3.starts_with('}')
                    || r3.starts_with(')')
                    || r3.is_empty()
                {
                    break;
                }
                let (r3, next_arg) = parse_listop_arg(r3)?;
                args.push(next_arg);
                rest_after = r3;
            }
            return Ok((rest_after, make_call_expr(call_name, input, args)));
        }
    }

    let rest_trimmed = rest.trim_start();
    let is_terminator = rest_trimmed.starts_with(';')
        || rest_trimmed.starts_with('}')
        || rest_trimmed.starts_with(')')
        || rest_trimmed.starts_with(']')
        || is_stmt_modifier_ahead(rest_trimmed)
        || rest_trimmed.is_empty();
    // For zero-arg bare-word functions (e.g. slurp, set), a following '.'
    // means a method call on the result, so treat it as a terminator too.
    let is_terminator_or_dot = is_terminator || rest_trimmed.starts_with('.');

    // User-declared and imported subs can be called with no args as bare words
    // in statement position (e.g., `make-temp-dir;`).
    if (crate::parser::stmt::simple::is_user_declared_sub(&name)
        || crate::parser::stmt::simple::is_imported_function(&name))
        && is_terminator
    {
        let args = vec![Expr::Binary {
            left: Box::new(Expr::Literal(Value::str(
                TEST_CALLSITE_LINE_KEY.to_string(),
            ))),
            op: crate::token_kind::TokenKind::FatArrow,
            right: Box::new(Expr::Literal(Value::Int(current_line_number(input)))),
        }];
        return Ok((rest, make_call_expr(name, input, args)));
    }

    // Functions that can be called with no arguments as bare words
    if matches!(name.as_str(), "await" | "slip" | "slurp") && is_terminator_or_dot {
        return Ok((rest, make_call_expr(name, input, vec![])));
    }

    // set/bag/mix without arguments or parens is a parse error in Raku
    if matches!(name.as_str(), "set" | "bag" | "mix") && is_terminator {
        let msg = format!(
            "Function \"{}\" may not be called without arguments \
             (please use () or whitespace to denote arguments, \
             or &{} to refer to the function as a noun, \
             or use .{} if you meant to call it as a method on $_)",
            name, name, name
        );
        return Err(PError::fatal(msg));
    }

    // set/bag/mix followed by '.' is a zero-arg call (method chain on result)
    if matches!(name.as_str(), "set" | "bag" | "mix") && rest_trimmed.starts_with('.') {
        return Ok((rest, make_call_expr(name, input, vec![])));
    }

    // callframe and caller are term-like functions: always a zero-arg call
    if matches!(name.as_str(), "callframe" | "caller") {
        return Ok((rest, make_call_expr(name, input, vec![])));
    }

    // Method-like: .new, .elems etc. is handled at expression level
    Ok((rest, Expr::BareWord(name)))
}
