use super::super::super::expr::expression;
use super::super::super::helpers::{ws, ws1};
use super::super::super::parse_result::{PError, PResult};
use super::super::{ident, keyword, var_name};
use super::destructure::parse_destructuring_decl;
use super::helpers::{
    is_supported_variable_trait, parse_array_shape_suffix, parse_export_trait_tags,
    register_term_symbol_from_decl_name,
};
use super::my_decl_assign::my_decl_assign_or_default;
use super::my_decl_dispatch::try_keyword_dispatch;
use super::my_decl_helpers::{
    check_invalid_type_smiley, parse_optional_type_constraint, parse_sigilless_decl,
    parse_typed_constant, try_dot_twigil_attr,
};
use super::{parse_char, proto_decl};
use crate::ast::{Expr, Stmt};

use super::super::sub::parse_type_constraint_expr;
use super::super::sub_param::parse_of_type_constraint_chain;

/// State accumulated during `my_decl_inner` that is passed to the
/// assignment/binding continuation in `my_decl_assign`.
pub(super) struct MyDeclState {
    pub apply_modifier: bool,
    pub is_state: bool,
    pub is_our: bool,
    pub name: String,
    pub is_array: bool,
    pub is_hash: bool,
    pub is_code: bool,
    pub type_constraint: Option<String>,
    pub shape_dims: Option<Vec<Expr>>,
    #[allow(dead_code)]
    pub hash_key_constraint: Option<String>,
    pub has_dynamic_trait: bool,
    pub has_export_trait: bool,
    pub export_tags: Vec<String>,
    pub custom_traits: Vec<(String, Option<Expr>)>,
    pub will_leave_body: Option<Vec<Stmt>>,
    pub where_constraint: Option<Box<Expr>>,
}

/// Parse `my`, `our`, or `state` variable declaration.
pub(in crate::parser::stmt) fn my_decl(input: &str) -> PResult<'_, Stmt> {
    let (rest, stmt) = my_decl_inner(input, true)?;
    // If postfix ++ / -- follows, reject from statement-level parsing so the
    // expression parser can handle `state $i++` as PostfixOp(DoStmt(VarDecl)).
    if rest.starts_with("++") || rest.starts_with("--") {
        return Err(PError::expected(
            "statement (postfix operator on declarator requires expression context)",
        ));
    }
    Ok((rest, stmt))
}

/// Parse a `my`/`our`/`state` declaration in expression context (no statement modifier).
/// This avoids consuming semicolons via `parse_statement_modifier` when called from
/// within an expression (e.g., colon-arg: `@a.push: my \p = expr;`).
pub(in crate::parser::stmt) fn my_decl_expr(input: &str) -> PResult<'_, Stmt> {
    my_decl_inner(input, false)
}

pub(super) fn my_decl_inner(input: &str, apply_modifier: bool) -> PResult<'_, Stmt> {
    let is_state = keyword("state", input).is_some();
    let is_our = keyword("our", input).is_some();
    let rest = keyword("my", input)
        .or_else(|| keyword("our", input))
        .or_else(|| keyword("state", input))
        .ok_or_else(|| PError::expected("my/our/state declaration"))?;
    // If the keyword is immediately followed by `=>`, it's being used as a
    // pair key (e.g., `(my => 1)`). Refuse to parse it as a declaration so
    // that the caller can fall back to treating it as a bareword.
    {
        let after_ws = rest.trim_start();
        if after_ws.starts_with("=>") && !after_ws.starts_with("==>") {
            return Err(PError::expected("my/our/state declaration"));
        }
    }
    let (mut rest, _) = ws1(rest)?;

    // my enum Foo <...>
    // my Array enum Foo <...>  (typed enum — base type is accepted but currently ignored)
    if let Some(r) = keyword("enum", rest) {
        let (r, _) = ws1(r)?;
        return super::enum_decl::parse_enum_decl_body(r);
    }
    // Check for `my <Type> enum <Name> ...` (e.g., `my Array enum PageSizes «...»`)
    {
        let saved = rest;
        if let Ok((after_type, _type_name)) = ident(rest)
            && let Ok((after_ws, _)) = ws1(after_type)
            && let Some(r) = keyword("enum", after_ws)
            && let Ok((r, _)) = ws1(r)
        {
            return super::enum_decl::parse_enum_decl_body_with_type(r, Some(_type_name));
        }
        rest = saved;
    }

    // my/our proto ...
    if keyword("proto", rest).is_some() {
        // If proto is followed by a variable sigil (e.g., `my proto $!`),
        // treat it as a regular variable declaration, ignoring proto.
        let after_proto = keyword("proto", rest).unwrap();
        let (after_ws, _) = ws(after_proto)?;
        if after_ws.starts_with('$')
            || after_ws.starts_with('@')
            || after_ws.starts_with('%')
            || after_ws.starts_with('&')
        {
            rest = after_ws;
            // Fall through to normal variable parsing below
        } else {
            return proto_decl(rest);
        }
    }

    // Try keyword dispatch (typed routines, multi, sub, method, class, etc.)
    if let Some(result) = try_keyword_dispatch(rest, is_state, is_our, apply_modifier)? {
        return Ok(result);
    }

    // Sigilless variable: my \name = expr / my \name := expr / my \name ::= expr
    if let Some(r) = rest.strip_prefix('\\') {
        return parse_sigilless_decl(r, is_state, is_our, None, apply_modifier);
    }

    // Handle type capture: my ::a $a
    let (rest, type_constraint) = if rest.starts_with("::")
        && !rest.starts_with("::=")
        && rest[2..]
            .chars()
            .next()
            .is_some_and(|c| c.is_alphabetic() || c == '_')
    {
        let ident_start = &rest[2..];
        let end = ident_start
            .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
            .unwrap_or(ident_start.len());
        let capture_name = &ident_start[..end];
        let tc = format!("::{}", capture_name);
        let r = &ident_start[end..];
        let (r, _) = ws(r)?;
        (r, Some(tc))
    } else {
        (rest, None)
    };

    // Optional type constraint: my Int $x or my Str(Match) $x (coercion type)
    let (mut rest, mut type_constraint) = if type_constraint.is_some() {
        (rest, type_constraint)
    } else {
        parse_optional_type_constraint(rest)?
    };

    if let Some(tc) = type_constraint.take() {
        let (r, tc) =
            parse_of_type_constraint_chain(rest, tc).ok_or_else(|| PError::expected("type"))?;
        type_constraint = Some(tc);
        rest = r;
    }

    // Check for invalid type smileys (e.g. Int:foo)
    check_invalid_type_smiley(&type_constraint)?;

    // my <Type> constant <name> = <expr>
    if keyword("constant", rest).is_some() {
        return parse_typed_constant(rest, type_constraint, is_our, apply_modifier);
    }

    // Sigilless variable after type: my Int \name = expr / my Int \name := expr / ::=
    if let Some(r) = rest.strip_prefix('\\') {
        return parse_sigilless_decl(r, is_state, is_our, type_constraint, apply_modifier);
    }

    // Destructuring: my ($a, $b) = expr  or  my Int:D ($x = 5)
    if rest.starts_with('(') {
        return parse_destructuring_decl(rest, is_state, is_our, type_constraint);
    }

    // Parse variable
    let sigil = rest.as_bytes().first().copied().unwrap_or(0);
    let is_array = sigil == b'@';
    let is_hash = sigil == b'%';
    let is_code = sigil == b'&';

    // Handle `our $.name` / `my $.name` — class-level (shared) attributes.
    if let Some(result) = try_dot_twigil_attr(
        rest,
        sigil,
        is_array,
        is_hash,
        is_our,
        is_state,
        &type_constraint,
        apply_modifier,
    )? {
        return Ok(result);
    }

    let prefix = if is_array {
        "@"
    } else if is_hash {
        "%"
    } else if is_code {
        "&"
    } else {
        ""
    };

    let (rest, name) = if sigil == b'$' || is_array || is_hash || is_code {
        let (r, n) = var_name(rest)?;
        (r, format!("{}{}", prefix, n))
    } else {
        return Err(PError::fatal(
            "X::Syntax::Malformed: Malformed my variable (did you mean to declare a sigilless variable with \\?)".to_string(),
        ));
    };
    let term_decl_name = if sigil == b'$' {
        format!("${name}")
    } else {
        name.clone()
    };
    register_term_symbol_from_decl_name(&term_decl_name);

    let (rest, _) = ws(rest)?;

    // Parse hash key-type parameterization: %h{Str}, %h{Int}, ...
    let (rest, hash_key_constraint) = if is_hash
        && rest.starts_with('{')
        && !rest.starts_with("{{")
        && let Some(end) = rest.find('}')
    {
        let key_type = rest[1..end].trim().to_string();
        let after = &rest[end + 1..];
        let (after, _) = ws(after)?;
        (after, Some(key_type))
    } else {
        (rest, None)
    };
    let mut hash_key_constraint = hash_key_constraint;

    // Optional shaped-array declaration suffix: my @arr[2;2];
    let (rest, shape_dims) = if is_array && rest.starts_with('[') {
        let (rest, dims) = parse_array_shape_suffix(rest)?;
        let (rest, _) = ws(rest)?;
        (rest, Some(dims))
    } else {
        (rest, None)
    };

    // Parse variable traits and build the state
    let (rest, state) = parse_variable_traits(
        rest,
        apply_modifier,
        is_state,
        is_our,
        &name,
        is_array,
        is_hash,
        is_code,
        &mut type_constraint,
        shape_dims,
        &mut hash_key_constraint,
    )?;

    // Delegate to assignment/binding handler
    my_decl_assign_or_default(rest, state)
}

/// Parse variable traits (`is dynamic`, `is export`, `is Type`, etc.),
/// `will leave { ... }`, `of Type`, and `where` constraint.
/// Returns the rest of the input and the accumulated state.
#[allow(clippy::too_many_arguments)]
fn parse_variable_traits<'a>(
    input: &'a str,
    apply_modifier: bool,
    is_state: bool,
    is_our: bool,
    name: &str,
    is_array: bool,
    is_hash: bool,
    is_code: bool,
    type_constraint: &mut Option<String>,
    shape_dims: Option<Vec<Expr>>,
    hash_key_constraint: &mut Option<String>,
) -> PResult<'a, MyDeclState> {
    let mut has_dynamic_trait = name.starts_with('*')
        || name.starts_with("@*")
        || name.starts_with("%*")
        || name.starts_with("&*");
    let mut has_export_trait = false;
    let mut export_tags: Vec<String> = Vec::new();
    let mut custom_traits: Vec<(String, Option<Expr>)> = Vec::new();
    let mut first_type_trait: Option<String> = None;
    let mut rest = {
        let mut r = input;
        while let Some(after_is) = keyword("is", r) {
            let (r2, _) = ws1(after_is)?;
            let (r2, trait_name) = ident(r2)?;
            if trait_name == "readonly" {
                return Err(PError::fatal(
                    "X::Comp::Trait::Unknown: Unknown variable trait 'is readonly'".to_string(),
                ));
            }
            let is_builtin = is_supported_variable_trait(&trait_name);
            if is_hash && (trait_name == "Mix" || trait_name == "MixHash") {
                *type_constraint = Some(trait_name.clone());
            }
            if trait_name == "dynamic" {
                has_dynamic_trait = true;
            }
            if trait_name == "export" {
                if !is_our {
                    return Err(PError::fatal(
                        "X::Comp::Trait::Scope: Trait 'is export' not allowed on my-scoped variable"
                            .to_string(),
                    ));
                }
                has_export_trait = true;
                let (r3, tags) = parse_export_trait_tags(r2)?;
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
                r = r3;
                continue;
            }
            let (r2, _) = ws(r2)?;
            let is_buf_trait = matches!(
                trait_name.as_str(),
                "Buf"
                    | "Blob"
                    | "buf"
                    | "blob"
                    | "buf8"
                    | "buf16"
                    | "buf32"
                    | "buf64"
                    | "blob8"
                    | "blob16"
                    | "blob32"
                    | "blob64"
                    | "utf8"
            );
            let is_list_trait = is_array && trait_name == "List";
            let is_uppercase_start = trait_name
                .chars()
                .next()
                .is_some_and(|c| c.is_ascii_uppercase());
            if is_uppercase_start {
                if let Some(ref outer) = first_type_trait {
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert(
                        "outer".to_string(),
                        crate::value::Value::Package(crate::symbol::Symbol::intern(outer)),
                    );
                    attrs.insert(
                        "inner".to_string(),
                        crate::value::Value::Package(crate::symbol::Symbol::intern(&trait_name)),
                    );
                    let msg = format!(
                        "{} not allowed here; variable list already declared with type {}",
                        trait_name, outer
                    );
                    attrs.insert("message".to_string(), crate::value::Value::str(msg.clone()));
                    let ex = crate::value::Value::make_instance(
                        crate::symbol::Symbol::intern("X::Syntax::Variable::ConflictingTypes"),
                        attrs,
                    );
                    return Err(PError::fatal_with_exception(
                        format!("X::Syntax::Variable::ConflictingTypes: {}", msg),
                        Box::new(ex),
                    ));
                }
                first_type_trait = Some(trait_name.clone());
            }
            // Handle parameterized type traits like `is Set[Int]`, `is Foo[Int,Str]`
            let mut trait_name = trait_name;
            let r2 = if r2.starts_with('[') {
                // Find matching ']', respecting nested brackets
                let mut depth = 0usize;
                let mut end = 0;
                for (i, ch) in r2.char_indices() {
                    match ch {
                        '[' => depth += 1,
                        ']' => {
                            depth -= 1;
                            if depth == 0 {
                                end = i;
                                break;
                            }
                        }
                        _ => {}
                    }
                }
                if end > 0 {
                    let param_text = &r2[1..end]; // content inside [...]
                    trait_name = format!("{}[{}]", trait_name, param_text.trim());
                    let after = &r2[end + 1..];
                    let (after, _) = ws(after)?;
                    after
                } else {
                    r2
                }
            } else {
                r2
            };
            let include_in_traits = !is_builtin
                || trait_name == "default"
                || is_buf_trait
                || is_list_trait
                || is_uppercase_start;
            if let Some(r3) = r2.strip_prefix('(') {
                let (r3, _) = ws(r3)?;
                let (r3, trait_arg) = expression(r3)?;
                let (r3, _) = ws(r3)?;
                let (r3, _) = parse_char(r3, ')')?;
                let (r3, _) = ws(r3)?;
                if include_in_traits {
                    custom_traits.push((trait_name.clone(), Some(trait_arg)));
                }
                r = r3;
            } else {
                if include_in_traits {
                    custom_traits.push((trait_name.clone(), None));
                }
                r = r2;
            }
        }
        r
    };

    // Parse `will leave { ... }` trait
    let will_leave_body: Option<Vec<Stmt>> = if let Some(after_will) = keyword("will", rest) {
        let (r, _) = ws1(after_will)?;
        let (r, phaser_name) = ident(r)?;
        if phaser_name != "leave" {
            return Err(PError::expected("'leave' after 'will'"));
        }
        let (r, _) = ws(r)?;
        let (r, body) = super::super::block(r)?;
        rest = r;
        let (rest2, _) = ws(rest)?;
        rest = rest2;
        Some(body)
    } else {
        None
    };

    // Postfix container typing: my @a of Int; my %h of Int;
    if let Some(after_of) = keyword("of", rest) {
        let (r, _) = ws1(after_of)?;
        let (r, tc) = parse_type_constraint_expr(r).ok_or_else(|| PError::expected("type"))?;
        let (r, _) = ws(r)?;
        *type_constraint = Some(tc);
        rest = r;
    }
    // Parse `is` traits that come after `of`
    {
        let mut r = rest;
        while let Some(after_is) = keyword("is", r) {
            let (r2, _) = ws1(after_is)?;
            let (r2, trait_name) = ident(r2)?;
            let is_builtin = is_supported_variable_trait(&trait_name);
            let include_in_traits = !is_builtin || trait_name == "default";
            let (r2, _) = ws(r2)?;
            if let Some(r3) = r2.strip_prefix('(') {
                let (r3, _) = ws(r3)?;
                let (r3, trait_arg) = expression(r3)?;
                let (r3, _) = ws(r3)?;
                let (r3, _) = parse_char(r3, ')')?;
                let (r3, _) = ws(r3)?;
                if include_in_traits {
                    custom_traits.push((trait_name.clone(), Some(trait_arg)));
                }
                r = r3;
            } else {
                if include_in_traits {
                    custom_traits.push((trait_name.clone(), None));
                }
                r = r2;
            }
        }
        rest = r;
    }
    if is_hash {
        *type_constraint = match (type_constraint.take(), hash_key_constraint.take()) {
            (Some(value_tc), Some(key_tc)) => Some(format!("{}{{{}}}", value_tc, key_tc)),
            (None, Some(key_tc)) => Some(format!("Any{{{}}}", key_tc)),
            (value_tc, None) => value_tc,
        };
    }

    // Optional `where` constraint on variable: my $x where * > 0
    let where_constraint = if let Some(r) = keyword("where", rest) {
        let (r, _) = ws1(r)?;
        let (r, pred) = expression(r)?;
        let (r, _) = ws(r)?;
        rest = r;
        Some(Box::new(pred))
    } else {
        None
    };

    Ok((
        rest,
        MyDeclState {
            apply_modifier,
            is_state,
            is_our,
            name: name.to_string(),
            is_array,
            is_hash,
            is_code,
            type_constraint: type_constraint.clone(),
            shape_dims,
            hash_key_constraint: None,
            has_dynamic_trait,
            has_export_trait,
            export_tags,
            custom_traits,
            will_leave_body,
            where_constraint,
        },
    ))
}
