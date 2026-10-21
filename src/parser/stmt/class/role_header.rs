//! Role header clauses before their execution fields are separated.
use super::*;

pub(super) struct Header {
    pub parent_roles: Vec<(String, Option<Vec<Expr>>, bool)>,
    pub is_hidden_role: bool,
    pub role_is_rw: bool,
    pub is_export: bool,
    pub export_tags: Vec<String>,
    pub custom_traits: Vec<(String, Option<Expr>)>,
    pub header_order: Vec<crate::ast::RoleHeaderTrait>,
}

pub(super) fn parse<'a>(
    mut rest: &'a str,
    name: &str,
    type_params: &[String],
) -> PResult<'a, Header> {
    // (name, bracket args, came-from-`is`). The third element keeps the
    // declarator: `role R is Unknown` is a custom `trait_mod:<is>` trait,
    // `role R does Unknown` is a typo, and the runtime cannot tell them apart
    // once both are folded into the same synthetic `DoesDecl` (#8100).
    let mut parent_roles: Vec<(String, Option<Vec<Expr>>, bool)> = Vec::new();
    let mut is_hidden_role = false;
    let mut role_is_rw = false;
    let mut is_export = false;
    let mut export_tags: Vec<String> = Vec::new();
    let mut custom_traits: Vec<(String, Option<Expr>)> = Vec::new();
    let mut header_order = Vec::new();
    let mut parent_count = 0;

    // Optional parent/trait clauses in any order.
    loop {
        if let Some(r) = keyword("does", rest) {
            let (r, _) = ws1(r)?;
            let (r, role_name) = qualified_ident(r)?;
            let (r, _) = ws(r)?;
            let (r, bracket_suffix) = parse_optional_bracket_suffix(r)?;
            // A parameterised role may compose the unparameterised candidate
            // of its own role group (`role R[Int $n] does R`).  Only reject
            // the same candidate: an unparameterised declaration composing
            // its unparameterised self (`role R does R`) remains invalid.
            if role_name == name && type_params.is_empty() && bracket_suffix.is_empty() {
                return Err(PError::fatal(format!(
                    "X::InvalidType: role '{}' cannot compose itself",
                    name
                )));
            }
            let (r, _) = ws(r)?;
            let args = super::class_decl::parse_bracket_arg_exprs(bracket_suffix);
            parent_roles.push((format!("{}{}", role_name, bracket_suffix), args, false));
            header_order.push(crate::ast::RoleHeaderTrait::Parent(parent_count));
            parent_count += 1;
            rest = r;
            continue;
        }
        if let Some(r) = keyword("is", rest) {
            let (r, _) = ws1(r)?;
            // `is` may name an inherited type with a qualified name, as in
            // `role SubModelHOW is Metamodel::SubsetHOW`.  Reading only the
            // first identifier leaves `::SubsetHOW` in the input and turns
            // the real parent into an unrelated `Metamodel` trait/role.
            // Preserve the leading absolute-name edge for RakuAST. Runtime
            // parent resolution still excludes the pending role's own name
            // (`role_body_does_decl`, #11072).
            let absolute = r.starts_with("::");
            let (r, mut trait_name) = qualified_ident(r.strip_prefix("::").unwrap_or(r))?;
            if absolute {
                trait_name.insert_str(0, "::");
            }
            if trait_name == "hidden" {
                header_order.push(crate::ast::RoleHeaderTrait::Hidden);
                is_hidden_role = true;
                let r = skip_balanced_parens(r);
                let (r, _) = ws(r)?;
                rest = r;
            } else if trait_name == "rw" {
                header_order.push(crate::ast::RoleHeaderTrait::Rw);
                role_is_rw = true;
                let r = skip_balanced_parens(r);
                let (r, _) = ws(r)?;
                rest = r;
            } else if trait_name == "export" {
                header_order.push(crate::ast::RoleHeaderTrait::Export);
                // `is export` / `is export(:TAG, :TAG2)`: exactly as for a
                // class, a bare `is export` uses DEFAULT and a tagged form
                // declares each named tag (and is importable under `:ALL`).
                is_export = true;
                super::class_decl::push_export_tags(r, &mut export_tags);
                let r = skip_balanced_parens(r);
                let (r, _) = ws(r)?;
                rest = r;
            } else if matches!(
                trait_name.as_str(),
                "ok" | "required"
                    | "readonly"
                    | "repr"
                    | "default"
                    | "raw"
                    | "built"
                    | "copy"
                    | "DEPRECATED"
                    | "nodal"
                    | "pure"
            ) {
                // Known lowercase trait keywords are skipped (RoleDecl has no
                // dedicated `repr` field, unlike ClassDecl — this only makes
                // `is repr<...>`/`is repr(...)` parse, matching the
                // pre-existing paren-only behaviour of silently discarding
                // the value rather than newly wiring it into trait_mod:<is>
                // dispatch, which would misfire on unrelated user-defined
                // trait_mod:<is> candidates that don't accept a `:$repr`).
                let r = if r.starts_with('<') {
                    parse_trait_angle_arg(r)?.0
                } else {
                    skip_balanced_parens(r)
                };
                let (r, _) = ws(r)?;
                rest = r;
            } else {
                // Unknown trait (e.g. `ctype`, which has no dedicated field
                // on RoleDecl either) — record name + argument via
                // custom_traits, same as a class's generic `is` fallback.
                if r.starts_with('<') {
                    let (r2, arg) = parse_trait_angle_arg(r)?;
                    header_order.push(crate::ast::RoleHeaderTrait::Custom(custom_traits.len()));
                    custom_traits.push((trait_name.clone(), Some(Expr::Literal(Value::str(arg)))));
                    let (r2, _) = ws(r2)?;
                    rest = r2;
                    continue;
                }
                // Unknown trait — check for parenthesized argument for
                // custom trait_mod:<is> dispatch.
                if let Some(inner_start) = r.strip_prefix('(') {
                    if let Ok((after_expr, expr)) = expression(inner_start) {
                        let after_expr = after_expr.trim_start();
                        if let Some(after_paren) = after_expr.strip_prefix(')') {
                            header_order
                                .push(crate::ast::RoleHeaderTrait::Custom(custom_traits.len()));
                            custom_traits.push((trait_name.clone(), Some(expr)));
                            let (r2, _) = ws(after_paren)?;
                            rest = r2;
                            continue;
                        }
                    }
                    let r2 = skip_balanced_parens(r);
                    header_order.push(crate::ast::RoleHeaderTrait::Custom(custom_traits.len()));
                    custom_traits.push((trait_name.clone(), None));
                    let (r2, _) = ws(r2)?;
                    rest = r2;
                } else {
                    // No parens — treat as parent role. If it's actually
                    // a custom trait, the runtime will handle it via
                    // trait_mod:<is> when the role name is not found.
                    parent_roles.push((trait_name, None, true));
                    header_order.push(crate::ast::RoleHeaderTrait::Parent(parent_count));
                    parent_count += 1;
                    let (r, _) = ws(r)?;
                    rest = r;
                }
            }
            continue;
        }
        if let Some(r) = keyword("hides", rest) {
            let (r, _) = ws1(r)?;
            let (r, hidden_name) = qualified_ident(r)?;
            let (r, _) = ws(r)?;
            // Track as a parent relationship
            parent_roles.push((hidden_name.clone(), None, false));
            header_order.push(crate::ast::RoleHeaderTrait::Parent(parent_count));
            parent_count += 1;
            // Also mark the hidden relationship with a special marker
            parent_roles.push((
                MetaNs::RoleHides.owned_key_for_str(&hidden_name),
                None,
                false,
            ));
            rest = r;
            continue;
        }
        break;
    }

    Ok((
        rest,
        Header {
            parent_roles,
            is_hidden_role,
            role_is_rw,
            is_export,
            export_tags,
            custom_traits,
            header_order,
        },
    ))
}
