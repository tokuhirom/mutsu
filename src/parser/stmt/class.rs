//! Class / role / grammar / module / package declaration parsers.
//!
//! This module is a facade: the parsing logic is split across themed
//! submodules and re-exported here, preserving each function's original
//! visibility so external callers (`class::class_decl`, etc.) keep working.

mod attr_checks;
mod class_decl;
mod grammar_module;
mod package_decl;
mod role_decl;
mod token_body;

// Shared attribute / body validation helpers used across submodules.
pub(crate) use attr_checks::{
    null_regex_error, reject_no_self_in_attr_where, reject_no_self_in_subs,
    reject_no_twigil_attr_at_body_level, stmt_also_is_parent, stmt_is_also_is_rw,
};

// Shared token / braced-body helpers used across submodules.
pub(crate) use token_body::{
    consume_raw_braced_body, inject_implicit_rule_ws, inject_separator_ws, normalize_token_pattern,
    parse_raw_braced_regex_body, parse_token_like_name,
};

// Shared declarator helpers used across submodules.
pub(crate) use class_decl::{
    meta_setter_stmt, parse_declarator_traits, parse_optional_bracket_suffix,
};
pub(crate) use package_decl::{
    check_pseudo_package_in_decl, export_name_clash_error, extract_exported_subs,
    find_export_name_clash,
};
pub(crate) use role_decl::skip_optional_role_args;

// Public entry points — preserve each function's original visibility.
pub(super) use class_decl::{also_trait_stmt, class_decl_body};
pub(crate) use class_decl::{anon_class_decl, augment_class_decl, class_decl};
pub(super) use grammar_module::{does_decl, grammar_decl, module_decl, token_decl, trusts_decl};
pub(super) use package_decl::{package_decl, package_decl_my, proto_decl, unit_module_stmt};
pub(super) use role_decl::role_decl;
