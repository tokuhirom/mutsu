use super::super::expr::expression;
use super::super::helpers::{skip_balanced_parens, ws, ws1};
use super::super::parse_result::{PError, PResult, parse_char, take_while1};
use crate::token_kind::{TokenKind, lookup_unicode_char_by_name};

use crate::ast::{Expr, ParamDef, Stmt, collect_placeholders_shallow};
use crate::symbol::Symbol;
use crate::value::Value;

use super::super::add_parse_warning;
use super::{block, block_inner, ident, keyword, parse_raku_ident};

mod op_name;
mod param_list;
mod param_validate;
mod return_type;
mod sub_decl;
mod sub_name;
mod traits;

// --- Re-exports preserving each function's original visibility. ---

// Signature/parameter validation (param_validate.rs).
pub(crate) use param_validate::placeholder_overrides_signature_error;
pub(super) use param_validate::{
    literal_value_from_expr, validate_param_trait, validate_param_trait_pub,
    validate_signature_params,
};

// Sub name / operator name parsing (sub_name.rs, op_name.rs).
pub(crate) use op_name::parse_bracket_op_name;
pub(crate) use sub_name::validate_categorical_parts;
pub(super) use sub_name::{null_operator_group_error, parse_sub_name, parse_sub_name_inner};

// Sub declaration parsing (sub_decl.rs).
pub(super) use sub_decl::{
    anon_multi_check, parse_indirect_decl_name, sub_decl, sub_decl_body,
    sub_decl_with_semicolon_mode, top_level_main_semicolon_decl,
};

// Trait parsing (traits.rs).
pub(crate) use traits::SubTraits;
pub(super) use traits::parse_sub_traits;
pub(crate) use traits::{reject_attr_params_in_sub, reject_invocant_in_sub};

// Parameter list parsing (param_list.rs, return_type.rs).
pub(super) use param_list::parse_param_list;
pub(crate) use param_list::{check_duplicate_params, invalid_param_smiley_error};
pub(super) use return_type::{
    parse_param_list_with_return, parse_return_type_annotation, skip_return_type_annotation,
};

// --- Re-exports forwarded from the sibling `sub_param` module. ---

pub(super) use super::sub_param::{
    is_anonymous_sigil_param, mark_params_as_invocant, parse_implicit_invocant_marker,
    starts_with_sigil_param,
};
pub(super) use super::sub_param::{
    method_decl, method_decl_body, method_decl_body_my, submethod_decl,
};
pub(super) use super::sub_param::{parse_single_param, parse_type_constraint_expr};
