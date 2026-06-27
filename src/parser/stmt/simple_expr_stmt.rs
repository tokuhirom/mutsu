//! Expression-statement parsing: facade re-exporting themed submodules.
mod core;
mod let_temp;
mod lvalue;
mod predicates;
mod sig_info;

pub(super) use core::expr_stmt;
pub(super) use let_temp::{let_stmt, temp_stmt};
pub(crate) use lvalue::decl_target_var_name;
