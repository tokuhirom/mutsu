//! String/quote parsing: facade re-exporting themed submodules.
//!
//! Split from the original monolithic `string.rs`. Each submodule owns a
//! cohesive group of free functions; this facade preserves every function's
//! original visibility through explicit named re-exports so external callers
//! (`string::<name>`) keep working unchanged.

mod escapes;
mod helpers;
mod heredoc;
mod interp_content;
mod interp_helpers;
mod interp_var;
mod q_string;
mod quoted;
mod quotewords;
mod qx;

pub(super) use escapes::process_escape_sequence;
pub(super) use helpers::{
    make_word_result_expr, non_variable_dollar_perror, quotewords_atom_expr,
    quotewords_atom_expr_allomorphic, read_bracketed, unicode_bracket_close_pub,
    unrecognized_backslash_perror,
};
pub(super) use interp_content::finalize_interpolation;
pub(super) use interp_var::try_interpolate_var;
pub(super) use q_string::{big_q_string, q_string};
pub(super) use quoted::{
    corner_bracket_string, double_quoted_string, parse_backslash_c_bracket, single_quoted_string,
    smart_double_quoted_string, smart_single_quoted_string,
};
pub(super) use quotewords::parse_quotewords_quoted_atom;
pub(super) use qx::backtick_qx_string;

pub(in crate::parser) use interp_content::{
    interpolate_string_content, interpolate_string_content_with_modes,
};
pub(in crate::parser) use qx::qx_string;

pub(crate) use helpers::{
    count_repeated_bracket, process_q_escapes, quote_delimiters, read_delimited_content,
    read_multi_bracketed, unicode_bracket_close,
};
pub(crate) use heredoc::{parse_to_heredoc, parse_to_heredoc_with_flags};
pub(crate) use interp_content::parse_single_quote_qq;
pub(crate) use interp_helpers::{
    has_malformed_angle_interpolation, parse_shell_words_index, try_double_sigil_interp,
    try_parse_interp_call, try_parse_interp_method_call,
};
pub(crate) use quotewords::parse_quotewords_items;
