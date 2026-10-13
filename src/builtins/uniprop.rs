mod age_table;
mod binary_props;
mod char_props;
mod lookup;
mod prop_names;
mod text_seg;

pub(crate) use binary_props::try_binary_property;
pub(crate) use char_props::unicode_numeric_type;
pub(crate) use lookup::{
    east_asian_width_abbrev, unicode_property_value, unicode_property_value_for_codepoint,
    unimatch, unimatch_for_codepoint,
};
pub(crate) use text_seg::unicode_grapheme_cluster_break;
