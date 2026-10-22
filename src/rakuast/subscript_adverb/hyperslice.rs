//! The six 6.e hash hyperslice adverbs, shared by RakuAST conversion and
//! lowering.

use crate::ast::HyperSliceAdverb;

pub(super) fn name(adverb: HyperSliceAdverb) -> &'static str {
    match adverb {
        HyperSliceAdverb::Kv => "kv",
        HyperSliceAdverb::K => "k",
        HyperSliceAdverb::V => "v",
        HyperSliceAdverb::Tree => "tree",
        HyperSliceAdverb::DeepK => "deepk",
        HyperSliceAdverb::DeepKv => "deepkv",
    }
}

pub(super) fn adverb(name: &str) -> Option<HyperSliceAdverb> {
    Some(match name {
        "kv" => HyperSliceAdverb::Kv,
        "k" => HyperSliceAdverb::K,
        "v" => HyperSliceAdverb::V,
        "tree" => HyperSliceAdverb::Tree,
        "deepk" => HyperSliceAdverb::DeepK,
        "deepkv" => HyperSliceAdverb::DeepKv,
        _ => return None,
    })
}
