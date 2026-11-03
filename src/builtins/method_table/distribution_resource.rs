//! Which `IO::Path` methods a `Distribution::Resource` answers (#16206).
//!
//! mutsu gives a `%?RESOURCES` entry the `IO::Path` rows (`DispatchShape::IoPath`,
//! #15818), but rakudo's `Distribution::Resource` is a small class: it has a short
//! list of path methods, and every other `IO::Path` method (`e`, `f`, `parent`,
//! `rename`, `move`, ...) is "No such method" on it. The list is the
//! `IO::Path` methods rakudo 2026.09 reports through `Distribution::Resource.can`
//! (`IO::Path.^methods(:local)` intersected with it), plus `platform-library-name`.

/// The `IO::Path` methods a resource answers.
const ANSWERED: &[&str] = &[
    "ACCEPTS",
    "IO",
    "Numeric",
    "Str",
    "absolute",
    "basename",
    "comb",
    "copy",
    "dirname",
    "extension",
    "gist",
    "is-absolute",
    "is-relative",
    "lines",
    "open",
    "parts",
    "platform-library-name",
    "raku",
    "relative",
    "resolve",
    "slurp",
    "split",
    "volume",
    "words",
];

/// Whether a `Distribution::Resource` answers the `IO::Path` method `method`.
// Cost: O(1) (a binary-search-free scan of a 24-name list, once per call on a resource).
pub(crate) fn resource_answers(method: &str) -> bool {
    ANSWERED.contains(&method)
}

/// Whether `target` is a `Distribution::Resource` instance and `method` is one
/// it does not answer: the `IO::Path` rows must decline it.
// Cost: O(1) for any other receiver; O(24) for a resource.
pub(crate) fn declines(target: &crate::value::Value, method: crate::symbol::Symbol) -> bool {
    matches!(
        target.view(),
        crate::value::ValueView::Instance { class_name, .. }
            if class_name == "Distribution::Resource"
    ) && !resource_answers(method.as_str())
}
