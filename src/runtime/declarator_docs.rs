//! Declarator docs of the running compilation unit (ADR-0136) and the
//! `.WHY` caches built over them. ADR-10779 first listed these under `io`;
//! they are per-compilation-unit declaration metadata, saved and restored
//! around a module load, so they form their own holder in the `module`
//! subsystem. A spawned thread starts with none.

use super::*;

#[derive(Default, Clone)]
pub(crate) struct DeclaratorDocs {
    /// Declarator docs keyed the way `.WHY` looks them up (see
    /// `install_doc_comments`).
    pub(crate) doc_comments: HashMap<String, DocComment>,
    /// Ordered list of doc comments for $=pod
    pub(crate) doc_comment_list: Vec<DocComment>,
    /// Cache for .WHY results so identity checks (=:=) work
    pub(crate) why_cache: ValueMap,
    /// Pod declarators keyed by the concrete WHEREFORE object's stable id.
    /// DOC INIT uses AST-built declarants before runtime registration, so a
    /// name key would collide for multis and same-named parameters.
    pub(crate) why_object_cache: HashMap<u64, Value>,
}
