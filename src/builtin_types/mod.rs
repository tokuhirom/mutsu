//! The built-in types' ancestry: the static MRO/roles catalog and the
//! membership and narrowness queries derived from it (ADR-0051). Pure tables
//! with no runtime dependency, so `Value`'s type checks can consult them
//! without naming `builtins` (issue #10779).

pub(crate) mod ancestry;
pub(crate) mod catalog;
