//! The receiver-mutating methods' rows (ADR-11276 §10, slice 3F).
//!
//! `push`, `pop`, `shift`, `unshift`, `append`, `prepend`, `splice`, the hash
//! and quant-hash mutators, `subst-mutate`, `substr-rw`: rows whose handler
//! writes through the receiver's container (`Handler::Mut`, which slice 3F
//! adds together with `ReceiverPlace`). A slice adds a family module here and
//! lists it in [`FAMILIES`]; no other file names it.

use super::MethodRow;

/// Every family of this group.
pub(super) static FAMILIES: &[&[MethodRow]] = &[];
