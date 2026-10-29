//! The fixed per-call names of a method frame (ADR-14633 §2.1, ADR-14843).
//!
//! `self`, `__ANON_STATE#`, `?CLASS` and `?ROLE` are not variables of the
//! frame's name-keyed `Env`: a method call pushes one of these records beside
//! its routine frame, a closure created in a method carries a copy of the
//! record it was created under, and the call of that closure pushes the copy.
//! The records are the only authority for the four names.

use crate::symbol::Symbol;
use crate::value::Value;

/// The invocant (`self` and `__ANON_STATE#`, one value for both names),
/// `?CLASS` and `?ROLE` of a method frame, or of a scope that stands in for
/// one: a class body has `?CLASS` and no invocant.
#[derive(Clone, Debug)]
pub(crate) struct FrameFixtures {
    pub(crate) invocant: Option<Value>,
    pub(crate) class: Option<Value>,
    pub(crate) role: Option<Symbol>,
}
