//! The decimal text of an `Int` hash key, formatted on the stack (#19289).
//! The one implementation lives with the other `Str` primitives
//! (`builtins::str_prim::DecimalKey`, ADR-0117); this re-export keeps the VM's
//! call sites short.

pub(super) use crate::builtins::str_prim::DecimalKey;
