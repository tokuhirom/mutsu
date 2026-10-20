//! The wrapper roles' `gist` rows (ADR-11276, #11276): `X::Promise::Broken`,
//! `X::React::Died` and `X::Await::Died`.
//!
//! Rakudo mixes one of these roles into the exception `Promise.result`, a
//! dying `react` or `await` rethrows, and the role declares `gist` alone, to
//! say why the exception surfaces there (`runtime::wrapper_role_gist`). The
//! roles have no shape: the receiver is a mixin over the exception, so the
//! rows are `OWNER_ONLY`, found like any mixed-in role's rows
//! ([`super::super::mixin_role_owners_of`]).

use super::{Handler, MethodRow, RowFlags};
use crate::runtime::Interpreter;
use crate::runtime::wrapper_role_gist::{AWAIT_DIED_ROLE, PROMISE_BROKEN_ROLE, REACT_DIED_ROLE};
use crate::value::{RuntimeError, Value};

macro_rules! gist {
    ($owner:expr) => {
        MethodRow {
            owner: $owner,
            name: "gist",
            arity: 0,
            handler: Handler::Interp(|interp, target, _, _| gist(interp, target)),
            flags: RowFlags::OWNER_ONLY,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    gist!(PROMISE_BROKEN_ROLE),
    gist!(REACT_DIED_ROLE),
    gist!(AWAIT_DIED_ROLE),
];

/// A wrapper role's `gist`: its header, then the wrapped exception's own
/// gist (`Interpreter::wrapper_role_gist`).
// Cost: see `Interpreter::wrapper_role_gist`.
fn gist(interp: &mut Interpreter, target: &Value) -> Option<Result<Value, RuntimeError>> {
    interp.wrapper_role_gist(target)
}
