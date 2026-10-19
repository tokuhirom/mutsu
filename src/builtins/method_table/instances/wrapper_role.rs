//! The wrapper roles' `gist` rows (ADR-11276, #11276): `X::Promise::Broken`,
//! `X::React::Died` and `X::Await::Died`.
//!
//! Rakudo mixes one of these roles into the exception `Promise.result`, a
//! dying `react` or `await` rethrows, and the role declares `gist` alone, to
//! say why the exception surfaces there (`runtime::wrapper_role_gist`). The
//! roles have no shape: the receiver is a mixin over the exception, so the
//! rows are `OWNER_ONLY`, found by the outermost wrapper role it carries
//! ([`owners_of`]) from the method dispatch and the VM's native lane.

use super::{Handler, MethodRow, RowFlags};
use crate::runtime::Interpreter;
use crate::runtime::wrapper_role_gist::{
    AWAIT_DIED_ROLE, PROMISE_BROKEN_ROLE, REACT_DIED_ROLE, outermost_wrapper_role_name,
};
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

/// The owner the rows reachable from `target` are found under: the wrapper
/// role composed into it most recently, whose `gist` wins.
// Cost: O(1), three mixin-map probes.
pub(crate) fn owners_of(target: &Value) -> Option<&'static [&'static str]> {
    static PROMISE_BROKEN: [&str; 1] = [PROMISE_BROKEN_ROLE];
    static REACT_DIED: [&str; 1] = [REACT_DIED_ROLE];
    static AWAIT_DIED: [&str; 1] = [AWAIT_DIED_ROLE];
    Some(match outermost_wrapper_role_name(target)? {
        PROMISE_BROKEN_ROLE => &PROMISE_BROKEN,
        REACT_DIED_ROLE => &REACT_DIED,
        _ => &AWAIT_DIED,
    })
}

/// A wrapper role's `gist`: its header, then the wrapped exception's own
/// gist (`Interpreter::wrapper_role_gist`).
// Cost: see `Interpreter::wrapper_role_gist`.
fn gist(interp: &mut Interpreter, target: &Value) -> Option<Result<Value, RuntimeError>> {
    interp.wrapper_role_gist(target)
}
