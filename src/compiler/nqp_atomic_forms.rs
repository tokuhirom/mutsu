//! The container-targeting atomic `nqp::` ops (#11502): `atomicload`,
//! `atomicstore`, `atomicinc_i`, `atomicdec_i`, `atomicadd_i`, `cas` and
//! their `_i` variants, plus `atomicbindattr`.
//!
//! Each of them names a CONTAINER (a scalar variable, an attribute, an `is rw`
//! parameter, or an array/hash element), which a value op never
//! sees: the `nqp::` value layer receives decontainerized operands. Raku's own
//! atomic operators (`⚛`, `⚛=`, `⚛++`, `atomic-fetch-add`, `cas`) need the
//! same thing and already compile their target to the one atomic primitive
//! (`runtime/builtins_atomic*.rs`: a shared cell, an attribute cell, or the
//! legacy name-keyed lane). ADR-0117 asks for one implementation per
//! primitive, so each op here compiles to exactly the Raku-level form with the
//! same answer:
//!
//! | nqp op                      | Raku form              | answers   |
//! |-----------------------------|------------------------|-----------|
//! | `atomicload(_i)($t)`        | `atomic-fetch($t)`     | the value |
//! | `atomicstore(_i)($t, $v)`   | `atomic-assign($t,$v)` | `$v`      |
//! | `atomicinc_i($t)`           | `atomic-fetch-inc($t)` | old value |
//! | `atomicdec_i($t)`           | `atomic-fetch-dec($t)` | old value |
//! | `atomicadd_i($t, $n)`       | `atomic-fetch-add`     | old value |
//! | `cas(_i)($t, $exp, $new)`   | `cas($t, $exp, $new)`  | old value |
//!
//! `atomicbindattr` is `bindattr`: an attribute store already goes through the
//! instance's locked attribute map, so the bind is atomic as it stands.
//!
//! `barrierfull` takes no container and is an ordinary value op
//! (`runtime/nqp_ops_process.rs`).

use super::*;

impl Compiler {
    /// Compile a container-targeting atomic `nqp::` op; `false` (with nothing
    /// emitted) for any other name or arity, which leaves the call to the
    /// runtime's loud unsupported-op error.
    pub(super) fn try_compile_nqp_atomic_form(&mut self, name: &str, args: &[Expr]) -> bool {
        let raku_form = match (name, args.len()) {
            // Cost: O(1) (compiles to `atomic-fetch`: one locked cell read).
            ("nqp::atomicload" | "nqp::atomicload_i", 1) => "atomic-fetch",
            // Cost: O(1) (compiles to `atomic-assign`: one locked cell write).
            ("nqp::atomicstore" | "nqp::atomicstore_i", 2) => "atomic-assign",
            // Cost: O(1) (compiles to `atomic-fetch-inc`: one locked RMW).
            ("nqp::atomicinc_i", 1) => "atomic-fetch-inc",
            // Cost: O(1) (compiles to `atomic-fetch-dec`: one locked RMW).
            ("nqp::atomicdec_i", 1) => "atomic-fetch-dec",
            // Cost: O(1) (compiles to `atomic-fetch-add`: one locked RMW).
            ("nqp::atomicadd_i", 2) => "atomic-fetch-add",
            // Cost: O(1) (compiles to `cas`: one locked compare-and-swap).
            ("nqp::cas" | "nqp::cas_i", 3) => "cas",
            // Cost: O(1) (compiles to `nqp::bindattr`).
            ("nqp::atomicbindattr", 4) => {
                return self.try_compile_nqp_value_op("nqp::bindattr", args);
            }
            _ => return false,
        };
        // The Raku routine each op stands for takes any `$target is rw`, but
        // the `_i` ops want a native integer container (MoarVM's own check), so
        // the lowering carries the op's spelling and that requirement.
        let spelling = super::atomic_target::AtomicSpelling {
            display: name.to_string(),
            int_only: name.ends_with("_i"),
        };
        self.with_atomic_spelling(spelling, |c| {
            c.compile_expr(&Expr::Call {
                name: crate::symbol::Symbol::intern(raku_form),
                args: args.to_vec(),
                listop: false,
            });
        });
        true
    }
}
