//! The object-model `nqp::` ops that compile to the Raku construct with the
//! same answer (#11499): `how`, `how_nd`, `who`, `what_nd`, `reprname`,
//! `objectid`, `callmethod`, `call`.
//!
//! Rakudo's `.HOW`, `.WHO`, `.WHAT`, `.REPR` and `.WHERE` are themselves these
//! ops on `self`, so each nqp op compiles to the method call rather than
//! keeping a second meta-object or identity scheme (the precedent is
//! `nqp::where`, which compiles to `.WHERE`). The `_nd` ("no decont") forms
//! look at the container itself, which mutsu answers through `.VAR`.
//! `findmethod` / `tryfindmethod` need a null on a miss and are value ops
//! (`runtime/nqp_ops_object.rs`).

use super::*;

impl Compiler {
    /// Compile one of the object-model forms; `false` (with nothing emitted)
    /// when the arity does not fit, which leaves the call to the runtime's
    /// loud unsupported-op error.
    pub(super) fn try_compile_nqp_object_form(&mut self, name: &str, args: &[Expr]) -> bool {
        let method_call = |target: Expr, method: &str| Expr::MethodCall {
            target: Box::new(target),
            name: Symbol::intern(method),
            args: Vec::new(),
            modifier: None,
            quoted: false,
        };
        let expr = match (name, args) {
            // nqp::how($obj) — the meta-object, `$obj.HOW`.
            // Cost: O(1) (the `.HOW` it compiles to).
            ("nqp::how", [obj]) => method_call(obj.clone(), "HOW"),
            // nqp::how_nd($obj) — the meta-object of the operand itself, not
            // of its contents: `$obj.VAR.HOW`.
            // Cost: O(1) (the `.VAR.HOW` it compiles to).
            ("nqp::how_nd", [obj]) => method_call(method_call(obj.clone(), "VAR"), "HOW"),
            // nqp::what_nd($obj) — the type of the operand itself (`Scalar`
            // for a variable): `$obj.VAR.WHAT`.
            // Cost: O(1) (the `.VAR.WHAT` it compiles to).
            ("nqp::what_nd", [obj]) => method_call(method_call(obj.clone(), "VAR"), "WHAT"),
            // nqp::who($obj) — the package stash, `$obj.WHO`.
            // Cost: O(1) (the `.WHO` it compiles to).
            ("nqp::who", [obj]) => method_call(obj.clone(), "WHO"),
            // nqp::reprname($obj) — the representation's name, `$obj.REPR`.
            // Cost: O(1) (the `.REPR` it compiles to).
            ("nqp::reprname", [obj]) => method_call(obj.clone(), "REPR"),
            // nqp::objectid($obj) — a stable identity integer. mutsu's GC does
            // not move objects, so the `.WHERE` address is already stable.
            // Cost: O(1) (the `.WHERE` it compiles to).
            ("nqp::objectid", [obj]) => method_call(obj.clone(), "WHERE"),
            // nqp::callmethod($obj, $name, ...args) — `$obj."$name"(|args)`.
            // Cost: O(1) plus the method call.
            ("nqp::callmethod", [obj, method, rest @ ..]) => Expr::DynamicMethodCall {
                target: Box::new(obj.clone()),
                name_expr: Box::new(method.clone()),
                args: rest.to_vec(),
                modifier: None,
                quoted: true,
            },
            // nqp::call($code, ...args) — `$code(|args)`.
            // Cost: O(1) plus the call.
            ("nqp::call", [code, rest @ ..]) => Expr::CallOn {
                target: Box::new(code.clone()),
                args: rest.to_vec(),
            },
            _ => return false,
        };
        self.compile_expr(&expr);
        true
    }
}
