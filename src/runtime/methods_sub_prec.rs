//! `Routine.prec`: an operator routine's precedence hash (see
//! [`crate::op_prec`]).

use super::*;
use crate::symbol::Symbol;
use crate::value::ValueMap;

impl Interpreter {
    /// `&infix:<+>.prec`: the hash a trait on the routine `package::name`
    /// declared, else the built-in operator's, else its category's default.
    /// A routine that is not an operator answers an empty hash, as in Rakudo.
    // TODO: compile to bytecode — `Routine` receivers have no
    // `DispatchShape` in the built-in method table yet (ADR-11276), so this
    // is answered next to its `Routine` introspection siblings
    // (`is-implementation-detail`, `line`, `file`); it becomes a row when
    // the `Code` family migrates.
    // Cost: O(c + k), c = candidates registered under the name, k = entries.
    pub(super) fn routine_prec(&mut self, package: Symbol, name: Symbol) -> Value {
        let declared = self.declared_op_prec(package, name);
        let prec = declared.or_else(|| {
            crate::op_prec::for_name(&name.resolve()).map(crate::op_prec::OpPrec::from_entries)
        });
        let mut map = ValueMap::default();
        for (key, value) in prec.iter().flat_map(|prec| prec.entries()) {
            let value = if crate::op_prec::is_int_key(key) {
                Value::int(value.parse().unwrap_or(1))
            } else {
                Value::str(value.to_string())
            };
            map.insert(key.to_string(), value);
        }
        Value::hash(map)
    }

    /// The `__prec` trait recorded on `package::name`, or on one of its
    /// `multi` candidates.
    fn declared_op_prec(
        &mut self,
        package: Symbol,
        name: Symbol,
    ) -> Option<crate::op_prec::OpPrec> {
        let key = crate::qualified::qualified(package, name);
        if let Some(prec) = self
            .registry()
            .functions
            .get(&key)
            .and_then(|def| def.op_prec.clone())
        {
            return Some(prec);
        }
        let keys = self.fn_keys_for_base(&name.resolve());
        keys.iter().find_map(|key| {
            self.registry()
                .functions
                .get(key)
                .filter(|def| def.package == package && def.name == name)
                .and_then(|def| def.op_prec.clone())
        })
    }
}
