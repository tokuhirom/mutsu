//! The atomic routines on an array or hash ELEMENT (#11812):
//! `atomic-fetch(@a[0])`, `atomic-assign(%h<k>, $v)`,
//! `atomic-fetch-inc(@a[$i])`, `atomic-add-fetch(@a[0], 5)`, ... — and so
//! `@a[0]⚛++`, `++⚛@a[0]` and `nqp::atomicinc_i(@a[0])`, which compile to
//! these routines.
//!
//! A variable target compiles to the name-keyed `__mutsu_atomic_*_var` helpers
//! (expr_call.rs). An element target compiles to the one
//! `__mutsu_atomic_elem(container, key, op, operand?)` helper
//! (runtime/builtins_atomic_elem.rs), which works on the same element cell
//! `cas(@a[0], ...)` swaps.

use super::*;

impl Compiler {
    /// Compile an atomic routine whose first argument is `@arr[i]` / `%h{k}`;
    /// `false` (with nothing emitted) for any other routine or target.
    pub(super) fn try_compile_atomic_elem_call(&mut self, name: &Symbol, args: &[Expr]) -> bool {
        // (op, constant delta for inc/dec, whether the operand is negated)
        let (op, delta, negate) = match (name.resolve().as_str(), args.len()) {
            ("atomic-fetch", 1) => ("fetch", None, false),
            ("atomic-assign", 2) => ("store", None, false),
            ("atomic-fetch-inc", 1) => ("fetch-add", Some(1), false),
            ("atomic-inc-fetch", 1) => ("add-fetch", Some(1), false),
            ("atomic-fetch-dec", 1) => ("fetch-add", Some(-1), false),
            // The operators, as calls of their own routines (`@a[0]⚛++`).
            ("postfix:<⚛++>", 1) => ("fetch-add", Some(1), false),
            ("prefix:<++⚛>", 1) => ("add-fetch", Some(1), false),
            ("postfix:<⚛-->", 1) => ("fetch-add", Some(-1), false),
            ("prefix:<--⚛>", 1) => ("add-fetch", Some(-1), false),
            ("atomic-dec-fetch", 1) => ("add-fetch", Some(-1), false),
            ("atomic-fetch-add", 2) => ("fetch-add", None, false),
            ("atomic-add-fetch", 2) => ("add-fetch", None, false),
            ("atomic-fetch-sub", 2) => ("fetch-add", None, true),
            ("atomic-sub-fetch", 2) => ("add-fetch", None, true),
            _ => return false,
        };
        let Expr::Index { target, index, .. } = &args[0] else {
            return false;
        };
        let Some(container) = target
            .container_var_key()
            .filter(|k| k.starts_with(['@', '%']))
        else {
            return false;
        };
        // The routine's spelling belongs to this one call (see
        // `atomic_target.rs`). The add / increment family needs a native-integer
        // container, so does an `nqp::` `_i` load, store or `cas`; the plain
        // routines take any element.
        let spelling = self.atomic_spelling.take();
        let guard_display = match (&spelling, op) {
            (spelling, "fetch-add" | "add-fetch") => Some(
                spelling
                    .as_ref()
                    .map_or_else(|| name.resolve(), |s| s.display.clone()),
            ),
            (Some(spelling), _) if spelling.int_only => Some(spelling.display.clone()),
            _ => None,
        };
        if let Some(display) = &guard_display
            && args.get(1).is_none()
        {
            // No operand of the user's (`atomic-fetch-inc(@a[0])`): the guard is
            // a statement. With one, the guard below passes it through.
            self.emit_int_atomic_guard(&container, display, None, true);
        }
        if guard_display.is_none() && spelling.is_none() {
            // A lenient routine (`atomic-fetch`, `atomic-assign`, `⚛@a[0]`) takes
            // any element, but not one of a narrow native-int array (#12008).
            self.emit_narrow_atomic_guard(&container, true);
        }
        let container_idx = self.code.add_constant(Value::str(container.clone()));
        self.code.emit(OpCode::LoadConst(container_idx));
        self.compile_expr(index);
        let op_idx = self.code.add_constant(Value::str_from(op));
        self.code.emit(OpCode::LoadConst(op_idx));
        let arity = match (delta, args.get(1)) {
            (Some(d), _) => {
                let d_idx = self.code.add_constant(Value::int(d));
                self.code.emit(OpCode::LoadConst(d_idx));
                4
            }
            (None, Some(operand)) => {
                // The guard answers its operand, so it replaces compiling it.
                match &guard_display {
                    Some(display) => {
                        self.emit_int_atomic_guard(&container, display, Some(operand), true);
                    }
                    None => self.compile_expr(operand),
                }
                if negate {
                    self.code.emit(OpCode::Negate);
                }
                4
            }
            (None, None) => 3,
        };
        let call_name_idx = self
            .code
            .add_constant(Value::str_from("__mutsu_atomic_elem"));
        self.code.emit(OpCode::CallFunc {
            name_idx: call_name_idx,
            arity,
            arg_sources_idx: None,
            literal_native_args: 0,
            static_arg_types: false,
        });
        true
    }
}
