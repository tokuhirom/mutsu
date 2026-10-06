//! The one check behind every integer atomic's target (#11834).
//!
//! `⚛++`, `⚛+=`, `atomic-fetch-add`, `nqp::atomicinc_i` and their kin take
//! `atomicint $target is rw`: the target has to be a native-integer container
//! -- an `int` / `atomicint` variable or attribute, or an element of a
//! native-int array. A plain `my $x` is refused, as a routine dispatch failure
//! at the Raku level and as MoarVM's own error at the `nqp::` level.
//!
//! The compiler (`compiler/atomic_target.rs`) settles every target whose
//! declaration it can see and emits **no guard at all** for a native one, so
//! the hot `$n⚛++` loop is untouched. What it cannot settle -- a parameter, an
//! attribute, an element, an alias, a name from a scope it does not see --
//! reaches [`Interpreter::builtin_atomic_int_target`] with the container to
//! ask. A refusal is only ever raised for a target positively known not to be
//! native: where the container does not answer, the atomic runs as it always
//! did, because a false refusal of a correct program is worse than the
//! leniency this check removes.

use super::*;
use crate::native_types::{is_atomic_int_target_type, is_narrow_atomic_int_type};
use crate::runtime::types::strip_type_smiley;
use crate::value::ValueView;

/// MoarVM's message for an `_i` atomic on a container that is not a native
/// integer (`nqp::atomicinc_i($plain)`).
const NQP_NOT_NATIVE_INT: &str =
    "Can only do integer atomic operations on a container referencing a native integer";

/// MoarVM's refusal of an atomic on a native integer narrower than the machine
/// word (`int8` / `int16` / `int32`, #12008). The message depends on where the
/// container lives, not on the operation.
const NARROW_LEXICAL: &str =
    "Cannot atomic load from an integer lexical not of the machine's native size";
const NARROW_ELEMENT: &str =
    "Can only do integer atomic operation on native integer array element of atomic size";
const NARROW_ATTRIBUTE: &str =
    "Can only do an atomic integer operation on an atomicint attribute";

/// What a target's declaration says about it.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Verdict {
    /// A native-integer container of the machine's native size.
    Native,
    /// A native integer narrower than that: every atomic on it is refused.
    Narrow,
    /// Known not to be one.
    Boxed,
    /// Nothing reachable here answers.
    Unknown,
}

/// The verdict for a binding declared with the type `ty`.
// Cost: O(|ty|).
fn verdict_for_type(ty: &str) -> Verdict {
    let ty = strip_type_smiley(ty).0;
    if is_atomic_int_target_type(ty) {
        Verdict::Native
    } else if is_narrow_atomic_int_type(ty) {
        Verdict::Narrow
    } else {
        Verdict::Boxed
    }
}

/// Whether a declared type is a native integer narrower than the machine word
/// (#12008). The lenient atomics already read the target's declared type for
/// their own coercion, so they ask this of it instead of a guard call of their
/// own, which cost a `cas` loop on an attribute or element ~30%.
// Cost: O(|ty|).
pub(super) fn is_narrow_declared_type(ty: &str) -> bool {
    is_narrow_atomic_int_type(strip_type_smiley(ty).0)
}

/// `array[int]` -> `int`.
fn native_array_inner(declared: &str) -> Option<&str> {
    declared.strip_prefix("array[")?.strip_suffix(']')
}

impl Interpreter {
    /// `__mutsu_atomic_int_target(target, declared, spelling[, operand])`: refuse
    /// an integer atomic whose target is not a native-integer container, and
    /// answer the operand (or `Nil`) otherwise.
    ///
    /// `target` is a scalar name, an attribute (`!v` / `.v`) or an element
    /// container (`@a` / `%h`). `declared` is the declared type the compiler
    /// read off the declaration (the empty string for an untyped one), or `Nil`
    /// when the declaration did not decide it. `spelling` is how the program
    /// named the routine.
    // Cost: O(1) for a declared target; O(d * a) for an attribute (the class
    // walk, memoized), O(1) otherwise.
    pub(crate) fn builtin_atomic_int_target(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let (Some(target), Some(declared), Some(spelling)) =
            (args.first(), args.get(1), args.get(2))
        else {
            return Err(RuntimeError::new(
                "__mutsu_atomic_int_target requires a target, a declared type and a spelling",
            ));
        };
        let operand = args.get(3);
        let target = target.to_string_value();
        match self.atomic_target_verdict(&target, declared) {
            Verdict::Narrow => return Err(Self::narrow_atomic_refusal(&target)),
            Verdict::Boxed => {
                return Err(self.int_atomic_refusal(
                    &target,
                    &spelling.to_string_value(),
                    operand,
                ));
            }
            Verdict::Native | Verdict::Unknown => {}
        }
        Ok(operand.cloned().unwrap_or(Value::NIL))
    }

    /// `__mutsu_atomic_narrow_target(target, declared)`: the guard of a *lenient*
    /// atomic (`atomic-fetch`, `atomic-assign`, `cas`, `⚛$x`, `$x ⚛= v`). Those
    /// take any `$target is rw`, so a plain scalar stays legal, but a native
    /// integer narrower than the machine word is refused by MoarVM for every
    /// operation (#12008). Only a target positively known to be narrow is
    /// refused; `target` and `declared` are `__mutsu_atomic_int_target`'s.
    // Cost: O(1) for a declared target; O(d * a) for an attribute (the class
    // walk, memoized), O(1) otherwise.
    pub(crate) fn builtin_atomic_narrow_target(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let (Some(target), Some(declared)) = (args.first(), args.get(1)) else {
            return Err(RuntimeError::new(
                "__mutsu_atomic_narrow_target requires a target and a declared type",
            ));
        };
        let target = target.to_string_value();
        if self.atomic_target_verdict(&target, declared) == Verdict::Narrow {
            return Err(Self::narrow_atomic_refusal(&target));
        }
        Ok(Value::NIL)
    }

    /// The verdict for `target`: the compiler's `declared` type when it settled
    /// the declaration, else what the container says at run time.
    fn atomic_target_verdict(&self, target: &str, declared: &Value) -> Verdict {
        match declared.view() {
            ValueView::Str(ty) => verdict_for_type(&ty),
            _ => self.container_target_verdict(target),
        }
    }

    /// MoarVM's error for an atomic on a native integer narrower than the
    /// machine word, worded by where the container lives.
    pub(super) fn narrow_atomic_refusal(target: &str) -> RuntimeError {
        let message = if target.starts_with(['@', '%']) {
            NARROW_ELEMENT
        } else if target.starts_with(['!', '.']) {
            NARROW_ATTRIBUTE
        } else {
            NARROW_LEXICAL
        };
        RuntimeError::new(message)
    }

    /// Refuse a lenient atomic (`atomic-fetch`, `atomic-assign`, `cas`, `⚛$!v`,
    /// `$!v ⚛= x`) on an attribute declared with a native integer narrower than
    /// the machine word (#12008). Any other name passes: a lexical's declaration
    /// is judged by the compiler, whose answer a by-name metadata lookup could
    /// not match under shadowing.
    ///
    /// The lenient builtins call this instead of the compiler emitting a guard
    /// call per operation: a separate call cost a `cas` loop on an attribute
    /// ~50%, and the type lookup is the memoized one (ADR-0121).
    // Cost: O(1) (a memoized per-class lookup), O(1) for any other name.
    pub(super) fn refuse_narrow_attribute(&self, name: &str) -> Result<(), RuntimeError> {
        if name.starts_with(['!', '.'])
            && self
                .self_attr_type_constraint(name)
                .is_some_and(|ty| is_narrow_declared_type(&ty))
        {
            return Err(Self::narrow_atomic_refusal(name));
        }
        Ok(())
    }

    /// What the container named by `target` says about itself, when the
    /// declaration did not decide.
    fn container_target_verdict(&self, target: &str) -> Verdict {
        if target.starts_with(['@', '%']) {
            return self.element_target_verdict(target);
        }
        if target.starts_with(['!', '.']) {
            return self.attribute_target_verdict(target);
        }
        self.lexical_target_verdict(target)
    }

    /// An array or hash element: the container carries its element type.
    fn element_target_verdict(&self, container: &str) -> Verdict {
        if container.starts_with('%') {
            // There is no native-integer hash.
            return Verdict::Boxed;
        }
        let base = self
            .env
            .get(container)
            .cloned()
            .or_else(|| self.get_shared_var(container));
        let Some(base) = base else {
            return Verdict::Unknown;
        };
        match base.view() {
            ValueView::Array(array, _) => {
                let element_type = array
                    .value_type
                    .as_deref()
                    .or_else(|| array.declared_type.as_deref().and_then(native_array_inner));
                element_type.map_or(Verdict::Boxed, verdict_for_type)
            }
            _ => Verdict::Unknown,
        }
    }

    /// An attribute of the invocant: the class declares its type.
    fn attribute_target_verdict(&self, attr: &str) -> Verdict {
        if self.get_env_self().is_none() {
            return Verdict::Unknown;
        }
        self.self_attr_type_constraint(attr)
            .map_or(Verdict::Boxed, |ty| verdict_for_type(&ty))
    }

    /// A scalar the compiler did not see declared (a parameter bound to a
    /// caller's container, a `:=` alias, an outer name): the type travels with
    /// the container, in its cell when it has one.
    ///
    /// Only a cell that *says* so refuses: one carrying a non-native `of`-type,
    /// or one its creator marked as made from an untyped variable (#12007). A
    /// cell with neither is not proof of an untyped variable: a cell is made at
    /// many sites (a closure's capture, a `:=` alias, an `is rw` argument) and
    /// not every one of them copies the declaring variable's type onto it, so
    /// `my atomicint $x; my $y := $x; $y⚛++` can reach a cell that says
    /// nothing and must run.
    fn lexical_target_verdict(&self, name: &str) -> Verdict {
        if let Some(ty) = self.var_type_constraint(name) {
            return verdict_for_type(&ty);
        }
        let Some(cell) = self.scalar_cell_target(name) else {
            return Verdict::Unknown;
        };
        // A rebound variable's cell is a binding cell in front of the
        // container it is bound to; that container is the one that answers.
        let value_cell = Self::value_cell_of(&cell);
        [cell, value_cell]
            .iter()
            .find_map(|cell| match crate::value::lookup_container_constraint(cell) {
                Some(ty) => Some(verdict_for_type(&ty)),
                None => cell.is_declared_untyped().then_some(Verdict::Boxed),
            })
            .unwrap_or(Verdict::Unknown)
    }

    /// The error Rakudo raises for an integer atomic on a non-native target.
    fn int_atomic_refusal(
        &self,
        target: &str,
        spelling: &str,
        operand: Option<&Value>,
    ) -> RuntimeError {
        if spelling.starts_with("nqp::") {
            return RuntimeError::new(NQP_NOT_NATIVE_INT);
        }
        let mut signature = self.int_atomic_target_label(target);
        let candidates = match operand {
            Some(operand) => {
                signature.push_str(", ");
                signature.push_str(&Self::typed_arg_label(operand));
                [
                    "    (atomicint $target is rw, int $add --> atomicint)",
                    "    (atomicint $target is rw, Int:D $add --> atomicint)",
                    "    (atomicint $target is rw, $add --> atomicint)",
                ]
                .join("\n")
            }
            None => "    (atomicint $target is rw --> atomicint)".to_string(),
        };
        crate::runtime::methods_signature_errors::make_no_match_error_with_message(format!(
            "Cannot resolve caller {spelling}({signature}); the following candidates\n\
             match the type but require mutable arguments:\n{candidates}"
        ))
    }

    /// `Int:D` / `Any:U`: a value's type with its definiteness smiley, as a
    /// dispatch failure names its arguments.
    fn typed_arg_label(value: &Value) -> String {
        // A type object names itself: `Int:U`, not the `Package` it is stored as.
        if let ValueView::Package(name) = value.view() {
            return format!("{name}:U");
        }
        let smiley = if crate::runtime::types::value_is_defined(value) {
            ":D"
        } else {
            ":U"
        };
        format!("{}{smiley}", crate::runtime::utils::value_type_name(value))
    }

    /// The label of the target's current value in a refusal. An element's own
    /// value is not at hand (the guard sees the container, not the index), so
    /// it reads as the `Int:D` an integer atomic's target nearly always is.
    fn int_atomic_target_label(&self, target: &str) -> String {
        if target.starts_with(['@', '%']) {
            return "Int:D".to_string();
        }
        let value = if target.starts_with(['!', '.']) {
            self.self_attr_cell_target(target)
                .and_then(|(attrs, key)| attrs.as_map().get(&key).cloned())
        } else {
            self.env
                .get(target)
                .or_else(|| self.env.get(target.trim_start_matches('$')))
                .cloned()
        };
        let value = value.map(|v| match v.view() {
            ValueView::ContainerRef(cell) => cell.lock().unwrap_or_else(|e| e.into_inner()).clone(),
            _ => v.clone(),
        });
        value.map_or_else(|| "Any:U".to_string(), |v| Self::typed_arg_label(&v))
    }
}
