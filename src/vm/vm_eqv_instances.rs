//! Rakudo's `infix:<eqv>(Any:D, Any:D)` rule for user instances, plugged into
//! the pure structural [`Value::eqv_with`] walk as an [`EqvInstanceHook`].
//!
//! Rakudo answers `True` for two defined objects when they are the same
//! object, or when they have the same `.WHAT` and equal `.raku` strings. For a
//! class with a user `raku` method that means calling it (VM-native, through
//! the compiled method dispatch); for a class using the default `.raku`, which
//! renders public attributes only, it means comparing the public attributes and
//! ignoring the private ones.
use super::*;
use crate::value::types_eqv::{EqvInstanceHook, InstanceEqv};
use std::collections::HashMap;
use std::rc::Rc;

/// Per-`eqv`-call hook state: the interpreter, the class metadata looked up so
/// far, and the first error a user `raku` threw (re-raised by the caller).
pub(super) struct RakudoInstanceEqv<'a> {
    interp: &'a mut Interpreter,
    /// Per class: `Some(private names)` for the default `.raku`, `None` for a
    /// class with a user `raku`. A class with no metadata maps to an empty
    /// private list (every slot compared).
    classes: HashMap<Symbol, Option<Rc<[String]>>>,
    pub(super) error: Option<RuntimeError>,
}

impl<'a> RakudoInstanceEqv<'a> {
    pub(super) fn new(interp: &'a mut Interpreter) -> Self {
        Self {
            interp,
            classes: HashMap::new(),
            error: None,
        }
    }

    /// The class's comparison mode, memoized for this `eqv` call.
    // Cost: O(1) on a hit; a miss is O(d + a), d = MRO depth, a = number of
    // declared attributes across the MRO.
    fn class_mode(&mut self, class: Symbol) -> Option<Rc<[String]>> {
        if let Some(mode) = self.classes.get(&class) {
            return mode.clone();
        }
        let name = class.resolve();
        let mode = if self.interp.has_user_method_including_role(&name, "raku") {
            None
        } else {
            let attrs = self.interp.collect_class_attributes_display_order(&name);
            let attrs = if attrs.is_empty() && self.interp.registry().roles.contains_key(&name) {
                self.interp.collect_role_attributes_for_class(&name)
            } else {
                attrs
            };
            let private: Vec<String> = attrs
                .iter()
                .filter(|attr| !attr.is_public)
                .filter(|attr| !attrs.iter().any(|a| a.is_public && a.name == attr.name))
                .map(|attr| attr.name.clone())
                .collect();
            Some(Rc::from(private))
        };
        self.classes.insert(class, mode.clone());
        mode
    }

    /// `$value.raku` through the compiled user method; `None` when the
    /// method has no bytecode dispatch (the caller then compares structurally).
    fn user_raku(&mut self, value: &Value) -> Option<String> {
        match self
            .interp
            .try_dispatch_compiled_method_direct(value, "raku", &[])?
        {
            Ok(rendered) => Some(rendered.to_string_value()),
            Err(err) => {
                self.error.get_or_insert(err);
                Some(String::new())
            }
        }
    }
}

impl EqvInstanceHook for RakudoInstanceEqv<'_> {
    // Cost: O(1) plus `class_mode`'s miss cost; a class with a user `raku`
    // adds two calls of it.
    fn instance_eqv(&mut self, a: &Value, b: &Value) -> InstanceEqv {
        let (
            ValueView::Instance {
                class_name, id: ia, ..
            },
            ValueView::Instance { id: ib, .. },
        ) = (a.view(), b.view())
        else {
            return InstanceEqv::Structural;
        };
        if ia == ib {
            return InstanceEqv::Decided(true);
        }
        if self.error.is_some() {
            return InstanceEqv::Decided(false);
        }
        match self.class_mode(class_name) {
            Some(private) if private.is_empty() => InstanceEqv::Structural,
            Some(private) => InstanceEqv::Public(private),
            None => match (self.user_raku(a), self.user_raku(b)) {
                (Some(ra), Some(rb)) => InstanceEqv::Rendered(self.error.is_none() && ra == rb),
                _ => InstanceEqv::Structural,
            },
        }
    }
}

impl Interpreter {
    /// [`Value::eqv`] with Rakudo's rule for user instances at every depth; an
    /// exception thrown by a user `raku` propagates.
    // Cost: O(n) in the size of the compared structures, plus the user `raku`
    // calls for classes that define one.
    pub(super) fn eqv_rakudo(&mut self, left: &Value, right: &Value) -> Result<bool, RuntimeError> {
        let mut hook = RakudoInstanceEqv::new(self);
        let answer = left.eqv_with(right, &mut hook);
        match hook.error {
            Some(err) => Err(err),
            None => Ok(answer),
        }
    }
}
