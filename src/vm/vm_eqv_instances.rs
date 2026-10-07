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

    // Cost: O(d), d = MRO depth of `class`.
    fn is_exception_class(&mut self, class: Symbol) -> bool {
        let exception = Symbol::intern("Exception");
        self.interp.class_mro(class.as_str()).contains(&exception)
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
    // Only a multi dispatcher's `.raku` is compared: it is its proto's, which
    // spells neither an id nor a package, so two dispatchers reached from
    // different packages are `eqv` when their protos read the same. A plain
    // routine's `.raku` carries its id in Rakudo, so the identity/name rules
    // already decided it.
    // Cost: O(f + p) per side, as `Interpreter::dispatcher_raku`.
    fn routine_eqv(&mut self, a: &Value, b: &Value) -> bool {
        match (
            self.interp.routine_dispatcher_raku(a),
            self.interp.routine_dispatcher_raku(b),
        ) {
            (Some(ra), Some(rb)) => ra == rb,
            _ => false,
        }
    }

    // Cost: O(r), r = the cost of the two user `raku` calls.
    fn mixin_eqv(&mut self, a: &Value, b: &Value) -> Option<bool> {
        if self.error.is_some() {
            return Some(false);
        }
        if !(self.interp.mixin_composes_method(a, "raku")
            && self.interp.mixin_composes_method(b, "raku"))
        {
            return None;
        }
        let render = |hook: &mut Self, value: &Value| match hook.interp.call_method_with_values(
            value.clone(),
            "raku",
            vec![],
        ) {
            Ok(rendered) => Some(rendered.to_string_value()),
            Err(err) => {
                hook.error.get_or_insert(err);
                None
            }
        };
        let ra = render(self, a)?;
        let rb = render(self, b)?;
        Some(ra == rb)
    }

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
        let mode = self.class_mode(class_name);
        // The default `.raku` Rakudo compares reads every public attribute,
        // which vivifies it (`nqp::attrinited` turns true, #11003). Two
        // different types are decided before any `.raku` is taken.
        if let (Some(private), ValueView::Instance { class_name: cb, .. }) = (&mode, b.view())
            && cb == class_name
        {
            vivify_public_attrs(a, private);
            vivify_public_attrs(b, private);
        }
        // A built-in exception declares no attribute metadata here, yet carries
        // hidden state (`$!bt`, `$!ex`) a thrown one has and a constructed one
        // lacks: compare what its default `.raku` renders, as Rakudo does.
        if let (Some(private), ValueView::Instance { class_name: cb, .. }) = (&mode, b.view())
            && private.is_empty()
            && cb == class_name
            && self.is_exception_class(class_name)
        {
            let render = |hook: &mut Self, value: &Value| match hook.interp.call_method_with_values(
                value.clone(),
                "raku",
                vec![],
            ) {
                Ok(rendered) => Some(rendered.to_string_value()),
                Err(err) => {
                    hook.error.get_or_insert(err);
                    None
                }
            };
            if let Some(ra) = render(self, a)
                && let Some(rb) = render(self, b)
            {
                return InstanceEqv::Rendered(ra == rb);
            }
            return InstanceEqv::Decided(false);
        }
        match mode {
            Some(private) if private.is_empty() => InstanceEqv::Structural,
            Some(private) => InstanceEqv::Public(private),
            None => {
                let ra = self.user_raku(a);
                // A throwing `raku` ends the comparison: the right operand's
                // is never called. Its handler already ran at the throw
                // (ADR-0072), so a second call would run it again.
                if self.error.is_some() {
                    return InstanceEqv::Decided(false);
                }
                match (ra, self.user_raku(b)) {
                    (Some(ra), Some(rb)) => InstanceEqv::Rendered(self.error.is_none() && ra == rb),
                    _ => InstanceEqv::Structural,
                }
            }
        }
    }
}

/// Mark every public attribute of the instance `value` as read, as the default
/// `.raku` does by reading it; `private` names the attributes it skips.
// Cost: O(a), a = attributes of the instance.
fn vivify_public_attrs(value: &Value, private: &[String]) {
    let ValueView::Instance { attributes, .. } = value.view() else {
        return;
    };
    let map = attributes.as_map();
    let public: Vec<Symbol> = map
        .keys()
        .filter(|k| !private.iter().any(|p| p == k.as_str()))
        .copied()
        .collect();
    for key in public {
        map.get_vivify(key);
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
