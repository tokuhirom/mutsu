//! `Metamodel::<X>HOW.new_type` — the native metamethod that mints a type
//! object, shared by the direct call on a builtin metaclass and by the
//! `callsame`/`nextsame` base candidate a user HOW subclass's override reaches.
//!
//! Split out of `metamodel.rs` only to keep that file under the 500-line limit.

use super::*;

impl Interpreter {
    /// The native `new_type` metamethod: mint a type object named by the
    /// `:name` argument, register an empty class definition for it (so `.new`
    /// and the rest of the class protocol work on it), and record which
    /// metaclass it came from.
    ///
    /// `how_class` is the class the call was dispatched on. For a builtin
    /// (`Metamodel::ParametricRoleHOW.new_type(...)`) the new type's `.HOW` is
    /// that builtin, so `Metamodel::ModuleHOW` yields a `ModuleHOW` and not the
    /// default `ClassHOW`. For a USER subclass of one (`class MyHOW is
    /// Metamodel::ClassHOW`, the shape Test::Async's `BundleHOW` and
    /// OO::Monitors' `MonitorHOW` both use), rakudo's new type carries an
    /// instance of that subclass as its `.HOW`, so one is built here and
    /// installed — and the builtin ancestor is recorded too, so behaviour that
    /// keys off the metaclass *kind* still sees a role/class/module HOW.
    pub(crate) fn metamodel_new_type(
        &mut self,
        how_class: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let name = args
            .iter()
            .find_map(|a| match a.view() {
                ValueView::Pair(k, v) if k == "name" => Some(v.to_string_value()),
                ValueView::ValuePair(k, v) if k.to_string_value() == "name" => {
                    Some(v.to_string_value())
                }
                _ => None,
            })
            .unwrap_or_else(|| "Anon".to_string());
        if !self.registry().classes.contains_key(&name) {
            self.registry_mut()
                .classes
                .insert(name.clone(), Default::default());
        }
        // The builtin metamodel class this call ultimately goes through: the
        // receiver itself when it is one, else the first one on its MRO.
        let native_how = if how_class.starts_with("Metamodel::") {
            Some(how_class.to_string())
        } else {
            self.class_mro(how_class)
                .iter()
                .map(|c| c.to_string())
                .find(|c| Self::is_metamodel_class_name(c))
        };
        if let Some(native) = native_how {
            let short = native
                .strip_prefix("Perl6::Metamodel::")
                .or_else(|| native.strip_prefix("Metamodel::"))
                .unwrap_or(native.as_str());
            self.registry_mut()
                .declared_native_how
                .insert(name.clone(), format!("Perl6::Metamodel::{short}"));
        }
        if !how_class.starts_with("Metamodel::") && self.registry().classes.contains_key(how_class)
        {
            let how_type = Value::package(Symbol::intern(how_class));
            let instance = self.call_method_with_values(how_type, "new", Vec::new())?;
            self.registry_mut()
                .class_how_values
                .insert(name.clone(), instance);
        }
        Ok(Value::package(Symbol::intern(&name)))
    }
}
