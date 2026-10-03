//! CORE's own `trait_mod:<is>` candidates for an `Attribute` (`:rw`,
//! `:built`), reached when a routine calls them by name.
//!
//! An `is rw` / `is built` written on the `has` line is decided by the parser.
//! A custom attribute trait re-dispatches to the same CORE candidates as a call
//! (`multi trait_mod:<is>(Attribute $a, :$html-attr!) { trait_mod:<is>($a,
//! :built); ... }`, HTML::Component), with only the meta-object in hand. The
//! candidate records the trait on that object; the declaration site then reads
//! it back ([`AttrCoreTraitEffects::read`]) and folds it into the attribute's
//! definition, so the accessor and `.new` see it as if it were written on the
//! `has` line.

use super::*;

/// What CORE's `trait_mod:<is>` candidates did to one attribute while its
/// custom traits ran.
#[derive(Clone, Copy, Default, PartialEq, Eq)]
pub(crate) struct AttrCoreTraitEffects {
    /// `trait_mod:<is>($attr, :rw)` ran: the accessor is writable.
    pub(crate) rw: bool,
    /// `trait_mod:<is>($attr, :built(...))` ran and changed whether `.new`
    /// initializes the attribute from a named argument.
    pub(crate) built: Option<bool>,
}

impl AttrCoreTraitEffects {
    /// Read the flags the CORE candidates left on an attribute meta-object
    /// built by `make_trait_attribute_object` (whose `is_built` starts as
    /// `is_public` and whose `is_rw` starts absent). A trait that mixed a role
    /// into the attribute (`$attr does HTML::Component::HTMLAttr`) left a
    /// Mixin whose inner value is that same meta-object.
    ///
    /// Cost: O(m), m = mixin layers on the attribute object.
    pub(crate) fn read(attr_obj: &Value, is_public: bool) -> Self {
        let mut obj = attr_obj;
        while let ValueView::Mixin(inner, _) = obj.view() {
            obj = inner.as_ref();
        }
        let ValueView::Instance { attributes, .. } = obj.view() else {
            return Self::default();
        };
        let map = attributes.as_map();
        let rw = map.get("is_rw").is_some_and(Value::truthy);
        let built = map
            .get("is_built")
            .map(Value::truthy)
            .filter(|built| *built != is_public);
        Self { rw, built }
    }

    pub(crate) fn is_empty(&self) -> bool {
        *self == Self::default()
    }

    /// Fold the effects into an attribute definition and its class's or
    /// role's `attribute_built` table.
    ///
    /// Cost: O(a), a = attributes declared by the class or role.
    pub(crate) fn fold_into(
        &self,
        attributes: &mut [crate::runtime::ClassAttributeDef],
        attribute_built: &mut HashMap<String, bool>,
        attr_name: &str,
    ) {
        if self.rw
            && let Some(attr) = attributes.iter_mut().find(|a| a.name == attr_name)
        {
            attr.is_rw = true;
        }
        if let Some(built) = self.built {
            attribute_built.insert(attr_name.to_string(), built);
        }
    }
}

impl Interpreter {
    /// CORE's `trait_mod:<is>(Attribute:D, :rw)` and
    /// `trait_mod:<is>(Attribute:D, :built)` candidates. User `trait_mod:<is>`
    /// candidates are tried first; this native arm supplies the CORE ones when
    /// no user candidate matches.
    ///
    // Cost: O(n), n = arguments.
    pub(crate) fn try_native_attribute_trait(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if name != "trait_mod:<is>" || args.len() < 2 {
            return None;
        }
        let attribute: &Value = match args[0].view() {
            ValueView::VarRef { value, .. } => value,
            _ => &args[0],
        };
        let mut obj = attribute;
        while let ValueView::Mixin(inner, _) = obj.view() {
            obj = inner.as_ref();
        }
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = obj.view()
        else {
            return None;
        };
        if class_name != "Attribute" {
            return None;
        }
        let named = |want: &str| {
            args[1..].iter().find_map(|arg| match arg.view() {
                ValueView::Pair(key, value) if key == want => Some(value.clone()),
                ValueView::ValuePair(key, value) if key.to_string_value() == want => {
                    Some(value.clone())
                }
                _ => None,
            })
        };
        if named("rw").is_some() {
            attributes.insert("is_rw".to_string(), Value::TRUE);
            attributes.insert("rw".to_string(), Value::TRUE);
            return Some(Ok(Value::NIL));
        }
        if let Some(built) = named("built") {
            attributes.insert("is_built".to_string(), Value::truth(built.truthy()));
            return Some(Ok(Value::NIL));
        }
        None
    }
}
