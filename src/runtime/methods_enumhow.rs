//! `Perl6::Metamodel::EnumHOW` metamethods.
//!
//! `.^enum_values`, `.^elems`, `.^enum_from_value` and `.^enum_value_list` are
//! EnumHOW-only: they answer from the enum's value list, in declaration order.
//! On any other HOW they do not exist at all, so the dispatch must report a
//! missing method rather than fall through to the generic value-level handler
//! of the same name.
//!
//! An enum reaches EnumHOW one of two ways:
//!
//! * the `enum` declarator, which registers `(key, EnumValue)` variants in
//!   `Registry::enum_types`;
//! * the MOP (`Metamodel::EnumHOW.new_type` → `.^add_enum_value` →
//!   `.^compose_values`), which keeps the value OBJECTS it was handed in
//!   `Registry::how_enums`, exactly as Rakudo's `EnumHOW` keeps its
//!   `@!enum_value_list`. Those objects are whatever the caller passed —
//!   `raku-doc`'s own `Type/Metamodel/EnumHOW.rakudoc` example adds plain
//!   `Pair`s — and every query reads them through their `.key` / `.value`.
//!
//! Both kinds answer the queries through [`Interpreter::enum_how_value_list`],
//! so the two stores cannot drift in what `enum_values` et al. mean.

use super::*;

/// The state `Metamodel::EnumHOW` keeps for a type minted by its `new_type`.
#[derive(Clone, Default)]
pub(crate) struct HowEnumState {
    /// The value objects added by `.^add_enum_value`, in order.
    pub(crate) values: Vec<Value>,
    /// Set by `.^compose`; read by `.^is_composed`.
    pub(crate) composed: bool,
    /// Set by `.^set_export_callback`; invoked (once) and cleared by
    /// `.^compose_values`.
    pub(crate) export_callback: Option<Value>,
}

impl Interpreter {
    /// The registry key of the enum type this MOP call's invocant stands for.
    fn enum_how_type_name(&mut self, type_value: &Value) -> String {
        match type_value.view() {
            ValueView::Package(name) => name.resolve(),
            ValueView::Str(name) => name.to_string(),
            ValueView::Enum { enum_type, .. } => enum_type.resolve(),
            _ => self.mop_receiver_owner(type_value),
        }
    }

    /// Whether `name` is a type minted by `Metamodel::EnumHOW.new_type`.
    pub(crate) fn is_how_enum(&self, name: &str) -> bool {
        self.registry().how_enums.contains_key(name)
    }

    /// The value objects of the enum `type_value` stands for, in order — the
    /// objects `.^add_enum_value` received for a MOP-built enum, the enum's
    /// own values for a declared one — or `None` when it is not an enum.
    // Cost: O(n), n = number of enum values (one clone each).
    pub(crate) fn enum_how_value_list(&mut self, type_value: &Value) -> Option<Vec<Value>> {
        let name = self.enum_how_type_name(type_value);
        let reg = self.registry();
        if let Some(state) = reg.how_enums.get(&name) {
            return Some(state.values.clone());
        }
        let variants = reg.enum_types.get(&name)?;
        let type_sym = Symbol::intern(&name);
        Some(
            variants
                .iter()
                .enumerate()
                .map(|(index, (key, val))| {
                    Value::enum_parts(type_sym, Symbol::intern(key), val.clone(), index)
                })
                .collect(),
        )
    }

    /// An enum value object's `.key` and `.value`. Pairs and enum values are
    /// read directly; any other object (a class doing `Enumeration`) answers
    /// through its own accessor methods, as it does in Rakudo.
    // Cost: O(1) for a Pair or enum value; one method call each otherwise.
    fn enum_member_key_value(&mut self, member: &Value) -> Result<(String, Value), RuntimeError> {
        match member.view() {
            ValueView::Pair(key, value) => Ok((key.to_string(), value.clone())),
            ValueView::ValuePair(key, value) => Ok((key.to_str_context(), value.clone())),
            ValueView::Enum { key, value, .. } => Ok((key.resolve(), value.to_value())),
            _ => {
                let key = self.call_method_with_values(member.clone(), "key", Vec::new())?;
                let value = self.call_method_with_values(member.clone(), "value", Vec::new())?;
                Ok((key.to_str_context(), value))
            }
        }
    }

    /// The type name of a MOP-built enum this call's first argument names,
    /// or the EnumHOW-only error when it is not one.
    fn how_enum_target(&mut self, method: &str, args: &[Value]) -> Result<String, RuntimeError> {
        let name = self.enum_how_type_name(&args[0]);
        if self.is_how_enum(&name) {
            Ok(name)
        } else {
            Err(Self::enum_how_method_missing(method, &args[0]))
        }
    }

    /// Dispatch one of `Metamodel::EnumHOW`'s own metamethods. `None` when
    /// `method` is not one of them (or `compose` on a non-MOP enum), so the
    /// caller continues with the shared ClassHOW table.
    pub(crate) fn dispatch_enumhow_method(
        &mut self,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let first = args.first()?;
        Some(match method {
            // `EnumHOW.enum_values`: a Map from each value's *name* to its
            // underlying value (`Numbers.^enum_values` is `{10 => 0, 20 => 1}`).
            // Cost: O(n), n = number of enum values.
            "enum_values" => (|| {
                let Some(members) = self.enum_how_value_list(first) else {
                    return Err(Self::enum_how_method_missing("enum_values", first));
                };
                let mut map = ValueMap::default();
                for member in &members {
                    let (key, value) = self.enum_member_key_value(member)?;
                    map.insert(key, value);
                }
                Ok(Value::hash(map))
            })(),
            // `EnumHOW.elems`: how many values the enum declares. On any other
            // metaobject `elems` is just the inherited `Any.elems`, which is 1
            // (`class C {}; C.HOW.elems` is 1 in raku). Handling it here is what
            // keeps a HOW invocant out of the generic `.elems` dispatch, which
            // has no implementation for a HOW instance and used to bounce
            // between `dispatch_elems_method` and `builtin_elems` until the
            // stack overflowed.
            // Cost: O(n), n = number of enum values (the list is cloned).
            "elems" => Ok(Value::int(
                self.enum_how_value_list(first)
                    .map_or(1, |members| members.len() as i64),
            )),
            // `EnumHOW.enum_from_value`: the value object whose `.value`
            // equals the argument, or `Mu` when none does.
            // Cost: O(n), n = number of enum values.
            "enum_from_value" if args.len() >= 2 => (|| {
                let Some(members) = self.enum_how_value_list(first) else {
                    return Err(Self::enum_how_method_missing("enum_from_value", first));
                };
                for member in members {
                    let (_, value) = self.enum_member_key_value(&member)?;
                    if value.eqv(&args[1]) {
                        return Ok(member);
                    }
                }
                Ok(Value::package(Symbol::intern("Mu")))
            })(),
            // `EnumHOW.enum_value_list`: the value objects, in order.
            // Cost: O(n), n = number of enum values.
            "enum_value_list" => Ok(Value::array(
                self.enum_how_value_list(first).unwrap_or_default(),
            )),
            // `EnumHOW.add_enum_value`: append one value object. Rakudo
            // documents that it "should be an instance of the enum itself"
            // but takes any object with a `.key` and a `.value`.
            // Cost: O(1) amortized.
            "add_enum_value" if args.len() >= 2 => self.how_enum_target(method, args).map(|name| {
                if let Some(state) = self.registry_mut().how_enums.get_mut(&name) {
                    state.values.push(args[1].clone());
                }
                Value::NIL
            }),
            // `EnumHOW.set_export_callback`: the routine `compose_values`
            // runs to export the values (what the `is export` trait installs).
            // Cost: O(1).
            "set_export_callback" if args.len() >= 2 => {
                self.how_enum_target(method, args).map(|name| {
                    if let Some(state) = self.registry_mut().how_enums.get_mut(&name) {
                        state.export_callback = Some(args[1].clone());
                    }
                    Value::NIL
                })
            }
            // `EnumHOW.compose_values`: run the export callback, once — it is
            // removed from the state, so a second call does nothing.
            // Cost: O(1) plus the callback.
            "compose_values" => (|| {
                let name = self.how_enum_target(method, args)?;
                let callback = self
                    .registry_mut()
                    .how_enums
                    .get_mut(&name)
                    .and_then(|state| state.export_callback.take());
                if let Some(callback) = callback {
                    self.call_sub_value(callback, Vec::new(), false)?;
                }
                Ok(Value::NIL)
            })(),
            // `EnumHOW.is_composed`: 1 once `.^compose` ran, else 0. A
            // declared enum is composed at its declaration.
            // Cost: O(1).
            "is_composed" => {
                let name = self.enum_how_type_name(first);
                let reg = self.registry();
                match reg.how_enums.get(&name) {
                    Some(state) => Ok(Value::int(i64::from(state.composed))),
                    None if reg.enum_types.contains_key(&name) => Ok(Value::int(1)),
                    None => return None,
                }
            }
            // `EnumHOW.compose` on a MOP-built enum records the composition,
            // then carries on into the shared ClassHOW `compose` (MRO rebuild).
            // Cost: O(1) here.
            "compose" => {
                let name = self.enum_how_type_name(first);
                if let Some(state) = self.registry_mut().how_enums.get_mut(&name) {
                    state.composed = true;
                }
                return None;
            }
            _ => return None,
        })
    }

    /// Whether the enum value or declared enum type object `value` does the
    /// marker role `role` (`NumericEnumeration` / `StringyEnumeration`) that
    /// Rakudo mixes into an enum by the kind of its values. A type object
    /// answers by its first value, which fixes the kind for the whole enum.
    // Cost: O(1) (one registry lookup).
    pub(crate) fn enum_does_marker_role(&self, value: &Value, role: &str) -> bool {
        if role != "NumericEnumeration" && role != "StringyEnumeration" {
            return false;
        }
        match value.view() {
            ValueView::Enum { value, .. } => value.marker_role() == Some(role),
            ValueView::Package(name) => self
                .registry()
                .enum_types
                .get(&*name.resolve())
                .and_then(|variants| variants.first())
                .is_some_and(|(_, first)| first.marker_role() == Some(role)),
            _ => false,
        }
    }

    /// The error a non-enum HOW answers for an EnumHOW-only method. Raku
    /// reports these as an unresolvable caller on the HOW itself (e.g.
    /// `elems(Perl6::Metamodel::ClassHOW:D: C:U)`); mutsu reports the
    /// equivalent missing-method error, naming the HOW that lacks it.
    pub(crate) fn enum_how_method_missing(method: &str, type_value: &Value) -> RuntimeError {
        let owner = match type_value.view() {
            ValueView::Package(name) => name.resolve(),
            _ => crate::runtime::utils::value_type_name(type_value).to_string(),
        };
        RuntimeError::new(format!(
            "Cannot resolve caller {method}({owner}.HOW: {owner}); \
             {method} is only defined on Metamodel::EnumHOW"
        ))
    }
}
