//! `Attribute`'s rows (ADR-11276 §9.121): the methods of the metaobject
//! `.^attributes` and a trait handler's `$attr` answer, which Rakudo declares
//! on `Attribute`.
//!
//! An `Attribute` is an instance whose attributes are the declaration's facts
//! (`name`, `type`, `is_rw`, `__mutsu_attr_owner`, ...). The class has no
//! dispatch shape; the rows are reached through their owner
//! (`RowFlags::OWNER_ONLY`, `invoke_owner`) from the instance dispatch, where the
//! arms that answered them by name were (`dispatch_instance_and_fallback`). The
//! methods that need the interpreter (the registry's build overrides, the
//! class table) are `Interpreter` methods in `runtime/methods_attribute_meta.rs`.

use super::super::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{InstanceAttrs, RuntimeError, Value, ValueView};
use std::collections::HashMap;

type Answer = Option<Result<Value, RuntimeError>>;

/// A row of `Attribute` taking `$arity` positional arguments.
macro_rules! row {
    ($name:literal, $arity:literal, $handler:expr) => {
        MethodRow {
            owner: "Attribute",
            name: $name,
            arity: $arity,
            handler: $handler,
            flags: if $arity == 0 {
                RowFlags::NONE
            } else {
                // The arguments are user objects (`get_value($obj)`).
                RowFlags::ANY_ARGS.or(RowFlags::OBJECT_ARGS)
            },
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("name", 0, Handler::Narrow(name)),
    row!("type", 0, Handler::Narrow(type_row)),
    row!("has_accessor", 0, Handler::Narrow(has_accessor)),
    row!("rw", 0, Handler::Narrow(rw)),
    row!("readonly", 0, Handler::Narrow(readonly)),
    row!("required", 0, Handler::Narrow(required)),
    row!("package", 0, Handler::Narrow(package)),
    row!("gist", 0, Handler::Narrow(gist)),
    row!("Str", 0, Handler::Narrow(str_row)),
    row!("container", 0, Handler::Narrow(container)),
    row!("auto_viv_container", 0, Handler::Interp(auto_viv_container)),
    row!("DEPRECATED", 0, Handler::Narrow(deprecated)),
    row!("set_rw", 0, Handler::Interp(set_rw)),
    row!("build", 0, Handler::Interp(build)),
    row!("set_build", 1, Handler::Interp(set_build)),
    row!("get_value", 1, Handler::Interp(get_value)),
    row!("set_value", 2, Handler::Interp(set_value)),
];

/// Runs `f` on the metaobject's facts; `None` for a receiver that is no
/// instance.
// Cost: O(1) plus `f`'s.
fn with_meta<R>(target: &Value, f: impl FnOnce(&InstanceAttrs) -> R) -> Option<R> {
    match target.view() {
        ValueView::Instance { attributes, .. } => Some(f(&attributes)),
        _ => None,
    }
}

/// The fact `key`, or `default` when it is not recorded.
// Cost: O(1).
fn fact(target: &Value, key: &str, default: Value) -> Option<Result<Value, RuntimeError>> {
    with_meta(target, |meta| {
        Ok(meta.as_map().get(key).cloned().unwrap_or(default))
    })
}

/// The fact `key` as a string, empty when it is not recorded.
// Cost: O(1).
fn fact_str(meta: &InstanceAttrs, key: &str) -> Option<String> {
    meta.as_map().get(key).map(|v| v.to_string_value())
}

fn mu() -> Value {
    Value::package(Symbol::intern("Mu"))
}

/// `Attribute.name`: the attribute's name with its sigil and twigil.
// Cost: O(1).
fn name(target: &Value, _args: &[Value]) -> Answer {
    fact(target, "name", Value::NIL)
}

/// `DEPRECATED` of an attribute declared `is DEPRECATED`: the message the
/// trait was given (empty for a bare trait). Rakudo mixes a role with this
/// method into the attribute; an attribute without the trait declines, so the
/// call finds no method.
// Cost: O(1).
fn deprecated(target: &Value, _args: &[Value]) -> Answer {
    with_meta(target, |meta| meta.as_map().get("DEPRECATED").cloned())?.map(Ok)
}

/// `Attribute.type`: the declared type constraint, `Mu` when there is none.
// Cost: O(1).
fn type_row(target: &Value, _args: &[Value]) -> Answer {
    fact(target, "type", mu())
}

/// `Attribute.has_accessor`: whether the attribute is public (`$.x`).
// Cost: O(1).
fn has_accessor(target: &Value, _args: &[Value]) -> Answer {
    fact(target, "has_accessor", Value::FALSE)
}

/// `Attribute.rw`: whether the attribute is `is rw`.
// Cost: O(1).
fn rw(target: &Value, _args: &[Value]) -> Answer {
    fact(target, "is_rw", Value::FALSE)
}

/// `Attribute.readonly`: the negation of [`rw`].
// Cost: O(1).
fn readonly(target: &Value, _args: &[Value]) -> Answer {
    with_meta(target, |meta| {
        let is_rw = meta.as_map().get("is_rw").is_some_and(|v| v.truthy());
        Ok(Value::truth(!is_rw))
    })
}

/// `Attribute.required`: `Mu` (not required), `1` (bare `is required`), or the
/// reason string (`is required("reason")`).
// Cost: O(1).
fn required(target: &Value, _args: &[Value]) -> Answer {
    fact(target, "required", mu())
}

/// `Attribute.package`: the type object of the class that declares it.
// Cost: O(1).
fn package(target: &Value, _args: &[Value]) -> Answer {
    with_meta(target, |meta| {
        let owner = fact_str(meta, "__mutsu_attr_owner").unwrap_or_else(|| "Any".to_string());
        Ok(Value::package(Symbol::intern(&owner)))
    })
}

/// `Attribute.gist`: the type and the name (`Int $!x`).
// Cost: O(1).
fn gist(target: &Value, _args: &[Value]) -> Answer {
    with_meta(target, |meta| {
        let type_name = meta
            .as_map()
            .get("type")
            .map(|v| match v.view() {
                ValueView::Package(name) => name.resolve(),
                _ => v.to_string_value(),
            })
            .unwrap_or_else(|| "Mu".to_string());
        let name = fact_str(meta, "name").unwrap_or_default();
        Ok(Value::str(format!("{} {}", type_name, name)))
    })
}

/// `Attribute.Str`: the name.
// Cost: O(1).
fn str_row(target: &Value, _args: &[Value]) -> Answer {
    with_meta(target, |meta| {
        Ok(Value::str(fact_str(meta, "name").unwrap_or_default()))
    })
}

/// `Attribute.container`: a per-attribute container descriptor tagged with
/// the owning class and attribute name, so that a custom `trait_mod:<is>` can
/// mix a role into it via `$a.container.VAR does Role(...)`. The recorded
/// mixin is applied to each instance's attribute value at construction (see
/// `apply_attribute_does_role_mixins`).
// Cost: O(1).
fn container(target: &Value, _args: &[Value]) -> Answer {
    with_meta(target, |meta| {
        let map = meta.as_map();
        let sigil = fact_str(meta, "sigil").unwrap_or_else(|| "$".to_string());
        let owner = fact_str(meta, "__mutsu_attr_owner").unwrap_or_default();
        let attr_name = map
            .get("__mutsu_attr_name")
            .or_else(|| map.get("name"))
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let mut cmeta = HashMap::new();
        cmeta.insert(
            "__mutsu_attr_container_owner".to_string(),
            Value::str(owner),
        );
        cmeta.insert(
            "__mutsu_attr_container_name".to_string(),
            Value::str(attr_name),
        );
        cmeta.insert(
            "__mutsu_attr_container_sigil".to_string(),
            Value::str(sigil),
        );
        Ok(Value::make_instance(Symbol::intern("Scalar"), cmeta))
    })
}

/// `Attribute.auto_viv_container`: a fresh container of the attribute's
/// kind, as an instance's attribute starts out: a Scalar holding the type's
/// default for `$`, an empty `Array[T]`/`Hash[T]` carrying the declared element
/// type for `@`/`%`.
// Cost: O(a), a = the owner's attributes (`seed_attr_value`) for `@`/`%`; O(1) for `$`.
fn auto_viv_container(
    i: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Answer {
    with_meta(target, |meta| {
        let sigil = fact_str(meta, "sigil").unwrap_or_else(|| "$".to_string());
        Ok(match sigil.as_str() {
            "@" | "%" => {
                let owner = fact_str(meta, "__mutsu_attr_owner").unwrap_or_default();
                let bare = fact_str(meta, "name")
                    .unwrap_or_default()
                    .trim_start_matches(['$', '@', '%', '&', '!', '.'])
                    .to_string();
                let sigil_char = sigil.chars().next().unwrap_or('@');
                let constraints = i.collect_attribute_type_constraints(&owner);
                i.seed_attr_value(&owner, &bare, sigil_char, &constraints)
            }
            _ => {
                let default = match meta.as_map().get("type").map(Value::view) {
                    Some(ValueView::Package(name)) if name != "Mu" => Value::package(name),
                    _ => Value::package(Symbol::intern("Any")),
                };
                Value::container_ref(crate::gc::Gc::new(crate::value::ContainerCell::new(
                    default,
                )))
            }
        })
    })
}

/// `Attribute.set_rw`: marks the attribute read-write. The generated accessor
/// for a `has $.x is rw` is already rw; for an attribute that a custom
/// `trait_mod:<is>` makes rw, the mark makes `.rw`/`.readonly` report it.
// Cost: O(1).
fn set_rw(_i: &mut Interpreter, target: &Value, _args: &[Value], _named: Named<'_>) -> Answer {
    with_meta(target, |meta| {
        meta.insert("is_rw".to_string(), Value::TRUE);
        Ok(target.clone())
    })
}

/// `Attribute.build`: what builds the attribute's value.
// Cost: O(1).
fn build(i: &mut Interpreter, target: &Value, _args: &[Value], _named: Named<'_>) -> Answer {
    with_meta(target, |meta| Ok(i.attribute_meta_build(meta)))
}

/// `Attribute.set_build(&build)`.
// Cost: O(1) amortized.
fn set_build(i: &mut Interpreter, target: &Value, args: &[Value], _named: Named<'_>) -> Answer {
    with_meta(target, |meta| {
        i.attribute_meta_set_build(target, meta, &args[0])
    })
}

/// `Attribute.get_value($obj)`.
// Cost: O(1).
fn get_value(i: &mut Interpreter, target: &Value, args: &[Value], _named: Named<'_>) -> Answer {
    with_meta(target, |meta| i.attribute_meta_get_value(meta, &args[0]))
}

/// `Attribute.set_value($obj, $value)`.
// Cost: O(1).
fn set_value(i: &mut Interpreter, target: &Value, args: &[Value], _named: Named<'_>) -> Answer {
    with_meta(target, |meta| {
        i.attribute_meta_set_value(meta, &args[0], &args[1])
    })
}
