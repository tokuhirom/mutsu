//! `Parameter`'s accessor rows (#15660): the properties of one signature
//! parameter, each an attribute the compiler set when it built the `Parameter`
//! (`build_parameter_attrs`), so `.can`, `.^can` and `.^methods` report them.
//!
//! A `Parameter` has an opaque shape, so every entry reaches these rows through
//! the table. A property the instance does not carry reads as `Nil`.

use super::{Handler, MethodRow, RowFlags};
use crate::value::sig_elem_flags::IS_OPTIONAL;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

/// A zero-argument accessor row named `$name` reading the attribute `$key`.
macro_rules! accessor {
    ($($name:literal => $handler:ident;)*) => {
        pub(super) static ROWS: &[MethodRow] = &[
            $(MethodRow {
                owner: "Parameter",
                name: $name,
                arity: 0,
                handler: Handler::Narrow($handler),
                flags: RowFlags::NONE,
                named: &[],
            },)*
        ];
        $(
            // Cost: O(1).
            fn $handler(target: &Value, _args: &[Value]) -> Answer {
                attribute(target, $name)
            }
        )*
    };
}

accessor! {
    "capture" => capture;
    "coerce_type" => coerce_type;
    "constraint_list" => constraint_list;
    "constraints" => constraints;
    "copy" => copy;
    "default" => default;
    "invocant" => invocant;
    "is-item" => is_item;
    "modifier" => modifier;
    "multi-invocant" => multi_invocant;
    "name" => name;
    "named" => named;
    "named_names" => named_names;
    "nominal_type" => nominal_type;
    "of" => of;
    "onearg" => onearg;
    "optional" => optional;
    "positional" => positional;
    "prefix" => prefix;
    "raw" => raw;
    "readonly" => readonly;
    "rw" => rw;
    "sigil" => sigil;
    "signature_constraint" => signature_constraint;
    "slurpy" => slurpy;
    "sub_signature" => sub_signature;
    "suffix" => suffix;
    "twigil" => twigil;
    "type" => type_;
    "type_captures" => type_captures;
    "untyped" => untyped;
    "usage-name" => usage_name;
}

/// The attribute `key` of the `Parameter` instance, `Nil` when it carries none.
fn attribute(target: &Value, key: &str) -> Answer {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    let map = attributes.as_map();
    // `.optional` is the `SIG_ELEM_IS_OPTIONAL` bit of `$!flags`, so a trait
    // that sets the bit through the MOP changes what the parameter reports.
    if key == "optional"
        && let Some(flags) = map.get("flags")
        && let ValueView::Int(bits) = flags.deref_container().view()
    {
        return Some(Ok(if bits & IS_OPTIONAL != 0 {
            Value::TRUE
        } else {
            Value::FALSE
        }));
    }
    Some(Ok(map.get(key).cloned().unwrap_or(Value::NIL)))
}
