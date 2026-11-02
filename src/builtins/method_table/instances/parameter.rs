//! `Parameter`'s accessor rows (#15660): the properties of one signature
//! parameter, each an attribute the compiler set when it built the `Parameter`
//! (`build_parameter_attrs`), so `.can`, `.^can` and `.^methods` report them.
//!
//! A `Parameter` has an opaque shape, so every entry reaches these rows through
//! the table. A property the instance does not carry reads as `Nil`.

use super::{Handler, MethodRow, RowFlags};
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
    "constraint_list" => constraint_list;
    "constraints" => constraints;
    "copy" => copy;
    "default" => default;
    "invocant" => invocant;
    "modifier" => modifier;
    "multi-invocant" => multi_invocant;
    "name" => name;
    "named" => named;
    "named_names" => named_names;
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
    "usage-name" => usage_name;
}

/// The attribute `key` of the `Parameter` instance, `Nil` when it carries none.
fn attribute(target: &Value, key: &str) -> Answer {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    Some(Ok(attributes
        .as_map()
        .get(key)
        .cloned()
        .unwrap_or(Value::NIL)))
}
