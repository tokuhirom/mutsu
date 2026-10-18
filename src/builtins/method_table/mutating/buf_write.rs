//! `Buf`'s `write-*` family (ADR-11276 §9.170): `write-int8` .. `write-int128`,
//! `write-uint8` .. `write-uint128`, `write-num32`, `write-num64`,
//! `write-bits` and `write-ubits`.
//!
//! Rakudo declares them on `Buf`, as a `Buf:U` candidate that answers a fresh
//! buffer and a `Buf:D` one that writes the receiver in place; a `Blob` or an
//! encoding buffer has none (`X::Method::NotFound`). The bytes are computed by
//! the one shared transform (`buf_write_int::apply_buf_write`) and committed
//! into the receiver's shared attribute cell, so every holder of the buffer
//! sees the write whether or not the call named a binding.

use crate::builtins::method_table::{Handler, MethodRow, Named, ReceiverPlace, RowFlags};
use crate::runtime::Interpreter;
use crate::value::value_buf::{buf_raw_bytes_or_empty, set_buf_raw_bytes};
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($name:literal) => {
        MethodRow {
            owner: "Buf",
            name: $name,
            arity: 0,
            handler: Handler::Mut({
                // Cost: see `run`.
                fn handler(
                    interp: &mut Interpreter,
                    place: &mut ReceiverPlace<'_>,
                    args: &[Value],
                    _named: Named<'_>,
                ) -> Option<Result<Value, RuntimeError>> {
                    run(interp, place, $name, args)
                }
                handler
            }),
            // The handler checks the count against Rakudo's signature, so a
            // wrong one raises its bind error rather than declining.
            flags: RowFlags::SLURPY.or(RowFlags::TYPE_OBJECT_OK),
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("write-uint8"),
    row!("write-int8"),
    row!("write-uint16"),
    row!("write-int16"),
    row!("write-uint32"),
    row!("write-int32"),
    row!("write-uint64"),
    row!("write-int64"),
    row!("write-uint128"),
    row!("write-int128"),
    row!("write-num32"),
    row!("write-num64"),
    row!("write-bits"),
    row!("write-ubits"),
];

/// `method(args)` on the `Buf` the place holds: a type object answers a fresh
/// buffer of its type, an instance is written in place and answered.
// Cost: O(e + w), e = the receiver's elements (decoded once), w = bytes
// written.
fn run(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    method: &str,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let target = place.value().descalarize().clone();
    match target.view() {
        ValueView::Package(name) => {
            let class_name = name.resolve();
            if !crate::runtime::utils::is_buf_like_class(&class_name) {
                return None;
            }
            let bytes = crate::builtins::buf_write_int::apply_buf_write(
                &class_name,
                Vec::new(),
                method,
                args,
            );
            Some(bytes.map(|bytes| {
                let normalized = crate::runtime::utils::normalize_buf_type_name(&class_name);
                crate::builtins::buf_write_num::make_buf_value_from_raw(&normalized, bytes)
            }))
        }
        ValueView::Instance {
            class_name,
            attributes,
            id,
        } => {
            let resolved = class_name.resolve();
            if !crate::runtime::utils::is_buf_like_class(&resolved) {
                return None;
            }
            let bytes = match crate::builtins::buf_write_int::apply_buf_write(
                &resolved,
                buf_raw_bytes_or_empty(&attributes),
                method,
                args,
            ) {
                Ok(bytes) => bytes,
                Err(err) => return Some(Err(err)),
            };
            let mut updated = attributes.to_map();
            set_buf_raw_bytes(&mut updated, class_name, bytes);
            let updated = Value::write_back_sharing(&attributes, class_name, updated, id);
            // Re-seat a named binding through its shared cell, as the other
            // `Buf` mutators do (`buf_mutate_method`): a closure's overlay must
            // not keep the pre-write value.
            if let Some(name) = place.name().filter(|name| !name.is_empty()) {
                interp
                    .env_mut()
                    .insert_through(name.to_string(), updated.clone());
            }
            Some(Ok(updated))
        }
        _ => None,
    }
}
