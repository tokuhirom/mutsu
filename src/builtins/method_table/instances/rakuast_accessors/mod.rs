//! Static RakuAST accessor rows and immutable model-class owner chains.
//!
//! The generated rows carry their field name in a plain closure. They use the
//! same reader as reflection; no name-matching entry or runtime table builder
//! selects an accessor. A per-class symbol memo caches only static row ids,
//! including a nearest interpreter row, never a node's mutable field values.

use super::super::owner_pure::{invoke_selected, resolve_owner_row};
use super::super::{Handler, MethodRow, Named, RowFlags, RowId};
use super::rakuast_node::{parameter_meta_object, sub_value};
use super::rakuast_visit::native_node;
use crate::runtime::Interpreter;
use crate::symbol::{Symbol, SymbolMemo};
use crate::value::{RuntimeError, Value, ValueView};
use std::sync::OnceLock;

macro_rules! row {
    ($owner:literal, $name:literal) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Narrow(|target, args| read(target, $name, args)),
            flags: RowFlags::OWNER_ONLY,
            named: &[],
        }
    };
}

macro_rules! interp_row {
    ($owner:literal, $name:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Interp($handler),
            flags: RowFlags::OWNER_ONLY,
            named: &[],
        }
    };
}

type OwnerChain = (&'static str, &'static [&'static str]);
include!("registry.rs");
include!("owner_registry.rs");

// Cost: O(log c), c = native model classes; computed once per class symbol.
fn class_index(class: &str) -> u16 {
    OWNER_CHAINS
        .binary_search_by_key(&class, |chain| chain.0)
        .ok()
        .and_then(|index| u16::try_from(index + 1).ok())
        .unwrap_or(0)
}

/// Every pure entry uses the same registered owner row. The cache is over
/// immutable model class metadata; a user override is resolved before here.
// Cost: O(d) plus the handler on a memo hit, d = payload wrapper depth; O(o * k) once per class/method, o = owners, k = same-named rows.
pub(crate) fn pure_answer(target: &Value, method: Symbol) -> Option<Result<Value, RuntimeError>> {
    if !super::super::names_a_row(method, 0) {
        return None;
    }
    let target = native_node(target)?;
    let ValueView::RakuAst(node) = target.view() else {
        return None;
    };
    static CLASSES: SymbolMemo = SymbolMemo::new();
    let index = CLASSES.get(Symbol::intern(node.class.printed_name()), class_index);
    let index = usize::from(index.checked_sub(1)?);
    let owners = OWNER_CHAINS.get(index)?.1;
    static METHODS: [OnceLock<Box<SymbolMemo>>; OWNER_N] = [const { OnceLock::new() }; OWNER_N];
    let memo = METHODS[index].get_or_init(|| Box::new(SymbolMemo::new()));
    const UNCACHED: u16 = (1 << 15) - 1;
    let selected = memo.get_with(method, |_| {
        resolve_owner_row(owners, method).map_or(0, |id| {
            let id = id.to_bits();
            if id < UNCACHED - 1 { id + 1 } else { UNCACHED }
        })
    });
    let id = match selected {
        0 => return None,
        UNCACHED => resolve_owner_row(owners, method)?,
        id => RowId::from_bits(id - 1),
    };
    invoke_selected(id, &target)
}

// Cost: O(d + f + c + b), d = wrappers, f = fields scanned, c = copied children, b = produced text bytes. Rakudo: O(c + b) -- see #16130.
fn read(target: &Value, name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() {
        return None;
    }
    let native = native_node(target)?;
    let ValueView::RakuAst(node) = native.view() else {
        return None;
    };
    crate::rakuast::node_accessor(node, name).map(Ok)
}

// Cost: O(d) plus evaluating the node, d = payload wrappers.
fn compiled_value(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let target = native_node(target)?;
    sub_value(interp, &target, args, named)
}

// Cost: O(d + n), d = payload wrappers, n = parameter node size.
fn meta_object(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let target = native_node(target)?;
    parameter_meta_object(interp, &target, args, named)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rakuast::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};

    fn literal(value: i64) -> Value {
        Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::IntLiteral,
            origin: Default::default(),
            fields: vec![RakuAstField {
                name: None,
                value: RakuAstFieldValue::Node(Value::int(value)),
            }],
        }))
    }

    #[test]
    fn rakuast_accessor_entry_caches_rows_not_field_values() {
        let method = Symbol::intern("value");
        for expected in [7, 9, 7] {
            assert!(matches!(pure_answer(&literal(expected), method),
                Some(Ok(value)) if value == Value::int(expected)));
        }
        let name = Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::VarLexical,
            origin: Default::default(),
            fields: vec![RakuAstField {
                name: None,
                value: RakuAstFieldValue::Node(Value::str_from("$x")),
            }],
        }));
        for _ in 0..2 {
            assert!(pure_answer(&name, method).is_none());
            assert!(matches!(pure_answer(&name, Symbol::intern("name")),
                Some(Ok(value)) if value == Value::str_from("$x")));
        }
    }

    #[test]
    fn rakuast_accessor_entry_reads_subclass_payloads() {
        let instance = Value::make_instance(
            Symbol::intern("CustomLiteral"),
            crate::value::AttrMap::new(),
        );
        if let ValueView::Instance { attributes, .. } = instance.view() {
            attributes.set_rakuast_base(literal(13));
        }
        assert!(matches!(pure_answer(&instance, Symbol::intern("value")),
            Some(Ok(value)) if value == Value::int(13)));
    }

    #[test]
    fn rakuast_accessor_rows_cover_the_model_schema_and_measured_owners() {
        let expected: std::collections::HashSet<_> = crate::rakuast::accessor_schema()
            .into_iter()
            .flat_map(|(class, _, names)| {
                assert!(
                    class_index(class) != 0,
                    "model owner chain missing: {class}"
                );
                names.into_iter().map(move |name| (class, name))
            })
            .collect();
        let mut actual = std::collections::HashSet::new();
        for (model, name, owner) in include_str!("owners.tsv")
            .lines()
            .filter(|line| !line.starts_with('#'))
            .filter_map(|line| {
                let mut fields = line.split('\t');
                Some((fields.next()?, fields.next()?, fields.next()?))
            })
        {
            actual.insert((model, name));
            assert!(
                super::super::super::owner_row(Symbol::intern(owner), Symbol::intern(name), 0)
                    .is_some(),
                "declared accessor row missing: {owner}.{name}"
            );
        }
        assert_eq!(
            expected, actual,
            "regenerate RakuAST accessor rows after changing the schema"
        );
    }
}
