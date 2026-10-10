//! Lexical label terms while lowering a constructed statement tree.

use std::cell::RefCell;

use crate::ast::Expr;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};

thread_local! {
    static LABELS: RefCell<Vec<(Symbol, Value)>> = const { RefCell::new(Vec::new()) };
}

pub(super) struct Scope(bool);

impl Drop for Scope {
    fn drop(&mut self) {
        if self.0 {
            LABELS.with(|labels| labels.borrow_mut().pop());
        }
    }
}

// Cost: O(n), n = label name length.
pub(super) fn enter(name: Option<&str>) -> Result<Scope, RuntimeError> {
    let Some(name) = name else {
        return Ok(Scope(false));
    };
    // A constructed tree has no source cursor. Parsed label terms already
    // carry their original Label value and retain its real source metadata.
    let value = crate::value::label::make_label(name, "", 0, "", "");
    LABELS.with(|labels| {
        let mut labels = labels.borrow_mut();
        labels
            .try_reserve(1)
            .map_err(|_| RuntimeError::new("RakuAST label scope is too large"))?;
        labels.push((Symbol::intern(name), value));
        Ok(Scope(true))
    })
}

// Cost: O(n + d), n = name length, d = labelled statement nesting depth.
pub(super) fn lookup(name: &str) -> Option<Value> {
    let name = Symbol::intern(name);
    LABELS.with(|labels| {
        labels
            .borrow()
            .iter()
            .rev()
            .find(|(label, _)| *label == name)
            .map(|(_, value)| value.clone())
    })
}

// Cost: O(n), n = label name length.
pub(super) fn expression_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::BareWord(name) => Some(name.clone()),
        Expr::Literal(value) => crate::value::label::label_name(value),
        _ => None,
    }
}
