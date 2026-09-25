//! `nqp::getattr` / `nqp::bindattr` with the attribute name resolved where
//! the site is compiled (ADR-0121 D1/D3).
//!
//! The generic ops in `nqp_ops_builtin.rs` take the name as a `Value` and
//! re-derive everything from its text on every call: `string_value_cow`, the
//! twigil strip, a string-keyed attribute probe, and before any of that the
//! op-name `match` of the dispatch table. A site whose name operand is a
//! literal knows all of it up front. [`NqpAttrName`] carries it, and the two
//! entry points below answer a plain instance straight from its attribute
//! store; every other receiver (a List/Map's storage, a Match, a lazy
//! Match) takes the generic body with the name as written, so the two paths
//! cannot disagree.

use super::*;

/// An `nqp::getattr`/`bindattr` attribute-name operand, resolved once.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct NqpAttrName {
    /// The operand as written (`'$!storage'`).
    pub(crate) name: Symbol,
    /// The key a read probes first: the name without its twigil, the key
    /// mutsu's instance maps store it under (see `nqp_attr_bare`).
    pub(crate) read_key: Symbol,
    /// The key a bind writes: every leading sigil and twigil stripped (see
    /// `nqp_bindattr_value`). `None` for a name that strips to nothing, which
    /// the bind reports as an error.
    pub(crate) write_key: Option<Symbol>,
}

impl NqpAttrName {
    // Cost: O(n), n = chars of the name; paid once per compiled site.
    pub(crate) fn new(name: &str) -> Self {
        let write = name
            .trim_start_matches(['$', '@', '%', '&'])
            .trim_start_matches(['!', '.']);
        Self {
            name: Symbol::intern(name),
            read_key: Symbol::intern(Interpreter::nqp_attr_bare(name)),
            write_key: (!write.is_empty()).then(|| Symbol::intern(write)),
        }
    }
}

/// Which of an attribute op's four forms a site is: the plain op, or its
/// `_i` / `_n` / `_s` native variant.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum NqpAttrConv {
    Obj,
    Int,
    Num,
    Str,
}

impl NqpAttrConv {
    /// The form of op `op` (`getattr_i`, `bindattr`, ...).
    pub(crate) fn of_op(op: &str) -> Self {
        if op.ends_with("_i") {
            Self::Int
        } else if op.ends_with("_n") {
            Self::Num
        } else if op.ends_with("_s") {
            Self::Str
        } else {
            Self::Obj
        }
    }

    /// What a read of this form hands back for the attribute value `value`
    /// (`None`: no such attribute): a native read answers its zero value.
    // Cost: O(1); the Str form adds O(n), n = chars of the value.
    pub(crate) fn read(self, value: Option<Value>) -> Value {
        match self {
            Self::Obj => value.unwrap_or(Value::NIL),
            Self::Int => Value::int(value.as_ref().map(to_int).unwrap_or(0)),
            Self::Num => Value::num(value.as_ref().map(|v| v.to_f64()).unwrap_or(0.0)),
            Self::Str => Value::str(
                value
                    .as_ref()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default(),
            ),
        }
    }

    /// The bind op of this form, as an error message names it.
    pub(crate) fn bind_op(self) -> &'static str {
        match self {
            Self::Obj => "bindattr",
            Self::Int => "bindattr_i",
            Self::Num => "bindattr_n",
            Self::Str => "bindattr_s",
        }
    }

    /// The value a bind of this form stores for operand `raw`: an nqp
    /// `int`/`num`/`str` attribute holds a native value, and code that reads
    /// it back with `getattr_i` expects one.
    // Cost: O(1); the Str form adds O(n), n = chars of the value.
    pub(crate) fn bind(self, raw: Value) -> Value {
        match self {
            Self::Obj => raw,
            Self::Int => Value::int(to_int(&raw)),
            Self::Num => Value::num(raw.to_f64()),
            Self::Str => Value::str(raw.to_string_value()),
        }
    }
}

impl Interpreter {
    /// `nqp::getattr` (and the typed reads, which convert the result) with a
    /// pre-resolved name. Same answer as the generic op for every receiver.
    // Cost: O(1) for a plain instance, one hash probe of an interned id (two when the
    // operand's twigil form is also probed); other receivers cost what the generic op does.
    pub(crate) fn nqp_getattr_named(obj: &Value, name: NqpAttrName) -> Option<Value> {
        // A plain instance only: a Match (lazy, a `Match` instance, or a
        // grammar cursor) has NQP-level attribute names that are not the keys
        // mutsu stores, so it takes the generic body.
        if !obj.is_lazy_match_value()
            && let ValueView::Instance {
                class_name,
                attributes,
                ..
            } = obj.view()
            && class_name != "Match"
        {
            let attrs = attributes.as_map();
            if !attrs.contains_key(crate::value::match_view::cursor_match_marker()) {
                return attrs
                    .get(name.read_key)
                    .or_else(|| {
                        if name.read_key == name.name {
                            None
                        } else {
                            attrs.get(name.name)
                        }
                    })
                    .cloned();
            }
        }
        Self::nqp_attr_value(obj, name.name.as_str())
    }

    /// `nqp::bindattr` with a pre-resolved name: the store of
    /// [`Self::nqp_bindattr_value`] for every receiver.
    // Cost: O(1) for a plain instance; other receivers cost what the generic op does.
    pub(crate) fn nqp_bindattr_named(
        op: &str,
        obj: &Value,
        name: NqpAttrName,
        val: Value,
    ) -> Result<(), RuntimeError> {
        if let Some(key) = name.write_key
            && !matches!(key.as_str(), "reified" | "storage")
            && let ValueView::Instance { attributes, .. } = obj.view()
        {
            attributes.bind_attr_through(key, val);
            return Ok(());
        }
        Self::nqp_bindattr_value(op, obj, name.name.as_str(), val)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn attr_names_resolve_like_the_generic_ops() {
        let n = NqpAttrName::new("@!items");
        assert_eq!(n.read_key, Symbol::intern("items"));
        assert_eq!(n.write_key, Some(Symbol::intern("items")));
        // No twigil: the name is its own key.
        let n = NqpAttrName::new("plain");
        assert_eq!(n.read_key, n.name);
        // A name that strips to nothing has no key to bind.
        assert_eq!(NqpAttrName::new("$!").write_key, None);
        // A public twigil is not stripped for a read (see `nqp_attr_bare`),
        // but is for a bind.
        let n = NqpAttrName::new("$.x");
        assert_eq!(n.read_key, Symbol::intern("$.x"));
        assert_eq!(n.write_key, Some(Symbol::intern("x")));
    }

    #[test]
    fn typed_forms_convert_like_the_generic_ops() {
        assert_eq!(NqpAttrConv::of_op("getattr_i"), NqpAttrConv::Int);
        assert_eq!(NqpAttrConv::of_op("bindattr_s"), NqpAttrConv::Str);
        assert_eq!(NqpAttrConv::of_op("getattr"), NqpAttrConv::Obj);
        assert_eq!(NqpAttrConv::Int.read(None).to_string_value(), "0");
        assert_eq!(NqpAttrConv::Num.read(None).to_f64(), 0.0);
        assert_eq!(NqpAttrConv::Str.read(None).to_string_value(), "");
        assert!(NqpAttrConv::Obj.read(None).is_nil());
        assert_eq!(
            NqpAttrConv::Int
                .bind(Value::str("7".to_string()))
                .to_string_value(),
            "7"
        );
        assert_eq!(NqpAttrConv::Str.bind_op(), "bindattr_s");
    }
}
