//! The runtime half of the NQP container tests `nqp::iscont`,
//! `nqp::iscont_i` / `_n` / `_s` and `nqp::isrwcont` (ADR-11553 §3.2, #12673).
//!
//! The compiler (`compiler::nqp_container_test`) answers a native-typed
//! variable statically and hands everything else here as the operand's
//! `.VAR` (or, for an expression, the value itself) plus the variable's name
//! when there is one. These ops are exempt from the nqp boundary's Proxy
//! FETCH, so a `Proxy` or a native element reference arrives as itself.

use super::*;
use crate::value::ValueView;

/// The native kind (`'i'`, `'n'`, `'s'`) a native reference's class name
/// denotes: `IntPosRef`, `UIntLexRef`, `NumAttrRef`, `StrPosRef`, ...
// Cost: O(1).
fn native_ref_kind(class: &str) -> Option<char> {
    let stem = ["PosRef", "LexRef", "AttrRef", "MultidimRef"]
        .iter()
        .find_map(|suffix| class.strip_suffix(suffix))?;
    match stem {
        "Int" | "UInt" => Some('i'),
        "Num" => Some('n'),
        "Str" => Some('s'),
        _ => None,
    }
}

/// Whether `v` is a container and, if it is a native reference, its kind.
// Cost: O(1).
fn classify_container(v: &Value) -> (bool, Option<char>) {
    match v.view() {
        ValueView::Proxy { subclass, .. } => (
            true,
            subclass
                .as_ref()
                .and_then(|(name, _)| native_ref_kind(name.as_str())),
        ),
        ValueView::Instance { class_name, .. } => {
            let name = class_name.resolve();
            let kind = native_ref_kind(&name);
            (name == "Scalar" || name == "Proxy" || kind.is_some(), kind)
        }
        ValueView::Package(name) => (name == "Scalar", None),
        // A shared element cell handed out as a value (an Array slot read
        // through `$!reified`, `atposref`) is that element's Scalar.
        ValueView::ContainerRef(_) => (true, None),
        _ => (false, None),
    }
}

/// The native kind a declared type stores.
// Cost: O(1).
fn native_kind_of_type(constraint: &str) -> Option<char> {
    if crate::runtime::native_types::is_native_int_type(constraint) {
        Some('i')
    } else if matches!(constraint, "num" | "num32" | "num64") {
        Some('n')
    } else if constraint == "str" {
        Some('s')
    } else {
        None
    }
}

impl Interpreter {
    /// One of the five container tests over its compiled operands: the
    /// operand (`.VAR` of an lvalue) and, for a variable, its name.
    // Cost: O(1); O(d) for an attribute, d = ancestors searched for its type.
    pub(crate) fn nqp_container_test(&mut self, op: &str, args: &[Value]) -> bool {
        let operand = args.first().cloned().unwrap_or(Value::NIL);
        let name = args.get(1).map(|n| n.to_string_value());
        let (is_container, mut kind) = classify_container(&operand);
        // A native attribute's `.VAR` is its element container; its declared
        // type says which native kind lives there.
        if is_container
            && kind.is_none()
            && let Some(attr) = name.as_deref().and_then(|n| n.strip_prefix(['!', '.']))
        {
            kind = self.native_attribute_kind(attr);
        }
        match op {
            "iscont" => is_container,
            // A read-only binding (a plain parameter bound to the caller's
            // container) still has a container, but not a writable one.
            "isrwcont" => {
                is_container
                    && !self.private_attr_is_bound(name.as_deref())
                    && name
                        .as_deref()
                        .is_none_or(|n| n.starts_with(['!', '.']) || !self.is_readonly(n))
            }
            "iscont_i" => kind == Some('i'),
            "iscont_n" => kind == Some('n'),
            "iscont_s" => kind == Some('s'),
            _ => false,
        }
    }

    /// Whether `$!attr` of the current invocant was bound (`:=`) to a plain
    /// value, which leaves it without a writable container.
    // Cost: O(1).
    fn private_attr_is_bound(&self, name: Option<&str>) -> bool {
        let Some(attr) = name.and_then(|n| n.strip_prefix('!')) else {
            return false;
        };
        let Some(invocant) = self.env_self_value() else {
            return false;
        };
        let marker = crate::meta_ns::MetaNs::BoundAttr.key_for_str(attr);
        self.with_self_attr(
            &invocant,
            None,
            crate::symbol::Symbol::intern(attr),
            true,
            '$',
            |attributes, _, _| attributes.contains_key(marker),
        )
        .unwrap_or(false)
    }

    /// The native kind of attribute `attr` of the current invocant's class.
    // Cost: O(d), d = ancestors of the class.
    fn native_attribute_kind(&self, attr: &str) -> Option<char> {
        let invocant = self.env_self_value()?;
        let class = match invocant.view() {
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            ValueView::Package(name) => name.resolve(),
            _ => return None,
        };
        let constraint = self.get_attr_type_constraint(&class, attr)?;
        native_kind_of_type(&constraint)
    }

    /// Whether nqp op `id` must see its operands unFETCHed: the container
    /// tests ask about the `Proxy` itself, and `decont` hands it back unfetched
    /// (rakudo; AttrX::Mooish's FETCH relies on it); the attribute ops take the object they
    /// read or write as it is, and `bindattr` stores its value operand as it is, so
    /// a `Proxy` becomes the attribute's container.
    // Cost: O(1).
    pub(crate) fn nqp_op_keeps_proxy_operands(id: u16) -> bool {
        matches!(
            crate::runtime::nqp_op_ids::nqp_op_name(id),
            "iscont" | "iscont_i" | "iscont_n" | "iscont_s" | "isrwcont" | "decont" | "istype_nd"
            | "getattr" | "getattr_i" | "getattr_n" | "getattr_s"
            | "bindattr" | "bindattr_i" | "bindattr_n" | "bindattr_s" | "p6bindattrinvres"
        )
    }
}
