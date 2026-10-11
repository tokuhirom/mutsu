//! `nqp::getattr` / `bindattr` of an `Array` / `Hash`'s `'$!descriptor'`
//! (ADR-0121 D3, #10956).
//!
//! Rakudo's descriptor is a mutable `ContainerDescriptor` the container owns.
//! mutsu keeps its pieces on the container's own data (`value_type`,
//! `default`, `descriptor_name`), so the descriptor handed out is a *live
//! view*: a `ContainerDescriptor` instance holding the container itself, whose
//! `.of` / `.default` read those fields at call time (see
//! [`CONTAINER_DESCRIPTOR_PRELUDE`]). A bind of a descriptor writes the same
//! fields back, so the descriptor and `.of` / `.default` cannot drift.
//!
//! `.name` is the container's stamped `descriptor_name`, else the bare sigil
//! rakudo prints for an anonymous container; the variable name a container was
//! declared under is not recoverable from the value alone.

use super::*;
use crate::ast::Stmt;
use crate::runtime::source_code_text::CodeText;

/// The `ContainerDescriptor` classes, in Raku. Injected into code that names
/// `$!descriptor`.
pub(super) const CONTAINER_DESCRIPTOR_PRELUDE: &str = r#"
class GLOBAL::ContainerDescriptor {
    has $!container;
    has $!of;
    has $!default;
    has $!name;
    has $!dynamic;
    submethod BUILD(:$!of = Mu, :$!default = Any, :$!name = '<anon>', :$!dynamic = False) { }
    method of { $!container.DEFINITE ?? $!container.of !! $!of }
    method default { $!container.DEFINITE ?? $!container.default !! $!default }
    method name { $!name }
    method dynamic { $!dynamic }
    method !sync {
        nqp::bindattr($!container, Mu, '$!descriptor', self) if $!container.DEFINITE;
        self
    }
    method set_of(Mu $of) { $!of = $of; self!sync; Nil }
    method set_default(Mu $default) { $!default = $default; self!sync; Nil }
    method set_dynamic($dynamic) { $!dynamic = so $dynamic; Nil }
}
class GLOBAL::ContainerDescriptor::Untyped is ContainerDescriptor { }
"#;

/// The type a descriptor's `of` names, when it constrains anything.
fn constraint_name(of: &Value) -> Option<String> {
    match of.view() {
        ValueView::Package(sym) if !matches!(sym.as_str(), "Mu" | "Any") => {
            Some(sym.as_str().to_string())
        }
        _ => None,
    }
}

impl Interpreter {
    /// The `'$!descriptor'` of an Array or Hash; `None` for any other receiver.
    // Cost: O(1).
    pub(super) fn nqp_container_descriptor(obj: &Value) -> Option<Value> {
        let (of, default_value, name, sigil) = match obj.view() {
            ValueView::Array(data, _) => (
                data.value_type.clone(),
                data.default.clone(),
                data.descriptor_name.as_ref().map(|s| s.to_string()),
                "@",
            ),
            ValueView::Hash(data) => (
                data.value_type.clone(),
                data.default.clone(),
                data.descriptor_name.as_ref().map(|s| s.to_string()),
                "%",
            ),
            _ => return None,
        };
        let typed = of.as_deref().is_some_and(|t| !matches!(t, "Mu" | "Any"));
        let class = if typed {
            "ContainerDescriptor"
        } else {
            "ContainerDescriptor::Untyped"
        };
        let mut attrs = crate::value::ValueMap::default();
        attrs.insert("container".to_string(), obj.clone());
        attrs.insert(
            "name".to_string(),
            Value::str(name.unwrap_or_else(|| sigil.to_string())),
        );
        attrs.insert("dynamic".to_string(), Value::FALSE);
        // The descriptor's own `of` / `default`: what the container holds now,
        // so binding the descriptor to another container copies them. (A
        // descriptor's own state also has to exist for `set_of` and friends
        // to persist -- an attribute write only updates keys already present.)
        let package = |name: &str| Value::package(Symbol::intern(name));
        attrs.insert(
            "of".to_string(),
            package(of.as_deref().unwrap_or("Mu")),
        );
        attrs.insert(
            "default".to_string(),
            match default_value {
                Some(v) => *v,
                None => package(of.as_deref().unwrap_or("Any")),
            },
        );
        Some(Value::make_instance(Symbol::intern(class), attrs))
    }

    /// `nqp::bindattr($container, Type, '$!descriptor', $descriptor)`: install
    /// the descriptor's element constraint and default on the container.
    // Cost: O(1).
    pub(super) fn nqp_bind_container_descriptor(
        container: &Value,
        desc: &Value,
    ) -> Result<(), RuntimeError> {
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = desc.view()
        else {
            return Err(RuntimeError::new(
                "nqp::bindattr: a container's '$!descriptor' must be bound to a ContainerDescriptor"
                    .to_string(),
            ));
        };
        if !class_name.as_str().starts_with("ContainerDescriptor") {
            return Err(RuntimeError::new(format!(
                "nqp::bindattr: a container's '$!descriptor' must be bound to a ContainerDescriptor, not {class_name}"
            )));
        }
        let map = attributes.as_map();
        let owner = Some(Symbol::intern("ContainerDescriptor"));
        let attr = |name: &str| {
            Self::attr_key_in_map(owner, Symbol::intern(name), true, '$', &map)
                .and_then(|key| map.get(key))
        };
        let of = attr("of").and_then(constraint_name);
        let default = attr("default")
            .filter(|d| !matches!(d.view(), ValueView::Package(_)))
            .map(|d| Box::new(d.clone()));
        match container.view() {
            ValueView::Array(data, _) => {
                // SAFETY: audited aliased in-place container write (see
                // value::aliased_mut); no borrow into the node is live.
                let data = unsafe { crate::value::gc_contents_mut(&data) };
                data.value_type = of;
                data.default = default;
            }
            ValueView::Hash(data) => {
                // SAFETY: as above.
                let data = unsafe { crate::value::gc_contents_mut(&data) };
                data.value_type = of;
                data.default = default;
            }
            _ => {}
        }
        Ok(())
    }

    /// Prepend [`CONTAINER_DESCRIPTOR_PRELUDE`] to code that reaches for a
    /// container's `$!descriptor`. Skipped when it declares its own.
    // Cost: O(n), n = source bytes (one substring search when absent).
    pub(super) fn inject_container_descriptor_prelude(
        source: &CodeText<'_>,
        stmts: &mut Vec<Stmt>,
    ) {
        if !source.contains("$!descriptor") || source.contains("class ContainerDescriptor") {
            return;
        }
        use std::sync::OnceLock;
        static STMTS: OnceLock<Vec<Stmt>> = OnceLock::new();
        let prelude = STMTS.get_or_init(|| {
            crate::runtime::prelude_source::parse_prelude_source(CONTAINER_DESCRIPTOR_PRELUDE)
                .map(|(s, _)| s)
                .unwrap_or_default()
        });
        if prelude.is_empty() {
            return;
        }
        let mut combined = prelude.clone();
        combined.append(stmts);
        *stmts = combined;
    }
}
