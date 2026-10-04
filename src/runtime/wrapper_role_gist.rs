//! `.gist` for an exception carrying one of rakudo's *wrapper* roles:
//! `X::Promise::Broken` or `X::React::Died`.
//!
//! Neither role wraps the exception in a new object. rakudo mixes the role
//! into the original exception on the way out -- `Promise.result` on a broken
//! promise rethrows its cause `but X::Promise::Broken` (see
//! `promise_errors.rs`), and a `react` block that dies rethrows the exception
//! `but X::React::Died` (see `react_died.rs`) -- so the type checks,
//! `.message` and `.Str` all still answer the original exception's. The role
//! overrides `gist` alone, to explain *why* the exception is surfacing here:
//!
//! ```text
//! Tried to get the result of a broken Promise     A react block:
//!   in block <unit> at f.raku line 1                in block <unit> at f.raku line 1
//!
//! Original exception:                             Died because of the exception:
//!     oh no                                           boom
//!       in block <unit> at f.raku line 1                in block  at f.raku line 1
//! ```
//!
//! The body after the intro line is the base exception's own gist, indented
//! by four -- rakudo writes it as `callsame().indent(4)`, and this module
//! reproduces that by re-dispatching `gist` on the same value with this one
//! role peeled back out of its mixin map.

use super::*;
use crate::meta_ns::MetaNs;

/// The role `Promise.result` composes into a broken promise's cause.
pub(crate) const PROMISE_BROKEN_ROLE: &str = "X::Promise::Broken";

/// The role a dying `react` block composes into the exception it rethrows.
pub(crate) const REACT_DIED_ROLE: &str = "X::React::Died";

/// `X::React::Died`'s own attribute (rakudo: `has $.react-backtrace`): where
/// the react block was when it died, as opposed to the exception's own
/// backtrace (where the `whenever` body threw).
pub(crate) const REACT_BACKTRACE_ATTR: &str = "react-backtrace";

/// One wrapper role's gist shape.
struct WrapperRole {
    role: &'static str,
    header: &'static str,
    intro: &'static str,
    /// A role attribute holding the backtrace printed under the header; when
    /// absent (or `None`), the exception's own backtrace is printed there.
    header_backtrace_attr: Option<&'static str>,
}

const WRAPPER_ROLES: [WrapperRole; 2] = [
    WrapperRole {
        role: PROMISE_BROKEN_ROLE,
        header: "Tried to get the result of a broken Promise",
        intro: "Original exception:",
        header_backtrace_attr: None,
    },
    WrapperRole {
        role: REACT_DIED_ROLE,
        header: "A react block:",
        intro: "Died because of the exception:",
        header_backtrace_attr: Some(REACT_BACKTRACE_ATTR),
    },
];

/// `compose_role_on_value` records a composed role as a set of
/// `__mutsu_role*__<name>` entries in the value's mixin map. These are the
/// keys that belong to one role, so peeling it back out is a key removal.
fn role_mixin_keys(role: &str) -> [String; 3] {
    [
        MetaNs::Role.owned_key_for_str(role),
        MetaNs::RoleSeq.owned_key_for_str(role),
        MetaNs::RoleTypeargs.owned_key_for_str(role),
    ]
}

impl Interpreter {
    /// The wrapper role composed into `target` most recently (the outermost
    /// rethrow), or `None` when it carries none of them.
    fn outermost_wrapper_role(target: &Value) -> Option<&'static WrapperRole> {
        let ValueView::Mixin(_, mixins) = target.view() else {
            return None;
        };
        WRAPPER_ROLES
            .iter()
            .filter(|w| mixins.contains_key(&MetaNs::Role.owned_key_for_str(w.role)))
            .max_by_key(|w| {
                mixins
                    .get(&MetaNs::RoleSeq.owned_key_for_str(w.role))
                    .and_then(Value::as_int)
                    .unwrap_or(0)
            })
    }

    /// The same value with `wrapper`'s role (and its attribute) peeled back
    /// off. Any *other* mixed-in role is kept -- only this one is removed, so
    /// the re-dispatch below is `callsame()` rather than "drop every mixin".
    fn wrapper_mixin_base(target: &Value, wrapper: &WrapperRole) -> Option<Value> {
        let ValueView::Mixin(inner, mixins) = target.view() else {
            return None;
        };
        let mut remaining = (**mixins).clone();
        for k in &role_mixin_keys(wrapper.role) {
            remaining.remove(k);
        }
        if let Some(attr) = wrapper.header_backtrace_attr {
            remaining.remove(&MetaNs::Attr.owned_key_for_str(attr));
        }
        let inner = inner.as_ref().clone();
        Some(if remaining.is_empty() {
            inner
        } else {
            Value::mixin_with_state(inner, remaining)
        })
    }

    /// The `.text` of the `Backtrace` held in a mixin's role attribute.
    fn mixin_attr_backtrace_text(target: &Value, attr: &str) -> Option<String> {
        let ValueView::Mixin(_, mixins) = target.view() else {
            return None;
        };
        let bt = mixins.get(&MetaNs::Attr.owned_key_for_str(attr))?;
        let ValueView::Instance { attributes, .. } = bt.view() else {
            return None;
        };
        let text = attributes.as_map().get("text")?.to_string_value();
        (!text.is_empty()).then_some(text)
    }

    /// `.gist` for an exception carrying a wrapper role, or `None` when
    /// `target` carries none (the ordinary dispatch path applies).
    pub(crate) fn wrapper_role_gist(
        &mut self,
        target: &Value,
    ) -> Option<Result<Value, RuntimeError>> {
        let wrapper = Self::outermost_wrapper_role(target)?;
        let base = Self::wrapper_mixin_base(target, wrapper)?;
        // Rakudo's `callsame()`. The peeled value no longer carries this
        // role, so this cannot recurse back into the same wrapper.
        // TODO: compile to bytecode -- `callsame` into the base exception's
        // `gist` goes through the slow method path.
        let inner = match self.call_method_with_values(base, "gist", vec![]) {
            Ok(v) => v.to_string_value(),
            Err(e) => return Some(Err(e)),
        };
        let header_bt = wrapper
            .header_backtrace_attr
            .and_then(|attr| Self::mixin_attr_backtrace_text(target, attr))
            .or_else(|| Self::exception_backtrace_text(target));
        Some(Ok(Value::str(render_wrapper_gist(
            wrapper,
            header_bt.as_deref(),
            &inner,
        ))))
    }
}

/// Lay out a wrapper role's gist: header, header backtrace, a blank line,
/// the intro line, then the base gist indented by four.
fn render_wrapper_gist(wrapper: &WrapperRole, header_bt: Option<&str>, inner: &str) -> String {
    let mut out = String::from(wrapper.header);
    if let Some(bt) = header_bt {
        out.push('\n');
        out.push_str(bt.trim_end_matches('\n'));
    }
    out.push_str("\n\n");
    out.push_str(wrapper.intro);
    out.push('\n');
    out.push_str(&indent_by_four(inner));
    out
}

/// Raku's `Str.indent(4)`: prefix every **non-empty** line with four spaces
/// (an empty line stays empty rather than gaining trailing whitespace).
fn indent_by_four(s: &str) -> String {
    s.split('\n')
        .map(|line| {
            if line.is_empty() {
                String::new()
            } else {
                format!("    {}", line)
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn role_keys_cover_the_three_composition_entries() {
        let keys = role_mixin_keys("R");
        assert_eq!(keys[0], "__mutsu_role__R");
        assert_eq!(keys[1], "__mutsu_role_seq__R");
        assert_eq!(keys[2], "__mutsu_role_typeargs__R");
    }

    #[test]
    fn indents_non_empty_lines_only() {
        assert_eq!(indent_by_four("a\n\nb"), "    a\n\n    b");
        assert_eq!(indent_by_four("x"), "    x");
    }

    #[test]
    fn react_died_gist_layout() {
        let gist = render_wrapper_gist(
            &WRAPPER_ROLES[1],
            Some("  in block <unit> at f line 1\n"),
            "boom\n  in block  at f line 2",
        );
        assert_eq!(
            gist,
            "A react block:\n  in block <unit> at f line 1\n\n\
             Died because of the exception:\n    boom\n      in block  at f line 2"
        );
    }
}
