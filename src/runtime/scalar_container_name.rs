//! The name a `$` variable's container reports (`$x.VAR.name`, and `dd`'s
//! `$x = ` prefix): the variable the container was declared as, which an
//! `is rw` / `is raw` / `\x` parameter or a `:=` alias shares with the caller.

use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{Value, ValueView};

/// The declared name of the container `v` holds, the aliased (inner)
/// container winning over a `:=` binding cell that wraps it.
// Cost: O(d), d = depth of nested binding cells.
fn declared_name(v: &Value) -> Option<Symbol> {
    match v.view() {
        ValueView::ContainerRef(cell) => {
            let inner = cell.load();
            if inner.is_container_ref() {
                declared_name(&inner).or_else(|| cell.descriptor_name())
            } else {
                cell.descriptor_name()
            }
        }
        _ => None,
    }
}

impl Interpreter {
    /// The name the container of the `$` variable `target_var` (holding
    /// `target`) was declared as, when it differs from what the key alone
    /// says: an `is rw` / `is raw` / `\x` parameter aliasing the caller's `$a`
    /// reports `$a` (#11196), and `my $c := $x` reports `$x` (#16931).
    /// `None` for an `@`/`%`/`&` variable or a container with no other name.
    // Cost: O(d + a), d = depth of nested binding cells, a = alias chain length.
    pub(crate) fn scalar_container_name(&self, target_var: &str, target: &Value) -> Option<String> {
        if target_var.starts_with(['@', '%', '&']) {
            return None;
        }
        declared_name(target)
            .or_else(|| self.env.get(target_var).and_then(declared_name))
            .map(|s| s.resolve())
            .or_else(|| {
                let root = self.resolve_alias_root(target_var);
                (root != target_var && !root.starts_with("__") && !root.starts_with(['@', '%', '&']))
                    .then(|| {
                        if root.starts_with('$') {
                            root.to_string()
                        } else {
                            format!("${root}")
                        }
                    })
            })
    }
}
