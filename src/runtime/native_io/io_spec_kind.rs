//! Which `IO::Spec` class a call's receiver is (ADR-11276 §9.19): the one thing
//! the path primitives of `io_spec_paths` and `io_spec_split` branch on.

use crate::value::{Value, ValueView};

/// The `IO::Spec` family: `Unix` is the base, the others differ from it by
/// separator, volume and root rules.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum SpecKind {
    Unix,
    Win32,
    Cygwin,
    Qnx,
}

impl SpecKind {
    /// The kind of the `IO::Spec` class `target` is the type object or an
    /// instance of, `None` for any other value.
    // Cost: O(1), one string compare per class.
    pub(crate) fn of(target: &Value) -> Option<SpecKind> {
        let class = match target.view() {
            ValueView::Package(name) => name,
            ValueView::Instance { class_name, .. } => class_name,
            _ => return None,
        };
        match class.as_str() {
            "IO::Spec::Unix" => Some(SpecKind::Unix),
            "IO::Spec::Win32" => Some(SpecKind::Win32),
            "IO::Spec::Cygwin" => Some(SpecKind::Cygwin),
            "IO::Spec::QNX" => Some(SpecKind::Qnx),
            _ => None,
        }
    }
}

impl SpecKind {
    /// The classes whose rows the table answers this class's methods from, most
    /// derived first: Rakudo's `IO::Spec::Win32` is an `IO::Spec::Unix`, so it
    /// answers what it does not override from the `Unix` rows.
    // Cost: O(1).
    pub(crate) const fn owners(self) -> &'static [&'static str] {
        match self {
            SpecKind::Unix => &["IO::Spec::Unix"],
            SpecKind::Win32 => &["IO::Spec::Win32", "IO::Spec::Unix"],
            SpecKind::Cygwin => &["IO::Spec::Cygwin", "IO::Spec::Unix"],
            SpecKind::Qnx => &["IO::Spec::QNX", "IO::Spec::Unix"],
        }
    }
}
