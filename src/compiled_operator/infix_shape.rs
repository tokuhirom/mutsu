//! The structural decode of a *runtime* infix spelling: the meta-operator
//! layers wrapped around a leaf operator, computed once instead of once per
//! element.
//!
//! [`super::ReductionSpec`] and [`super::MetaKind`] cover the operators whose
//! shape the *compiler* knows. This covers the ones it does not: the operator
//! string an already-lowered opcode hands to
//! `Interpreter::eval_reduction_operator_values`, which used to re-derive its
//! whole structure — `[op]` brackets, `R` reverse prefixes, a bare `Z`, `Zop`,
//! the `>>op<<` hyper delimiters and the Unicode alias fold — on every call.
//!
//! That call is **per element**: `@a Z+ @b` over 200-element operands ran the
//! decode 200 times, a `>>+<<` hyper once per leaf pair, and `[+] @list` once
//! per fold step. [`InfixShape::lower`] runs it once; [`InfixRef`] is the
//! borrowed, `Copy` cursor the element loops recurse with, so descending one
//! meta layer is a slice bump rather than another parse.

use super::{canonical_infix, strip_hyper_delimiters};

/// One structural meta-operator wrapped around a leaf infix.
///
/// `[op]` records no layer: as an inner operator it is exactly `op` applied
/// once (the same identity [`super::MetaKind::Reduce`] documents), so the
/// bracket is transparent.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MetaLayer {
    /// `Rop` — apply the inner operator with the operands swapped.
    Reverse,
    /// A bare `Z` — zip two lists into tuples. Terminal: it has no inner
    /// operator, so nothing below it is ever read.
    ZipTuple,
    /// `Zop` — zip two lists element-wise with the inner operator.
    Zip,
    /// `>>op<<`, `>>op>>`, `<<op<<`, `<<op>>` and their Unicode spellings.
    Hyper { dwim_left: bool, dwim_right: bool },
}

/// An infix spelling decoded into [`MetaLayer`]s around a leaf.
///
/// Owns the layer stack; the leaf borrows the spelling it was decoded from
/// (every layer strips a prefix or a suffix, so the leaf is a subslice — no
/// allocation, and no `Symbol::intern` on a path where interning the key would
/// itself be the per-element work this removes).
///
/// A plain operator (`+`, `eq`, a user's `myop`) decodes to zero layers, so the
/// common case allocates nothing at all.
pub(crate) struct InfixShape<'a> {
    layers: Vec<MetaLayer>,
    leaf: &'a str,
    canonical: &'a str,
}

impl<'a> InfixShape<'a> {
    /// Decode `op`, outermost meta-operator first.
    ///
    /// The checks are applied in the order the recursive string version used,
    /// so the decoded shape is what each element used to re-derive: brackets,
    /// `R`, a bare `Z`, `Zop`, then the hyper delimiters.
    pub(crate) fn lower(op: &'a str) -> Self {
        let mut layers = Vec::new();
        let mut rest = op;
        loop {
            if let Some(inner) = rest.strip_prefix('[')
                && let Some(inner) = inner.strip_suffix(']')
                && !inner.is_empty()
            {
                rest = inner;
                continue;
            }
            if let Some(inner) = rest.strip_prefix('R')
                && !inner.is_empty()
            {
                layers.push(MetaLayer::Reverse);
                rest = inner;
                continue;
            }
            if rest == "Z" {
                layers.push(MetaLayer::ZipTuple);
                break;
            }
            if let Some(inner) = rest.strip_prefix('Z')
                && !inner.is_empty()
            {
                layers.push(MetaLayer::Zip);
                rest = inner;
                continue;
            }
            if let Some(inner) = strip_hyper_delimiters(rest) {
                layers.push(MetaLayer::Hyper {
                    dwim_left: rest.starts_with("<<") || rest.starts_with('\u{00AB}'),
                    dwim_right: rest.ends_with(">>") || rest.ends_with('\u{00BB}'),
                });
                rest = inner;
                continue;
            }
            break;
        }
        Self {
            layers,
            leaf: rest,
            canonical: canonical_infix(rest),
        }
    }

    /// The borrowed cursor the VM walks. Cheap to copy, so recursing into an
    /// inner layer costs a slice bump.
    pub(crate) fn as_ref(&self) -> InfixRef<'_> {
        InfixRef {
            layers: &self.layers,
            leaf: self.leaf,
            canonical: self.canonical,
        }
    }
}

/// A borrowed view of an [`InfixShape`], or of one of its inner layers.
#[derive(Debug, Clone, Copy)]
pub(crate) struct InfixRef<'a> {
    layers: &'a [MetaLayer],
    leaf: &'a str,
    canonical: &'a str,
}

impl<'a> InfixRef<'a> {
    /// The outermost remaining meta layer and the operator inside it.
    pub(crate) fn split_first(self) -> Option<(MetaLayer, Self)> {
        let (first, rest) = self.layers.split_first()?;
        Some((
            *first,
            Self {
                layers: rest,
                ..self
            },
        ))
    }

    /// The leaf operator's own spelling, Unicode aliases *not* folded.
    pub(crate) fn leaf(self) -> &'a str {
        self.leaf
    }

    /// The leaf operator with the Unicode aliases folded to ASCII
    /// (`×` → `*`, `≤` → `<=`, …) — the spelling the operator tables are keyed
    /// by.
    pub(crate) fn canonical(self) -> &'a str {
        self.canonical
    }

    /// The leaf spelling when no meta layer wraps it, for the leaf-only special
    /// cases (`»=»`'s `=`, `»~~»`'s `~~`) that must not fire for a `Z=`/`R~~`.
    pub(crate) fn as_plain(self) -> Option<&'a str> {
        self.layers.is_empty().then_some(self.leaf)
    }
}
