//! The `:sym<…>` proto variants of every `token_defs` prefix, indexed once
//! per token-definition generation (#17640).
//!
//! [`Interpreter::proto_variant_keys_sorted`] answers "which registered keys
//! spell a proto variant of `prefix`" once per `<subrule>` resolution. It used
//! to scan the whole key set for each distinct prefix it was asked about: a
//! grammar's first parse asks about one prefix per rule per scope of its MRO,
//! so a large grammar (CSS::Module's CSS3, thousands of token keys) paid
//! O(prefixes × keys), ~170M instructions of a ~1.1G first parse. The index
//! is built in one pass over the keys instead: every split point of a key
//! that leaves a proto-variant suffix files the key under the text before it,
//! which is exactly the set the per-prefix scan selected.

use super::*;
use crate::symbol::Symbol;
use std::sync::Arc;

/// prefix -> its proto-variant keys, sorted (see
/// [`Interpreter::proto_variant_keys_sorted`]).
type VariantIndex = rustc_hash::FxHashMap<&'static str, Arc<Vec<Symbol>>>;

thread_local! {
    /// The index and the `TOKEN_DEFS_GEN` it was built under. Anything that
    /// (re)defines, restores or wholesale-replaces `token_defs` bumps the
    /// generation, the same invalidation discipline as `REGEX_PARSE_CACHE`.
    static PROTO_VARIANT_INDEX: std::cell::RefCell<Option<(u64, Arc<VariantIndex>)>> =
        const { std::cell::RefCell::new(None) };
}

/// True when `rest` (the portion of a `token_defs` key AFTER the proto's
/// fully-qualified name) begins a proto-regex variant adverb. Three spellings
/// register a candidate under the proto:
///
/// | spelling | binds `<sym>`? |
/// | --- | --- |
/// | `:sym<int>` / `:sym«int»` | yes |
/// | `:<int>` / `:«int»` (shorthand) | no |
/// | `:int` (bare identifier adverb) | no |
///
/// A resolver that recognized only `:sym<` dropped every `token element:<int>
/// {...}` candidate (YAMLish's `Schema::JSON`); one that additionally missed
/// the bare form dropped every `token gap:spacer {...}` candidate
/// (`Config::TOML`'s grammar, which names all of its alternatives that way).
///
/// `::` is a package separator, never an adverb, so it must not count.
// Cost: O(1).
pub(crate) fn is_proto_variant_suffix(rest: &str) -> bool {
    let Some(after) = rest.strip_prefix(':') else {
        return false;
    };
    after.starts_with('<')
        || after.starts_with('\u{ab}')
        || after.starts_with(|c: char| c.is_alphabetic() || c == '_')
}

impl Interpreter {
    /// Every `token_defs` key that spells a `:sym<…>` proto variant of
    /// `prefix`, sorted by declaration order (Rakudo's LTM tie-break) and
    /// falling back to alphabetical order for keys that share a declaration
    /// order or are unregistered, so the result stays deterministic.
    ///
    /// The key symbols come back as symbols, so no call site re-interns them.
    // Cost: O(1) amortized: a hash probe of the per-generation index; the
    // index is rebuilt once per generation in O(k * c + v log v), k = token
    // keys, c = colons per key, v = the largest variant list.
    pub(crate) fn proto_variant_keys_sorted(&self, prefix: &str) -> Arc<Vec<Symbol>> {
        static EMPTY: std::sync::OnceLock<Arc<Vec<Symbol>>> = std::sync::OnceLock::new();
        let index = self.proto_variant_index();
        index
            .get(prefix)
            .cloned()
            .unwrap_or_else(|| Arc::clone(EMPTY.get_or_init(|| Arc::new(Vec::new()))))
    }

    /// The index for the current generation, built on first use.
    // Cost: as `proto_variant_keys_sorted`.
    fn proto_variant_index(&self) -> Arc<VariantIndex> {
        let generation =
            crate::runtime::regex_parse::TOKEN_DEFS_GEN.load(std::sync::atomic::Ordering::Relaxed);
        if let Some(hit) = PROTO_VARIANT_INDEX.with(|c| {
            c.borrow()
                .as_ref()
                .filter(|(built, _)| *built == generation)
                .map(|(_, index)| Arc::clone(index))
        }) {
            return hit;
        }
        let index = Arc::new(self.build_proto_variant_index());
        PROTO_VARIANT_INDEX.with(|c| *c.borrow_mut() = Some((generation, Arc::clone(&index))));
        index
    }

    // Cost: O(k * c + v log v), see `proto_variant_keys_sorted`.
    fn build_proto_variant_index(&self) -> VariantIndex {
        let mut buckets: rustc_hash::FxHashMap<&'static str, Vec<Symbol>> =
            rustc_hash::FxHashMap::default();
        for &key in self.routines().token_defs.keys() {
            let text = key.as_str();
            for (i, _) in text.match_indices(':') {
                if is_proto_variant_suffix(&text[i..]) {
                    buckets.entry(&text[..i]).or_default().push(key);
                }
            }
        }
        buckets
            .into_iter()
            .map(|(prefix, mut keys)| {
                keys.sort_by(|a, b| {
                    self.token_key_decl_order_sym(*a)
                        .cmp(&self.token_key_decl_order_sym(*b))
                        .then_with(|| a.as_str().cmp(b.as_str()))
                });
                (prefix, Arc::new(keys))
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::is_proto_variant_suffix;

    #[test]
    fn proto_variant_suffixes() {
        assert!(is_proto_variant_suffix(":sym<a>"));
        assert!(is_proto_variant_suffix(":sym\u{ab}a\u{bb}"));
        assert!(is_proto_variant_suffix(":foo"));
        assert!(!is_proto_variant_suffix("::foo"));
        assert!(!is_proto_variant_suffix("sym<a>"));
    }
}
