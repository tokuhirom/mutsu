//! What a match derives from a regex value's source tree, decided once per
//! tree instead of once per match (#18003).
//!
//! A `$s ~~ rx/.../` in a loop matches the same regex value every iteration,
//! and that value carries the same `Arc<RegexTree>`. Before it reached the
//! matcher, every match re-decided the same facts about it: whether the
//! pattern needs the string path (an extension node, an anchored single
//! subrule, a declarative prefix), which variables it interpolates, and the
//! execution plan, looked up by hashing the whole tree and then the pattern
//! text. Matching `rx/:i libreadline\.so\.(\d+) $/` against a short file name
//! spent ~11k of its ~17.6k instructions there (Readline's `LIBREADLINE` does
//! that for every entry of a directory listing).
//!
//! All of these are functions of the tree and the pattern text alone, except
//! the plan, which also depends on the package the match runs in and on the
//! token-definition generation (`TOKEN_DEFS_GEN`), exactly the key of the
//! plan cache it stands in front of. So the facts are kept per tree, keyed by
//! the tree's address (the AST names no runtime type, and a field on the tree
//! would widen `Expr`, which holds one inline), together with the pattern text
//! they were derived from, and the plan together with its key.

use std::sync::{Arc, Mutex, Weak};

use crate::regex_tree::RegexTree;
use crate::runtime::Interpreter;
use crate::runtime::regex_types::RegexPattern;
use crate::symbol::Symbol;
use crate::value::Value;

/// The facts of one regex source tree, for the pattern text `pattern`.
pub(crate) struct RegexValueFacts {
    pattern: String,
    /// `regex_match_with_captures_value` must hand the pattern to the string
    /// entry point (see there for the reasons).
    pub(crate) string_path: bool,
    pub(crate) contains_array_interpolation: bool,
    pub(crate) contains_regex_value_interpolation: bool,
    pub(crate) interpolation_names: Vec<String>,
    /// The pattern folds case or ignores marks (`static_execution_policy`).
    pub(crate) folds: bool,
    /// The string parser's plan for this pattern depends on nothing but the
    /// pattern, the package and the token generation: the pattern names no
    /// scalar read at match time and is static to `parse_regex`, which caches
    /// it under exactly that key. When the tree does not lower directly, that
    /// plan may be kept here too.
    pub(crate) parse_plan_is_static: bool,
    /// The lowered plan, with the (package, token generation) it was cached
    /// under.
    plan: Mutex<Option<(Symbol, u64, Arc<RegexPattern>)>>,
}

impl RegexValueFacts {
    /// The plan stored for `pkg` at token generation `tok_gen`, if any.
    // Cost: O(1).
    pub(crate) fn plan_for(&self, pkg: Symbol, tok_gen: u64) -> Option<Arc<RegexPattern>> {
        let slot = self.plan.lock().ok()?;
        match slot.as_ref() {
            Some((p, g, plan)) if *p == pkg && *g == tok_gen => Some(Arc::clone(plan)),
            _ => None,
        }
    }

    /// Remember `plan` as the one for `pkg` at `tok_gen`.
    // Cost: O(1).
    pub(crate) fn store_plan(&self, pkg: Symbol, tok_gen: u64, plan: Arc<RegexPattern>) {
        if let Ok(mut slot) = self.plan.lock() {
            *slot = Some((pkg, tok_gen, plan));
        }
    }
}

/// How many trees the memo table holds before it starts over. A program that
/// builds regexes at run time (an `EVAL` in a loop) would otherwise keep every
/// tree it ever matched alive through the table's `Weak` handles.
const MEMO_TREES_MAX: usize = 4096;

/// The per-tree table: address → (the tree's allocation, its facts).
type TreeFactsTable = rustc_hash::FxHashMap<usize, (Weak<RegexTree>, Arc<RegexValueFacts>)>;

thread_local! {
    /// Facts per source tree, keyed by the tree's address. The `Weak` keeps
    /// that allocation reserved while the entry exists, so a live tree at the
    /// same address is the same tree.
    static TREE_FACTS: std::cell::RefCell<TreeFactsTable> =
        std::cell::RefCell::new(TreeFactsTable::default());
}

/// The facts of `tree` (the source tree of `value`) for the pattern text
/// `pattern`, computed on first use and kept per tree.
// Cost: O(p) on a hit (the pattern text is compared), p = pattern length;
// O(p + t) on a miss, t = size of the tree.
pub(crate) fn regex_value_facts(
    value: &Value,
    tree: &RegexTree,
    pattern: &str,
) -> Arc<RegexValueFacts> {
    let key = std::ptr::from_ref(tree) as usize;
    let hit = TREE_FACTS.with(|table| {
        table
            .borrow()
            .get(&key)
            .filter(|(_, facts)| facts.pattern == pattern)
            .map(|(_, facts)| Arc::clone(facts))
    });
    if let Some(facts) = hit {
        return facts;
    }
    let facts = Arc::new(derive_facts(tree, pattern));
    if let Some(owner) = value.regex_source_tree_arc()
        && std::ptr::eq(Arc::as_ptr(&owner), tree)
    {
        TREE_FACTS.with(|table| {
            let mut table = table.borrow_mut();
            if table.len() >= MEMO_TREES_MAX {
                table.clear();
            }
            table.insert(key, (Arc::downgrade(&owner), Arc::clone(&facts)));
        });
    }
    facts
}

// Cost: O(p + t), p = pattern length, t = size of the tree.
fn derive_facts(tree: &RegexTree, pattern: &str) -> RegexValueFacts {
    let interpolation_names = tree.interpolation_names();
    let string_path = tree.holds_extension()
        || Interpreter::parse_anchored_single_subrule(pattern).is_some()
        || (!Interpreter::parse_regex_declarative_prefix(pattern).0.is_empty()
            && tree.declaration_kind.is_none())
        || (tree.contains_anchor() && !interpolation_names.is_empty());
    RegexValueFacts {
        pattern: pattern.to_string(),
        string_path,
        contains_array_interpolation: tree.contains_array_interpolation(),
        contains_regex_value_interpolation: tree.contains_regex_value_interpolation(),
        folds: crate::runtime::regex_parse_ltm::static_execution_policy(pattern)
            .is_some_and(|(_, ignore_case, ignore_mark, _)| ignore_case || ignore_mark),
        interpolation_names,
        parse_plan_is_static: super::regex_late_scalars::scalars_in_pattern(pattern).0.is_empty()
            && crate::runtime::regex_parse::regex_pattern_is_static(pattern),
        plan: Mutex::new(None),
    }
}
