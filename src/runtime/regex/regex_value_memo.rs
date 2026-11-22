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
//! plan cache it stands in front of. So the facts are stored with the tree
//! ([`crate::regex_tree::ExecMemo`]) together with the pattern text they were
//! derived from, and the plan together with its key. A tree that is cloned or
//! edited starts with an empty memo.

use std::any::Any;
use std::sync::{Arc, Mutex};

use crate::regex_tree::RegexTree;
use crate::runtime::Interpreter;
use crate::runtime::regex_types::RegexPattern;
use crate::symbol::Symbol;

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

/// The facts of `tree` for the pattern text `pattern`, computed on first use
/// and kept with the tree.
// Cost: O(p) on a hit (the pattern text is compared), p = pattern length;
// O(p + t) on a miss, t = size of the tree.
pub(crate) fn regex_value_facts(tree: &RegexTree, pattern: &str) -> Arc<RegexValueFacts> {
    if let Some(stored) = tree.exec_memo.get()
        && let Ok(facts) = Arc::downcast::<RegexValueFacts>(stored)
        && facts.pattern == pattern
    {
        return facts;
    }
    let interpolation_names = tree.interpolation_names();
    let string_path = tree.holds_extension()
        || Interpreter::parse_anchored_single_subrule(pattern).is_some()
        || (!Interpreter::parse_regex_declarative_prefix(pattern).0.is_empty()
            && tree.declaration_kind.is_none())
        || (tree.contains_anchor() && !interpolation_names.is_empty());
    let facts = Arc::new(RegexValueFacts {
        pattern: pattern.to_string(),
        string_path,
        contains_array_interpolation: tree.contains_array_interpolation(),
        contains_regex_value_interpolation: tree.contains_regex_value_interpolation(),
        folds: crate::runtime::regex_parse_ltm::static_execution_policy(pattern)
            .is_some_and(|(_, ignore_case, ignore_mark, _)| ignore_case || ignore_mark),
        interpolation_names,
        plan: Mutex::new(None),
    });
    tree.exec_memo
        .set(Arc::clone(&facts) as Arc<dyn Any + Send + Sync>);
    facts
}
