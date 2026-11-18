//! A memo for the rendered pattern of an argument-less, literal-bodied rule.
//!
//! `Grammar.parse` resolves its start rule through
//! `Interpreter::eval_token_def`, which evaluates the rule's body in a frame
//! of its own (an `env` clone, the declaring scope's captures, a readonly
//! frame, a routine frame) and then renders the pattern: the bound
//! parameters are interpolated and baked into subrule argument lists. For a
//! rule with no parameters, called with no arguments, whose body is a single
//! regex literal, every step of that is decided by the literal alone: the
//! body evaluates to the literal, the interpolation pass substitutes only
//! bound parameters (there are none), and the baking pass renders only bound
//! parameters too (it does normalize argument-list spacing, which is again a
//! function of the text). So the rendered pattern is a pure function of the
//! instantiated literal (`token_pattern_from_def`), and it is remembered per
//! thread under that text. That frame and render cost ~25k of the ~116k a
//! three-rule `G.parse('xy')` took.

use super::*;
use std::cell::RefCell;

thread_local! {
    static RENDERED: RefCell<rustc_hash::FxHashMap<String, String>> =
        RefCell::new(rustc_hash::FxHashMap::default());
}

/// More distinct start-rule patterns than a program has: past it the memo
/// starts over rather than grow without bound.
const MAX_ENTRIES: usize = 1024;

impl Interpreter {
    /// The instantiated literal of `def` when its rendering is a pure
    /// function of that text (see the module docs), else `None`.
    // Cost: O(|pattern|) to instantiate the literal.
    pub(super) fn static_token_source(def: &FunctionDef, arg_values: &[Value]) -> Option<String> {
        if !arg_values.is_empty()
            || !def.params.is_empty()
            || !def.param_defs.is_empty()
            || def.body.len() != 1
        {
            return None;
        }
        Self::token_pattern_from_def(def)
    }

    /// The pattern `source` rendered to, if it was rendered before.
    // Cost: O(|source|), one hash probe and a copy of the answer.
    pub(super) fn rendered_token_pattern(source: &str) -> Option<String> {
        RENDERED.with(|memo| memo.borrow().get(source).cloned())
    }

    /// Remember that `source` renders to `rendered`.
    // Cost: O(|source| + |rendered|).
    pub(super) fn remember_rendered_token_pattern(source: String, rendered: &str) {
        RENDERED.with(|memo| {
            let mut memo = memo.borrow_mut();
            if memo.len() >= MAX_ENTRIES {
                memo.clear();
            }
            memo.insert(source, rendered.to_string());
        });
    }
}
