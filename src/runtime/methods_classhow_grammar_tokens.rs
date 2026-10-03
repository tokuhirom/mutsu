//! `.^methods` of a grammar lists its `token`/`rule`/`regex` declarations.
//!
//! In Rakudo a grammar's regexes are ordinary methods (of type `Regex`), so
//! `G.^methods` enumerates them next to the grammar's `method`s and
//! `G.^methods.grep({ .WHAT ~~ Regex })` is the usual way to find them all —
//! Grammar::Extractor wraps each one that way. mutsu keeps them in
//! `Registry::token_defs` / `Registry::proto_tokens`, keyed `Grammar::name`,
//! which `.^lookup` already consulted but `.^methods` did not.

use super::*;

impl Interpreter {
    /// Append a `Regex` method object for each token/rule/regex `owner`
    /// declares (its `:sym<..>` candidates and bare `proto token`s included),
    /// skipping names already in `result`.
    // Cost: O(t), t = registered tokens across all grammars (one prefix test
    // each); `.^methods` is not on a hot path.
    pub(super) fn collect_grammar_token_methods(&self, owner: &str, result: &mut Vec<Value>) {
        let owner_sym = Symbol::intern(owner);
        let registry = self.registry();
        let mut entries: Vec<(
            String,
            Option<i64>,
            Option<String>,
            Vec<crate::ast::ParamDef>,
        )> = Vec::new();
        for (key, defs) in registry.token_defs.iter() {
            let Some(name) = Self::grammar_token_member(*key, owner_sym) else {
                continue;
            };
            let first = defs.first();
            entries.push((
                name.to_string(),
                first.and_then(|d| d.source_line),
                first.and_then(|d| d.source_file.clone()),
                first.map(|d| d.param_defs.clone()).unwrap_or_default(),
            ));
        }
        for key in registry.proto_tokens.iter() {
            if let Some(name) = Self::grammar_token_member(Symbol::intern(key), owner_sym)
                && !entries.iter().any(|(n, ..)| n == name)
            {
                entries.push((name.to_string(), None, None, Vec::new()));
            }
        }
        drop(registry);
        // Declaration order is not recorded per grammar; sort for a stable answer.
        entries.sort_by(|a, b| a.0.cmp(&b.0));
        for (name, line, file, params) in entries {
            if result.iter().any(|v| Self::method_object_name_is(v, &name)) {
                continue;
            }
            result.push(self.make_native_method_object_ex_loc(
                &name,
                owner,
                true,
                line,
                file,
                Some(&params),
            ));
        }
    }

    /// The member name of a `token_defs` key that `owner` itself declares
    /// (`G::a` -> `a` for `G`; `G::Inner::a` is `G::Inner`'s).
    // Cost: O(1) amortized (memoized `package_parent` / `unqualified_part`).
    fn grammar_token_member(key: Symbol, owner: Symbol) -> Option<&'static str> {
        (crate::qualified::package_parent(key) == Some(owner))
            .then(|| crate::qualified::unqualified_part(key).as_str())
    }

    /// Whether `value` is a method object named `name`.
    // Cost: O(1) plus the name comparison.
    fn method_object_name_is(value: &Value, name: &str) -> bool {
        match value.view() {
            ValueView::Instance { attributes, .. } => attributes
                .as_map()
                .get("name")
                .is_some_and(|n| n.to_string_value() == name),
            _ => false,
        }
    }
}
