//! Seed an EVAL's parser with routines visible in its caller's package.

use crate::qualified::{is_global_package, last_segment, package_ancestors, split_qualified};
use crate::runtime::Interpreter;
use crate::symbol::Symbol;

impl Interpreter {
    /// Preserve caller routine parsing without exposing a loaded module's
    /// private short names as declarations in a later EVAL.
    // Cost: O(F * d + E), F = registered routines, d = caller package depth,
    // E = environment entries.
    pub(crate) fn collect_eval_user_sub_names(&self) -> Vec<String> {
        let package = self.current_package_sym();
        let method_package = self.method_class_stack_top_str().map(Symbol::intern);
        let mut names = Vec::new();
        for key in self.registry().functions.keys() {
            if let Some((owner, _)) = split_qualified(*key)
                && !is_global_package(owner)
                && !package_ancestors(package).any(|ancestor| ancestor == owner)
                && !method_package.is_some_and(|class| {
                    package_ancestors(class).any(|ancestor| ancestor == owner)
                })
            {
                continue;
            }
            // Multi candidate keys carry `/arity...`; only their routine
            // name belongs in the parser's declaration preseed.
            let short = last_segment(*key).as_str();
            let short = short.split('/').next().unwrap_or(short);
            if !short.is_empty() && !short.contains(':') {
                names.push(short.to_string());
            }
        }
        // Lexical callable variables are visible independently of registry
        // ownership, including aliases imported into the caller's scope.
        for key in self.env.keys() {
            let text = key.resolve();
            if let Some(bare) = text.strip_prefix('&')
                && !bare.is_empty()
                && !bare.contains(':')
            {
                names.push(bare.to_string());
            }
        }
        names
    }
}
