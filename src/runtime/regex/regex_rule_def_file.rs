//! The file a grammar rule was declared in, for the routine frame a rule
//! invocation pushes (#13915).
//!
//! A rule's embedded code (`token TOP { x { $s } }`) closes over the scope
//! the rule was declared in. A rule declared in a module reads that module's
//! file-scope lexicals through the compunit bucket keyed by the running
//! frame's defining file (`Interpreter::compunit_lexical_bucket`), as a
//! method or sub of the module does. The rule frame used to carry no defining
//! file, so a parse started after the module finished loading resolved the
//! name in whatever scope ran the parse.

use super::super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// The defining file of the rule `name` of grammar `pkg`, or of the
    /// nearest parent grammar that declares it.
    // Cost: O(m) hash probes, m = the grammar's MRO length; O(|file|) to
    // intern the file name once found.
    pub(crate) fn rule_def_file_sym(&self, pkg: Symbol, name: &str) -> Option<Symbol> {
        let reg = self.registry();
        let mro = reg.class_mro_readonly(&pkg.resolve());
        let packages = std::iter::once(pkg).chain(mro.iter().flat_map(|m| m.iter().copied()));
        for package in packages {
            let key = crate::runtime::dispatch_key::qualified_intern(&package.resolve(), name);
            if let Some(defs) = reg.token_defs.get(&key) {
                return defs
                    .first()?
                    .source_file
                    .as_deref()
                    .map(Symbol::intern);
            }
        }
        None
    }

    /// [`Self::rule_def_file_sym`], memoized per `TOKEN_DEFS_GEN`.
    // Cost: O(1) expected after the first call per (grammar, rule).
    pub(crate) fn rule_def_file_cached(&mut self, pkg: Symbol, rule: Symbol) -> Option<Symbol> {
        let generation =
            crate::runtime::regex_parse::TOKEN_DEFS_GEN.load(std::sync::atomic::Ordering::Relaxed);
        if let Some((cached_gen, file)) = self.caches.rule_def_file_cache.get(&(pkg, rule))
            && *cached_gen == generation
        {
            return *file;
        }
        let file = self.rule_def_file_sym(pkg, &rule.resolve());
        self.caches
            .rule_def_file_cache
            .insert((pkg, rule), (generation, file));
        file
    }

    /// Run `f` with the running frame's defining file switched to the one
    /// the rule `rule` (of grammar `pkg`, or a parent) was declared in, when
    /// that differs: a rule's embedded code closes over the scope of the file
    /// declaring the rule, whichever rule called it (#13915).
    // Cost: O(1) expected beyond `f` (see `rule_def_file_cached`); two frame
    // copies when the file differs.
    pub(crate) fn with_rule_def_file<T>(
        &mut self,
        pkg: Symbol,
        rule: Option<Symbol>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let Some(file) = rule.and_then(|rule| self.rule_def_file_cached(pkg, rule)) else {
            return f(self);
        };
        let Some(top) = self.routine_stack().last() else {
            return f(self);
        };
        if top.def_file == Some(file) {
            return f(self);
        }
        let mut switched = *top;
        switched.def_file = Some(file);
        switched.lexical_package = self.lexical_package_for_frame(Some(file));
        // Replaced in place and put back, so a method frame keeps the record
        // it names (popping it would cut the record off the stack).
        let original = *top;
        self.routine_stack.replace_top(switched);
        let result = f(self);
        self.routine_stack.replace_top(original);
        result
    }
}
