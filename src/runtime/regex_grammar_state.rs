//! The `regex` subsystem of ADR-10779: the state of the regex engine, grammar
//! parses and slang declarations that lives across calls -- the current match
//! cursor, the grammar's actions object and `make` value, the dynamic
//! variables a grammar rule declares, and the slang rules and declarators
//! defined so far.

use super::*;

#[derive(Default)]
pub(crate) struct RegexGrammarState {
    /// Grammar-rule overrides recorded by `$*LANG.define_slang` during a slang
    /// activation run (ADR-0026). Only ever populated in the dedicated
    /// activation sub-interpreter; read once by its thread runner.
    pub(crate) defined_slang_rules: Vec<crate::runtime::slang_activation::SlangRuleOverride>,
    /// Package declarators a slang grammar role registered via
    /// `token package_declarator:sym<name>` (ADR-0091). Populated by
    /// `$*LANG.define_slang` in both the activation sub-interpreter (where the
    /// parser reads them back to learn the keyword) and the ordinary
    /// interpreter (where the declaration protocol looks the HOW up in it).
    pub(crate) defined_slang_declarators: Vec<crate::runtime::slang_declarator::SlangDeclarator>,
    /// `$*LANG.set_how($pkgdecl, $HOW)`: the metaclass a package declaration
    /// of each kind is currently built with. Keyed by the `$*PKGDECL` name
    /// (`'role'`, `'test-hub'`, ...).
    pub(crate) slang_declarator_hows: ValueMap,
    /// `rule name -> its own `:my $*/%*/@*x = …;` declarations`, for the grammar
    /// currently being parsed. `establish_grammar_dynamic_vars` also evaluates
    /// them once into `env` (a parse-wide slot, which is what a non-declaring
    /// rule's action reads); this map is what lets the reduce walk give each
    /// *match* of a declaring rule its own binding on top of that, so a
    /// per-match `:my $*FINAL` is not read as the last match's value.
    pub(crate) grammar_rule_dynvar_decls: HashMap<String, Vec<String>>,
    /// The grammar instance (Rakudo's cursor) the compiled regex engine hands to
    /// the grammar METHOD a `<.name>` subrule is about to call: the one the
    /// rule invocation that makes the call owns, so what the method writes to
    /// its attributes survives onto that rule's Match (#9803). Published by
    /// the engine for the duration of that one call and taken by
    /// `try_regex_subrule_as_method`; `None` everywhere else, where the method
    /// gets a throwaway instance.
    pub(crate) rx_cursor: Option<Value>,
    /// The same for rule invocations the WALK evaluates (the eager and streamed
    /// subrule arms, the ratcheted `<x>*` scan, the single-candidate arm): one
    /// entry per invocation in flight, innermost last, created lazily by the
    /// first grammar method the invocation calls. The walk pops its entry when
    /// the invocation's ends are produced and files the instance on each of them
    /// (#9803). Empty outside a walked rule body.
    pub(crate) walk_cursors: Vec<Option<Value>>,
    /// The built invocant `.parse` hands its start rule (#10848).
    pub(crate) start_invocant: regex::regex_grammar_cursor::StartRuleInvocant,
    /// Value set by `make()` inside grammar action methods.
    /// Persists across env save/restore in method dispatch.
    pub(crate) action_made: Option<Value>,
    /// The `:actions` object of an in-progress `Grammar.parse`, if any. Set for
    /// the duration of a parse so that `<?{ ... }>` code assertions can run the
    /// relevant action method on a just-matched named capture and expose its
    /// `.made` result during parsing (raku runs actions incrementally at reduce
    /// time; mutsu otherwise only runs them post-parse). Saved/restored around
    /// nested/re-entrant parses.
    pub(crate) current_grammar_actions: Option<Value>,
    /// True while evaluating an embedded regex `{ ... }` code block from a grammar
    /// rule (`execute_regex_code_blocks`). Such a block closes over the lexical
    /// scope where the grammar was defined, so a bare free variable the compiler
    /// auto-qualified to the grammar package (`$x` -> `SetGlobal("G::x")`) must
    /// fall back to an existing outer lexical of the same bare name. Scopes that
    /// outer-lexical-write fallback to exactly this context so ordinary `our`/
    /// package-qualified writes elsewhere are unaffected.
    pub(crate) in_regex_code_block: bool,
}

impl RegexGrammarState {
    /// A spawned thread starts with no match or grammar parse in progress and
    /// no slang declarations (as `clone_for_thread` did field by field).
    // Cost: O(1).
    pub(crate) fn fork_for_thread(&self) -> Self {
        Self::default()
    }
}
