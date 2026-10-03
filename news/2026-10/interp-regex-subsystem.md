# Regex and grammar state leaves `Interpreter`

The third subsystem extraction under ADR-10779 (#10779) moved ten fields out of
`struct Interpreter` into `RegexGrammarState`
(`src/runtime/regex_grammar_state.rs`). This is the regex, grammar and slang
state that lives across calls:

- the match cursors (`rx_cursor`, `walk_cursors`, `start_invocant`);
- the grammar's actions object and its `make` value;
- the dynamic variables a grammar rule declares;
- the slang rules and declarators defined so far.

Code reaches the fields as `self.regex_state.<field>`. A spawned thread starts
with all of this state empty, as `clone_for_thread` already did field by field;
`RegexGrammarState::fork_for_thread` now states that policy in one place.
`Interpreter` went from 386 to 377 direct fields. This is a field move only:
behaviour does not change.
