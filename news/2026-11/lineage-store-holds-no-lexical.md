# The cross-thread lineage store no longer answers for a user lexical

ADR-16595 §6 (#17837). After slice 5 no thread spawn publishes a lexical, but
the runtime still asked the name-keyed lineage store about lexicals on every
variable read fallback and mirrored every env store into it while a thread was
active. An oracle run over every `t/` file and the roast whitelist found that
nothing ever inserted a user-lexical key there any more: all of those reads
missed.

So the rule is now structural. `shared_store::is_user_lexical_key` (`x`, `@a`,
`%h`, `&f`, but not `@*x`, `!x`, `__mutsu_*` or qualified names) answers a read
as absent without walking a lineage, a debug build stops at any insertion
under such a key, and the env write mirror skips it. The store stays for what
is not a lexical: aggregate dynamics, attribute aliases, `state` cells and the
runtime's internal keys.

The ADR-12204 capture oracle (`MUTSU_SPAWN_CAPTURE_CHECK`), which recorded
which aggregates each spawn captured, is gone with the plan it measured.
