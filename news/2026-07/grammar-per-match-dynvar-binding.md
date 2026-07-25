# A rule's `:my $*FOO` is now one binding per match, not one per parse

```raku
grammar G {
    token TOP { <part>+ % '/' }
    token part {
        :my $*FINAL;
        \w+ {}
        [<?before '/'? $> { $*FINAL = True }]?
    }
}
class A {
    method TOP($/) { make $<part>.map(*.ast).join('|') }
    method part($/) { make ($*FINAL ?? "FIN" !! "mid") ~ ":$/" }
}
say G.parse('a/b/c', :actions(A)).ast;
# raku: mid:a|mid:b|FIN:c
# was:  FIN:a|FIN:b|FIN:c
```

## Root cause

`establish_grammar_dynamic_vars` ran **once per parse**: it scanned every rule
pattern of the grammar for `:my $*/%*/@*NAME = INIT;`, evaluated each into
`self.env`, and restored the prior values when the parse ended. So `$*FINAL` was
a **single parse-wide slot** shared by every match of `part` — the last code
block to write it won for every reader.

The entry recording this bug blamed something else: that inline `{ … }` blocks
and action methods run in two separate bottom-up passes, so every write finishes
before any action runs. That is what makes the bug *visible*, but even a
perfectly interleaved walk would still have been reading one shared slot. Rakudo
gives each match of the declaring rule its own binding.

## Fix

`establish_grammar_dynamic_vars` now also records **which rule declared what**
(`grammar_rule_dynvar_decls`), keyed by the bare rule name — which is exactly the
capture key a match is stored under, so the reduce walk can identify a node's
rule from its parent. It still evaluates the declarations once into `env`; that
parse-wide slot is what a *non*-declaring rule's action reads, and that behaviour
is load-bearing (`t/grammar-reduce-time-dynvar.t`).

On top of that, `reduce_regex_captures_made` threads the rule name
(`reduce_regex_captures_made_for_rule`) and, for a node whose rule declares
dynamic variables:

1. re-evaluates those declarations **before** the node's subtree reduces, so the
   match starts from the declared value rather than a sibling's leftover — and
   so a child's code block accumulates into *this* match's binding, which keeps
   the `:my %*PLAYED = (); <card>+` shape working;
2. records what they hold **after** the node's own code blocks run, onto the node
   (`RegexCaptures::regex_vars`);
3. `make_subcap_match` carries that onto the node's Match as `reduce_time_vars`,
   alongside the `ast` / `action_name` it already carried;
4. `invoke_grammar_actions` installs those values around that node's action and
   restores them afterwards.

A `$`-sigil dynamic variable lives in env *without* its sigil (`$*S` → `*S`)
while `@*A` / `%*H` keep theirs — the install normalises for that, or it would
write a key nothing reads.

Cost for a grammar that declares nothing: one `HashMap::is_empty()` per reduce
node.

## Impact

`File::Ignore` (`TODO_dist` T-050): `t/wildcard.rakutest` goes 38/44 → **44/44**,
and six of its seven test files now pass completely. Its `path-part:sym<matcher>`
token uses exactly this shape to decide whether a path segment is the last one,
so under the shared slot every segment believed it was final and the module built
a wrong pattern string for `a/**/b`.

Pin: `t/grammar-per-match-dynvar-action.t` (6 assertions, each verified against
raku, including the accumulate-across-matches shape and a non-declaring rule as
the control).

## The other two halves, for context

The **failed-parse half** of the original report landed earlier the same day:
when the overall parse FAILS, mutsu used to run no actions at all. The matcher
now logs every named-subrule reduce (`REDUCED_SUBRULES`), `.parse` keeps the
longest partial match, and the failure path dispatches that partial tree plus the
reduces that fell outside it — HTTP::UserAgent's upstream suite 23/27 → 25/27.
Pin: `t/grammar-actions-on-failed-parse.t`. The **proto-token LTM equal-length
tie-break** was fixed the same day too (`t/proto-token-ltm-tiebreak.t`), taking
`wildcard.rakutest` 36/44 → 38/44.
