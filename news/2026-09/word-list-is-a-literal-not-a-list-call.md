# A word list is a literal, so a user's `sub list` no longer captures it

`qw<a b c>`, `<a b c>`, `qw//`, `qqw//` and `q:w//` are quoting constructs: they
build a `List`, and no routine the program declares takes part. mutsu lowered
every one of them to a plain `list(...)` call — and `list` is an ordinary Raku
routine name a program may declare, so declaring one captured every word list
in that compilation unit:

```raku
sub list($a, $b) { "user-list" }

my $x = qw<zero one two>;
# rakudo: $x is $("zero", "one", "two")
# mutsu:  Too many positionals passed; expected 2 arguments but got 3
```

Word lists now lower to the reserved `__mutsu_word_list`, which the builtin
dispatcher serves with the same `builtin_list` the `list` builtin uses. Every
other lowering the parser synthesizes already used a `__mutsu_`-prefixed name
for exactly this reason — `make_word_result_expr`'s own `__mutsu_qw_result`
sits three lines below the site that was fixed. Calling `list(...)` by name is
unaffected: a user's routine still wins there, as it should.

Found re-measuring [#7539](https://github.com/tokuhirom/mutsu/issues/7539)'s
`Config::TOML` + `Crane` battery pair. `Crane::List.rakumod` declares a whole
`multi sub list` set, so its own word lists bound against its own candidates —
which is where `t/list.rakutest` and `t/flatten.rakutest` got
`Unknown function: list` and
`Cannot resolve caller list(Str:D, Str:D, Str:D)`, aborting both files at their
first subtest. Three earlier passes on that ticket recorded the symptom as a
multi-dispatch bug with an unidentified trigger, and four hand-built reductions
behaved correctly — because each of them had renamed the sub. Renaming it back
was the whole bisection: two module copies differing only in `list` vs `lst`.

`Crane` goes from 10/15 files to 12/15; `Config::TOML` is unchanged at 14/19,
with no regression.

Pinned by `t/lang/quoting/qw-word-list-is-not-a-list-call.t` (17 assertions),
verified to pass under rakudo v2026.07 as well as under mutsu.
