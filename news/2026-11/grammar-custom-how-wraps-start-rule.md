# A custom GrammarHOW's find_method wraps the start rule, with a grammar cursor

`Grammar::Profiler::Simple` replaces `find_method` of the grammar's metaclass
(`EXPORTHOW.WHO.<grammar>`) with a wrapper that counts and times every rule. Three
things differed from rakudo:

- the wrapper's cursor was a `Match`, so `$c.^name` (the key the profile is filed
  under) was `Match` instead of the grammar; it is now an instance of the grammar,
  as a `<?{ }>` block's `self` already was;
- the start rule never went through `find_method`: `.parse` matched `TOP`
  directly, so it was never counted. `.parse`/`.parsefile` now ask the HOW for the
  start rule and, when it answers a wrapper, run it; the wrapper's call of the
  original rule is the real parse;
- the failure probe (and the LTM measurement) walked the rules again and ran the
  wrapper a second time; user wrapper code no longer runs in those walks.

`Grammar::Profiler::Simple` `t/csv.rakutest` is 5/5 like rakudo. Pinned by
`t/grammar/grammar-custom-how-wraps-start-rule.t`. `.subparse` is not wrapped yet.

`Grammar::Tracer::Compact` (same kind of `find_method` wrapper) needed two more
pieces: `find_method`'s invocant is the cursor (a grammar instance, so the
wrapper's `$meth($obj, |args)` can run), and a cursor answers `.MATCH`. Its
`t/tracer.t` passes like rakudo.
