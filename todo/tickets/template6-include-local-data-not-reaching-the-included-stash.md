# `Template6` INCLUDE with inline data renders the variable name instead of its value

Found 2026-09-06 while taking `Template6` 0.16.0 from 0/12 to 10/12 test files
(`news/2026-09/template6-split-captures-and-topic-writeback.md`). This is the
second of the two remaining blockers for that dist: `t/05-includes.rakutest`
subtest 2, "INCLUDE statement with local template data".

With `Template6` 0.16.0 unpacked and `-I lib`:

```
t/templates/include2.tt:  [% INCLUDE "included" name = "World" %]
t/templates/included.tt:  <h1>Hello [% name %]</h1>
```

```raku
use Template6;
my $t6 = Template6.new;
$t6.add-path: 't/templates';
print $t6.process('include2');
```

raku renders `<h1>Hello World</h1>`; mutsu renders `<h1>Hello name</h1>` — the
included template gets the *name* of the local variable where its value should
be. Every other subtest in the file passes, so plain `INSERT`/`INCLUDE` without
inline data is fine; it is specifically the `name = "World"` local-data form.

Not yet reduced to a language-level repro. What has been ruled out:

- `Template6::Parser`'s `resolve-value` is correct under mutsu — `'"World"'`,
  `"'World'"`, `'42'` and `'name'` all resolve to the same strings raku
  produces (`given`/`when` over a regex with a `(...)` capture and `$0`
  interpolation all behave).

So the divergence is downstream, in how the generated
`%localdata<name> = 'World';` line reaches
`$context.process($tfile, :localise, |%localdata)` and then
`Context.localise` → `Stash.make-clone`. Likely candidates to check next: the
`|%localdata` flatten into a `*%params` slurpy, `Stash.make-clone(|%params)`,
and `Parser.compile`'s
`$script.subst(/ 'my %localdata;' /, '', :nd(2..*))` — that `:nd(2..*)` was
itself broken until this session (it panicked the process), so the generated
script's `%localdata` scoping is worth dumping first (uncomment the
`note "<DEBUG:template>..."` line in `Parser.compile` and diff the generated
script against raku's).
