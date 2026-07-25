# `Template::Mustache`: the last two upstream test files

The template battery passes 11 of its 13 upstream test files under mutsu,
including the whole official mustache spec suite (`91-specs`, 10/10). Two remain,
and both pass under raku.

## `06-logging.rakutest` — 2/3

```
ok 1 - Warn missing field(s)
ok 2 - Warn missing . field
not ok 3 - Set log routine for Warn level to &die
```

The failing assertion is:

```raku
my $m = Template::Mustache.new: :log-level<Info>;
$m.logger.routines<Warn> = &die;
dies-ok { $m.render: '{{missing}}', {} }, "Set log routine for Warn level to \&die";
```

i.e. replacing an entry in the logger's `%.routines` with `&die` must make the
render throw. The obvious reduction — storing `&die` in a hash of routines and
calling it directly or through a sub — **behaves correctly** in mutsu (verified
against raku), so the divergence is further in: the module's `proto method log` /
`multi method log(LogLevel :$level, *@msgs)` dispatch, or the `LogLevel` enum
constraint, is not reaching the replaced routine.

## `92-specs-file.rakutest` — 1/10

Every `subtest` plans its N tests and then runs **0** of them:

```
# Subtest: comments.json
    1..11
    # You planned 11 tests, but ran 0
not ok 2 - comments.json
```

So the subtest body dies before its first assertion. The individual pieces have
been checked in isolation and all work: `load-specs` from the test's own
`Template::Mustache::TestUtil`, the anonymous state counter `++$` used to build a
unique directory, `.mkdir`, and the two-at-a-time
`for 'name', $_<template>, |$_<partials>.kv -> $name, $text` loop. The remaining
suspects are the `LEAVE` block inside the loop body, or `$m.render` with the
`:from` file-lookup path plus the partial cache.

`91-specs.rakutest` — the same spec corpus rendered from *strings* rather than
files — passes 10/10, which localises this to the file/partial loading path.

## Status

Not a release blocker: the gate is a per-file baseline, so the 11 passing files
are pinned in `batteries-whitelist.txt` and a regression in them fails a release,
while these two stay tracked here. See `docs/batteries/templates.md`.
