# `--doc=module`, and `E<...>` inside `=begin pod`

Rakudo spells the Pod renderer selection `--doc=module`, meaning
`Pod::To::[module]`. mutsu only accepted the bare `--doc`, so the whole
`--doc=Text` token fell through to the "this must be the program file" branch
and died with `Could not open --doc=Text`. That is what
`roast/S26-documentation/02-paragraph.t`'s last assertion — an `is_run` with
`:compiler-args['--doc=Text']` — hit, and it is the sort of failure that reads
as a Pod bug when it is really argument parsing.

`--doc=Text` now selects the doc renderer mutsu has (its `--doc` output *is*
Pod::To::Text's). Any other module name is reported the way rakudo reports a
renderer it cannot load:

```
===SORRY!===
Could not find Pod::To::Nonesuch
```

rather than being mistaken for a filename.

Fixing the option surfaced a second, unrelated bug behind it: `E<...>` is a Pod
formatting code wherever it appears, but only the `=for pod` branch of
`doc_mode`'s renderer decoded it. A `=begin pod` block emitted its lines
verbatim, so

```raku
=begin pod
Hello E<alpha>
=end pod
```

rendered as literal `Hello E<alpha>` under `--doc` where rakudo renders
`Hello α`. Headings and `=item` text had the same hole. All three now go
through `decode_pod_entities`.

Pin: `t/doc-renderer-option.t`, verified under both implementations. With this
in, `roast/S26-documentation/02-paragraph.t` passes under the real
`Test::Util`, taking the residue in
`todo/tickets/retire-native-test-util-overrides.md` to 4 files.
