# An unknown import tag is a compile-time error even beside `:ALL`

`use M :ALL, :NOPE` used to import everything and say nothing about `:NOPE`:
`:ALL` skipped the check of the requested tags. Every tag is now checked, and
an undeclared one is `X::Import::NoSuchTag` at compile time, as rakudo reports
it:

```
$ mutsu -e 'say "ran"; use M :ALL, :NOPE;'
Error while importing from 'M': no such tag 'NOPE'
```

The check runs as soon as the `use` loads its module at parse time, so nothing
of the program runs first. The message also drops the word "declared", which
rakudo's does not have. roast's `S11-modules/re-export.t`, whose
`use InnerModule :ALL, :EXPORT` rakudo rejects the same way, now fails as it
does on rakudo.
