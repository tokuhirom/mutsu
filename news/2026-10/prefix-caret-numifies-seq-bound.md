# Prefix `^` numifies a Seq bound

`^$seq` (and `^$obj.method`, `^f()`) parsed as `0..^$seq` and so refused a Seq
bound with "Seq objects are not valid endpoints for Ranges". The prefix form now
compiles to the numifying `UptoRange` op, so a Seq counts its items, as in
rakudo; the infix `0..^$seq` still refuses. Found with Terminal::Boxer, whose
`t/01-basic.rakutest` now matches raku.
