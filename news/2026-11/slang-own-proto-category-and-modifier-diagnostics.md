# Slangs may declare their own proto categories; `.+`/`.*` work on the topic

Slang activation refused a `:sym<...>` candidate of a proto category the slang
declares itself (Polyglot::Brainfuck's `proto token BF-command` with
`BF-command:sym<+>` and friends) as an unsupported override of the core grammar,
so `use Polyglot::Brainfuck` died while rakudo loads it. Such a candidate is now
treated like a helper token: only the slang's own productions reach it, and they
run on the slang host.

Running that distribution also showed that the topic forms `.+name` and `.*name`
did not parse at all, and that a method-call modifier followed by something that
cannot start a method name (`1.*(1)`, `.+(1)`, `1.?3`, `1.+ Str`) was accepted or
misdiagnosed. They now call every candidate along the MRO, and the bad forms are
compile-time errors with rakudo's `X::Comp::AdHoc` / `X::Syntax::Malformed`
diagnosis.
