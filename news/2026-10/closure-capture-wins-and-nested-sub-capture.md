# A closure's captured lexical wins over the caller's, and a sub nested in a closure keeps its activation

A closure made inside a named sub that is itself declared inside a closure body
lost every variable of the closure's activation: the capture of the sub's frame
stopped at the frame root, which is right for a sub declared at program scope
and wrong for one whose lexical outer is an enclosing activation. It returned
`Nil` for `my $ro` of the factory (`sub mk($n) { my $ro; -> { sub go() { -> {
$ro } }; go() } }`). The capture now stops only at a program-scope routine or a
closure body.

That bug was masked by the caller's binding winning in `call_compiled_closure`
(ADR-0055 slice 2): a caller lexical that merely shared a name shadowed the
closure's own binding. With the capture fixed, the merge is flipped: the whole
plain-lexical capture overwrites the caller's binding (dynamics, `self`, the
topic and `__mutsu_*` keep their live resolution; a closure whose outer is the
live program scope still reads it through the static link). Cro::HTTP's client,
which follows a redirect by calling `request` again inside the first call's
`whenever`, was the case that made the masking visible (#7546).
