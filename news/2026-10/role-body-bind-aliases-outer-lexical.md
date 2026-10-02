# A role-body `:=` bind aliases the outer lexical

`my $z = 1; role R { my $w := $z; method m { $w } }; class C does R {};
$z = 7; say C.new.m` printed `1`; Rakudo prints `7` (#11087). A write through
the alias (`method set($v) { $w = $v }`) never reached `$z` either, and the
sigilless `my \x := $z` behaved the same.

A role body's statements are deferred to composition, where each runs as its
own chunk that reaches `$z` only by name. The class-body twin (#10682) already
made such a chunk's block-final `:=` declaration emit a real bind and let a
declaration bind adopt a by-name source's existing cell; what a role lacked was
the cell. Role registration now boxes the declaring frame's slots the body's
binds name (`CompiledRoleDeclPlan::body_bind_source_slots`, computed by the
same helper the class plan uses) into shared cells at declaration time, while
that frame still owns them — composition may run later and in another frame (a
`my class` composed inside a routine).

Every composition of a parametric role binds the same outer container, and
the composed methods that capture the body static see the outer variable's
current value.
