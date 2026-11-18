use Test;

# A package-scoped class declared inside a routine is one type, created once
# (rakudo builds it at compile time). Re-registering it on every entry
# rewrote its method table while other threads dispatched through it, so a
# concurrent caller saw "No such method 'out'" (Proc::Q's
# `$c.send: class Res { has $.out ... }.new: ...` inside `start`).

plan 3;

sub make($i) {
    class RoutineRes {
        has Stringy $.out is required;
        has Mu $.tag is required;
    }.new: :out("o$i"), :tag($i)
}

my @promises = (^200).map: -> $i { start { make($i).out } };
my $ok = 0;
for @promises -> $p { try { await $p; $ok++ } }
is $ok, 200, 'every concurrent call reaches the accessor';

ok make(1).WHAT === make(2).WHAT, 'each entry yields the same type';
is make(3).out, 'o3', 'the attribute is set per instance';
