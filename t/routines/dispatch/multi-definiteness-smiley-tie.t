use Test;

plan 5;

# Rakudo ranks a definiteness smiley (:D/:U) as no narrower than the bare
# type, so a call both candidates bind is ambiguous (#13064).
{
    multi kind(Int $x) { 'any' }
    multi kind(Int:U $x) { 'undef' }
    my $r = try kind(Int);
    ok $!.defined, 'Int vs Int:U for a type object is ambiguous';
    isa-ok $!, X::Multi::Ambiguous, '... X::Multi::Ambiguous';
}

{
    multi k2(Int:D $x) { 'D' }
    multi k2(Int $x) { 'any' }
    try k2(5);
    isa-ok $!, X::Multi::Ambiguous, 'Int:D vs Int for an Int is ambiguous';
}

# A :D / :U pair is never ambiguous: only one of them binds.
{
    multi k3(Int:D $x) { 'D' }
    multi k3(Int:U $x) { 'U' }
    is k3(5), 'D', ':D candidate takes a defined value';
    is k3(Int), 'U', ':U candidate takes a type object';
}

done-testing;
