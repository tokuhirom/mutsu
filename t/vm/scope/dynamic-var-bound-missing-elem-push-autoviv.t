use Test;

plan 5;

# A push on a dynamic variable bound to a missing hash element writes through
# the binding (the lexical form is covered by bound-hash-elem push tests).
{
    my $*tld = {};
    my $*tml = $*tld;
    $*tml := $*tml{'a'};
    $*tml.push: {};
    is $*tml.raku, '$[{},]', 'dynamic var reads the autovivified Array';
    is $*tld.raku, '${:a($[{},])}', 'the hash gained the entry';
}

# Rebinding from a callee, as TOML::NQP does for array tables.
sub table() {
    $*t := $*root;
    $*t := $*t{'a'};
    $*t.push: {};
    $*t := $*t[*-1];
}
{
    my $*root = {};
    my $*t = $*root;
    table();
    is $*t.raku, '${}', 'caller sees the callee-rebound last element';
    my $*p := $*t;
    $*p := $*p{'k'};
    $*p = 5;
    is-deeply $*root, {a => [{k => 5},]}, 'assignment lands in the autovivified table';
}

{
    my $*h = {};
    my $*e := $*h{'x'};
    $*e.push: 1, 2;
    is-deeply $*h, {x => [1, 2]}, 'push of several values';
}
