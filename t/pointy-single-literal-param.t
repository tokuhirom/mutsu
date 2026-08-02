use v6;
use Test;

plan 8;

# A pointy block with exactly ONE parameter took a name-only fast path that
# cannot carry the parameter's literal value, so `-> 'about' { … }` lost its
# constraint entirely: it reported an unconstrained `Any` parameter, while two
# or more literals already worked. (Cro's router compiles a route's URL segments
# from exactly that constraint.)

{
    my $b = -> 'about' { "hit" };
    my $p = $b.signature.params[0];
    is $p.type.^name, 'Str', 'a single literal parameter is typed Str';
    ok $p.constraints.ACCEPTS('about'), 'and carries the literal as a constraint';
    nok $p.constraints.ACCEPTS('nope'), 'which rejects a different string';
    ok $b.signature.ACCEPTS(\('about')), 'the signature accepts the literal';
    nok $b.signature.ACCEPTS(\('nope')), 'and rejects anything else';
    is $b("about"), "hit", 'the block still runs';
}

{
    my $b = -> 'company', 'careers' { "two" };
    ok $b.signature.params[0].constraints.ACCEPTS('company')
        && $b.signature.params[1].constraints.ACCEPTS('careers'),
        'two literal parameters keep their constraints';
}

# A plain single parameter is unchanged.
{
    my $b = -> $x { $x * 2 };
    is $b(21), 42, 'an ordinary single parameter still binds';
}
