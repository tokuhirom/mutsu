use Test;

# `Any`'s autovivifying rows refuse a receiver with no container to vivify
# (ADR-11276 §9.175): a read-only parameter or `for` alias, a name bound
# straight to a type object, a sigilless binding, a bare type word. A writable
# binding still vivifies. Expected values are Rakudo's.

plan 16;

# Read-only bindings refuse with the assignment's own error.
{
    sub f($p) { $p.push(1) }
    throws-like { f(Any) }, X::AdHoc, message => 'Cannot assign to a readonly variable or a value',
        'push on a read-only parameter';
    sub g($p) { $p.ASSIGN-KEY('a', 1) }
    throws-like { g(Any) }, X::AdHoc, message => 'Cannot assign to a readonly variable or a value',
        'ASSIGN-KEY on a read-only parameter';
    throws-like { for (Array,) -> $t { $t.push(1) } }, X::AdHoc,
        message => 'Cannot assign to a readonly variable or a value', 'push on a for alias';
    sub h($p is copy) { $p.push(1); $p }
    is h(Any).raku, '$[1]', 'an is copy parameter vivifies';
    sub k($p is rw) { $p.push(1) }
    my $v;
    k($v);
    is $v.raku, '$[1]', 'an is rw parameter vivifies the caller variable';
}

# A name bound straight to a type object has no container.
{
    my $x := Any;
    throws-like { $x.push(1) }, X::Assignment::RO,
        message => "Cannot modify an immutable 'Any' type object", 'push on a := type object';
    my \s = Any;
    throws-like { s.push(1) }, X::Assignment::RO, 'push on a sigilless type object';
    my $h := Hash;
    throws-like { $h.ASSIGN-KEY('a', 1) }, X::AdHoc,
        message => 'assign requires a concrete object (got a Hash type object instead)',
        'ASSIGN-KEY on a := type object';
}

# A bare type word is not a variable to vivify.
{
    throws-like { Hash.ASSIGN-KEY('a', 1) }, X::AdHoc,
        message => 'assign requires a concrete object (got a Hash type object instead)',
        'Hash.ASSIGN-KEY';
    throws-like { Hash.BIND-KEY('a', 1) }, X::Multi::NoMatch, 'Hash.BIND-KEY';
    dies-ok { Array.push(1) }, 'Array.push';
}

# Writable bindings still vivify.
{
    my $z;
    $z.push(3);
    is $z.raku, '$[3]', 'a my variable';
    my $Any;
    $Any.push(1);
    is $Any.raku, '$[1]', 'a variable that happens to be called $Any';
    our $o;
    $o.push(1);
    is $o.raku, '$[1]', 'an our variable';
    my @a;
    @a[2].push(1);
    is @a.raku, '[Any, Any, [1]]', 'an array element';
    my $y;
    $y.ASSIGN-KEY('a', 1);
    is $y.raku, '${:a(1)}', 'ASSIGN-KEY on a my variable';
}
