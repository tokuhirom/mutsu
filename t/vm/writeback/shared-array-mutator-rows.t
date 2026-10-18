use Test;

# `Array`'s mutator rows write a binding that several threads reach to its one
# shared home (ADR-11276 §9.169): every element a worker adds lands, through
# the method-call opcode and through a run-time method name alike.

plan 8;

{
    my @a;
    await (^8).map: -> $t { start { @a.push($t * 100 + $_) for ^50 } };
    is @a.elems, 400, 'concurrent push lands every element';
}

{
    my @a;
    await (^4).map: -> $t { start { @a.append(($t, $t)) for ^25 } };
    is @a.elems, 200, 'concurrent append flattens and lands';
}

{
    my @a;
    await (^4).map: -> $t { start { @a.unshift($t) for ^25 } };
    await (^4).map: -> $t { start { @a.prepend(($t, $t)) for ^25 } };
    is @a.elems, 300, 'concurrent unshift and prepend land';
}

{
    my @a;
    my $m = "push";
    await (^4).map: -> $t { start { @a."$m"($t) for ^50 } };
    is @a.elems, 200, 'a run-time method name takes the same lane';
}

{
    my @a;
    await (^4).map: -> $t { start { @a.push($_) for ^50 } };
    my @popped;
    await (^4).map: -> $t { start { my $x = @a.pop for ^25 } };
    is @a.elems, 100, 'concurrent pop removes every element it answers';
    await (^2).map: -> $t { start { @a.shift for ^25 } };
    is @a.elems, 50, 'concurrent shift too';
}

{
    my @a = 1..10;
    my @r;
    await start { @r = @a.splice(2, 3) };
    is-deeply @r, [3, 4, 5], 'splice from a worker answers the removed elements';
    is @a.elems, 7, '... and removes them';
}
