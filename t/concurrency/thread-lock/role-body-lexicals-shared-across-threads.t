use Test;

# #12825: a class or role body's `my` lexicals were recorded per thread, so a
# method run on a worker updated its own copy and the main thread never saw the
# write. Rakudo shares the one container. A mixin type adopted from another
# thread also re-ran the role body, so its lexicals started fresh there.

plan 6;

{
    role R { my $n = 0; method inc { ++$n } }
    class A does R {}
    my $a = A.new;
    is (await start { $a.inc }), 1, 'a role-body lexical is updated on a worker';
    is $a.inc, 2, 'the main thread sees the worker\'s write to a role-body lexical';
}

{
    class C { my $n = 0; method inc { ++$n } }
    my $c = C.new;
    await start { $c.inc };
    is $c.inc, 2, 'a class-body lexical is shared across threads';
}

{
    my @ran;
    role M { @ran.push('body'); my $n = 0; method bump { ++$n } }
    class Foo {}
    my $o = Foo.new;
    is (await start { $o does M; $o.bump }), 1, 'a mixin on a worker uses the role-body lexical';
    is $o.bump, 2, 'the adopting thread binds the same role-body lexical';
    is @ran.elems, 1, 'the role body runs once, not again on the adopting thread';
}
