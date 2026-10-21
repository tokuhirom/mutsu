use Test;

# IO::Path::Parts does Iterable, Associative and Positional: its roles are
# listed and Iterable's hyper/race/lazy read its three pairs.
plan 7;

my $p = IO::Path::Parts.new("C:", "/d", "f");
is $p.^roles.map(*.^name).join(' '), 'Iterable Associative Positional', '.^roles lists the composed roles';
ok $p ~~ Iterable, 'it is Iterable';
is $p.lazy.raku, '(:volume("C:"), :dirname("/d"), :basename("f")).lazy.Seq', '.lazy';
is $p.hyper.map(*.key).list.join(','), 'volume,dirname,basename', '.hyper';
is $p.race.map(*.key).list.join(','), 'volume,dirname,basename', '.race';
is IO::Path::Parts.^roles.map(*.^name).join(' '), 'Iterable Associative Positional', 'the type object lists them too';
is $p.^roles(:local).elems, 3, ':local lists the three';
