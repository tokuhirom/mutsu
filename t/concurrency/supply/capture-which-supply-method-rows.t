use Test;
plan 16;

# Capture.WHICH and List.Supply are rows (ADR-11276 §9.66); expectations checked against raku.
my $c = \(1, 2, :a(3));
is $c.WHICH, 'Capture|(Int|1)(Int|2)|a(Int|3)', 'Capture.WHICH: positionals, then nameds';
is $c.WHICH.WHAT.raku, 'ValueObjAt', 'a Capture has a value identity';
is \(1, "a").WHICH, 'Capture|(Int|1)(Str|a)', 'positionals only';
is \(:a(1)).WHICH, 'Capture|a(Int|1)', 'nameds only';
is \(:b(2), :a(3)).WHICH, 'Capture|a(Int|3)b(Int|2)', 'nameds are sorted by key';
is \().WHICH, 'Capture', 'an empty Capture';
ok \(1, 2) === \(1, 2), 'equal Captures are ===';
nok \(1, 2) === \(1, 3), 'different Captures are not ===';
ok $c === $c, 'a Capture is === itself';
ok \(1, (2, 3)).WHICH.Str ~~ /^ 'Capture|(Int|1)(' \w+ '|' \d+ ')'$/, 'a reference element keeps its object identity';

my @got;
(1, 2, 3).Supply.tap({ @got.push($_) });
is @got.join(' '), '1 2 3', 'List.Supply emits the elements';
@got = ();
my @a = <a b>;
@a.Supply.tap({ @got.push($_) });
is @got.join(' '), 'a b', 'Array.Supply emits the elements';
@got = ();
(1..3).Supply.tap({ @got.push($_) });
is @got.join(' '), '1 2 3', 'Range.Supply emits the elements';
is (1, 2).Supply.WHAT.raku, 'Supply', 'a Supply';
is 5.Supply.WHAT.raku, 'Supply', 'a scalar is a one-element Supply';
@got = ();
5.Supply.tap({ @got.push($_) });
is @got.join(' '), '5', 'its element';
