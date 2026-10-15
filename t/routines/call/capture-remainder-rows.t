use Test;

# ADR-11276 slice 3C remainder: Capture's last cascade arms are rows. Expected
# values are Rakudo's.
plan 12;

my $c = \(1, 2, :a(3));
ok $c.Bool, 'a capture with parts is true';
nok \().Bool, 'an empty capture is false';
is-deeply $c.Hash, {a => 3}, '.Hash is the named part';
is-deeply $c.List, (1, 2), '.List is the positional part, not a list holding the capture';
is $c.Seq.raku, '(1, 2).Seq', '.Seq reads the positionals';
is $c.flat.raku, '(1, 2).Seq', '.flat reads the positionals';
nok $c.is-lazy, '.is-lazy';
is $c.raku, '\(1, 2, :a(3))', '.raku';
is $c.gist, '\(1, 2, :a(3))', '.gist';
is $c.WHAT.gist, '(Capture)', '.WHAT';
dies-ok { $c.Int }, 'Capture has no .Int (Rakudo: no matching candidate)';
is +$c, 2, 'numeric context counts the positionals';
