use Test;

# Found via IRC::Log::Textual (IRC::Log's `search`): a native-typed pointy `if`
# (`if $c -> str $t { ... }`) lowers to a block call; binding a Seq to an outer
# variable as its last statement must not sink (consume) the Seq.
plan 3;

sub f($rev, $lt) {
    my $seq;
    if $lt -> str $target {
        $seq := 0 .. 0;
        $seq := $seq.reverse if $rev;
    }
    $seq := $seq.map: { $_ + 1 };
    $seq
}
is-deeply f(True, "x").Seq, (1,).Seq, 'bind behind a trailing statement modifier';

my $s;
if "x" -> str $t { $s := (1, 2).map: { $_ * 2 } }
is-deeply $s.Seq, (2, 4).Seq, 'plain bind as the block tail';

my $u;
if "x" -> Str $t { $u := (1, 2).map: { $_ * 2 } }
is-deeply $u.Seq, (2, 4).Seq, 'untyped-native pointy still behaves the same';

done-testing;
