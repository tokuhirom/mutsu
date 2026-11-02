use Test;

plan 4;

my @seen;
class R is Array {
    method AT-POS($i) is rw { @seen.push($i.raku); callsame }
}
my @r is R = <a b c>;
my $a = @r["2"];
is $a, "c", 'string index on an is-Array subclass is numified';
my $b = @r[1e0];
is $b, "b", 'Num index works';
is-deeply @seen, ['2', '1'], 'user AT-POS received the numified indexes';
my @plain = <x y z>;
is @plain["1"], "y", 'plain Array string index still numifies';
