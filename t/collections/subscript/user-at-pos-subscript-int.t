use Test;

# #15996: a `[ ]` subscript hands the object's own `AT-POS` / `ASSIGN-POS` /
# `BIND-POS` / `EXISTS-POS` the index as an `Int` (`@o[0.6] := v` calls
# `BIND-POS(0, v)`). The expected values are rakudo's.

plan 11;

my @seen;
class R is Array {
    method AT-POS($i) is rw { @seen.push("AT-POS " ~ $i.raku); callsame }
    method ASSIGN-POS($i, \v) { @seen.push("ASSIGN-POS " ~ $i.raku); nextsame }
    method BIND-POS($i, \v) { @seen.push("BIND-POS " ~ $i.raku); nextwith $i, v }
    method EXISTS-POS($i) { @seen.push("EXISTS-POS " ~ $i.raku); nextsame }
}
my @r is R = <a b c>;

@seen = ();
my $x = @r[1.7];
is @seen.join(","), 'AT-POS 1', 'a Rat is truncated for AT-POS';

@seen = ();
@r[2.9] = "Q";
is @seen.join(","), 'ASSIGN-POS 2', 'and for ASSIGN-POS';
is @r.join(","), 'a,b,Q', 'which stores at the truncated index';

@seen = ();
@r[0.6] := "Z";
is @seen.join(","), 'BIND-POS 0', 'and for BIND-POS';
is @r.join(","), 'Z,b,Q', 'which binds at the truncated index';

@seen = ();
my $e = @r[1.2]:exists;
is @seen.join(","), 'EXISTS-POS 1', 'and for :exists';
ok $e, 'which answers the element at that index';

@seen = ();
my $n = @r[1e0 + 0.5];
is @seen.join(","), 'AT-POS 1', 'a Num is truncated too';

# An Int index is unchanged, and a plain array is unaffected.
@seen = ();
my $y = @r[2];
my @plain = 1, 2, 3;
is (@plain[1.7], (@plain[0.2]:exists)).join(","), '2,True', 'a plain array reads the truncated element';

# Inf and NaN have no Int: the store raises instead of addressing slot 0.
my @fin = 1, 2, 3;
my $threw = False;
{ @fin[NaN] = 9; CATCH { default { $threw = True } } }
ok $threw, 'assigning to @a[NaN] throws';
is @fin.join(","), '1,2,3', 'and leaves the array alone';
