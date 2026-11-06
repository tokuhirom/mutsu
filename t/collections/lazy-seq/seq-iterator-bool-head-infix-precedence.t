use Test;

plan 7;

# A `multi infix:<...>` without a precedence trait adds a candidate to the core
# operator and keeps the sequence operator's precedence (looser than a comma).
multi sub infix:<...>(Str $a, Str $b where "zzz") { "custom" }
is (1, 2 ... 4).head(4).join(","), "1,2,3,4", "trait-less ... candidate keeps core precedence";
is ("a" ... "zzz").head(1).join(","), "custom", "the declared candidate is still dispatched";

# `.Bool` on an unbounded Seq.new($iterator) pulls one element, not all of them.
my class It does Iterator {
    has $!n = 0;
    method pull-one() { $!n++ }
    method is-lazy(--> True) { }
}
sub mk() { Seq.new: It.new }
is mk().Bool, True, ".Bool of an unbounded iterator Seq answers";
my $s = mk();
is $s.Bool, True, ".Bool before reading";
is $s.head(3).raku, "(0, 1, 2).Seq", ".head after .Bool keeps the pulled element";
my $t = mk();
is so($t), True, "so before reading";
is $t.head(3).raku, "(0, 1, 2).Seq", ".head after so keeps the pulled element";
