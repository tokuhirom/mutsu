use Test;

# `.split` pieces share the invocant's buffer (a `StrBody::Slice`) instead of
# copying it (#19201). Sharing must stay invisible: each piece is an ordinary,
# independent Str.

plan 13;

my $s = "héllo wörld ñ 😀x";
my @p = $s.split(" ");
is-deeply @p.List, ("héllo", "wörld", "ñ", "😀x"), 'non-ASCII pieces';
is @p.map(*.chars).join(","), "5,5,1,2", 'piece lengths count graphemes';

my $w = @p[1];
$w ~= "!";
is $w, "wörld!", 'appending to a piece';
is @p[1], "wörld", 'leaves the other holder of the piece alone';
is $s, "héllo wörld ñ 😀x", 'and the invocant';

my @q = "ab cd".split(" ");
@q[0] ~~ s/a/Z/;
is-deeply @q, ["Zb", "cd"], 'substitution on a piece';

my %h;
%h{$_}++ for "a b a c a".split(" ");
is-deeply %h, %(a => 3, b => 1, c => 1), 'pieces as hash keys';
ok "a,b".split(",")[0] eq "a", 'eq against a literal';
is "a,b".split(",")[1].uc, "B", 'methods on a piece';
is-deeply "a  b".split(" ").List, ("a", "", "b"), 'empty pieces';

my $big = ("x" x 100_000) ~ " y z";
is $big.split(" ")[1], "y", 'a small piece of a large invocant';
is $big.split(" ")[0].chars, 100_000, 'a large piece of a large invocant';
is ("a b" ~ " c").split(" ")[2], "c", 'a strand-list invocant';
