use Test;
plan 4;

# A user sub named like a statement-modifier keyword, called with glued
# parens right after a comma, is a call and not a modifier.
sub with(*@a) { "w" }
sub given(*@a) { "g" }

my $got = "";
class Cap { method print(*@a) { $got ~= @a.join; True }; method flush {} }
{
    my $*OUT = Cap.new;
    say 1, with(1);
    put 2, given(2);
}
is $got, "1w\n2g\n", "say/put list with keyword-named sub call";
say 3, if 0;
pass "trailing comma modifier still a modifier";
my @l = 1, with(2);
is @l.elems, 2, "assignment list";
my $o = "";
$o ~= ("a", with(1)).join;
is $o, "aw", "parenthesised list";
