use v6;
use Test;

# A destructive s/// / tr/// against a non-Var lvalue LHS must write the
# modified topic back through the lvalue, as Raku's container semantics do:
#   - an rw accessor LHS   ($obj.text ~~ s///)   — Text::CSV allow_whitespace
#   - an array element LHS (@a[$i]   ~~ s///)    — Text::CSV escaped-NUL "=0"
#   - a hash element LHS   (%h<k>    ~~ s///)
# (found via Text::CSV's 65_allow.t, tests 171..417 and 1022)

plan 13;

class F { has Str $.text is rw }

# rw accessor LHS
{
    my $f = F.new(text => "foo  ");
    $f.text ~~ s{ <[\ \t]>+ $ } = "";
    is $f.text, "foo", 'accessor LHS: s{} = "" writes back through rw accessor';
}
{
    my $f = F.new(text => "bar  ");
    my $r = ($f.text ~~ s/\s+$//);
    is $f.text, "bar", 'accessor LHS: s/// writes back';
    ok $r, 's/// on accessor returns a truthy Match on success';
}
{
    my $f = F.new(text => "keep");
    my $r = ($f.text ~~ s/zz/x/);
    is $f.text, "keep", 'accessor LHS: failed match leaves value untouched';
    nok $r, 'failed s/// returns falsy';
}

# array element LHS
{
    my @a = "0abc", "x";
    @a[0] ~~ s{^ "0"} = "";
    is-deeply @a, ["abc", "x"], 'array element LHS: literal index';
}
{
    my $i = 0;
    my @a = "ya", "0zb";
    @a[$i + 1] ~~ s{^ "0"} = "";
    is-deeply @a, ["ya", "zb"], 'array element LHS: computed index evaluated once';
}
{
    my @a = "aa", "bb";
    @a[0] ~~ s/zz/x/;
    is-deeply @a, ["aa", "bb"], 'array element LHS: failed match stores nothing';
    my $r = (@a[1] ~~ s/b/X/);
    is-deeply @a, ["aa", "Xb"], 'array element LHS: match writes element';
    ok $r ~~ Match, 'element s/// returns the Match';
}

# hash element LHS
{
    my %h = k => "0v";
    %h<k> ~~ s/^0//;
    is %h<k>, "v", 'hash element LHS: s/// writes back';
    %h<k> ~~ s/zz/x/;
    is %h<k>, "v", 'hash element LHS: failed match leaves value';
}

# tr/// through an accessor LHS
{
    my $f = F.new(text => "abc");
    $f.text ~~ tr/a/z/;
    is $f.text, "zbc", 'accessor LHS: destructive tr/// writes back';
}

done-testing;
