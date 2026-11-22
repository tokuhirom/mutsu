use Test;

# A method call on a role type object puns the role and calls the pun's
# method, except for the names rakudo's RolePunning keeps on `Mu`: those are
# answered by `Mu` even when the role declares its own (#18062).

my @mu = <ACCEPTS item Bool gist raku Str WHICH WHY set_why Numeric Real defined perl Stringy>;
my @punned = <new bless hello Int list hash elems>;

plan @mu + @punned + 2;

for @mu -> $n {
    my $r = EVAL "role R$n \{ method $n\(|) \{ 'USER' \} \}; R$n";
    my $got = quietly try { $r."$n"() };
    isnt ($got // ''), 'USER', "R.$n answers from Mu";
}
for @punned -> $n {
    my $r = EVAL "role P$n \{ method $n\(|) \{ 'USER' \} \}; P$n";
    my $got = try { $r."$n"() };
    is ($got // ''), 'USER', "R.$n puns the role";
}

role S { method list(|) { 'own list' }; method Str(|) { 'own Str' } }
is S.list, 'own list', 'the static spelling puns too';
is (quietly S.Str), '', 'and keeps Str on Mu';
