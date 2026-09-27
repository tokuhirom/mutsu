use Test;

# A double-quoted regex atom follows qq-string rules: subscripts, method
# calls, `&`-calls and embedded blocks all interpolate their result, which
# then matches literally (issue #9628).

plan 40;

my @a = <p q>;
my %h = a => 'p';
my $s = 'p';

ok "x p"   ~~ /"x @a[0]"/,        q{"@a[0]"};
ok "x q"   ~~ /"x @a[1]"/,        q{"@a[1]"};
ok "x p,q" ~~ /"x @a.join(",")"/, q{"@a.join(",")" with a quote in the arguments};
ok "x p"   ~~ /"x %h<a>"/,        q{"%h<a>"};
ok "x p"   ~~ /"x %h{'a'}"/,      q{"%h{'a'}"};
ok "x P"   ~~ /"x $s.uc()"/,      q{"$s.uc()"};
ok "x p"   ~~ /"x {@a[0]}"/,      q{"{@a[0]}"};
ok "x 3"   ~~ /"x {1+2}"/,        'a block with no variable in it';
nok "x q"  ~~ /"x @a[0]"/,        'the interpolated value is what must match';
ok "x p q" ~~ /"x @a[]"/,         'a zen slice still joins the elements';
ok "x @a"  ~~ /"x @a"/,           'a bare @a is still literal text';

sub f { 'p' }
ok "x p" ~~ /"x &f()"/, q{"&f()"};

# The result is a literal: metacharacters and spaces in it are not regex.
my @meta = 'a.b c';
ok  "a.b c" ~~ /^ "@meta[0]" $/, 'metacharacters in the result are literal';
nok "axb c" ~~ /^ "@meta[0]" $/, 'a . in the result does not match any char';

ok "X P" ~~ m:i/"x @a[0]"/, ':i applies to the interpolated text';
is ~("pqpq" ~~ /"@a[0]@a[1]"+/), 'pqpq', 'a quantifier applies to the whole atom';

$_ = 'x q';
ok m/"x @a[1]"/, 'a bare m// against $_';

# A stored regex sees its defining scope's current values.
my $r = /"v=%h<a>"/;
%h<a> = 'w';
ok "v=w" ~~ $r, 'a stored regex re-evaluates the atom at match time';

sub make-re { my @w = 'z'; /"x @w[0]"/ }
ok "x z" ~~ make-re(), 'an escaping regex keeps its defining scope';

is "a p b p".subst(/"@a[0]"/, 'Q', :g), 'a Q b Q', '.subst';

# The atom is evaluated once per match.
my $calls = 0;
sub g { $calls++; 'p' }
"x p" ~~ /"x {g()}"/;
is $calls, 1, 'the atom is evaluated once per match';

# s/// and S/// carry the thunks too (#9673).
{
    my $s = "x p y";
    $s ~~ s/"x @a[0]"/Z/;
    is $s, 'Z y', 's/// with a subscript in a "..." atom';
    is (S/"@a[1]"/W/ given "q r"), 'W r', 'S///';
    my $g = "p p p";
    $g ~~ s:g/"@a[0]"/X/;
    is $g, 'X X X', 's:g///';
    my $i = "P";
    $i ~~ s:i/"@a[0]"/lower/;
    is $i, 'lower', 's:i///';
    sub subst-local { my @w = <m n>; my $t = "m n"; $t ~~ s/"@w[1]"/N/; $t }
    is subst-local(), 'm N', 's/// in a sub sees its own lexicals';
}

# token / rule declarations.
grammar QQ-G1 { token TOP { "@a[0]" } }
ok QQ-G1.parse("p"), 'a grammar token';
grammar QQ-G2 { token TOP { <w>+ % ',' }; token w { "@a[0]" | "@a[1]" } }
is ~QQ-G2.parse("p,q,p"), 'p,q,p', 'a subrule in an alternation';
grammar QQ-G3 { rule TOP { "@a[0]" "@a[1]" } }
ok QQ-G3.parse("p q"), 'a rule';
grammar QQ-G4 { token TOP { :i "@a[1]" } }
ok QQ-G4.parse("Q"), ':i in a token';
sub make-grammar { my @z = <zz>; my grammar LG { token TOP { "@z[0]" } }; LG }
ok make-grammar().parse("zz"), 'a lexical grammar sees its defining scope';
role QQ-R { token rt { "@a[1]" } }
grammar QQ-G5 does QQ-R { token TOP { <rt> } }
ok QQ-G5.parse("q"), 'a token composed from a role';
my token qq-tt { "@a[1]" }
ok "q" ~~ &qq-tt, 'a my token matched directly';
ok "xqy" ~~ /^ x <&qq-tt> y $/, 'a my token called as <&name>';
grammar QQ-G6 { token TOP { "@a[0]" } }
@a[0] = 'again';
ok QQ-G6.parse("again"), 'a token reads the current value at match time';
@a[0] = 'p';
my $tcalls = 0;
sub tg { $tcalls++; 'p' }
grammar QQ-G7 { token TOP { "{tg()}" } }
QQ-G7.parse("p");
is $tcalls, 1, 'a token atom is evaluated once per parse';

# <$re> interpolation.
my $re = /"@a[1]"/;
ok "q" ~~ /<$re>/, '<$re>';
sub mk-re { my @m = <mm>; /"@m[0]"/ }
my $re2 = mk-re();
ok "mm" ~~ /^ <$re2> $/, '<$re> of an escaping regex uses its own scope';
my @res = /"@a[1]"/, /"%h<a>"/;
ok "w" ~~ /^ <@res> $/, '<@res>';
my $re3 = /"@a[0]"/;
ok "pq" ~~ /^ <$re3> "@a[1]" $/, 'an inner and an outer atom in one match';
