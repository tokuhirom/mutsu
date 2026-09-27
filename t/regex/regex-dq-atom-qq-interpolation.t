use Test;

# A double-quoted regex atom follows qq-string rules: subscripts, method
# calls, `&`-calls and embedded blocks all interpolate their result, which
# then matches literally (issue #9628).

plan 21;

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
