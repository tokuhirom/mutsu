use Test;

# `.VAR.of` / `.VAR.default` of a name `:=`-bound to another container report
# that container's descriptor (#18764).

plan 7;

my Int $x = 1;
my $y := $x;
is $y.VAR.of.raku, 'Int', '.of of a name bound to a typed lexical';
is $y.VAR.default.raku, 'Int', '.default of a name bound to a typed lexical';

my $a is default(5) = 1;
my $b := $a;
is $b.VAR.default, 5, '.default of a name bound to an is-default lexical';

class D { has Int $.x is rw = 1 }
my $d = D.new;
my $z := $d.x;
is $z.VAR.of.raku, 'Int', '.of of a name bound to a typed attribute';
is $d.x.VAR.of.raku, 'Int', '.of of the attribute itself';

my $u;
my $w := $u;
is $w.VAR.of.raku, 'Mu', 'an untyped binding stays Mu';
is $w.VAR.default.raku, 'Any', 'an untyped binding defaults to Any';

