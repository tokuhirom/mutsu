use Test;

# `.VAR.name` is the name of the container a variable holds, as rakudo keeps
# it on the container's descriptor: the declaration that named it, however
# many parameters it was passed through, else "element" for an anonymous one.

plan 16;

sub f(@kh) { @kh.VAR.name }
sub g(%kh) { %kh.VAR.name }
sub chain(@kh) { f(@kh) }

my @named = 1, 2;
is f(@named), '@named', 'a declared array passed to a parameter';
is f([1, 2]), 'element', 'a literal array is anonymous';
is chain(@named), '@named', 'the name survives a second parameter';
is chain([1]), 'element', 'an anonymous container stays anonymous through a chain';

my @bound := [1, 2];
is @bound.VAR.name, 'element', 'a `:=`-bound literal has no name of its own';
is f(@bound), 'element', '... nor through a parameter';

my @src = 1;
my @alias := @src;
is f(@alias), '@src', 'a `:=` alias reports the container it aliases';

state @st = 1;
is f(@st), '@st', 'a state array names its container';

my %h = a => 1;
is g(%h), '%h', 'a declared hash passed to a parameter';
is g(Hash.new(a => 1)), 'element', 'a constructed hash is anonymous';

class K {
    has @.pub = 1;
    has %!priv;
    method pass-pub { f(@!pub) }
    method pass-priv { g(%!priv) }
}
is K.new.pass-pub, '@!pub', 'a public attribute passed to a parameter';
is K.new.pass-priv, '%!priv', 'a private attribute passed to a parameter';

sub rw($x is rw) { $x.VAR.name }
sub raw(\x) { x.VAR.name }
my $*DYN = 1;
is rw($*DYN), '$*DYN', 'an `is rw` parameter aliasing a dynamic variable';
is raw($*DYN), '$*DYN', 'a sigilless parameter aliasing a dynamic variable';
is rw(my $*FRESH), '$*FRESH', 'a dynamic variable declared in the argument list';

sub writes(\x) { x = 5 }
writes($*DYN);
is $*DYN, 5, 'a write through the parameter reaches the dynamic variable';
