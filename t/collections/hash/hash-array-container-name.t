use Test;

# `.name` on a Hash/Array is its container descriptor's name (#11415): the
# declaring variable, which travels with the container through every
# pass-by-binding chain, or "element" when no declaration named it.

plan 14;

sub var-name(\m) { m.VAR.name }

my %h = a => 1;
my @a;
is var-name(%h), '%h', '.VAR.name through a sigilless param bound to a Hash';
is var-name(@a), '@a', '.VAR.name through a sigilless param bound to an Array';

my role R { method x { var-name(self) } }
%h does R;
is %h.x, '%h', 'self in a role mixed into a Hash keeps the descriptor name';

my $x = %h;
is $x.name, '%h', '.name on a Hash held in a scalar';
my $y = @a;
is $y.name, '@a', '.name on an Array held in a scalar';

is Hash.new.name, 'element', 'an anonymous Hash.new is "element"';
is Array.new.name, 'element', 'an anonymous Array.new is "element"';
is [1].name, 'element', 'an array literal is "element"';
dies-ok { (1, 2).name }, 'a List has no container descriptor';

sub param-name(@p) { @p.name }
my @b;
is param-name(@b), '@b', 'a @-param reports the bound caller container';

our @o;
is @o.name, '@o', '.name on an `our` array';
state %s;
is %s.name, '%s', '.name on a `state` hash';

class C {
    has @!x;
    has %!y;
    method names { @!x.name ~ ' ' ~ %!y.name }
}
is C.new.names, '@!x %!y', '.name on private attribute containers';

my Int @t;
is @t.name, '@t', '.name on a typed array';
