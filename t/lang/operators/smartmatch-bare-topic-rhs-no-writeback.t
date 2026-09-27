use Test;

# `$x ~~ $_` with a bare `$_` on the right reads the ENCLOSING topic (the RHS
# topic is not replaced by the LHS for that spelling). The topic therefore
# still holding the outer value after the match is not a destructive
# modification of the LHS — mutsu took it for one and wrote the element into
# `$x`, so `@list.grep({ $obj !~~ $_ })` overwrote `$obj` with each element.
# Found through Tinky's `t/030-apply-simple.t`.

plan 5;

class O { }
class T { multi method ACCEPTS(O:D $o) { True } }
my $obj = O.new;
my @t = T.new, T.new;

is @t.map({ so $obj ~~ $_ }), (True, True), 'every element matches';
ok $obj ~~ O, 'the LHS variable is unchanged';
is @t.grep({ $obj !~~ $_ }).elems, 0, 'a negated match in grep sees the same LHS every time';
ok $obj ~~ O, 'and still leaves it unchanged';

my $s = 'abc';
for <x> { $s ~~ s/b/B/ }
is $s, 'aBc', 'a destructive RHS still writes back';
