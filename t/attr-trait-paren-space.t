use v6;
use Test;

# A custom attribute trait argument must survive whitespace between the
# opening paren and the argument expression:
#   has Date $.d is unmarshalled-by( -> $d { ... });
# The parser used to hand the un-trimmed inner text to the expression parser,
# which failed on the leading space and silently dropped the argument
# (License::SPDX's `is unmarshalled-by( -> $d { DateTime.new($d).Date })`).

plan 6;

my %seen;

multi sub trait_mod:<is> (Attribute $attr, :&by!) {
    %seen{$attr.name} = &by;
}

class C {
    has Str $.tight is by(-> $v { "tight:$v" });
    has Str $.space is by( -> $v { "space:$v" });
    has Str $.both is by(  -> $v { "both:$v" }  );
}

ok %seen<$!tight>.defined, 'trait arg without space is captured';
ok %seen<$!space>.defined, 'trait arg with leading space is captured';
ok %seen<$!both>.defined, 'trait arg with leading and trailing space is captured';

is %seen<$!tight>.('a'), 'tight:a', 'no-space closure is callable';
is %seen<$!space>.('b'), 'space:b', 'leading-space closure is callable';
is %seen<$!both>.('c'), 'both:c', 'both-space closure is callable';

done-testing;
