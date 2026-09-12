use Test;

# A `?` or `^` modifier may precede a QUOTED method name on the topic:
# `.?"$name"()`. The explicit-invocant spelling (`$x.?"$name"()`) has always
# parsed; the topic parser tried the quoted name before consuming the
# modifier, so the two could not be combined and the whole enclosing block
# failed to parse.
# From Red 0.2.5 (`-> $_ { .?"{ $attr.type.^name }"() // .self }`), which
# blocked Red, RedFactory and RedX::HashedPassword from loading.

plan 6;

my $inflate = -> $_ { .?"no-such-method"() // .self };
is $inflate(1), 1, '.?"literal"() on the topic falls back when absent';

my $name = 'chars';
with 'xy' {
    is .?"$name"(), 2, '.?"$interpolated"() on the topic calls the method';
}

with 42 {
    is .?"no-such-method"() // 'nope', 'nope', '.? on the topic is Nil when absent';
}

# An interpolated block in the name, as Red spells it.
with 'xy' {
    is .?"{ 'ch' ~ 'ars' }"(), 2, '.?"{ ... }"() interpolates a block';
}

# The spellings this one combines keep working on their own.
with 'xy' {
    is ."$name"(), 2, '."$name"() without a modifier';
    is .?chars, 2, '.?method without a quote';
}
