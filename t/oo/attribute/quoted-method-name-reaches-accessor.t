use Test;

# A quoted or run-time method name (`$obj."hash"()`, `$obj."$n"()`) reaches a
# generated attribute accessor whose name is also a built-in method (`hash`,
# `keys`, `list`), just as the plain `$obj.hash` does. It used to run the
# built-in coercion instead (JSON::Marshal reads every attribute through
# `$obj."$accessor-name"()`).

plan 8;

class I {
    has %.hash = A => 1;
    has %.keys = B => 2;
    has @.list = 1, 2;
    has $.Str = 'mine';
}

my $o = I.new;
my $n = 'hash';
is-deeply $o.hash, {A => 1}, 'plain accessor call (baseline)';
is-deeply $o."hash"(), {A => 1}, 'quoted name reaches the %.hash accessor';
is-deeply $o."$n"(), {A => 1}, 'run-time name reaches the %.hash accessor';
is-deeply I.new."hash"(), {A => 1}, 'on a non-variable receiver';
my $k = 'keys';
is-deeply $o."$k"(), {B => 2}, 'a %.keys accessor';
my $l = 'list';
is-deeply $o."$l"(), [1, 2], 'an @.list accessor';
my $s = 'Str';
is $o."$s"(), 'mine', 'a $.Str accessor';

class Plain { has $.x = 1 }
my $h = 'hash';
dies-ok { Plain.new."$h"() }, 'without such an accessor the built-in still runs';
