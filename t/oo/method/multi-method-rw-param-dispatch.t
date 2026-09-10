use v6;
use Test;

plan 6;

# An `is rw` parameter only accepts a writable argument, so multi dispatch uses
# it to narrow candidates. For a `proto method f(|) {*}` the call-site argument
# sources have to survive the proto body into the `{*}` redispatch, or the rw
# candidate becomes unmatchable. (HTTP::UserAgent's
# `multi method get-content(Connection, Blob $content is rw)`.)

class A {
    proto method f(|) {*}
    multi method f(Int $b, $len) { "2-arg" }
    multi method f(Int $b is rw) { "1-arg-rw" }
}

my $i = 5;
is A.new.f($i, 3), "2-arg", 'the two-arg candidate still wins with two args';
is A.new.f($i), "1-arg-rw", 'the rw candidate is reachable through a proto method';

# Same shape with a role-typed invocant argument and a Buf, as in the module
# that surfaced this.
role R { }
class C { }

class B {
    proto method g(|) {*}
    multi method g(R $c, Blob $b, $len) { "3-arg" }
    multi method g(R $c, Blob $b is rw) { "2-arg" }
}

my $conn = C.new but R;
my $content = Buf.new(1, 2, 3);
is B.new.g($conn, $content, 3), "3-arg", 'mixin invocant, three args';
is B.new.g($conn, $content), "2-arg", 'mixin invocant, rw candidate';

# A literal is not writable, so the rw candidate must not match it.
class D {
    proto method h(|) {*}
    multi method h(Int $b is rw) { "rw" }
    multi method h(Int $b) { "ro" }
}
is D.new.h(5), "ro", 'a literal argument picks the readonly candidate';
my $w = 5;
is D.new.h($w), "rw", 'a variable argument picks the rw candidate';
