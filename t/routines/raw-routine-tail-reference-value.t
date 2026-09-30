use Test;

# An `is raw` / `is rw` routine whose tail is a lexical holding a List or an
# object hands that value back. The tail compiles to a container capture, and
# a local holding a reference value is not re-boxed -- which the capture used
# to mistake for "no local slot here" and answer from a stale env entry, so
# the caller got `Any`. ValueList's `!SET-SELF` (`my $valuelist = self` as
# the tail of an `is raw` method) is the ecosystem case.

plan 8;

class A {
    method self-raw() is raw { my $v = self }
    method self-tail() is raw { my $v = self; $v }
    method type-tail() is raw { my $v = A; $v }
}

isa-ok A.new.self-raw, A, 'is raw method: declaration tail holding self';
isa-ok A.new.self-tail, A, 'is raw method: variable tail holding self';
is A.new.type-tail.^name, 'A', 'is raw method: variable tail holding a type object';

sub raw-list() is raw { my $v = (1, 2); $v }
is-deeply raw-list().List, (1, 2), 'is raw sub: variable tail holding a List';

sub raw-decl-list() is raw { my $v = (1, 2) }
is-deeply raw-decl-list().List, (1, 2), 'is raw sub: declaration tail holding a List';

sub rw-object() is rw { my $v = A.new; $v }
isa-ok rw-object(), A, 'is rw sub: variable tail holding an object';

sub raw-int() is raw { my $v = 42; $v }
is raw-int(), 42, 'is raw sub: a plain scalar tail still works';

my $box = 1;
sub raw-outer() is raw { $box }
raw-outer() = 5;
is $box, 5, 'is raw sub: an outer scalar tail is still its container';
