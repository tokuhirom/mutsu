use v6;
use Test;

# A `class`/`role`/`grammar`/`enum` declared and RUN inside a sub or closure
# body installs into the enclosing PACKAGE, not the sub's own call-frame
# lexical scope -- regardless of how deeply the declaration is nested inside
# call frames. `::('Name')` (indirect symbolic package lookup) must therefore
# still find it after the declaring sub has returned, even though the
# declaration only ever ran while that frame was live.
#
# Regression (#8683): the bareword binding a declaration installs was written
# only into the CURRENT lexical env tier, which is exactly what a sub call
# frame discards on return. The value the sub actually produced (`Foo.new`)
# still reported the right `.WHAT`/`.^name`, since that identity comes from
# the registry, not the env -- only the indirect, name-based re-lookup from
# outside the frame silently returned a `Failure`, which is what fed a false
# negative into `isa-ok`'s internal `nqp::istype($x, ::('Foo').WHAT)`.
plan 10;

sub make-class() {
    class Foo8683 {
        has $.bar;
    }
}
make-class();
is ::('Foo8683').^name, 'Foo8683',
    'a class declared inside a named sub is visible via ::() after it returns';

my $c;
(sub {
    class Bar8683 {
        has $.baz;
    }
    $c = Bar8683.new;
})();
is $c.^name, 'Bar8683',
    'the value produced inside the closure already carried the right type';
is ::('Bar8683').^name, 'Bar8683',
    'a class declared inside an invoked closure is visible via ::() too';

sub make-role() {
    role Quux8683 {
        method greet { 'hi' }
    }
}
make-role();
is ::('Quux8683').^name, 'Quux8683',
    'a role declared inside a sub is visible via ::() after it returns';

sub make-grammar() {
    grammar Gram8683 {
        token TOP { \d+ }
    }
}
make-grammar();
is ::('Gram8683').^name, 'Gram8683',
    'a grammar declared inside a sub is visible via ::() after it returns';
ok ::('Gram8683').parse('123'), 'and the recovered grammar still parses';

sub make-enum() {
    enum Enum8683 <Red8683 Green8683 Blue8683>;
}
make-enum();
is ::('Enum8683').^name, 'Enum8683',
    'an enum declared inside a sub is visible via ::() after it returns';

# The real-world trigger: `Test.rakumod`'s `isa-ok` calls
# `nqp::istype($var, $type.WHAT)` for a non-Str expected type, so a `Failure`
# from ::() silently read as a mismatch instead of throwing.
sub make-isa-target() {
    class IsaTarget8683 { }
}
make-isa-target();
isa-ok IsaTarget8683.new, ::('IsaTarget8683'),
    'isa-ok against a ::()-recovered type finds a sub-declared class';

# `my class`/`my role` stay lexically scoped to their declaring block: they
# must NOT leak into the enclosing package just because the general (non-`my`)
# case now does. This is the guard the fix must not break.
sub make-lexical-class() {
    my class LexFoo8683 { }
}
make-lexical-class();
isa-ok ::('LexFoo8683'), Failure,
    'a my class declared inside a sub stays invisible to ::() after it returns';

sub make-lexical-enum() {
    my enum LexEnum8683 <A8683 B8683>;
}
make-lexical-enum();
isa-ok ::('LexEnum8683'), Failure,
    'a my enum declared inside a sub stays invisible to ::() after it returns';

done-testing;
