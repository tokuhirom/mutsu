use Test;

# Private method call failures are structured exceptions:
#  - a fully-qualified call `$o!Owner::meth` where Owner does not trust the
#    caller throws X::Method::Private::Permission (method / source-package /
#    calling-package);
#  - an unqualified call `$o!meth` on something other than self throws
#    X::Method::Private::Unqualified (method).

plan 3;

throws-like 'my class A { my @a; @a!List::foo() }',
    X::Method::Private::Permission,
    'qualified call without trust',
    method          => 'foo',
    calling-package => 'A',
    source-package  => 'List';

throws-like '1!foo()',
    X::Method::Private::Unqualified,
    'unqualified call on a non-self invocant',
    method => 'foo';

# A legitimate trusted call still works.
lives-ok {
    EVAL q:to/CODE/;
        class Safe {
            trusts Caller;
            has $!secret = 42;
            method !reveal() { $!secret }
        }
        class Caller {
            method peek(Safe $s) { $s!Safe::reveal() }
        }
        die "wrong" unless Caller.new.peek(Safe.new) == 42;
        CODE
}, 'a trusted qualified private call works';
