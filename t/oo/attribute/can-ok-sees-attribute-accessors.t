use Test;

# In Raku the auto-generated accessor for `has $.x` is an ordinary method of its
# declaring class, so it is exactly as `can`-able as a written one. mutsu's
# `can-ok` walked only the method tables, so it answered False for every
# accessor -- while `$obj.can('attr')`, which resolves through
# `resolve_user_method_or_accessor`, answered True. The two disagreed about the
# same question.

plan 12;

class C {
    has $.a;
    has Str $.b-c;
    has @.d;
    has %.e;
    has $!private;
    method m() { }
    method n-o() { }
}

my $c = C.new;

can-ok $c, 'm', 'a written method';
can-ok $c, 'n-o', 'a written method with a hyphen';
can-ok $c, 'a', 'a `$` accessor';
can-ok $c, 'b-c', 'a typed `$` accessor with a hyphen';
can-ok $c, 'd', 'an `@` accessor';
can-ok $c, 'e', 'a `%` accessor';
can-ok $c, 'new', 'the default constructor';

# The type object can do them too.
can-ok C, 'a', 'a `$` accessor on the type object';
can-ok C, 'm', 'a written method on the type object';

# `.can` and `can-ok` must agree -- that they did not is what this pins.
ok $c.can('a').so, '.can agrees for an accessor';
ok $c.can('m').so, '.can agrees for a method';

# A private attribute generates no accessor, so neither should see one.
nok $c.can('private').so, 'a private attribute has no accessor';
