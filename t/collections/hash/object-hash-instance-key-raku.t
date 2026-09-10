use Test;

# An object-hash *instance* key (as opposed to a type-object key, already
# pinned by t/object-hash-raku-key-parens.t) rendered as `U()` instead of
# `U.new` in `.raku` -- and a user-defined `method raku` on the key's class
# was not dispatched at all. The value side already dispatched the real
# `.raku` method (via call_method_with_values); the key side used the
# allocation-free raku_value fast path, which cannot call into the
# interpreter. (todo/tickets/object-hash-instance-key-raku-rendering.md)

plan 4;

my class U { }
my %q{Mu};
%q{U.new} = 1;
is %q.raku, '(my Any %{Mu} = U.new => 1)', 'a plain instance key renders as ClassName.new';

my class Point { has $.x; has $.y }
my %p{Mu};
%p{Point.new(x => 1, y => 2)} = "v";
is %p.raku, '(my Any %{Mu} = Point.new(x => 1, y => 2) => "v")',
    'an instance key with attributes renders its full constructor form';

my class WithRaku {
    method raku { "MyU.new" }
}
my %r{Mu};
%r{WithRaku.new} = 1;
is %r.raku, '(my Any %{Mu} = MyU.new => 1)',
    "a user-defined method raku on the key's class is dispatched";

# The value side must still work unchanged alongside an instance key.
my %v{Mu};
%v{U.new} = WithRaku.new;
is %v.raku, '(my Any %{Mu} = U.new => MyU.new)',
    'the value side keeps dispatching method raku independently of the key fix';
