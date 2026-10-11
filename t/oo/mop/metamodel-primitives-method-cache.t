use Test;

plan 7;

# Metamodel::Primitives.install_method_cache (#9797)
my $t = Metamodel::Primitives.create_type(
    Metamodel::ClassHOW.new_type(name => "CachedT"), 'Uninstantiable');
Metamodel::Primitives.install_method_cache($t, { foo => -> $ { "foo!" } }, :authoritative);
is $t.foo, "foo!", 'a cached method is called with the type object';

throws-like { $t.bar }, X::Method::NotFound,
    'an authoritative cache rejects a name it does not hold';

my $lax = Metamodel::Primitives.create_type(
    Metamodel::ClassHOW.new_type(name => "LaxT"), 'Uninstantiable');
Metamodel::Primitives.install_method_cache($lax, { baz => -> $, $x { "baz $x" } }, :!authoritative);
is $lax.baz(3), "baz 3", 'cached methods receive their arguments';
ok $lax.WHAT.defined.not, 'introspection methods are unaffected by the cache';

# Metamodel::Primitives.set_parameterizer / parameterize_type (#9797)
my $made = 0;
package Cache {
    our sub parameterize(+args) is raw {
        Metamodel::Primitives.parameterize_type: $?PACKAGE, args
    }
    sub make(Mu, Mu \args) is raw { $made++; "made " ~ args.join(",") }
    BEGIN Metamodel::Primitives.set_parameterizer: $?PACKAGE, &make;
}
is Cache::parameterize(1, 2), "made 1,2", 'the parameterizer builds the parameterization';
Cache::parameterize(1, 2);
is $made, 1, 'the same arguments hit the parameterization cache';
is Cache::parameterize(3), "made 3", 'different arguments build a new one';
