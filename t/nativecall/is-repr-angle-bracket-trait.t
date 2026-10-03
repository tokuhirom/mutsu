use Test;

plan 8;

# Raku accepts `is repr<Name>` (angle/word-quoting) as an alternative spelling
# of `is repr('Name')` for the `repr` trait — real dists such as
# NativeCall::Types use the angle form exclusively (`native long is Int is
# ctype<long> is repr<P6int> { }`). Previously mutsu's `is`/`does`/`hides`
# loops only recognised a parenthesized trait argument, so `is repr<...>`
# silently failed to parse the class and the parser fell back to treating
# `class Foo` and `is repr<CStruct> { }` as separate (nonsensical)
# expressions, surfacing as a confusing `Unknown function: is` runtime error.

class AStruct is repr<CStruct> {
    has uint64 $.a;
}
is AStruct.REPR, 'CStruct', 'class: is repr<CStruct> (angle form) sets REPR like is repr(\'CStruct\')';

class Uninstantiable is repr<Uninstantiable> { }
is Uninstantiable.^name, 'Uninstantiable', 'class: is repr<Uninstantiable> parses (non-C repr name)';

# --- trait_mod:<is> dispatch: an angle-bracket argument reaches user code
# exactly like a parenthesized one, both for classes and for roles. The
# trait is a made-up one (`flavour`) with no dedicated AST field (unlike
# `repr`), so it is only observable through the custom_traits ->
# trait_mod:<is> dispatch mechanism. (`ctype` cannot serve here: it is a core
# NativeHOW trait with its own core candidate, so a user candidate of the same
# shape is a redeclaration in rakudo -- see native-type-repr-and-traits.t.)
# Traits apply at compile time in rakudo, so @log is never reset between
# declarations; each check greps for its own type's entry.

my @log;
multi sub trait_mod:<is>(Mu:U $t, :$flavour!) { @log.push("{$t.^name}:flavour:{$flavour}") }

class WithFlavour is flavour<long> { }
ok @log.grep('WithFlavour:flavour:long'),
    'class: is flavour<long> (angle form, no dedicated field) reaches trait_mod:<is>';

role RoleWithRepr is repr<CStruct> { }
nok @log.grep(/RoleWithRepr/), 'role: is repr<...> does not spuriously dispatch an unrelated trait';

role RoleWithFlavour is flavour<long> { }
ok @log.grep('RoleWithFlavour:flavour:long'),
    'role: is flavour<long> (angle form) reaches trait_mod:<is>, same as class';

# --- sanity: the pre-existing parenthesized form keeps working unchanged.

class ParenStruct is repr('CStruct') {
    has uint64 $.a;
}
is ParenStruct.REPR, 'CStruct', 'sanity: is repr(\'CStruct\') (paren form) still works';

class ParenFlavour is flavour('long') { }
ok @log.grep('ParenFlavour:flavour:long'),
    'sanity: is flavour(\'long\') (paren form) still reaches trait_mod:<is>';

class Ordinary { has $.x }
is Ordinary.REPR, 'P6opaque', 'sanity: an ordinary class is still P6opaque';
