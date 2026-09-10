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
# exactly like a parenthesized one, both for classes and for roles. `ctype`
# has no dedicated AST field (unlike `repr`), so it is only observable
# through the custom_traits -> trait_mod:<is> dispatch mechanism.

my @log;
multi sub trait_mod:<is>(Mu:U $t, :$ctype!) { @log.push("{$t.^name}:ctype:{$ctype}") }

class WithCtype is ctype<long> { }
ok @log.grep('WithCtype:ctype:long'),
    'class: is ctype<long> (angle form, no dedicated field) reaches trait_mod:<is>';

@log = ();
role RoleWithRepr is repr<CStruct> { }
is +@log, 0, 'role: is repr<...> does not spuriously dispatch an unrelated trait';

@log = ();
role RoleWithCtype is ctype<long> { }
ok @log.grep('RoleWithCtype:ctype:long'),
    'role: is ctype<long> (angle form) reaches trait_mod:<is>, same as class';

# --- sanity: the pre-existing parenthesized form keeps working unchanged.

class ParenStruct is repr('CStruct') {
    has uint64 $.a;
}
is ParenStruct.REPR, 'CStruct', 'sanity: is repr(\'CStruct\') (paren form) still works';

@log = ();
class ParenCtype is ctype('long') { }
ok @log.grep('ParenCtype:ctype:long'),
    'sanity: is ctype(\'long\') (paren form) still reaches trait_mod:<is>';

class Ordinary { has $.x }
is Ordinary.REPR, 'P6opaque', 'sanity: an ordinary class is still P6opaque';
