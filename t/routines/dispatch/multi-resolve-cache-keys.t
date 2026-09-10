use Test;

# The sound multi-resolution caches (`func_multi_resolve_cache` and its method
# twin) memoize a winner per (package, name, argument-type keys). Several
# properties beyond an argument's plain runtime type participate in dispatch,
# so the key has to carry each of them -- or the candidate set has to stay out
# of the cache entirely -- or the SECOND call of an alternating pair is served
# the first one's winner:
#
#   * definedness, for a `:D`/`:U` smiley candidate set -- a type object and an
#     instance of the same type share one `value_type_name`;
#   * the declared type of the source variable a `VarRef` argument came from,
#     which `unwrap_varref_for_dispatch` feeds into the specificity ranking;
#   * an enum member's own identity (`Less` and `More` are both `Order`);
#   * the INVOCANT's definedness, which is not in the argument list at all;
#   * the declared return type of a routine passed to a constrained `&`-sigil
#     parameter -- unkeyable, so that candidate set is refused outright.
#
# Every case below is run repeatedly and with the two arms INTERLEAVED, so a
# key that dropped either property fails on the second iteration rather than
# accidentally passing on a cold cache.

plan 44;

multi sub smiley(Int:U $x) { 'U' }
multi sub smiley(Int:D $x) { 'D' }

for ^3 -> $run {
    is smiley(Int), 'U', "type object picks the :U candidate (run $run)";
    is smiley(42), 'D', "instance picks the :D candidate (run $run)";
}

# The same split reached through variables rather than literals, so the keys go
# through the `VarRef` arm as well.
my Int $undef;
my Int $def = 7;
for ^3 -> $run {
    is smiley($undef), 'U', "undefined variable picks :U (run $run)";
    is smiley($def), 'D', "defined variable picks :D (run $run)";
}

# A native/boxed pair: both arguments are `Int` at runtime, so only the source
# variable's DECLARED type separates the candidates.
multi sub declared(int $x) { 'native' }
multi sub declared(Int $x) { 'boxed' }

my int $native = 5;
my Int $boxed = 5;
for ^3 -> $run {
    is declared($native), 'native', "native-typed variable picks int (run $run)";
    is declared($boxed), 'boxed', "boxed-typed variable picks Int (run $run)";
}

# A literal keeps resolving to whatever it resolved to before the two
# variable-shaped keys above were cached -- i.e. the declared-type keys must
# not leak into the literal's own bucket. (mutsu answers 'boxed' where rakudo
# answers 'native' for a bare literal against an `int`/`Int` pair; that
# divergence is `todo/tickets/native-int-candidate-loses-to-int-for-a-literal.md`
# and is deliberately NOT what this file pins.)
is declared(5), declared(5), 'literal keeps a stable winner across calls';

# Method-side twin of the smiley case.
class SmileyHost {
    multi method m(Str:U $s) { 'mU' }
    multi method m(Str:D $s) { 'mD' }
}
my $host = SmileyHost.new;
for ^3 -> $run {
    is $host.m(Str), 'mU', "method :U candidate (run $run)";
    is $host.m('x'), 'mD', "method :D candidate (run $run)";
}


# An enum VALUE parameter refines within one `value_type_name` (`Less` and
# `More` are both `Order`), so the key has to carry the member identity.
multi sub member(Less) { 'less' }
multi sub member(More) { 'more' }
multi sub member(Any) { 'other' }

for ^3 -> $run {
    is member(Less), 'less', "enum member Less (run $run)";
    is member(More), 'more', "enum member More (run $run)";
    is member(Same), 'other', "enum member Same falls to Any (run $run)";
}

# The invocant's own definedness, which lives outside the argument list: the
# receiver class is identical for both candidates here.
class InvocantHost {
    multi method g(InvocantHost:U:) { 'iU' }
    multi method g(InvocantHost:D:) { 'iD' }
}
my $inst = InvocantHost.new;
for ^2 -> $run {
    is InvocantHost.g, 'iU', "invocant :U candidate (run $run)";
    is $inst.g, 'iD', "invocant :D candidate (run $run)";
}

# A CONSTRAINED `&`-sigil parameter dispatches on the passed routine's declared
# RETURN type. Every routine shares one `value_type_name`, so no argument key
# can separate these two candidates and the name has to stay out of the
# type-keyed cache altogether (roast S06-multi/type-based.t).
sub ret-int() returns Int { 3 }
sub ret-str() returns Str { 'pigs' }
multi sub by-return(Int &x) { 'int:' ~ x() }
multi sub by-return(Str &x) { 'str:' ~ x() }

for ^3 -> $run {
    is by-return(&ret-str), 'str:pigs', "routine returning Str (run $run)";
    is by-return(&ret-int), 'int:3', "routine returning Int (run $run)";
}

done-testing;
