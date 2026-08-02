use Test;

# Rakudo splits the two undeclared families by what the symbol *is*, not by
# when it was noticed:
#
#   $undeclared      -> X::Undeclared           "Variable '$undeclared' is not declared"
#   zzz()            -> X::Undeclared::Symbols  "Undeclared routine:\n    zzz used at line 1"
#
# and the two classes are unrelated -- they share the `X::Comp` *role*, not a
# superclass -- so `X::Undeclared::Symbols ~~ X::Undeclared` is False and
# answering with the wrong one is visible to `throws-like`.
#
# mutsu's runtime read of an undeclared variable used to answer
# X::Undeclared::Symbols with the X::Undeclared message text, and it dropped
# the sigil from the reported symbol.

plan 9;

nok X::Undeclared::Symbols ~~ X::Undeclared,
    'the two undeclared classes are not related';

throws-like '{ my $inner = 42 }; $inner', X::Undeclared,
    'a block lexical read from outside is X::Undeclared';

# The chained form escapes the compile-time scan and is caught by the VM's
# variable read instead -- the same class has to come out of both paths.
throws-like '{ our $sa2 = my $sb2 = 42 }; ($sa2, $sb2)', X::Undeclared,
    'a chained our/my block lexical is X::Undeclared too',
    symbol => '$sa2';

throws-like 'my @a = 1; { my @b = 2 }; @b', X::Undeclared,
    'an undeclared array keeps its sigil',
    symbol => '@b';

throws-like 'zzz()', X::Undeclared::Symbols,
    'a routine nobody declared is X::Undeclared::Symbols';

# A CORE term constant exists as a symbol -- just not under the `&` sigil --
# so calling it is the variable-shaped error, naming `&e`.
throws-like 'e()', X::Undeclared,
    'calling a CORE term constant is X::Undeclared',
    symbol => '&e';

throws-like 'my $e = e; e()', X::Undeclared,
    'and a same-named lexical does not change that';

throws-like 'pi()', X::Undeclared, 'pi() likewise', symbol => '&pi';

throws-like 'True()', X::Undeclared, 'True() likewise', symbol => '&True';
