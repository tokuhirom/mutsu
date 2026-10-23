use Test;
use experimental :rakuast;

# `$name = ATOM` and `@name=ATOM` alias a variable in a regex: rakudo keeps only
# the interpolation, whatever whitespace surrounds the `=` (measured on rakudo
# 2026.09).

plan 7;

my $foo = 'bc';
my regex two { .. }

for 'a $foo=[<two>] d', 'a $foo = [<two>] d', 'a $foo= [<two>] d', 'a $foo =[<two>] d' -> $src {
    my $ast = ("rx/$src/").AST;
    like $ast.raku.subst(/\s+/, ' ', :g), /'Interpolation.new( sequential => False, var => RakuAST::Var::Lexical.new("\$foo") )'/,
        "the atom of '$src' is not in the tree";
}

my @x = <a b>;
like ("rx/a @x=. b/").AST.raku.subst(/\s+/, ' ', :g), /'Var::Lexical.new("\@x")'/,
    'an array alias keeps the array interpolation';

# The round trip still matches with the aliased atom.
ok ("abcd" ~~ m/a $foo = [<two>] d/).so, 'the spaced alias matches';
nok ("abxd" ~~ m/a $foo = 'x' d/).so, 'and drops a literal atom';
