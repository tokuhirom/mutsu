use Test;

# A subscript's range runs over the adverbs written after it, which carry
# their own (`@x[0] :v`, `%h<a> :delete`, `@x[0] :!v`), and an interpolated
# subscript or method call in a string (`"a @x[0] b"`, `"@x.elems()"`) is
# placed with its variable, its subscript and the literal text around it
# (ADR-10723 Stage 2). Ranges measured on rakudo 2026.09.

plan 7;

# Each visited node of `$src` whose class matches `$classes`, with its range.
sub nodes(Str $src, $classes) {
    my @r;
    sub walk($node) {
        my $name = $node.^name.subst('RakuAST::', '');
        if $name ~~ $classes {
            @r.push: $name ~ ' ' ~ ($node.origin andthen "{.from}..{.to}" orelse '-');
        }
        $node.visit-children(&walk);
    }
    $src.AST.visit-children(&walk);
    @r.join(', ')
}

my $decl = 'my @x = 1, 2; my %h = a => 1; ';

is nodes($decl ~ 'say @x[0] :v;', /Postcircumfix|ColonPair|SemiList/),
    'Postcircumfix::ArrayIndex 36..42, SemiList 37..38, ColonPair::True 40..42',
    'an adverb after an array subscript';
is nodes($decl ~ 'say @x[0]:k;', /Postcircumfix|ColonPair/),
    'Postcircumfix::ArrayIndex 36..41, ColonPair::True 39..41',
    'an adverb written against the bracket';
is nodes($decl ~ 'say %h{"a"} :exists;', /Postcircumfix|ColonPair/),
    'Postcircumfix::HashIndex 36..49, ColonPair::True 42..49',
    'an adverb after a hash subscript';
is nodes($decl ~ 'say %h<a> :delete;', /Postcircumfix|ColonPair/),
    'Postcircumfix::LiteralHashIndex 36..47, ColonPair::True 40..47',
    'an adverb after a literal hash subscript';
is nodes($decl ~ 'say @x[0] :!v;', /Postcircumfix|ColonPair/),
    'Postcircumfix::ArrayIndex 36..43, ColonPair::False 40..43',
    'a negated adverb';

is nodes($decl ~ 'say "a @x[0] b";', /StrLiteral|ApplyPostfix|'Var::'|Postcircumfix/),
    'StrLiteral 35..37, ApplyPostfix 37..42, Var::Lexical 37..39, '
    ~ 'Postcircumfix::ArrayIndex 39..42, StrLiteral 42..44',
    'an interpolated subscript and the text around it';
is nodes($decl ~ 'say "a @x.elems() b";', /StrLiteral|ApplyPostfix|'Call::Method'|ArgList/),
    'ArgList 34..50, StrLiteral 35..37, ApplyPostfix 37..47, '
    ~ 'Call::Method 40..47, ArgList 46..46, StrLiteral 47..49',
    'an interpolated method call';
