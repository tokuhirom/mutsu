use Test;

# The `ArgList` of a call runs from its first argument to its last (a
# trailing `,` included), not over the whole inside of its parentheses; an
# argless call still has one, visited by `visit-children` and placed just
# inside the parentheses or right after the name; an empty pointy block has
# an empty `Signature` right after its arrow (ADR-10723 Stage 2). Ranges
# measured on rakudo 2026.09.

plan 14;

# Each visited node of `$src` whose class is in `@classes`, with its range.
sub nodes(Str $src, *@classes) {
    my @r;
    sub walk($node) {
        my $name = $node.^name.subst('RakuAST::', '');
        if $name eq any(@classes) {
            @r.push: $name ~ ' ' ~ ($node.origin andthen "{.from}..{.to}" orelse '-');
        }
        $node.visit-children(&walk);
    }
    $src.AST.visit-children(&walk);
    @r.join(', ')
}

my $decl = 'sub foo(*@a) { }; ';

is nodes($decl ~ 'foo( 1 );', 'ArgList'), 'ArgList 23..24',
    'the spaces inside the parentheses are not part of the list';
is nodes($decl ~ 'foo(1, 2 );', 'ArgList'), 'ArgList 22..26',
    'from the first argument to the last';
is nodes($decl ~ 'foo(1, );', 'ArgList'), 'ArgList 22..24',
    'a trailing comma is part of the list';
is nodes($decl ~ "foo(1 # c\n);", 'ArgList'), 'ArgList 22..23',
    'a comment after the last argument is not';
is nodes($decl ~ 'foo(:a, b => 2);', 'ArgList'), 'ArgList 22..32',
    'named arguments';
is nodes($decl ~ 'foo();', 'ArgList'), 'ArgList 22..22',
    'an argless call has an empty list just inside its parentheses';
is nodes($decl ~ 'foo(  );', 'ArgList'), 'ArgList 22..22',
    'even when blanks follow the paren';
is nodes($decl ~ 'foo;', 'ArgList'), 'ArgList 21..21',
    'a bare call has one right after its name';
is nodes('1.abs;', 'Call::Method', 'ArgList'), 'Call::Method 2..5, ArgList 5..5',
    'a method call without arguments';
is nodes('1.abs( );', 'Call::Method', 'ArgList'), 'Call::Method 2..8, ArgList 6..6',
    'a method call with empty parentheses';
is nodes('say (1,2).join( "," );', 'Call::Method', 'ArgList'),
    'ArgList 4..21, Call::Method 10..21, ArgList 16..19',
    'a method call with an argument';
is nodes($decl ~ '1.&foo;', 'Call::NameAsMethod', 'ArgList'),
    'Call::NameAsMethod 21..24, ArgList 24..24',
    'a routine called as a method';
is nodes('class A { method m { self!p }; method !p { } }', 'Call::PrivateMethod', 'ArgList'),
    'Call::PrivateMethod 26..27, ArgList 27..27',
    'a private method call';
is nodes('my $p = -> { 1 }; for 1 -> { }', 'Signature'), 'Signature 10..10, Signature 26..26',
    'an empty pointy block has an empty signature after its arrow';
