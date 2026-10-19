use Test;

# A variable inside a lookaround (`<?before $x>`, `<!before $*x>`,
# `<?after $x>`) is interpolated when the lookaround body is parsed, a step
# below the enclosing pattern's own interpolation. The enclosing pattern's
# text does not change with the variable, so its parse must not be reused
# for a later value (#13928).

plan 9;

{
    my @got;
    for <a b> -> $c { @got.push: ~('ab' ~~ / <?before $c> . /) }
    is-deeply @got, ['a', 'b'], 'positive lookahead sees each new value of a loop variable';
}

{
    my @got;
    for <a b> -> $c { @got.push: ~('ab' ~~ / <!before $c> . /) }
    is-deeply @got, ['b', 'a'], 'negative lookahead sees each new value of a loop variable';
}

{
    my $d = 'y';
    my sub f { ~(('xy' ~~ / <?after $d> . /) // '') }
    my $first = f();
    $d = 'x';
    is $first, '', 'lookbehind with the first value finds no match';
    is f(), 'y', 'lookbehind sees the reassigned variable';
}

grammar CallerDynvar {
    token TOP   { <p> }
    token p     { :my $*c = ')'; <t> }
    token t     { <!before $*c> . }
}
nok CallerDynvar.parse(')'), 'a lookahead reads the dynamic variable its calling rule declares';
ok CallerDynvar.parse('x'), '... and still matches what it does not exclude';

{
    my $*c = 'q';
    nok CallerDynvar.parse(')'), '... rather than one the caller of the parse declares';
}

grammar NotFoundFirst {
    token TOP   { <t> <p> }
    token p     { :my $*closer = ')'; <t> }
    token t     { <!before <?{ $*closer }> $*closer> . }
}
nok NotFoundFirst.parse('a)'), 'a lookahead parsed while the variable was unset is not reused';

grammar Rainbowish {
    token TOP { <top> }
    token top {
        :my $*seen = False;
        ( || <paren> || <lt> { $*seen = False } || <word> || <text> )*
    }
    token paren { '(' :my $*closer; { $*closer = ')' } { $*seen = False } <top> ')' { $*seen = True } }
    token lt    { <?{ $*seen }> '<' }
    token word  { <!{ $*seen }> '<' }
    token text  { <!before <?{ $*closer }> $*closer> . }
}
ok Rainbowish.parse('f() <')<top>[0][3]<lt>, 'an operator after a closed paren group is told apart from a word';

# vim: expandtab shiftwidth=4
