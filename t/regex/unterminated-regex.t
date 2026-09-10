# A regex whose closing delimiter never arrives is an `X::Comp::Group` in
# rakudo, every time: `Regex not terminated.` as the sorrow, and a panic naming
# the delimiter it was after. mutsu used to backtrack out of the regex literal
# and let whichever alternative parse failed last supply the exception, so three
# unterminated regexes gave three unrelated classes (X::Undeclared::Symbols,
# X::Syntax::Confused, X::Str::Numeric).
#
# The commit is deliberately limited to delimiters that cannot mean anything
# else after the keyword — `/` and the four bracket pairs. `m-bar` stays an
# ordinary identifier, and a bare `/` in *infix* position is still division.
use Test;

plan 11;

throws-like q['x' ~~ m/foo], X::Comp::Group, 'unterminated m//';
throws-like q['x' ~~ /foo], X::Comp::Group, 'unterminated bare //';
throws-like q[my $r = rx/abc], X::Comp::Group, 'unterminated rx//';
throws-like q[my $r = rx{abc], X::Comp::Group, 'unterminated rx{}';

# The `#` comment eats the rest of the line, closing paren and delimiter alike,
# so this is unterminated too — roast/S05-metasyntax/regex.t asks for it.
throws-like q['x' ~~ m/foo (#) bar /], X::Comp::Group, 'commented capture end';

{
    my $group = ((try EVAL q['x' ~~ m/foo]) // $!);
    is $group.sorrows».message.head, 'Regex not terminated.', 'the sorrow names the problem';
    ok $group.panic.defined, 'the group carries a panic';
}

# Division must still be division.
is 6 / 2, 3, 'infix / on literals';
is do { my $x = 10; $x / 2 }, 5, 'infix / on a variable';
is-deeply (1, 2).map(* / 2).List, (0.5, 1.0), 'whatever-code division';

# A terminated regex is unaffected.
ok 'abc' ~~ m/b/, 'a terminated m// still matches';
