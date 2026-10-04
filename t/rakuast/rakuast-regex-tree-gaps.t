use Test;

# Regex forms the source-tree parser now models, as rakudo 2026.09 renders
# them: a quantifier after whitespace, a quantified separator, `<?>` / `<!>`,
# a leading `|` / `||`, and an escaped space in a character class. EVAL of
# the tree matches as the parsed program does.

plan 14;

sub body($src) { $src.AST.statements.head.expression.body }

isa-ok body(Q[/<?>/]), RakuAST::Regex::Assertion::Pass, '`<?>` is Assertion::Pass';
isa-ok body(Q[/<!>/]), RakuAST::Regex::Assertion::Fail, '`<!>` is Assertion::Fail';
my $q = body(Q[/<w> +%% ";"/]);
isa-ok $q.atom, RakuAST::Regex::WithWhitespace, 'a spaced quantifier wraps its atom';
ok $q.trailing-separator, 'and keeps the `%%`';
isa-ok body(Q[/<w>+%\s+/]).separator, RakuAST::Regex::QuantifiedAtom,
    'a separator may be quantified';
isa-ok body(Q[/| a | b/]), RakuAST::Regex::Alternation, 'a leading `|` opens no branch';
is body(Q[/|| a || b/]).branches.elems, 2, 'nor does a leading `||`';

sub run($src) { EVAL($src.AST) }
is run(Q[grammar G1 { token TOP { <w>+ % \s+ }; token w { \w+ } }; G1.parse("ab cd  ef")<w>.elems]),
    3, 'a quantified separator matches every gap';
is run(Q[grammar G2 { token TOP { <x> +%% ";" }; token x { \d } }; G2.parse("1;2;3;")<x>.elems]),
    3, 'a spaced quantifier with a trailing separator';
is run(Q[grammar G3 { token TOP { <a> | <b> }; token a { <!> x }; token b { <?> y } }; ~G3.parse("y")]),
    'y', '`<!>` fails and `<?>` passes';
is run(Q[grammar G4 {
        token TOP {
            | <num>
            | <word>
        }
        token num { \d+ }
        token word { \w+ }
    }; G4.parse("hi")<word>.Str]), 'hi', 'a grammar\'s leading-`|` layout';
is run(Q[~("ab" ~~ / || a || ab /)]), 'a', 'a leading `||` keeps the sequential order';
is run(Q[~("x #" ~~ /<-[#\ ]>+/)]), 'x', 'an escaped space is a class member';
is run(Q[~("a b" ~~ /<[\ a]>+/)]), 'a ', 'in a positive class too';
