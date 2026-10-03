# ADR-0135 Slice E: `<~~>` (recurse into the whole enclosing regex) compiles on
# the compiled regex engine instead of declining the pattern to the tree walk.
# Values are rakudo's.
use Test;

plan 6;

nok "((()))" ~~ /^ "(" <~~>? ")" $/, 'the recursion re-applies the anchors too';
is ~("aab" ~~ / a <~~> | b /), 'aab', 'recursion inside an alternation';
is ~("(()" ~~ / "(" <~~>* ")" /), '()', 'a quantified recursion';
is ~("(()())" ~~ / "(" <~~>* ")" /), '(()())', 'nested balanced parentheses';

my $paren = rx/ '(' [ <-[()]>+ | <~~> ]* ')' /;
is ~("x(a(b)c)y" ~~ $paren), '(a(b)c)', 'a stored regex recursing into itself';

{
    my %env = %*ENV;
    %env<MUTSU_VM_STATS> = '1';
    my $proc = run($*EXECUTABLE, '-e', 'say ~("(()())" ~~ / "(" <~~>* ")" /)', :out, :err, :%env);
    $proc.out.slurp(:close);
    my $line = $proc.err.slurp(:close).lines.first(*.contains('regex-vm:')) // '';
    like $line, /'declined=0 '/, 'a pattern with <~~> is not declined';
}
