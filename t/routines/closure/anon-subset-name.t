use Test;

# The refined type of `my Int $x where ...` has no name: rakudo calls it <anon>.

plan 3;

my Int $x where * > 0 = 5;
is $x.VAR.of.^name, '<anon>', 'the anonymous subset is named <anon>';
is $x.VAR.of.raku, '<anon>', '... and so is its raku';
try { $x = -1; CATCH { default { is .message, 'Type check failed in assignment to $x; expected <anon> but got Int (-1)', 'the type check message names it' } } }
