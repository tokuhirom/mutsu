use Test;

# A look-behind is matched as rakudo matches it: the pattern reversed and run
# forward over the reversed subject, with `^^`/`$$` (and `^`/`$`, `<<`/`>>`)
# swapped for their mirrors (#16031). So under `:ratchet` a quantifier is
# greedy leftwards and never gives back, and `^^`/`$$` keep their
# end-of-string exceptions on the mirrored side. Expectations were measured
# against rakudo 2026.09.

plan 21;

# The ticket's repro (Sway::Config's `variable_name` token).
grammar G {
    token TOP { [<comment> | <config> | \s]* }
    token comment { '#' \N* }
    rule config { <command> <value>? }
    token command { 'set' }
    token value { [ || <variable_name> || <generic_value> ]+ }
    token generic_value { [<!before <variable_name> | \h* <[{}\\]> \h* \n> \N]+ }
    token variable_name { <!after ^^ \s* 'set' \s+ | '$'> '$' <[\w-]>+ }
}
{
    my $m = G.parse("# c\n\nset \$mod Mod4\n");
    is-deeply $m<config>[0]<value><variable_name>.map(*.Str).List, ('$mod',),
        'a variable after a non-initial `set` line is a variable';
    is-deeply $m<config>[0]<value><generic_value>.map(*.Str).List, (' Mod4',),
        'and the rest of the line is a generic value';
}

# Under :ratchet the backward `\s*` eats every space and newline before `set`.
ok  "set\$x"     ~~ rx:r/<?after ^^ \s* 'set'> '$'/, 'at the start of the subject';
ok  " set\$x"    ~~ rx:r/<?after ^^ \s* 'set'> '$'/, 'after a leading space';
nok "\nset\$x"   ~~ rx:r/<?after ^^ \s* 'set'> '$'/, 'not after a leading newline';
nok "\n\nset\$x" ~~ rx:r/<?after ^^ \s* 'set'> '$'/, 'nor two';
ok  " \nset\$x"  ~~ rx:r/<?after ^^ \s* 'set'> '$'/, 'after a space then a newline';
nok "a\nset\$x"  ~~ rx:r/<?after ^^ \s* 'set'> '$'/, 'the ratcheted run does not give the newline back';
ok  "a\nset\$x"  ~~ rx/<?after ^^ \s* 'set'> '$'/,   'without :ratchet it backtracks';
nok "x\n\nc"     ~~ rx:r/<?after ^^ \n*> c/,         'a ratcheted \n* run';
ok  "x\n\nc"     ~~ rx:r/<?after $$ \n*> c/,         '$$ before it';
nok "\n\nc"      ~~ rx:r/<?after \n \n*> c/,         'the run leaves nothing for a following atom';

# The mirrored end-of-string exceptions.
ok  "ab\n" ~~ /ab \n <?after \n $$>/,  '$$ holds after a trailing newline';
nok "\nab" ~~ /^ \n <?after ^^ \n>/,   '^^ fails before a leading newline';
ok  "\nab" ~~ /^ \n <?after ^ \n ^^>/, 'but holds after it';
ok  "ab\n" ~~ /<?after \n ^^>/,        '^^ after a final newline';

# Plain shapes are unaffected.
ok  "xab" ~~ /x ab <?after a b>/,       'a literal sequence';
ok  "xab" ~~ /x ab <?after [a|xa] b>/,  'an alternation';
ok  "aab" ~~ /^ aab <?after a+ b>/,     'a quantifier';
nok "xaab" ~~ /x aab <?after ^ a+ b>/,  'a string anchor';
ok  "aaab" ~~ /aaab <?after ^ a+: b>/,  'a possessive quantifier reaching the start';
