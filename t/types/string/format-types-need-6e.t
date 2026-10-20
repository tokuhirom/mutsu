use Test;

# `Format` and `Formatter` exist only at language revision 6.e: below it the
# names are undeclared.
plan 3;

dies-ok { EVAL 'Format.new("%d")(3)' }, 'Format is undeclared under 6.d';
dies-ok { EVAL 'Formatter.new("%d")' }, 'Formatter is undeclared under 6.d';
is EVAL('use v6.e.PREVIEW; Format.new("%d")(3)'), '3', 'declared under 6.e.PREVIEW';
