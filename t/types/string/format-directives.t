use v6.e.PREVIEW;
use Test;

plan 3;

is-deeply Format.new('%05d%3x:%s').directives, ('d', 'x', 's'),
    'directives strips flags and widths';
is-deeply Format.new('plain %%').directives, (),
    'literal percent has no directive';
is-deeply Format.new('%*.*f').directives, ('*', '*', 'f'),
    'dynamic width and precision consume directives';
