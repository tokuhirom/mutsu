use Test;

# Distilled from Terminal::UI: Terminal::ANSI's `set-scroll-region` does
# `$state.scroll-bottom = $bottom` on its file-scope `my $state`, called from
# an emit callback of Terminal::ANSIParser, whose parser closure also has a
# `$state`. The accessor-assign writeback went by name and replaced the
# parser's variable with the other module's object (the routine ran as a TRIR
# chunk, which skipped the compunit-cell redirect of #11275).

plan 2;

use lib 't/lib';
use LvalueLexParser;
use LvalueLexHolder;

my &step := make-stepper(cb => -> $b { bump() });
is step(1), 7, 'closure variable survives a callee accessor-assign on its own $state';
is step(2), 7, 'and on the next call';
