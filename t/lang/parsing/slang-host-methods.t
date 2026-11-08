use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang token calls the core `<arglist>` and the `<.kok>` /
# `<.set_braid_from(self)>` helpers; its grammar role overrides `arglist` and
# delegates to the method `HOW.find_method` answers (ADR-16888).

plan 1;

use HostMethodsSlang;

my @got;
sub capture(*@a) { @got.push(@a.join('|')); 'r' }

shout (1, 2, 3);
shout (4, 5);
is @got.join(','), '1|2|3,4|5', 'the arglist form parses and runs';
