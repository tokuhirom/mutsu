use Test;

plan 12;

# Two correctness bugs from the Array complexity audit (issue #9160).
#
# 1. `zip` capped its rows at 1000 even when every input was finite. The cap
#    only exists to bound an infinite column, so an all-finite zip keeps
#    every row, with or without `:with`.
# 2. `say @a` / `note @a` (the function forms) rendered every element, while
#    `@a.gist` stops after 100 and appends `...`. They now print the gist.

is zip(^5000, ^5000).elems, 5000, 'zip of two finite lists keeps every row';
is zip((^5000, ^5000)).elems, 5000, 'single-argument zip too';
is zip(^5000, ^5000, :with(&[+])).elems, 5000, 'zip :with keeps every row';
is zip(^5000, ^5000, :with(&[+]))[4999], 9998, '... and combines the last one';
is (^5000 Z+ ^5000).elems, 5000, 'the Z+ metaop agrees';
is zip(1..*, 1..*)[^3].gist, '((1 1) (2 2) (3 3))', 'an infinite zip still works';
ok zip(1..*, 1..*).is-lazy, '... and stays lazy';

class FakeIO {
    has $.Str is rw = '';
    method print($arg) { $!Str ~= $arg }
}

sub cap-out(&code) { my $*OUT = FakeIO.new; code(); $*OUT.Str }
sub cap-err(&code) { my $*ERR = FakeIO.new; code(); $*ERR.Str }

my @a = ^200;
is cap-out({ say @a }), @a.gist ~ "\n", 'say @a prints the capped gist';
is cap-err({ note @a }), @a.gist ~ "\n", 'note @a prints the capped gist';
is cap-out({ say (^200).Seq }), (^200).Seq.gist ~ "\n", 'say of a Seq is capped too';
is cap-out({ say [[^200],] }).words.elems, 101, 'a nested array is capped as well';
is cap-out({ say [^100] }).words.tail, '99]', 'exactly 100 elements are not capped';
