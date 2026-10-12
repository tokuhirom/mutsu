# RIPEMD-160-style compression rounds, written the way Digest::RIPEMD
# (Digest 1.1.0) writes them: a functional `reduce` over `^80` whose step
# calls a frame-lexical `my sub rotl` with a subscript argument (`$A[2]`),
# picks its round function from a `BEGIN` array of WhateverCodes and blocks,
# and slips three accumulator elements into it.
#
# `use Test` is deliberate: it registers a few hundred routines, and #11686
# found a per-call cost that scaled with the registry -- the subscript
# argument's "does the callee bind my container?" question missed the
# frame-lexical `rotl` and walked every registered routine's key instead.
# A program that loads no modules would not show that cost.
use Test;

proto rounds(|) {*}
multi rounds(@words, $blocks) {
    sub rotl(uint32 $n, $b) { $n +< $b +| $n +> (32 - $b) }
    my @r = (^80).map: { ($_ * 7 + 3) % 16 };
    my @s = (^80).map: { $_ % 10 + 5 };
    my @K = flat (0x00000000, 0x5a827999, 0x6ed9eba1, 0x8f1bbcdc, 0xa953fd4e) »xx» 16;
    my $h = (0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476, 0xc3d2e1f0);
    for ^$blocks {
        $h = reduce -> $A, $j {
            $A[4],
            rotl(
                ($A[0] + (BEGIN [
                    * +^ * +^ *,
                    { $^x +& $^y +| +^$x +& $^z },
                    (* +| +^*) +^ *,
                    { $^x +& $^z +| $^y +& +^$^z },
                    { $^x +^ ($^y +| +^$^z) }
                ])[$j div 16](|$A[1..3])
                + @words[@r[$j]] + @K[$j]) mod 2**32,
                @s[$j]
            ) + $A[4],
            $A[1],
            rotl($A[2], 10),
            $A[3]
        }, $h, |^80;
    }
    $h
}

my @words = (^16).map: { ($_ * 0x01010101 + 0x61626364) mod 2**32 };
my $t = now;
my $h = rounds(@words, 40);
my $elapsed = now - $t;
say $h.map({ ($_ mod 2**32).fmt('%08x') }).join;
say "bench-section-seconds: $elapsed";
