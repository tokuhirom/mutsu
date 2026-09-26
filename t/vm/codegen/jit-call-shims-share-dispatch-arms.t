use Test;

# #9452: the Tier A JIT shims for `CallFunc`/`CallFuncNamed`, `CallMethod`,
# `CallMethodMut`, `Return` and the conditional jumps used to be hand-kept
# copies of their `exec_one_dispatch` arms, and the copies had drifted: none of
# the call shims ran the `use fatal` argument check, so a Failure argument
# exploded while the calling chunk was interpreted and slipped through once it
# went native. Both now call the same per-site function
# (src/vm/vm_call_site_ops.rs), so every program below must print exactly the
# same thing with the JIT off and with it on at a threshold of 1.

plan 4;

sub run-both(Str $code) {
    my @out;
    for <off on> -> $jit {
        my %env = %*ENV;
        %env<MUTSU_JIT> = $jit;
        %env<MUTSU_JIT_THRESHOLD> = '1';
        my $proc = run($*EXECUTABLE, '-e', $code, :%env, :out, :err);
        @out.push: $proc.out.slurp(:close) ~ $proc.err.slurp(:close);
    }
    @out
}

# `f`/`h` are declared outside the `use fatal` block, so their bodies carry no
# `ThrowIfFailure` and are JIT candidates; the Failure argument reaches the
# call opcode's own argument check. (That mutsu applies `use fatal` inside a
# callee declared outside the fatal scope at all is a separate divergence from
# Rakudo, where the pragma is lexical -- #9521; this file pins JIT parity only.)
my $fatal-args = q:to/CODE/;
    sub g($x) { "g-ran" }
    class C { method m($x) { "m-ran" }; method mm($x) { "mm-ran" } }
    sub f($s) { g($s.Int) }
    sub h($s) { C.m($s.Int) }
    sub k($s) { my $c = C; $c.=mm($s.Int); $c }
    {
        use fatal;
        my @r;
        for ^6 -> $i {
            my $s = $i %% 2 ?? "12" !! "abc";
            @r.push: (try { f($s) }) // "died";
            @r.push: (try { h($s) }) // "died";
            @r.push: (try { k($s) }) // "died";
        }
        say @r.join(",");
    }
    CODE
my ($off, $on) = run-both($fatal-args);
is $on, $off, 'use fatal Failure arguments explode identically with the JIT on and off';
like $off, /died/, 'and the interpreted run does explode them';

# A call-heavy program touching every shared site: named-argument calls,
# mutating method calls, `return`, `&&`/`||`/`//` jumps and `state` guards.
my $calls = q:to/CODE/;
    class Acc { has $.n = 0; method add($x) { $!n += $x; self } }
    sub named(:$a = 1, :$b = 2) { $a * 10 + $b }
    sub pick($x) { return $x > 3 ?? "big" !! "small" }
    sub tally() { state $t = 0; ++$t }
    my @out;
    for ^20 -> $i {
        my $acc = Acc.new;
        $acc.=add($i);
        @out.push: named(:a($i), :b(1));
        @out.push: pick($i);
        @out.push: ($i %% 3 && "fizz") || ($i // 0);
        @out.push: $acc.n;
        @out.push: tally();
    }
    say @out.join(" ");
    CODE
($off, $on) = run-both($calls);
is $on, $off, 'call-heavy output is byte-identical with the JIT on and off';
like $off, /^ "1 small fizz 0 1 11 small 1 1 2 21 small 2 2 3"/, 'and it matches Rakudo';
