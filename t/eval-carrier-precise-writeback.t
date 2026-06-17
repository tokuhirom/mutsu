use Test;

# Slice F prereq (docs/vm-single-store.md): the EVAL carrier reconciles the
# caller lexicals it wrote *precisely* on return, so the blanket per-EVAL
# `env_dirty` barrier pull can be dropped. These pins assert the reconcile stays
# correct across the frame arrangements that make the drop subtle: same-frame
# slots, ancestor lexicals read via a bare GetLocal, diverged containers, and an
# EVAL whose body writes a caller lexical through a non-logged path (regex :let).

plan 14;

# 1. same-frame scalar
{
    my $x = 0;
    EVAL '$x = 5';
    is $x, 5, 'same-frame scalar reconciled after EVAL';
    is $x + 1, 6, 'bare GetLocal sees the reconciled value';
}

# 2. ancestor lexical written from a descendant frame, read via bare GetLocal
{
    my $x = 0;
    sub set_x() { EVAL '$x = 5' }
    set_x();
    is $x, 5, 'ancestor lexical reconciled after descendant EVAL';
    is $x + 1, 6, 'ancestor bare GetLocal sees the EVAL write';
}

# 3. two-deep nesting
{
    my $deep = 7;
    sub inner_eval() { EVAL '$deep = 42' }
    sub outer_call() { inner_eval() }
    outer_call();
    is $deep * 2, 84, 'two-deep nested EVAL write reaches the bare read';
}

# 4. loop-body lexical (fresh slot each iteration)
{
    my @log;
    for ^3 -> $i {
        my $z = $i;
        EVAL '$z = $z * 100';
        @log.push($z);
    }
    is-deeply @log.List, (0, 100, 200), 'per-iteration EVAL writes reconciled';
}

# 5. container whole-reassignment via EVAL (must NOT be silently lost)
{
    my @a = 1, 2, 3;
    EVAL '@a = (10, 20)';
    is-deeply @a.List, (10, 20), 'EVAL whole-array reassignment reconciled';
    my $h = %(:x(1));
    my %hh = $h;
    EVAL '%hh = (y => 2)';
    is %hh<y>, 2, 'EVAL whole-hash reassignment reconciled';
    nok %hh<x>.defined, 'old hash key gone after reassignment';
}

# 6. in-place container mutation through EVAL
{
    my @a = 1, 2, 3;
    EVAL '@a.push(4)';
    is-deeply @a.List, (1, 2, 3, 4), 'EVAL in-place push reconciled';
}

# 7. EVAL body writes a caller lexical through a bypass path (regex :let)
{
    my $a = 1;
    my regex la { :let $a = 5; <&lma> };
    my regex lma { $a $a };
    my $m = EVAL 'so "55" ~~ m/ ^ <la> $ /';
    ok $m, 'EVAL-run regex matched';
    is $a + 0, 5, ':let write inside EVAL still visible to bare read';
}

# 8. interleaved EVAL + unrelated hot local (the benchmark shape)
{
    my $x = 0;
    my $acc = 0;
    my $hot = 5;
    for ^10 {
        EVAL '$x = $x + 1';
        $acc = $acc + $hot;
    }
    is $x, 10, 'interleaved EVAL writes accumulate correctly';
    is $acc, 50, 'interleaved hot local untouched by the carrier drop';
}
