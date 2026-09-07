# A decimal literal whose integer part exceeds i64 silently loses the integer part

`1000000000000000000000000000000.5` evaluates to `0.5` in mutsu. The value is wrong, not
merely imprecise, and nothing warns — the integer part is simply dropped.

## Repro

```
$ ./target/debug/mutsu -e 'say 1000000000000000000000000000000.5; say 12345678901234567890.5'
0.5
0.5

$ raku -e 'say 1000000000000000000000000000000.5; say 12345678901234567890.5'
1000000000000000000000000000000.5
12345678901234567890.5
```

The threshold is exactly `i64::MAX`: `9223372036854775807.5` is correct, `9223372036854775808.5`
is not. Both mutsu and raku type the literal `Rat`, so this is purely about the value.

## Root cause

`src/parser/primary/number.rs`, in the no-exponent decimal branch (around line 472):

```rust
let denom = 10i64.pow(frac_digits);
let int_val: i64 = int_clean.parse().unwrap_or(0);   // <-- silently 0 past i64::MAX
let frac_val: i64 = frac_clean.parse().unwrap_or(0);
let numer = int_val
    .checked_mul(denom)
    .and_then(|v| v.checked_add(frac_val));
match numer {
    Some(numer) => ...,
    None => { /* BigInt path */ }
}
```

The function already has a correct BigInt fallback, but it is only reached through the
`checked_mul`/`checked_add` overflow arm. An integer part above `i64::MAX` fails `parse::<i64>()`
first and `unwrap_or(0)` turns it into `0`; `0 * denom + frac_val` then does *not* overflow, so
the `Some` arm produces `0.5` and the BigInt fallback never runs. The `unwrap_or(0)` swallows the
one signal that should have selected it.

## Fix sketch

Select the path on whether the integer part parses at all, rather than on arithmetic overflow —
e.g. take the BigInt branch whenever `int_clean.parse::<i64>()` is `Err`, or parse into `i128`
(or straight to `BigInt`) and narrow afterwards. The existing BigInt arm already builds the exact
`make_big_rat(int * 10^frac_digits + frac, 10^frac_digits)`, so it is a routing fix, not new
arithmetic. Check the sibling branch near line 539 (leading-`.` literals) and the exponent path
for the same `unwrap_or(0)` shape while you are there.

## Notes

Found while writing `t/bigint-arith-borrowed-operands.t` for the big-integer arithmetic
borrow work (`news/2026-09/bigint-arith-borrowed-operands.md`): that test had to spell its
expected value as `(2 * 10 ** 30 + 1) / 2` because the literal form is wrong. A fix should
also pin the literal form directly.
