# Adding two custom `Real`-subclass instances (via `.Bridge`) produces an exact `Rat` where raku produces an approximate `Num`

Discovered via the doc-diff harness on `raku-doc/doc/Type/Real.rakudoc` (around line 31) — the
"Temperature" tutorial example that defines a `class Temperature is Real` with a `method
Bridge`.

## Minimal repro

```raku
class Temperature is Real {
    has Str:D  $.unit  is required where any <K F C>;
    has Real:D $.value is required;
    method new ($value, :$unit = 'K') { self.bless :$value :$unit }

    method Bridge {
        when $!unit eq 'F' { ($!value + 459.67) * 5/9 }
        when $!unit eq 'C' {  $!value + 273.15 }
        $!value
    }
    method gist { self.Str }
    method Str  { "$!value degrees $!unit" }
}

sub postfix:<C> { Temperature.new: $^value, :unit<C> }
sub postfix:<F> { Temperature.new: $^value, :unit<F> }
sub postfix:<K> { Temperature.new: $^value, :unit<K> }

my $human := 36.6C;
my $book  := 451F;
my $sun   := 5778K;
my $sum = $human + $book + $sun;
say $sum;       # raku: 6593.677777777778   mutsu: 6593.677778
say $sum.WHAT;  # raku: (Num)               mutsu: (Rat)
```

- `raku`: `$sum` is a `Num` (`6593.677777777778e0`), printed at full float precision.
- `mutsu` (`target/debug/mutsu`): `$sum` is an exact `Rat` (`593431/90`), which then goes
  through the (correct, separately-fixed) Rat-to-Str digit-budget rounding, printing the
  truncated `6593.677778`.

## Root cause hypothesis

mutsu's generic `Real` `+` operator (used when neither operand has its own `infix:<+>`
overload — dispatch via each operand's `.Bridge`) apparently just adds the two `.Bridge` return
values directly (`Rat + Rat + Int` here, since every `.Bridge` call in this example returns
exact-Rat arithmetic), yielding an exact `Rat`. Rakudo's actual generic `Real`-role arithmetic
does not preserve exactness this way for *heterogeneous, arbitrary user-defined* `Real`
subclasses — it appears to coerce through `.Num` (approximate double) when combining values from
different concrete `Real` subtypes, since a user-defined `Real.Bridge` gives no general guarantee
of exactness/compatible denominators across subclasses. This needs checking against Rakudo's
actual `Real` role source (or further `raku -e` probing of mixed-type `Real` arithmetic) to
confirm the precise rule before implementing — it is not simply "always coerce Bridge results to
Num" since same-type or plain-Rat/Int arithmetic elsewhere in the codebase correctly stays exact.

## Affected files (starting point)

- `src/runtime/builtins_operators_fallback.rs` or wherever the generic cross-type `Real`
  arithmetic fallback (via `.Bridge`) is implemented — need to find the exact `+` dispatch path
  taken when both operands are instances of user-defined classes that only implement `Real`
  through `.Bridge` (not native `Int`/`Rat`/`Num` values).
