use Test;

# §D(b) substrate: a user coercion method (Numeric/Str/Bool) run by an internal
# op-level redispatch (`+$obj`, `~$obj`, `?$obj`/`if $obj`, comparison coercion)
# can mutate a captured-outer caller lexical. Unlike an explicit `$obj.Numeric`
# call, that internal redispatch has no surrounding CallMethod op to drain the
# captured-outer writeback into the caller's local slot. These regressed to
# losing the write (counter stuck at 0).

plan 10;

# --- Numeric coercion via prefix:<+> ---
{
    my $calls = 0;
    class CNum { has $.x = 5; method Numeric { $calls++; $!x } }
    my $c = CNum.new;
    my $a = +$c;
    my $b = +$c;
    is $a, 5, 'prefix:<+> returns Numeric result';
    is $calls, 2, 'Numeric method captured-outer write propagates (prefix:<+>)';
}

# --- Numeric coercion via a numeric comparison operator ---
{
    my $calls = 0;
    class CCmp { method Numeric { $calls++; 7 } }
    my $c = CCmp.new;
    my $r = ($c == 7);
    ok $r, 'numeric == coercion uses Numeric';
    is $calls, 1, 'Numeric write propagates through comparison coercion';
}

# --- Str coercion via prefix:<~> ---
{
    my $calls = 0;
    class CStr { method Str { $calls++; "hi" } }
    my $c = CStr.new;
    my $s = ~$c;
    my $t = ~$c;
    is $s, "hi", 'prefix:<~> returns Str result';
    is $calls, 2, 'Str method captured-outer write propagates (prefix:<~>)';
}

# --- Bool coercion via `if` / prefix:<?> ---
{
    my $calls = 0;
    class CBool { method Bool { $calls++; True } }
    my $c = CBool.new;
    if $c { }
    my $x = ?$c;
    is $calls, 2, 'Bool method captured-outer write propagates (if / prefix:<?>)';
}

# --- accumulation across many calls (slot must stay coherent) ---
{
    my $n = 0;
    class CAcc { method Numeric { $n++; 1 } }
    my $c = CAcc.new;
    my $sum = 0;
    $sum += +$c for ^5;
    is $n, 5, 'accumulating Numeric write coherent across loop';
    is $sum, 5, 'returned value correct each iteration';
}

# --- explicit call still works (no regression) ---
{
    my $calls = 0;
    class CExp { method bump { $calls++ } }
    my $c = CExp.new;
    $c.bump;
    $c.bump;
    is $calls, 2, 'explicit method call writeback unchanged';
}
