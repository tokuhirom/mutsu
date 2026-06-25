use Test;

# §D(b) substrate (Slice 1b): a user `.Str`/`.Stringy` method run by an internal
# render redispatch — string interpolation (`"...$obj..."`), `put`, `print` — can
# mutate a captured-outer caller lexical. These ops have no surrounding CallMethod
# op to drain the captured-outer writeback into the caller's local slot, so the
# write regressed to being lost (counter stuck at 0). Mirrors the coercion-method
# writeback fix (t/coercion-method-captured-writeback.t); `say`/`note` were already
# handled.

plan 8;

# --- string interpolation calls .Str ---
{
    my $calls = 0;
    class CInterp { method Str { $calls++; "s" } }
    my $c = CInterp.new;
    my $a = "v=$c";
    my $b = "v=$c";
    is $a, "v=s", 'interpolation uses .Str';
    is $calls, 2, '.Str captured-outer write propagates (interpolation)';
}

# --- put dispatches .Str ---
{
    my $calls = 0;
    class CPut { method Str { $calls++; "p" } }
    my $c = CPut.new;
    put $c;
    put $c;
    is $calls, 2, '.Str captured-outer write propagates (put)';
}

# --- print dispatches .Str ---
{
    my $calls = 0;
    class CPrint { method Str { $calls++; "x" } }
    my $c = CPrint.new;
    print $c;
    print $c;
    print "\n";
    is $calls, 2, '.Str captured-outer write propagates (print)';
}

# --- accumulation across a loop (slot must stay coherent) ---
{
    my $n = 0;
    class CAcc { method Str { $n++; "z" } }
    my $c = CAcc.new;
    my $s = "";
    $s ~= "$c" for ^4;
    is $n, 4, 'accumulating .Str write coherent across loop';
    is $s, "zzzz", 'interpolated value correct each iteration';
}

# --- interpolation inside a sibling submethod BUILD must NOT consume another
#     BUILD's pending captured-outer writeback (retain-on-miss reconcile) ---
{
    my $parent-counter = 0;
    my $child-counter = 0;
    class Parent {
        submethod BUILD (:$a) { $parent-counter++ }
    }
    class Child is Parent {
        # the interpolation here previously drained Parent.BUILD's pending
        # `$parent-counter` writeback (drop-on-miss), losing it
        submethod BUILD (:$a, :$b) { $child-counter++; my $ignore = "x=$a" }
    }
    Child.new(:b(5), :a(7));
    is $parent-counter, 1, "sibling BUILD's captured write survives child interpolation";
    is $child-counter, 1, "child BUILD ran once";
}
