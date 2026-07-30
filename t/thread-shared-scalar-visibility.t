# Pin: cross-thread visibility of scalar mutations performed inside a thread.
#
# These are the shapes `box_captured_lexicals` (src/vm/vm_register_ops.rs)
# deliberately declines to box into a shared cell -- type-constrained scalars,
# scalars holding an Instance/Proxy/Package, and frames with no cell locals at
# all. They therefore relied on the plain-scalar lane of `shared_vars`.
# Retiring that lane (PLAN.md §6) must not regress them.

use Test;

plan 9;

# --- untyped scalar: the baseline shape (boxed into a cell) ---------------
{
    my $c = 0;
    await start { $c = 5 };
    is $c, 5, 'untyped scalar mutated inside start is visible to the parent';
}

# --- type-constrained scalars: skipped at vm_register_ops.rs:650-656 ------
{
    my Int $c = 0;
    await start { $c = 5 };
    is $c, 5, 'Int-constrained scalar mutated inside start is visible';
}

{
    my Str $s = 'before';
    await start { $s = 'after' };
    is $s, 'after', 'Str-constrained scalar mutated inside start is visible';
}

{
    subset Even of Int where * %% 2;
    my Even $e = 2;
    await start { $e = 8 };
    is $e, 8, 'subset-constrained scalar mutated inside start is visible';
}

{
    my Int $c = 0;
    await Promise.allof((^4).map({ start { $c = $c + 1 } }));
    ok $c > 0, 'concurrent writes to a typed scalar still land';
}

# --- scalars holding an Instance: skipped at vm_register_ops.rs:669-679 ---
{
    class Holder { has $.v is rw }
    my $obj = Holder.new(v => 1);
    await start { $obj.v = 42 };
    is $obj.v, 42, 'attribute mutation through a captured Instance is visible';
}

{
    class Holder2 { has $.v is rw }
    my $obj = Holder2.new(v => 1);
    await start { $obj = Holder2.new(v => 7) };
    is $obj.v, 7, 'rebinding a scalar holding an Instance is visible';
}

# --- scalar holding a Proxy ----------------------------------------------
{
    todo 'STORE through a Proxy bound in the parent does not reach the parent (PLAN.md §6)';
    my $backing = 0;
    my $p := Proxy.new(FETCH => method () { $backing }, STORE => method ($v) { $backing = $v });
    await start { $p = 9 };
    is $backing, 9, 'STORE through a captured Proxy is visible';
}

# --- Thread.start (a non-Promise spawn path) ------------------------------
{
    my $t = 0;
    my $thr = Thread.start({ $t = 3 });
    $thr.finish;
    is $t, 3, 'scalar mutated inside Thread.start is visible';
}
