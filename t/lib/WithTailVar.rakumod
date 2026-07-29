unit module WithTailVar;

# Pin for the module-loaded with-tail-var bug: a statement-position `with`
# block's value must not replace the bare-variable tail that follows it
# (NativeHelpers::Blob's blob-from-pointer has exactly this shape).
sub with-then-tail(\ptr) is export {
    my $b = 42;
    with ptr { 2.so; }
    $b;
}

sub given-then-tail($p) is export {
    my $b = 42;
    given $p { 2.so; }
    $b;
}

# The NativeHelpers::Blob `str-to-blob` shape: LEAVE phaser + tail `with`.
# The phaser body routes through compile_phaser_block_scope, whose tail
# statement used to collapse every non-Expr statement to `True`.
sub leave-then-tail-with(\ptr) is export {
    my $x = 42;
    LEAVE { 1 if ptr }
    with ptr { $x } else { die "no" };
}

sub leave-then-tail-if(\ptr) is export {
    my $x = 42;
    LEAVE { 1 if ptr }
    if ptr.defined { $x } else { die "no" };
}

# The DBDish::Pg::Native `str-to-blob` call shape: the `with` branch's tail is
# a call to a sub imported from another module — the parser resolves a known
# routine name to a *statement* call, which compile_when_tail_stmt must still
# compile in value position.
use WithTailHelper;

sub with-tail-imported-named(\ptr, :$type = Str) is export {
    with ptr {
        tail-helper-named(ptr, :elems(3), :$type)
    } else { die "no" };
}

sub with-tail-imported-pos(\ptr) is export {
    with ptr {
        tail-helper-pos(ptr)
    } else { die "no" };
}
