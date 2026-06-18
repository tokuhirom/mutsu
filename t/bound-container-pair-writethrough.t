use Test;

plan 6;

# A `:=`-bound array/hash shares one `ContainerRef` cell between both variables
# (env-locals coherence Stage 1). Mutating that container *through a Pair value*
# (which aliases the same array/hash) must write back THROUGH the shared cell so
# every alias observes the change. Previously the write-through-by-identity scan
# only matched bare `Value::Array`/`Value::Hash` bindings and silently skipped
# variables holding a `ContainerRef` cell.

# Bound array, element assign through a pair value.
{
    my @b = <a b c>;
    my @a := @b;
    my $p = (k => @a);
    $p.value[3] = "x";
    is $p.value[3], "x", 'array element assigned through the pair value';
    is @a[3], "x", 'write-through reaches the bound alias @a';
    is @b[3], "x", 'write-through reaches the source @b';
}

# Bound hash, value assign through a pair value.
{
    my %g = :d(1), :e(2);
    my %h := %g;
    my $p = (k => %h);
    $p.value<f> = 3;
    is $p.value<f>, 3, 'hash element assigned through the pair value';
    is %h<f>, 3, 'write-through reaches the bound alias %h';
    is %g<f>, 3, 'write-through reaches the source %g';
}
