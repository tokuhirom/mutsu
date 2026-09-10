use Test;

plan 6;

# Mutating an Array/Hash element *through* a Pair value (or key) writes back to
# the source variable the Pair aliases, because they share the same container.
# (roast S02-types/pair.t "=> should not stringify the key")

# Hash value: $pair.value<f> = 3 reaches the source hash.
{
    my $h = { :d(1), :e(2) };
    my $p = ($h => $h);
    $p.value<f> = 3;
    is $p.value<f>, 3, 'hash element assigned through pair value';
    is $h<f>, 3, 'hash element write-through reaches the source variable';
    is ~$p.value, ~$h, 'pair value and source hash stay equal';
}

# Array value: $pair.value[3] = "x" reaches the source array.
{
    my @src = <a b c>;
    my $a = @src;
    my $p = (k => $a);
    $p.value[3] = "x";
    is $p.value[3], "x", 'array element assigned through pair value';
    is $a[3], "x", 'array element write-through reaches the source variable';
}

# Array key: push $pair.key reaches the source array (already worked, regression
# guard).
{
    my $a = [< a b c >];
    my $p = ($a => 1);
    push $p.key, "d";
    is ~$p.key, ~$a, 'pushing the pair key reaches the source array';
}
