use Test;
plan 4;

sub collect(*@args) {
    return @args.elems;
}
is collect(1, 2, 3), 3, 'slurpy @args collects positional args';

sub first-and-rest($first, *@rest) {
    return $first ~ ":" ~ @rest.join(",");
}
is first-and-rest("a", "b", "c"), "a:b,c", 'slurpy with preceding positional param';

sub no-args(*@args) {
    return @args.elems;
}
is no-args(), 0, 'slurpy with no args gives empty array';

sub sum-all(*@nums) {
    my $total = 0;
    for @nums -> $n { $total = $total + $n; }
    return $total;
}
is sum-all(1, 2, 3, 4), 10, 'slurpy args can be iterated';
