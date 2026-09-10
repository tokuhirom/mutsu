use Test;
plan 11;

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

is collect($[1, 2, 3]), 1, 'slurpy keeps itemized array as one positional argument';

sub first-elems(*@args) {
    return @args[0].elems;
}
is first-elems($[1, 2, 3]), 3, 'itemized array stays Array inside slurpy parameter';

# Slurpy scalar parameters capture the leading elements of the variadic args
# (S06 "List parameters").
sub s-first(*$f, *$s, *@r) { return $f }
sub s-second(*$f, *$s, *@r) { return $s }
sub s-rest(*$f, *$s, *@r) { return [+] @r }
is s-first(1, 2, 3, 4, 5), 1, 'first slurpy scalar captures first positional';
is s-second(1, 2, 3, 4, 5), 2, 'second slurpy scalar captures second positional';
is s-rest(1, 2, 3, 4, 5), 12, 'remaining slurpy array captures the rest';

sub s-undef(*$f) { return $f.defined }
is s-undef(), False, 'slurpy scalar with no positional is undefined';

sub s-named(*$f, *%h) { return "$f:%h<n>" }
is s-named(7, n => 9), "7:9", 'slurpy scalar skips named args, *%h captures them';
