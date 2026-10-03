use Test;

# `Capture.list` is its positional part, so list methods on a Capture see the
# positional arguments (`c.head` inside `sub (|c)`, Grammar::Extractor).

plan 6;

sub f(|c) { c.head }
is f(5, 6, :x(7)), 5, '.head of a capture parameter is the first positional';

my $c = \(1, 2, :a(3));
is $c.head, 1, '.head';
is $c.tail, 2, '.tail';
is \(1, 2).join(','), '1,2', '.join';
my @seen;
for \(1, 2) { @seen.push: $_ }
is-deeply @seen, [1, 2], 'for over a capture literal';
is (\(1, 2), \(3, 4)).elems, 2, 'a list of captures keeps them as elements';
