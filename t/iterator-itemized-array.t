use v6;
use Test;

# `.iterator` on an itemized array iterates the ELEMENTS: itemization only
# prevents flattening in list context; the iterator protocol still walks the
# array (Text::CSV's CSV::Diag.iterator returns `$[ ... ].iterator`).

plan 6;

my $i = $[1, 2, 3].iterator;
is $i.pull-one, 1, 'pull-one yields the first element';
is $i.pull-one, 2, 'pull-one advances';
is $i.pull-one, 3, 'pull-one reaches the last element';

# A user class doing Iterable via such an iterator populates an array
# assignment element-wise.
class D does Iterable does Positional {
    has Int $.error = 42;
    method iterator { $[ $!error, "msg", 7 ].iterator }
    method AT-POS (int $i) { $i == 0 ?? $!error !! $i == 1 ?? "msg" !! 7 }
}

my @ed = D.new;
is @ed.elems, 3, 'my @a = $iterable reifies the user iterator';
is @ed[0], 42, 'first element';
is @ed[2], 7, 'third element';
