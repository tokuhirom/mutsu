use Test;

# `.substr-rw(...) = v` on an element reached through a chained subscript
# writes the element back (found via PDF::COS::Type::ObjStm's test, #12907).
plan 5;

my %h = a => [1, 2, { b => { c => "abcdefgh" } }];
%h<a>[2]<b><c>.substr-rw(3, 1) = 'x';
is %h<a>[2]<b><c>, 'abcxefgh', 'three-level hash/array chain';

my %n = a => { c => "abcdefgh" };
%n<a><c>.substr-rw(3, 1) = 'x';
is %n<a><c>, 'abcxefgh', 'two-level hash chain';

my @a = [["abcdefgh"],];
@a[0][0].substr-rw(1, 2) = 'YY';
is @a[0][0], 'aYYdefgh', 'two-level array chain';

my %g = c => "abcdefgh";
%g<c>.substr-rw(3, 1) = 'x';
is %g<c>, 'abcxefgh', 'single subscript still works';

my %k = a => { c => "abcdefgh" };
my $r = (%k<a><c>.substr-rw(0, 1) = 'Q');
is %k<a><c>, 'Qbcdefgh', 'chained target in expression position';
