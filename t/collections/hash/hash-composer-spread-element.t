use Test;

# A `{ ... }` hash composer with a non-pair element (a `%hash`, a `|slip`, a
# bare expression) hashes the list of its elements; a user-visible `&hash`
# does not take it over (#18079).

plan 8;

sub hash(|c) { 'user-hash' }

my %x = a => 1;
my $h = { b => 2, %x };
isa-ok $h, Hash, 'a composer with a hash element is a Hash';
is $h.sort.join(' '), 'a	1 b	2', '... holding both the pair and the spread hash';

my @l = c => 3;
my $s = { d => 4, |@l };
is $s.sort.join(' '), 'c	3 d	4', 'a slip element';

my $q = 5;
is { e => 1, $q > 3 ?? (:f) !! () }.sort.join(' '), 'e	1 f	True',
    'a bare conditional element';
is { e => 1, $q < 3 ?? (:f) !! () }.keys.join(','), 'e', '... that is empty';

is { %x, z => 26 }<z>, 26, 'a spread first, a pair after it';
is { b => 2, %x }.raku, '{:a(1), :b(2)}', 'renders as a hash';

use experimental :rakuast;
is Q|my %y; my $h = { b => 2, %y }|.AST.DEPARSE, "my %y;\nmy \$h = \{b => 2, %y}\n",
    'DEPARSE keeps the composer spelling';
