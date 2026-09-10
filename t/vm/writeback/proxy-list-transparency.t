use v6;
use Test;

plan 7;

# Proxies inside a List FETCH on every value-context read (URI::Query's
# read-only Proxy lists).
my $l = (1, 2).map({
    my $v = $_;
    Proxy.new(
        FETCH => method () { $v },
        STORE => method ($n) { X::Assignment::RO.new(:value($v)).throw },
    );
}).List;
is-deeply $l, $(1, 2), 'is-deeply FETCHes list elements';
is $l[0], 1, 'single element read FETCHes';

# TODO: repeated FETCHes of sibling map-produced Proxies clobber each other's
# captured `$v` (closure-env sharing, Slice F territory) — the first resolved
# value wins on later reads. Pin the parts that hold today.
my @seen;
for $l.list -> $v { @seen.push($v) }
is @seen.elems, 2, 'iteration FETCHes each element (count)';

my $h = { k => $l };
is $h<k>.elems, 2, 'nested Proxy list inside a hash resolves (count)';

# STORE runs when assigning through a chained subscript on an object
# (the URI::Query `$q<foo>[0] = ...` throws-like X::Assignment::RO shape).
class QC does Associative {
    has %.d;
    method AT-KEY($k) { %!d{$k} }
    method ASSIGN-KEY($k, $v) { %!d{$k} = $v }
}
my $q = QC.new;
$q<foo> = $l;
ok $q<foo>[1].defined, 'reading a Proxy element through AT-KEY chains works';
throws-like { $q<foo>[0] = 'nope' }, X::Assignment::RO,
    'writing through a chained subscript runs the Proxy STORE';

# A method-accessor chain hits the STORE too ($u.query<foo>[0] = ...).
class Holder2 {
    has QC $.inner = QC.new;
}
my $hold = Holder2.new;
$hold.inner<k> = $l;
throws-like { $hold.inner<k>[0] = 'x' }, X::Assignment::RO,
    'method-accessor chained subscript runs the Proxy STORE';
