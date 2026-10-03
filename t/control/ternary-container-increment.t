use Test;

plan 7;

# A ternary yields the container of the branch it selects, so an increment
# through it updates that variable.
my ($p, $q) = 0, 0;
(True ?? $p !! $q)++;
is "$p $q", "1 0", 'postfix ++ through a ternary';
++(False ?? $p !! $q);
is "$p $q", "1 1", 'prefix ++ through a ternary';
(False ?? $p !! $q)--;
is "$p $q", "1 0", 'postfix -- through a ternary';
is (True ?? $p !! $q)++, 1, 'postfix ++ returns the old value';
is ++(True ?? $p !! (False ?? $q !! $p)), 3, 'nested selector';

my @a = 1, 2;
(True ?? @a[0] !! $q)++;
is-deeply @a, [2, 2], 'an element branch';

class C {
    has $!passed = 0;
    has $!failed = 0;
    method record($ok) { ($ok ?? $!passed !! $!failed)++ }
    method counts { "$!passed $!failed" }
}
my $c = C.new;
$c.record($_) for True, False, True;
is $c.counts, "2 1", 'attribute branches (TAP State.handle-entry)';
