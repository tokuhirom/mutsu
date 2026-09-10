use Test;

# `$@(...)` and `$%(...)` are the itemizing forms of the list/hash
# contextualizers: the inner `@(...)` / `%(...)` builds a List / Hash, and the
# leading `$` itemizes it (wraps it in a Scalar container). Previously mutsu
# mis-parsed `$%(...)` as a modulo on an anonymous state variable.

plan 12;

is $%(x => 1).raku, '${:x(1)}', '$%(...) single pair itemized hash';
is $%(a => 1, b => 2).raku, '${:a(1), :b(2)}', '$%(...) multi pair itemized hash';
is $%().raku, '${}', '$%() empty itemized hash';
is $%(x => 1).WHAT.gist, '(Hash)', '$%(...) is a Hash';

is $@(1, 2, 3).raku, '$(1, 2, 3)', '$@(...) itemized list';
is $@().raku, '$( )', '$@() empty itemized list';
is $@(1, 2, 3).WHAT.gist, '(List)', '$@(...) is a List';

# Assigning to a scalar keeps the itemized value.
my $h = $%(k => 42);
is $h.raku, '${:k(42)}', '$%(...) assigned to scalar';
is $h<k>, 42, 'itemized hash is indexable';

my $l = $@(10, 20);
is $l.raku, '$(10, 20)', '$@(...) assigned to scalar';
is $l[1], 20, 'itemized list is indexable';

# Plain contextualizers still work (guard against regression).
is %(x => 1).raku, '{:x(1)}', '%(...) plain hash contextualizer still works';
