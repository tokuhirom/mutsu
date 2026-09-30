use Test;

# A multi-dim subscript with a *dynamic* :delete adverb (`:$delete` is
# `:delete($delete)`, a runtime boolean rather than a literal) must go through
# the normal subscript path. Previously it routed through a builtin that
# resolved the container by name via `self.env.get`, which missed an outer hash
# assigned inside a sub (returning Nil instead of the value).
#
# This file used to spell the flag `:$no` / `:$f` / `:$g`, which are named
# adverbs `:no`, `:f`, `:g` — not `:delete`. The statement parser stopped in
# front of them and ran each as a separate statement, so the reads passed by
# accident (#10257). The expectations below are rakudo's.

plan 7;

# Hash assigned indirectly via a sub (the failure mode).
my %hash;
sub set-up(--> Nil) { %hash = a => { b => { c => 42 } } }
set-up;

my $delete = False;
ok %hash{"a";"b";"c"}:exists, ':exists reads a sub-assigned hash';
is %hash{"a";"b";"c"}, 42, 'the plain multi-dim read sees the sub-assigned hash';
dies-ok { %hash{"a";"b";"c"}:$delete }, 'a hash multi-dim :$delete has no candidate (as in rakudo)';

# In a for loop with bound keys (the roast multislice shape).
for "a", "b", "c", 42 -> $a, $b, $c, $result {
    ok %hash{$a;$b;$c}:exists, ':exists with for-bound keys';
    last;
}

# Array multi-dim dynamic adverb reads.
my @a = [[1, 2], [3, 4]];
is @a[1;0]:$delete, 3, 'dynamic :delete(False) on an array multi-dim subscript reads';
ok @a[1;0]:exists, 'the element is still there';
is-deeply @a, [[1, 2], [3, 4]], 'the array is unchanged';
