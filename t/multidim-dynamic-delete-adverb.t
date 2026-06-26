use Test;

# `%h{a;b;c}:$delete` (a multi-dim subscript with a *dynamic* :delete adverb —
# a runtime boolean rather than a literal) must read through the normal
# subscript path when the flag is false. Previously it routed through a builtin
# that resolved the container by name via `self.env.get`, which missed an outer
# hash assigned inside a sub (returning Nil instead of the value).

plan 5;

# Hash assigned indirectly via a sub (the failure mode).
my %hash;
sub set-up(--> Nil) { %hash = a => { b => { c => 42 } } }
set-up;

my $no = False;
my $r1 = %hash{"a";"b";"c"}:$no;
is $r1, 42, 'dynamic :delete(False) reads the value (sub-assigned hash)';

# In a for loop with bound keys (the roast multislice shape).
for "a", "b", "c", 42 -> $a, $b, $c, $result {
    my $rr = %hash{$a;$b;$c}:$no;
    is $rr, 42, 'dynamic :delete(False) reads with for-bound keys';
    last;
}

# Direct (non-sub) assignment.
my %h;
%h{"x";"y"} = 99;
my $f = False;
my $r2 = %h{"x";"y"}:$f;
is $r2, 99, 'dynamic :delete(False) direct hash read';
ok %h{"x";"y"}:exists, 'key still present after :delete(False)';

# Array multi-dim dynamic adverb reads too.
my @a = [[1, 2], [3, 4]];
my $g = False;
my $r3 = @a[1;0]:$g;
is $r3, 3, 'dynamic :delete(False) on an array multi-dim subscript';
