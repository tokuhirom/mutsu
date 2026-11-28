use Test;

# An element store statement in a JIT-compiled loop body runs the store and
# its statement sink as one shim. A plain scalar stored by the lean lane needs
# no sink work; everything else (a lazy value, a Seq, a typed container) must
# still be sunk exactly as the interpreter sinks it.

plan 8;

my %h;
for ^200 { %h{"k$_"} = $_ * 3 }
is %h.elems, 200, 'hash element stores in a hot loop';
is %h<k199>, 597, 'last stored value';

my @a;
for ^200 { @a[$_] = $_ + 1 }
is @a.sum, 20100, 'array element stores in a hot loop';

# A Seq stored into an element is not forced by the statement sink.
my %s;
my $forced = 0;
for ^50 { %s{"s$_"} = (1..2).map({ $forced++; $_ }) }
is $forced, 0, 'a stored Seq is not forced by the sink';
is %s<s7>.List.join(','), '1,2', 'the stored Seq still yields its values';

# A gather stored into an element stays lazy, as in rakudo: it reads the
# variable when it is first read, after the loop.
my @g;
my $n = 0;
for ^30 { @g[$_] = gather { take $n } ; $n++ }
is @g[5].join, '30', 'a stored gather is read after the loop';

# A typed hash declines the lean lane; its store and sink run generically.
my Int %t;
for ^100 { %t{$_} = $_ }
is %t.values.sum, 4950, 'typed hash stores in a hot loop';
throws-like { for ^20 { %t{"x$_"} = "str" } }, X::TypeCheck::Assignment,
    'a typed hash still type-checks in a hot loop';
