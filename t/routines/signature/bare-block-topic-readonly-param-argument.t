use Test;

plan 9;

# A bare block's implicit `$_` is bound to its argument itself. When that
# argument is a read-only binding (a plain parameter, a `for` loop alias),
# writing `$_` dies, as it does for a literal argument.

my &dec = { --$_ };
my &set = { $_ = 1 };

dies-ok { dec(2) }, 'literal argument';
sub via-param($x) { dec($x) }
dies-ok { via-param(2) }, 'read-only parameter';
sub via-run(&b, $x) { b($x) }
dies-ok { via-run({ --$_ }, 2) }, 'block passed to a routine, called with its parameter';
dies-ok { via-run({ $_ = 1 }, 2) }, 'assignment to $_ likewise';
for (2, 3) -> $y {
    dies-ok { dec($y) }, 'for-loop alias';
    last;
}

# Writable variables still write through.
my $v = 5;
dec($v);
is $v, 4, 'a plain variable is written through';
sub via-copy($x is copy) { dec($x); $x }
is via-copy(2), 1, 'an `is copy` parameter is writable';
sub via-rw($x is rw) { dec($x) }
my $w = 9;
via-rw($w);
is $w, 8, 'an `is rw` parameter writes the caller variable';
my $u = 7;
set($u);
is $u, 1, 'assignment through a plain variable';

done-testing;
