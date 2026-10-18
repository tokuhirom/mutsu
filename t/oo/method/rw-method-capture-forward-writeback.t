use Test;

plan 1;

class X1 {
    method add(\blob, $o is rw) { $o += 5; self }
    method mk(|c) { self.add(|c) }
}

sub inner2(\blob, $o is rw) {
    X1.mk(blob, $o);
    $o += 1;
}

sub outer2(|c) { inner2(|c) }

my $r = 0;
outer2(1, $r);
is $r, 6, 'rw capture forwarding preserves the caller shared cell after a method call';
