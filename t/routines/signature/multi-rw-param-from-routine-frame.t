use Test;

plan 6;

# A multi candidate with an `is rw` parameter is reachable from inside a
# routine whose own frame runs on the typed tier: the variable argument is
# handed over writable, so the candidate is not ruled out.
multi k($a, Int:D $p is rw) { $p++; "k $p" }
multi k(Str $s) { "s" }

sub t1($x) { my $r = 0; my $res = k($x, $r); "$res/$r" }
is t1(1), 'k 1/1', 'an existing variable binds the is-rw parameter';

sub t2($x) { k($x, my $r = 0) }
is t2(1), 'k 1', 'a declaration argument binds the is-rw parameter';

sub t3($x) { my $r = 5; k($x, $r); $r }
is t3(1), 6, 'the write lands in the caller';

multi f(Int $x) { "int $x" }
multi f(Str $x is rw) { $x ~= "!"; "str $x" }
sub t4($a) { my $i = 1; my $s = "a"; (f($i), f($s), $s).join(',') }
is t4(0), 'int 1,str a!,a!', 'rw and non-rw candidates both dispatch';

# A plain sub keeps working.
sub g($a, Int:D $p is rw) { $p++; "g $p" }
sub t5($x) { g($x, my $r = 0) }
is t5(1), 'g 1', 'single routine with a declaration argument';

# The two-argument wrapper shape of BSON::Simple's bson-decode.
multi dec(Blob:D $b) { my $v := dec($b, my $pos = 0); "$v@$pos" }
multi dec(Blob:D $b, Int:D $pos is rw) { $pos += $b.elems; "decoded" }
is dec(Buf.new(1, 2, 3)), 'decoded@3', 'is-rw position threaded through a multi wrapper';
