use Test;

# A nested `if` in a `do` branch whose body carries a `... without $x;`
# statement modifier still yields the branch's last value. Found via Red's
# `new-from-data`, distribution RedX::HashedPassword.

plan 4;

sub a1($v) { do with $v { if True { die "x" without $v; 5 } } }
is a1(1), 5, 'do with / if / without modifier';
sub a5($v) { do if True { if True { die "x" without $v; 5 } } }
is a5(1), 5, 'do if / if / without modifier';
sub a6($v) { do if True { if True { say "never" with Nil; 6 } } }
is a6(1), 6, 'with modifier';
sub a7($c, $v) {
    do with $v {
        if $c eq 'z' { Empty }
        elsif !$c.contains: "." {
            my $col = 3;
            die "x" without $col;
            $c => $v
        }
    } else { Empty }
}
is-deeply a7('a', 1), (a => 1), 'pair value survives the modifier';
