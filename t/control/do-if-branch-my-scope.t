use v6;
use Test;

plan 12;

# A `my` declared in a value-position `if`/`with` branch belongs to that branch:
# the outer variable of the same name keeps its value (the branch used to leave
# its env mirror behind for a later scoped branch to restore into the outer slot).
my $x = 1;
my $t = 1;
my $r1 = do if $t { my $x = 2; 7 };
my $r2 = do with $t { my $x = 3; 8 };
is $x, 1, "two value branches that redeclare `my \$x` leave the outer one alone";
is "$r1 $r2", "7 8", "and still yield their values";

my $y = 10;
my $r3 = do if $t { my $y = 4; $y * 2 } else { my $y = 5; $y };
is $y, 10, "if/else value branches";
is $r3, 8, "the value of the taken branch";

sub in-sub {
    my $v = do if $t { my $x = 2; 7 };
    "$v $x";
}
is in-sub(), "7 1", "inside a sub the outer variable is still seen after the branch";
is $x, 1, "and the mainline is untouched";

# Control flow out of a scoped value branch still works.
sub early($n) { my $r = do if $n { my $v = $n * 2; return "ret:$v" }; "no" }
is early(4), "ret:8", "return from a scoped value branch";
is early(0), "no", "and the branch not taken";

my @seen;
for 1..3 -> $i { my $r = do if $i == 2 { my $z = $i; next }; @seen.push($i) }
is-deeply @seen, [1, 3], "next from a scoped value branch";

# A gather that suspends inside a value branch with a `my` still resumes.
sub tree($n) {
    gather {
        take $n;
        if $n > 0 { my $half = $n div 2; .take for tree($half) }
    }
}
is tree(4).join(","), "4,2,1,0", "a gather whose tail `if` declares a `my`";

my $s = do if True { my $s = 9; $s };
is $s, 9, "the new variable may share the name of the one being assigned";

my $c = 0;
for 1..3 { my $v = do if True { my $x = $_; $x * 2 }; $c += $v }
is $c, 12, "a scoped value branch in a loop body";
