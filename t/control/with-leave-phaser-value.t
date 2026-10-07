use Test;

# From the UpRooted distribution: DBDish::Pg::Native's `quote` ends in
# `with COND ?? A !! B { LEAVE { ... }; nativecast(Str, $_) } else { Nil }`.
# A `with`/`given` body carrying a LEAVE phaser lost its value in tail position.

plan 8;

my $left = 0;
sub leave-hook($x) { $left++ }

sub a { given 5 { LEAVE { leave-hook(1) }; "v" } }
is a(), "v", 'tail given with LEAVE yields its value';
is $left, 1, 'LEAVE ran';

sub b { with 5 { LEAVE { leave-hook(1) }; "q$_" } else { Nil } }
is b(), "q5", 'tail with/else with LEAVE yields its value';

sub c($as-id) {
    with $as-id ?? 42 !! 7 {
        LEAVE { leave-hook($_) }
        "got $_";
    } else {
        Nil
    }
}
is c(True), "got 42", 'ternary condition, true';
is c(False), "got 7", 'ternary condition, false';

sub d { with Nil { LEAVE { leave-hook(1) }; "x" } else { "else" } }
is d(), "else", 'else branch still taken';

sub f {
    given 1 { LEAVE { leave-hook(1) }; "inner" }
    "outer"
}
is f(), "outer", 'non-final given with LEAVE does not shadow the block value';

is (do given 3 { LEAVE { leave-hook(1) }; "w$_" }), "w3", 'do given with LEAVE';
