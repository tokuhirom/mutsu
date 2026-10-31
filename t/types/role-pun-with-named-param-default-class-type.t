use Test;

# Found via Concurrent::PriorityQueue: the pun of a role whose parameters have
# defaults (`R.new`, `my @a is R`) is registered under a name that carries the
# named parameter (`R\0default[Any,:c(5)]`). `::?CLASS` (and a method's
# `::?CLASS:D:` invocant) names that class, and was trimmed of its named
# argument before the match, so no instance of the pun matched it.
plan 8;

role C[::T = Any, :$c = 5] {
    method me(::?CLASS:D:) { "ok" }
    method me2(::?CLASS $x:) { "ok2" }
    method s { self ~~ ::?CLASS }
    method s2 { self.WHAT ~~ ::?CLASS }
    multi method m(::?CLASS:D: T:D $x) { "t" }
}

my $o = C.new;
is $o.me, "ok", "::?CLASS:D: invocant of a pun with a named parameter default";
is $o.me2, "ok2", "::?CLASS invocant";
ok $o.s, "self ~~ ::?CLASS";
ok $o.s2, "self.WHAT ~~ ::?CLASS";
is $o.m(1), "t", "multi with ::?CLASS:D: and T:D";

my @a is C;
is @a.m(1), "t", "role applied to an array variable";
is C[Int, :c(3)].new.m(1), "t", "explicit parameterization";
is C.new.^name, "C", "pun keeps the role's name";
