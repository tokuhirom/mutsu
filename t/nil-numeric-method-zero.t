use v6;
use Test;

# The Real methods that numify their invocant (`abs`, `floor`, `ceiling`,
# `round`, `truncate`, `sign`) treat Nil as the numeric zero: they warn "Use of
# Nil in numeric context" and resume with the method applied to 0 — which is the
# Int 0 for all of them — instead of absorbing to Nil. (This extends the same
# treatment `Nil.Int` / `Nil.Num` / `Nil.Rat` already get.)

is (quietly Nil.abs),      0, "Nil.abs is 0";
is (quietly Nil.floor),    0, "Nil.floor is 0";
is (quietly Nil.ceiling),  0, "Nil.ceiling is 0";
is (quietly Nil.round),    0, "Nil.round is 0";
is (quietly Nil.truncate), 0, "Nil.truncate is 0";
is (quietly Nil.sign),     0, "Nil.sign is 0";

# The zero is a defined Int, not a type object.
is (quietly Nil.abs).^name,   "Int", "Nil.abs is an Int";
ok (quietly Nil.abs).defined,        "Nil.abs is defined";

# The coercion is a warning, not a hard failure — it resumes.
{
    my @warnings;
    my $result = do {
        CONTROL { when CX::Warn { @warnings.push(.message); .resume } }
        Nil.abs;
    };
    is $result, 0, "Nil.abs resumes with 0";
    ok @warnings.grep(*.contains("Nil")).elems >= 1, "Nil.abs warns about Nil in numeric context";
}

# Regression: a genuinely Nil-absorbing (unknown) method still returns Nil.
is Nil.foo.gist, "Nil", "unknown method on Nil is still absorbed to Nil";

# Regression: the numify methods still work on real numbers.
is (-5).abs, 5, "abs on a real Int is unchanged";
is 3.7.floor, 3, "floor on a real Rat is unchanged";
is (-2).sign, -1, "sign on a real Int is unchanged";

done-testing;
