use Test;

# `Mu.perl` is `self.raku` (ADR-11276 §9.152): a call to `perl` with no
# user-declared `perl` in reach is a call to `raku`, so every receiver
# answers `.perl` exactly as it answers `.raku`, a user `raku` included.

plan 26;

# Built-in values: `.perl` and `.raku` agree.
for 42, 1.5, 1e10, <1/0>, 3+4i, "a\"b", True, Nil, Any, Int, (1, 2), [1, [2]],
    {a => 1}, set(1), bag(2), 1..3, (a => 1), \(1, :b), v1.2, "x".NFD -> $v {
    is $v.perl, $v.raku, "{$v.^name}.perl is .raku";
}

# A class with its own `raku` renders through it under either name.
class R { method raku { "R!" } }
is R.new.perl, "R!", 'a user raku answers .perl';

# A class with its own `perl` keeps it.
class P { method perl { "P!" } ; method raku { "praku" } }
is P.new.perl, "P!", 'a user perl answers .perl';

# A role mixed in at runtime that declares `raku`.
role RR { method raku { "mixed" } }
is (5 but RR).perl, "mixed", 'a mixin raku answers .perl';

# A role composed into a class that declares `perl`.
role PR { method perl { "role-perl" } }
class CP does PR { }
is CP.new.perl, "role-perl", 'a composed role perl answers .perl';

# A junction renders as a whole under `.perl`, as under `.raku`.
is any(1, 2).perl, any(1, 2).raku, 'Junction.perl is .raku';

# `.perl` inside a list's rendering of a user object reaches its `raku`.
is (R.new,).perl, (R.new,).raku, 'nested .perl reaches the user raku';
