use Test;

plan 4;

# A sigilless role parameter is a term inside the role body, so a name that
# is also a quote-like operator (`m`, `s`, `y`, `tr`, `q`) is the parameter and
# does not open a regex/quote (Red's `role ProjectionModel[Mu:U \m]`).

role P[\m] { method of { m } }
is P[Int].of.^name, 'Int', 'bare m as the last term of a method body';

role Q[Mu:U \m] { method of { m } }
is Q[Str].of.^name, 'Str', 'typed sigilless parameter named m';

role R[\s, \y] { method both { (s, y) } }
is R[Int, Str].both.map(*.^name).join(','), 'Int,Str', 'several quote-like names';

role S[\tr] { method foo { tr } }
is S[Num].foo.^name, 'Num', 'tr as a role parameter';
