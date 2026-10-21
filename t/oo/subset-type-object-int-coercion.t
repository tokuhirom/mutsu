use Test;

# A user subset's type object coerces as the type it refines: Int.Int and
# Num.Num answer their invocant, so the subset's type object is the answer.
plan 7;

subset S of Int;
is-deeply S.Int, S, 'subset S of Int; S.Int is S';
subset P of Int where * > 0;
is-deeply P.Int, P, 'a where-subset too';
subset S2 of S;
is-deeply S2.Int, S2, 'a subset of a subset';
subset Q of Num;
is-deeply Q.Num, Q, 'subset of Num; Q.Num';
is-deeply UInt.Int, UInt, 'UInt.Int is unchanged';
is-deeply Int.Int, Int, 'Int.Int is unchanged';
nok S.defined, 'the subset type object is undefined';
