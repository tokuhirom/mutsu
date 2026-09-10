use Test;

plan 5;

throws-like 'my Int:D $x = Int', X::TypeCheck::Assignment,
    message => 'Type check failed in assignment to $x; expected Int:D but got Int (Int) (perhaps Nil was assigned to a :D which had no default?)',
    'a matching type object under :D keeps the smiley and gets the Nil hint';

throws-like 'my Int:U $x = 5', X::TypeCheck::Assignment,
    message => 'Type check failed in assignment to $x; expected Int:U but got Int (5)',
    'a defined value under :U keeps the smiley';

throws-like 'my Str:D $x = Nil', X::TypeCheck::Assignment,
    message => 'Type check failed in assignment to $x; expected Str:D but got Str (Str) (perhaps Nil was assigned to a :D which had no default?)',
    'Nil under :D reports the declared type object it resets to';

throws-like 'my Str:D $x = Int', X::TypeCheck::Assignment,
    message => 'Type check failed in assignment to $x; expected Str:D but got Int (Int)',
    'an unrelated type object under :D has no Nil hint';

throws-like 'my Int $x = "s"', X::TypeCheck::Assignment,
    message => 'Type check failed in assignment to $x; expected Int but got Str ("s")',
    'a plain nominal mismatch keeps its existing message';
