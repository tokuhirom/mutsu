use Test;

plan 29;

# Type objects are undefined
is Int.defined, False, 'Int.defined is False';
is Str.defined, False, 'Str.defined is False';
is Num.defined, False, 'Num.defined is False';
is Bool.defined, False, 'Bool.defined is False';
is Rat.defined, False, 'Rat.defined is False';

# Instances are defined
is 42.defined, True, '42.defined is True';
is "hello".defined, True, '"hello".defined is True';
is 3.14e0.defined, True, '3.14e0.defined is True';
is True.defined, True, 'True.defined is True';

# .WHAT returns the type object
is 42.WHAT.gist, '(Int)', '42.WHAT is (Int)';
is "hello".WHAT.gist, '(Str)', '"hello".WHAT is (Str)';
is 3.14e0.WHAT.gist, '(Num)', '3.14e0.WHAT is (Num)';
is True.WHAT.gist, '(Bool)', 'True.WHAT is (Bool)';
is Int.WHAT.gist, '(Int)', 'Int.WHAT is (Int)';

# Type objects match their own type via smartmatch
ok Int ~~ Int, 'Int ~~ Int';
ok Str ~~ Str, 'Str ~~ Str';
ok 42 ~~ Int, '42 ~~ Int';
ok "hello" ~~ Str, '"hello" ~~ Str';

# .new on built-in type objects creates default instances
is Int.new, 0, 'Int.new is 0';
is Int.new.defined, True, 'Int.new.defined is True';
is Str.new, '', 'Str.new is empty string';
is Str.new.defined, True, 'Str.new.defined is True';
is Num.new, 0e0, 'Num.new is 0e0';
is Num.new.defined, True, 'Num.new.defined is True';

# Type smileys in smartmatch
ok Int:U ~~ Int, 'Int:U ~~ Int';
ok 42 ~~ Int:D, '42 ~~ Int:D';
ok Int ~~ Int:U, 'Int ~~ Int:U';
nok 42 ~~ Int:U, '42 ~~ Int:U is False';
nok Int ~~ Int:D, 'Int ~~ Int:D is False (type object is not defined)';
