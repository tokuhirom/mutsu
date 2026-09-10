use v6;
use Test;

plan 13;

# Builtin routine handles are Subs in Rakudo (never "Routine"), and a
# builtin-method lookup handle is a Method; gist/Str render like Rakudo.

is &say.^name, 'Sub', '&say.^name is Sub';
is CORE::<&say>.^name, 'Sub', 'CORE::<&say>.^name is Sub';
is &print.^name, 'Sub', '&print.^name is Sub';
is &say.WHAT.gist, '(Sub)', '&say.WHAT is (Sub)';
ok &say ~~ Sub, '&say ~~ Sub';
ok &say ~~ Routine, '&say ~~ Routine (Sub isa Routine)';
ok &say ~~ Callable, '&say ~~ Callable';

is 'abc'.^lookup('uc').^name, 'Method', 'builtin method lookup is a Method';
is 'abc'.^lookup('uc').gist, 'uc', 'Method gists as the bare name';

is &say.gist, '&say', 'Sub handle gists as &name';
is &infix:<+>.gist, '&infix:<+>', 'operator handle gists with the operator name';
is &say.Str, 'say', 'Sub handle .Str is the bare name';
is "x{&say}y", 'xsayy', 'interpolation stringifies to the bare name';

done-testing;
