use Test;

plan 8;

# Calling a Callable held in a `&` variable must not pull a deferred Seq
# argument: the Seq keeps its single-use rules, as for a named sub (#13105).
my &r = -> \s { my $x = s.List };
my \t = (1..3).map(*+1);
r(t);
throws-like { t.List }, X::Seq::Consumed, 'a pointy block that reads its Seq argument consumes it';

my &r2 = -> \s { 1 };
my \t2 = (1..3).map(*+1);
r2(t2);
t2.List;
throws-like { t2.List }, X::Seq::Consumed, 'a pointy block that ignores its Seq argument leaves it single-use';

my &r3 = sub (\s) { my $x = s.List };
my \t3 = (1..3).map(*+1);
r3(t3);
throws-like { t3.List }, X::Seq::Consumed, 'an anonymous sub held in a variable';

# The binder still pulls a deferred Seq where it must read the elements.
my &slurp-sum = { [+] @_ };
is slurp-sum((1,2,3).map(* * 2)), 12, '@_ flattens a deferred map Seq';
my &star = -> *@x { @x.elems };
is star((1,2,3).map(* + 1)), 3, '*@ flattens a deferred map Seq';
my &plus = -> +@x { @x.join(",") };
is plus((1,2,3).grep(* > 1)), '2,3', '+@ flattens a deferred grep Seq';
my &pos = -> @x { @x.join(",") };
is pos((1,2,3).map(* + 1)), '2,3,4', 'an @ parameter reads a deferred map Seq';
my &scalar = -> $x { $x.join(",") };
is scalar((1,2,3).map(* + 1)), '2,3,4', 'a $ parameter keeps the Seq';
