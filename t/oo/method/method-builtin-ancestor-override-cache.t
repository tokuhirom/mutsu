use Test;
use MONKEY-TYPING;
plan 5;
class S is Str { method uc { "<" ~ callsame() ~ ">" } }
my $s = S.new(value => "ab");
is $s.uc, "<AB>", 'override defers to the built-in';
is $s.uc, "<AB>", 'second call (memoized) agrees';
class P { method foo { "p" } }
class Q is P { method foo { "q" ~ callsame() } }
is Q.new.foo, "qp", 'user-only chain defers';
augment class P { method bar { "b" } }
is Q.new.bar, "b", 'a method added after the first call is seen';
class R is Q { method foo { "r" ~ callsame() } }
is R.new.foo, "rqp", 'a later subclass does not reuse a stale answer';
