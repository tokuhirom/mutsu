use Test;

# `raku` of a collection holding an object whose `raku` is user code renders
# that object through its own method, whichever way the call is made
# (ADR-11276 §9.194; every answer checked against Rakudo).

plan 17;

class F { method raku { "F!" } }
class G { has $.x = 1 }
my $m = "raku";

is [F.new, 1]."$m"(), '[F!, 1]', 'an Array';
is (F.new, 2)."$m"(), '(F!, 2)', 'a List';
is (a => F.new)."$m"(), ':a(F!)', 'a Pair value';
is {a => F.new}."$m"(), '{:a(F!)}', 'a Hash';
is (F.new => 1)."$m"(), 'F! => 1', 'a Pair key';
is \(F.new, :k(F.new))."$m"(), '\(F!, :k(F!))', 'a Capture';
is (F.new, 3).Seq."$m"(), '(F!, 3).Seq', 'a Seq';
is [G.new]."$m"(), '[G.new(x => 1)]', 'an object with the default raku';
my @a = F.new;
is @a."$m"(), '[F!]', 'an @-array';
my %h = a => F.new;
is %h."$m"(), '{:a(F!)}', 'a %-hash';
is $(F.new, 1)."$m"(), '$(F!, 1)', 'an itemized List keeps its $';
is $[F.new]."$m"(), '$[F!]', 'an itemized Array keeps its $';
is [[F.new], 2]."$m"(), '[[F!], 2]', 'a nested Array';
is (1, (F.new, 3))."$m"(), '(1, (F!, 3))', 'a nested List';
is (F.new xx 2).Seq."$m"(), '(F!, F!).Seq', 'a Seq of objects';
is [F.new, 1].raku, '[F!, 1]', 'a direct call';
ok set(F.new)."$m"().starts-with('Set.new('), 'a Set';
