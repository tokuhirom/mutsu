use v6;
use Test;

plan 12;

# A Str is one item: grep's matcher decides whether it stays.
is-deeply "a".grep(Str).list, ("a",), "a Str passes a Str type matcher";
is-deeply "a".grep(Int).list, (), "a Str does not pass an Int type matcher";
is-deeply "a".grep(/a/).list, ("a",), "a matching regex keeps it";
is-deeply "a".grep(/b/).list, (), "a failing regex drops it";
is-deeply "a".grep(* eq "a").list, ("a",), "a passing closure keeps it";
is-deeply "a".grep(* eq "b").list, (), "a failing closure drops it";

is-deeply "a".grep(Str, :k).list, (0,), ":k of a match";
is-deeply "a".grep(Int, :k).list, (), ":k of a miss";
is-deeply "a".grep(Str, :kv).list, (0, "a"), ":kv of a match";
is-deeply "a".grep(Int, :p).list, (), ":p of a miss";

role Marker { }
class Node does Marker { }
is ("*", Node.new).grep(Marker).elems, 1, "a list of a Str and a role-typed object keeps only the object";
is "*".grep(Marker).elems, 0, "a lone Str is not an instance of an unrelated role";
