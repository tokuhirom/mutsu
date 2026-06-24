use Test;

# Multi/proto candidates with default parameter values are now VM-dispatched
# (OTF compiled) instead of falling back to the tree-walk interpreter. The
# compiled binding path (`bind_function_args_values`) evaluates defaults the
# same way the interpreter does; behaviour must stay byte-identical.
# (ledger §D, multi-dispatch VM-ization)

plan 16;

# literal default + arity-distinguished candidates.
multi sub greet($name, $greeting = "Hello") { "$greeting, $name" }
multi sub greet() { "nobody" }
is greet("Alice"), "Hello, Alice", "default used when omitted";
is greet("Bob", "Hi"), "Hi, Bob", "default overridden";
is greet(), "nobody", "zero-arg candidate";

# proto + default-param candidate.
proto sub power(|) {*}
multi sub power($base, $exp = 2) { $base ** $exp }
is power(3), 9, "proto default exp=2";
is power(2, 10), 1024, "proto explicit exp";

# default referencing an earlier positional param.
multi sub pr($x, $y = $x * 2) { "$x,$y" }
multi sub pr() { "none" }
is pr(3), "3,6", "default references earlier param";
is pr(3, 100), "3,100", "earlier-param default overridden";
is pr(), "none", "zero-arg sibling";

# default referencing a (top-level) closure variable.
my $fallback = "World";
multi sub hi($name = $fallback) { "Hi $name" }
multi sub hi(Int $n) { "num $n" }
is hi(), "Hi World", "default references closure var";
is hi("Bob"), "Hi Bob", "explicit string arg";
is hi(42), "num 42", "Int candidate wins";

# default referencing a method call on an earlier param.
multi sub fmt($n, $w = $n.chars) { "$n width $w" }
multi sub fmt() { "empty" }
is fmt(12345), "12345 width 5", "default via method call on earlier param";
is fmt(1, 9), "1 width 9", "explicit width";

# default defined inside an enclosing sub references that sub's lexical.
sub outer($base) {
    my $extra = $base + 100;
    multi sub inner($x = $extra) { "x=$x" }
    multi sub inner(Str $s) { "s=$s" }
    return (inner(), inner("hello"), inner(7));
}
is outer(5), ("x=105", "s=hello", "x=7"), "nested-scope default lexical";

# default + nextsame redispatch still defers in the right order. nextsame
# passes the ORIGINAL args (just `5`), so the deferred candidate's $tag falls
# back to ITS own default "B" (matches raku).
my @ord;
proto sub chain(|) {*}
multi sub chain(Int $n, $tag = "A") { @ord.push("int:$tag"); nextsame }
multi sub chain($n, $tag = "B") { @ord.push("any:$tag") }
chain(5);
is @ord, ["int:A", "any:B"], "default param + nextsame chain (orig args deferred)";

# recursion through a default-bearing multi.
multi sub countdown($n, $acc = "") { $n <= 0 ?? $acc !! countdown($n - 1, $acc ~ $n) }
is countdown(3), "321", "recursive default multi";
