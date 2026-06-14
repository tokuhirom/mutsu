use v6;
use Test;

# VM-native dispatch of a pure lexical &-var call made with a *slip*
# (`op(|@args)`) — Track A follow-up. The slip call path
# (`exec_call_func_slip_op`) used to reach the tree-walking interpreter
# terminal (`call_function_compiled_first`) for a `&op` parameter; it now
# dispatches through `vm_call_on_value`, the same as the non-slip case. The
# cases below pin the semantics that must survive: slip expansion position,
# dynamic-var visibility, lexotic control flow, and instance mutation.

plan 6;

# --- basic slurpy slip through a &-param ---
sub apply(&op, *@args) { op(|@args) }
is apply({ $^a + $^b }, 3, 4), 7,
    'placeholder block via &-param with slip args';

# --- slip in the middle, regular args around it ---
sub mid(&op) { op(1, |(2, 3), 4) }
is mid(-> *@a { @a.join("-") }), "1-2-3-4",
    'slip expands at its position with regular args on both sides';

# --- dynamic var rebinding visible through a slip call ---
class FakeIO {
    has @!lines;
    method print(*@stuff) { @!lines.push(@stuff.join); True }
    method lines() { @!lines }
}
sub cap(&code, @args) {
    my $*ERR = FakeIO.new;
    code(|@args);
    $*ERR.lines.join("|");
}
is cap(-> $m { note $m }, ["hi"]), "hi\n",
    'note inside a slip &-param call writes to the caller-rebound $*ERR';

# --- lexotic control flow through the slip call ---
sub invoke(&c, @a) { c(|@a); "after" }
sub g {
    invoke(-> $x { return "early:$x" }, [9]);
    "late";
}
is g(), "early:9", 'return inside a slip &-param block exits the enclosing routine';

# --- take propagates through the slip call into gather ---
sub trampoline(&c, @a) { c(|@a) }
my @vals = gather trampoline(-> $n { take $_ for 1..$n }, [3]);
is @vals.join(","), "1,2,3", 'take propagates through a slip &-param call';

# --- instance mutation inside the slip closure (cell visibility) ---
class Counter {
    has $.n is rw = 0;
    method add($d) { $!n += $d }
}
sub run(&f, @a) {
    my $c = Counter.new;
    f($c, |@a);
    f($c, |@a);
    $c.n;
}
is run(-> $c, $d { $c.add($d) }, [5]), 10,
    'instance attr mutation inside a slip &-param call is visible in caller';
