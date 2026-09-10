use v6;
use Test;

# VM-native dispatch of a lexical &-var that *shadows* a same-named package
# sub (Track A follow-up). When a `&foo` parameter (or `my &foo`) shadows an
# existing package `sub foo`, the call used to reach the tree-walking
# interpreter terminal via `call_sub_value`; it now dispatches through
# `vm_call_on_value` just like the pure-lexical case. The cases below pin the
# semantics that must survive the move: argument passing, lexotic control flow,
# dynamic-var visibility, and instance-mutation visibility across frames.

plan 8;

# --- a lexical &-param shadows a package sub of the same name ---
sub foo() { "package-foo" }
sub callit(&foo) { foo() }
is callit(-> { "lexical-foo" }), "lexical-foo",
    'anon block bound to &foo shadows the package sub foo';

# --- argument passing through the shadowed call ---
sub add($a, $b) { $a + $b }
sub useadd(&add) { add(10, 20) }
is useadd(-> $x, $y { $x * $y }), 200,
    'args flow to the shadowing lexical, not the package sub';

# --- a named sub with a *different* name bound to the param ---
sub orig() { "orig" }
sub other() { "other" }
sub run(&orig) { orig() }
is run(&other), "other",
    'a differently-named package sub bound to &orig shadows orig';

# --- dynamic var rebinding visible inside the shadowed call ---
class FakeIO {
    has @!lines;
    method print(*@stuff) { @!lines.push(@stuff.join); True }
    method lines() { @!lines }
}
sub greet() { "package-greet" }
sub cap(&greet) {
    my $*ERR = FakeIO.new;
    note greet();
    $*ERR.lines.join("|");
}
is cap(-> { "lexical-greet" }), "lexical-greet\n",
    'note inside the shadowing call writes to the caller-rebound $*ERR';

# --- lexotic control flow through the shadowed call ---
sub stepper() { }
sub invoke(&stepper) { stepper(); "after" }
sub g {
    invoke(-> { return "early" });
    "late";
}
is g(), "early", 'return inside a shadowing block exits the enclosing routine';

my @seen;
sub probe() { }
sub run-once(&probe) { probe() }
for 1..5 -> $i {
    run-once(-> { last if $i == 3; @seen.push($i) });
}
is @seen.join(","), "1,2",
    'last inside a shadowing block breaks the enclosing loop';

sub gen() { 0 }
sub trampoline(&gen) { gen() }
my @vals = gather trampoline(-> { take $_ for 1..3 });
is @vals.join(","), "1,2,3", 'take propagates through the shadowing call';

# --- instance mutation inside the shadowing closure (cell visibility) ---
class Counter {
    has $.n is rw = 0;
    method bump() { $!n++ }
}
sub step() { }
sub driver(&step) {
    my $c = Counter.new;
    step($c); step($c);
    $c.n;
}
is driver(-> $c { $c.bump }), 2,
    'instance attr mutation inside the shadowing call is visible in caller';
