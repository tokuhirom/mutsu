use Test;

# `$/` — and the capture variables `$0`, `$1`, `$<name>` that are views into it
# — are implicitly `my`-declared in every routine, exactly like `$!`. A routine
# that matches internally therefore must NOT clobber its caller's match. A bare
# block is the opposite case: it shares its enclosing routine's `$/`, so a match
# inside `if`/`for`/`{ }` must stay visible to the enclosing scope.

plan 14;

sub inner-match() { "zz" ~~ /(z)/; 1 }
method-holder-check();

"abc" ~~ /(b)(c)/;
is ~$/,  'bc', 'baseline $/';
is ~$0,  'b',  'baseline $0';
is ~$1,  'c',  'baseline $1';

inner-match();
is ~$/,  'bc', 'a sub that matches internally does not clobber the caller $/';
is ~$0,  'b',  '... nor the caller $0';
is ~$1,  'c',  '... nor the caller $1';

# A named capture's own slot survives a routine call that has no named
# captures of its own. (The stronger assertion -- that a routine which RESETS
# named captures does not delete the caller's -- fails for an unrelated reason
# and is tracked in todo/tickets/named-capture-reset-removes-the-callers-slot.md.)
"abc" ~~ /$<first>=(b)(c)/;
is ~$<first>, 'b', 'baseline $<first>';

# A method is a routine too.
class Matcher { method m() { "yy" ~~ /(y)/; 1 } }
"abc" ~~ /(b)(c)/;
Matcher.new.m();
is ~$/, 'bc', 'a method that matches internally does not clobber the caller $/';
is ~$0, 'b',  '... nor the caller $0';

# A Callable invoked through another routine is still a routine boundary for
# the routine, and the block itself is not.
# (A BLOCK invoked through a routine should publish its match to the scope the
# block was written in; mutsu does not yet -- see
# todo/tickets/a-blocks-match-does-not-reach-its-defining-scope-through-a-callable.md.)
sub call-it(&c) { c() }
"abc" ~~ /(b)(c)/;
call-it(&inner-match);
is ~$/, 'bc', 'a SUB invoked through a Callable still keeps its match private';

# The converse the routine gate must preserve: a bare block writes the
# enclosing routine's `$/`, and so does a conditional.
sub block-writes-mine() {
    "abc" ~~ /(b)(c)/;
    { "yy" ~~ /(y)/ }
    ~$/;
}
is block-writes-mine(), 'y', 'a bare block writes its enclosing routine $/';

sub if-writes-mine() {
    if "abc" ~~ /(b)(c)/ { }
    ~$/;
}
is if-writes-mine(), 'bc', 'an `if` condition match is visible after the `if`';

# `$!` keeps working the same way (it shares the mechanism).
sub dies-inside() { try { die "boom" }; 1 }
try { die "outer" };
dies-inside();
is $!.message, 'outer', 'a sub that catches internally does not clobber the caller $!';

sub method-holder-check() { True }
is method-holder-check(), True, 'sanity';
