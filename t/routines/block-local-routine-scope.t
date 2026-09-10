use Test;

plan 9;

# A `sub` declared inside a block is lexical to that block. A statement-level
# block has always got this from its BlockScope; a block compiled as a
# *callable* had no such boundary, so its declarations outlived it.
#
# The negative assertions go through EVAL because naming an undeclared routine
# directly is a *compile-time* error in rakudo, which would reject this whole
# file before it ran.

sub run(&c) { c() }

run { sub block-local-a { 1 } }
nok (try EVAL 'block-local-a()').defined,
    'a sub declared in a callable block is not callable after it returns';

my $b = { sub block-local-b { 2 } };
$b();
nok (try EVAL 'block-local-b()').defined,
    'a sub declared in a block stored in a variable does not leak either';

# The statement-level form was already correct; keep it pinned.
{ sub block-local-c { 3 } }
nok (try EVAL 'block-local-c()').defined,
    'a sub declared in a statement-level block still does not leak';

# Inside the block it is of course callable, and two sibling blocks each
# declaring a sub must not collide.
is run({ sub block-local-d { 4 }; block-local-d() }), 4,
    'the block can call its own sub';
is run({ sub block-local-e { 5 }; block-local-e() }), 5,
    'and a second block declaring a sub does not collide';

# Calling the same block twice re-registers its sub each time.
my $twice = 0;
my $c = { sub block-local-f { 6 }; block-local-f() };
$twice += $c();
$twice += $c();
is $twice, 12, 'calling the same block twice re-registers its sub each time';

# An exception escaping the block must still unwind the declaration.
try { run { sub block-local-g { 7 }; die "boom" } };
nok (try EVAL 'block-local-g()').defined,
    'a die escaping the block still unwinds its declaration';

# A named routine keeps its declarations -- a sub declared at compunit level is
# not affected by any of this.
sub outer-visible { 8 }
is outer-visible(), 8, 'a compunit-level sub is unaffected';

# The reason this matters beyond callability: the EVAL parser's operator
# pre-seed is built by walking the routine registry, so a leaked
# `sub infix:<@>` changed how a later EVAL string *parsed*.
run { sub infix:["@"] ($a, $b) { 42 } }
is EVAL('sub circumfix:["@", "@"] ($a) { $a }; @ 5 @'), 5,
    'a block-local operator does not change how a later EVAL parses';
