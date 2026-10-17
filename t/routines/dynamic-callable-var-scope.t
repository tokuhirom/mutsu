use Test;

# A `my &*name` is a binding of the frame that declares it: callees see it,
# and nothing does once the frame is gone (#13705). Reading one no frame in
# the dynamic scope declares is X::Dynamic::NotFound, as for `$*name`.
# Checked against rakudo.
plan 9;

sub h() { my &*cb = { 1 }; 2 }
h();
is (try &*cb) // 'no cb', 'no cb', 'a routine\'s `my &*cb` is gone after it returns';
is (try ::('&*cb')) // 'no cb', 'no cb', 'an indirect lookup does not find it either';

{ my &*blk = { 1 } }
is (try &*blk) // 'no blk', 'no blk', 'nor does a block\'s once the block exits';

sub inner() { &*cb3() }
sub outer() { my &*cb3 = { 'from outer' }; inner() }
is outer(), 'from outer', 'a callee sees its caller\'s `my &*cb`';

sub shadow() { my &*cb4 = { 'inner' }; inner4() }
sub inner4() { &*cb4() }
{
    my &*cb4 = { 'outer' };
    is shadow(), 'inner', 'the innermost declaration wins';
    is &*cb4(), 'outer', 'and the outer one is back after the call';
}

isa-ok &*nope, Failure, 'an undeclared `&*name` reads as a Failure';
throws-like { &*nope.() }, X::Dynamic::NotFound, 'which throws X::Dynamic::NotFound';

PROCESS::<&pcb> = { 'process' };
is &*pcb(), 'process', 'a `PROCESS::<&name>` binding is still found';
