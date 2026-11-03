use Test;

# A `my sub` that mentions a type its routine declares stays registry-resolved
# (it is not a frame lexical), but it is still lexical to the routine: the
# `EVAL` of a plain sub that routine CALLS must not find it (#16099). The
# routine's own `EVAL`, a closure and a nested sub still do.

plan 11;

sub callee { (try EVAL q[st(5)]) // 'nv-sub' }

sub with-subset { my subset Small of Int where * < 10; my sub st($x) { $x ~~ Small }; callee() }
sub with-enum   { my enum E <A B>; my sub st($x) { $x ~~ E }; callee() }
sub with-class  { my class K { }; my sub st($x) { $x ~~ K }; callee() }
sub with-role   { my role R { }; my sub st($x) { $x ~~ R }; callee() }

is with-subset(), 'nv-sub', "caller's my sub beside a my subset is invisible to the callee's EVAL";
is with-enum(),   'nv-sub', 'same beside a my enum';
is with-class(),  'nv-sub', 'same beside a my class';
is with-role(),   'nv-sub', 'same beside a my role';

sub own-eval { my class K { }; my sub st($x) { $x ~~ K }; EVAL q[st(K.new)] }
is own-eval(), True, "the declaring routine's own EVAL sees its my sub";

sub nested-eval {
    my class K { }
    my sub st($x) { $x ~~ K }
    my sub inner { EVAL q[st(K.new)] }
    inner()
}
is nested-eval(), True, "a sub nested in the declaring routine sees it in its EVAL";

sub via-closure { my class K { }; my sub st($x) { $x ~~ K }; my &c = -> { EVAL q[st(K.new)] }; c() }
is via-closure(), True, "a closure in the declaring routine sees it in its EVAL";

sub direct { my class K { }; my sub st($x) { $x ~~ K }; st(K.new) }
is direct(), True, 'a direct call still works';

# After the callee's EVAL the sub is back for the rest of the routine.
sub after { my class K { }; my sub st($x) { $x ~~ K }; callee(); st(K.new) }
is after(), True, "the my sub is back after the callee's EVAL";

# A sub declared in a mainline block and recursing through EVAL sees itself
# (roast/integration/weird-errors.t): the block, not a routine, is its scope.
{
    my $i = 0;
    sub rec {
        return if ++$i == 20;
        EVAL 'rec';
    }
    lives-ok { rec }, 'a block-level sub can recurse through EVAL';
    is $i, 20, '... all the way down';
}
