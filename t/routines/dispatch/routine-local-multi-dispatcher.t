use Test;

plan 7;

# The multi family is local to this routine call, even though its candidate
# values can be captured by a dispatcher before the call returns.
sub declare-local-multi() {
    my $candidate := multi routine-local-multi(Int $x) { "int:$x" };
    42
}
is declare-local-multi(), 42, 'routine can declare and use a multi candidate';
my $leaked = try EVAL 'routine-local-multi(1)';
nok $leaked.defined, 'routine-local multi name is gone after the call returns';

sub make-local-dispatcher() {
    my $candidate := multi captured-local-multi(Int $x) { "int:$x" };
    multi captured-local-multi(Str $x) { "str:$x" }
    multi captured-local-multi($x) { "any:$x" }
    $candidate.dispatcher
}
my $dispatcher = make-local-dispatcher();
is $dispatcher(3), 'int:3', 'captured dispatcher keeps its typed candidate';
is $dispatcher('x'), 'str:x', 'captured dispatcher keeps its other candidate';
is $dispatcher(1.5), 'any:1.5', 'captured dispatcher keeps its untyped candidate';
my $captured_name_leaked = try EVAL 'captured-local-multi(3)';
nok $captured_name_leaked.defined, 'captured dispatcher does not keep the name global';
is $dispatcher.candidates.elems, 3, 'captured dispatcher retains the whole candidate set';
