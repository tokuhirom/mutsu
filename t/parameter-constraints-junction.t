use Test;

plan 9;

# `Parameter.constraints` is an `all()` junction of the parameter's value
# constraints. An unconstrained parameter yields the EMPTY `all()`, which is
# what lets a caller detect "no constraints" by autothreading a sub over it —
# the idiom Cro's route compiler uses to decide whether a signature bind test
# is needed.

sub collect($param) {
    my @out;
    sub extract($v --> Nil) { @out.push($v) }
    extract($param.constraints);
    @out
}

my $slurpy = (-> *@ { }).signature.params[0];
isa-ok $slurpy.constraints, Junction, 'slurpy parameter .constraints is a Junction';
is collect($slurpy).elems, 0, 'unconstrained slurpy autothreads zero times';

my $plain = (-> $x { }).signature.params[0];
is collect($plain).elems, 0, 'unconstrained scalar autothreads zero times';

my $typed = (-> Int $y { }).signature.params[0];
is collect($typed).elems, 0, 'a type constraint is not a value constraint';

my $literal = (-> "lit" { }).signature.params[0];
is collect($literal).elems, 1, 'a literal parameter has one constraint';
is collect($literal)[0], 'lit', 'the literal is the constraint';

my $whered = (-> $z where * > 5 { }).signature.params[0];
is collect($whered).elems, 1, 'a where clause is one constraint';
ok collect($whered)[0] ~~ Callable, 'the where constraint is callable';

# The empty junction still smartmatches truely against anything, so code that
# uses .constraints as a matcher keeps working.
ok 42 ~~ $plain.constraints, 'empty all() accepts any value';
