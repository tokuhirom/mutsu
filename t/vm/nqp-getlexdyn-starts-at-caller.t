use Test;
use nqp;

# nqp::getlexdyn starts its search at the caller of the running frame, as on
# MoarVM: a frame that declares `my $*x` further down (bound from the scope's
# entry, #19174) still reads the caller's binding through it (#19184).

plan 6;

sub inner() {
    my $seen = nqp::getlexdyn('$*FLAG');
    my $*FLAG = False;
    ($seen, $*FLAG)
}
sub outer() { my $*FLAG = True; inner() }
my ($seen, $own) = outer();
is $seen, True, 'getlexdyn ahead of the own my $*x reads the caller binding';
is $own, False, 'the own declaration still binds afterwards';

sub inner-none() {
    my $seen = nqp::getlexdyn('$*NOBODY');
    my $*NOBODY = 1;
    $seen
}
ok nqp::isnull(inner-none()) || !inner-none().defined,
    'no caller binding answers null, not the own pending declaration';

sub reader() { nqp::getlexdyn('$*PLAIN') }
sub setter() { my $*PLAIN = 42; reader() }
is setter(), 42, 'a frame that declares nothing reads the caller binding';

# The shape EEL's slang uses: a method saves a caller flag, then resets it
# for its own callees.
class C {
    method callee() { $*COLON }
    method outer-call() {
        my $*COLON = 'caller';
        self.reset-and-call
    }
    method reset-and-call() {
        my $was = nqp::getlexdyn('$*COLON');
        my $*COLON = 'reset';
        ($was, self.callee)
    }
}
my ($was, $inner) = C.outer-call;
is $was, 'caller', 'a method reads its caller method binding';
is $inner, 'reset', 'its callee sees the reset binding';
