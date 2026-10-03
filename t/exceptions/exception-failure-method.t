use Test;

# `$exception.Failure` wraps the exception in an unhandled Failure, the same
# value `Failure.new($exception)` builds -- the idiom `CATCH { return .Failure }`
# Needle::Compile's `compile-needle` relies on.

plan 8;

my $f = X::AdHoc.new(payload => "boom").Failure;
isa-ok $f, Failure, '.Failure on an exception gives a Failure';
is $f.exception.message, "boom", '...carrying the exception';
nok $f.handled, '...unhandled';

class X::Mine is Exception { method message { "mine" } }
is X::Mine.new.Failure.exception.message, "mine", 'works on a user exception class';

sub t() {
    CATCH { return .Failure }
    die "oops";
}
my $r = t();
isa-ok $r, Failure, 'CATCH { return .Failure } returns a Failure';
is $r.exception.message, "oops", '...for the caught exception';

class X::Own is Exception { method Failure { "own" } }
is X::Own.new.Failure, "own", 'a user Failure method still wins';

dies-ok { X::AdHoc.new(payload => "z").Failure.sink }, 'the Failure throws when sunk';
