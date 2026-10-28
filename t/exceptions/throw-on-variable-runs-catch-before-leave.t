use Test;

plan 9;

# `$e.throw` on a named receiver is a throw site like `X.new.throw` and `die`:
# the CATCH of the routine runs first, at the throw, and the routine's LEAVE
# after it. A variable receiver took the unwinding path instead, so the LEAVE
# ran before the CATCH. Red's `run-transaction` (`CATCH { rollback-and-rethrow }`
# plus `LEAVE $guard.release`) then released -- and rolled back -- the guard
# before its handler looked, so a failing rollback was never reported as
# X::Red::Transaction::Cleanup.

my @log;
my $e = X::AdHoc.new(payload => "p");

sub body-var  { CATCH { default { @log.push: "catch" } }; LEAVE @log.push: "leave"; $e.throw }
sub body-local { CATCH { default { @log.push: "catch" } }; LEAVE @log.push: "leave"; my $l = X::AdHoc.new(payload => "q"); $l.throw }
sub body-fresh { CATCH { default { @log.push: "catch" } }; LEAVE @log.push: "leave"; X::AdHoc.new(payload => "r").throw }
sub via(&b)   { CATCH { default { @log.push: "catch" } }; LEAVE @log.push: "leave"; b() }

body-var();
is @log.join(","), "catch,leave", 'a variable receiver in the routine body';
@log = ();
body-local();
is @log.join(","), "catch,leave", 'a local variable receiver';
@log = ();
body-fresh();
is @log.join(","), "catch,leave", 'a fresh exception (already so)';
@log = ();
via({ $e.throw });
is @log.join(","), "catch,leave", 'a variable receiver thrown from a closure';
@log = ();
via({ ($e).throw });
is @log.join(","), "catch,leave", 'a parenthesised receiver';
@log = ();
via({ $e.rethrow });
is @log.join(","), "catch,leave", '.rethrow is the same throw site';

# A LEAVE that fails after the exception is in flight does not hide it from the CATCH.
class Guard {
    has $.state is rw = 'open';
    method rollback { return self unless $!state eq 'open'; $!state = 'failed'; die "rollback-error" }
    method release { self.rollback; self }
}
sub rar($g, $cause) {
    my $cleanup;
    try { $g.rollback; CATCH { default { $cleanup = $_ } } }
    X::AdHoc.new(payload => "Cleanup(" ~ $cause.message ~ "+" ~ $cleanup.message ~ ")").throw if $cleanup.defined;
    $cause.rethrow
}
sub run-transaction(Guard:D $guard, @blocks) {
    CATCH { default { rar($guard, $_) } }
    LEAVE $guard.release;
    return @blocks[0](:$guard);
}
my $primary = X::AdHoc.new(payload => "body");
my $g = Guard.new;
try run-transaction($g, [-> :$guard! { $primary.throw }]);
is $!.message, "Cleanup(body+rollback-error)", 'the handler sees the open guard and reports both errors';
is $g.state, 'failed', 'the guard was rolled back exactly once';
my $g2 = Guard.new;
try run-transaction($g2, [-> :$guard! { die "plain" }]);
is $!.message, "Cleanup(plain+rollback-error)", 'the same with die';
