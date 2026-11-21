use Test;

# `fail` returns a Failure from its lexical routine: it is a return, not an
# exception, so a CATCH on the way does not handle it. Only `try` (which is
# fatal), `use fatal`, and a `fail` outside any routine throw. Red's
# `automatic-transaction` (a CATCH, KEEP and UNDO around the user's block) is
# where a `fail` in a `red-do` block was mistaken for an error.

plan 17;

class E is Exception { method message { 'my error' } }

my @log;
sub f { CATCH { default { @log.push: 'f' } }; fail 'x' }
my $r = f();
isa-ok $r, Failure, 'fail beside a CATCH returns a Failure';
is-deeply @log, [], 'the CATCH does not run';

sub g { CATCH { default { @log.push: 'g' } }; h() }
sub h { fail 'y' }
isa-ok g(), Failure, "a callee's Failure passes the caller's CATCH as a value";

sub k { CATCH { default { @log.push: 'k' } }; { fail 'z' }; 1 }
isa-ok k(), Failure, 'fail in a bare block returns from the routine';

sub wrap(&b) {
    CATCH { default { @log.push: 'wrap'; .rethrow } }
    UNDO @log.push: 'undo';
    b()
}
sub attempt { wrap({ fail E.new }) }
my $w = attempt();
isa-ok $w, Failure, "fail in a closure returns from the closure's routine";
isa-ok $w.exception, E, 'the Failure keeps its exception';
is-deeply @log, ['undo'], "only the intermediate routine's UNDO runs";

sub t { my $x = try { fail 'x' }; $!.^name }
is t(), 'X::AdHoc', 'try is fatal: it catches the fail';

{
    use fatal;
    my $seen = '';
    sub ff { CATCH { default { $seen = 'caught' } }; fail 'x' }
    ff();
    is $seen, 'caught', 'under use fatal the CATCH handles fail';
}

my $top = '';
{ CATCH { default { $top = .message } }; fail 'toplevel' }
is $top, 'toplevel', 'fail outside a routine throws to the CATCH';

sub l { CATCH { default { } }; fail 'x'; LEAVE @log.push: 'leave' }
my $l = l();
is @log.tail, 'leave', 'LEAVE still runs';

# The routine-call spellings raise through builtins, and decide from the
# calling code the same way (#17913).
my $seen-call = '';
sub fc { CATCH { default { $seen-call = 'caught' } }; my $x = fail 'x'; 1 }
is fc().^name, 'Failure', 'fail in expression form returns the Failure';
is $seen-call, '', 'and the routine CATCH does not see it';

sub fm { CATCH { default { } }; X::AdHoc.new(:payload<p>).fail; 2 }
is fm().^name, 'Failure', '.fail returns the Failure past the routine CATCH';

sub fs { CATCH { default { } }; ...; 3 }
is fs().^name, 'Failure', '... returns the Failure past the routine CATCH';

my $where = '';
subset PosFail of Int where { $_ > 0 or fail 'neg' };
sub fw { CATCH { default { $where = .message } }; my PosFail $s = -1; 5 }
fw();
is $where, 'neg', 'a fail in a where block declared outside the routine is caught there';

sub fb { my $blk = { fail 'inner' }; CATCH { default { } }; $blk(); 7 }
is fb().^name, 'Failure', 'a block inside the routine fails from the routine';
