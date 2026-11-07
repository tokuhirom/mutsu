use Test;

plan 6;

# A `return` in a routine's CATCH handler is not offered to that routine's own
# CONTROL block (rakudo parity), while a `return` in the body still is.
# Text::CodeProcessing's REPL sandbox has this shape: its `repl-eval` CATCH
# returns with the exception stored, and its CONTROL would otherwise replace
# the result with a sentinel.
sub in-catch { CATCH { default { return 7 } }; CONTROL { return 9 }; die "x" }
is in-catch(), 7, 'return in CATCH keeps its value';

sub in-body { CONTROL { return 9 }; return 5 }
is in-body(), 9, 'return in the body still reaches CONTROL';

sub bare-return { CATCH { default { return } }; CONTROL { return 9 }; die "x" }
is-deeply bare-return(), Nil, 'a bare return in CATCH is Nil';

class Repl {
    has $!sentinel = {};
    method eval($code, \exception, *%adverbs) {
        CATCH { default { exception = $_; return; } }
        CONTROL { return $!sentinel }
        die "failed: $code";
    }
}
my $exception;
my $result = Repl.new.eval('x', $exception, :interactive(1));
ok !$result.defined, 'the method result is undefined, not the CONTROL sentinel';
isa-ok $exception, X::AdHoc, 'the exception was stored';

sub warned { CONTROL { when CX::Warn { .resume } }; warn "w"; 3 }
is warned(), 3, 'a CONTROL block still handles a warning';

done-testing;
