use Test;

# A `Failure` that explodes when sunk throws its exception the way
# `Failure.sink` does in Rakudo (`.throw`), so a `CATCH` that `.resume`s
# continues after the sinking statement instead of leaving the routine.
# From REPL's `rakudo-history`: `$path.spurt` into a missing directory, in a
# block whose CATCH notes the problem and resumes.

plan 5;

sub f() { fail "boom" }

sub stmt() {
    CATCH { default { .resume } }
    f();
    "after the sunk call"
}
is stmt(), 'after the sunk call', 'resuming a sunk Failure continues with the next statement';

sub trailing() {
    my $x = 'before';
    if True {
        CATCH { default { .resume } }
        f();
    }
    $x ~ ' and after'
}
is trailing(), 'before and after', 'resuming a Failure that ends a CATCH-protected block continues past it';

class P { method h($x) { "P.h($x)" } }
class R {
    has $.p = P.new;
    method path() {
        my $path = "no-such-dir-$*PID/sub/file".IO;
        if !$path.e {
            CATCH { default { .resume } }
            $path.spurt;
        }
        $path.basename
    }
    method go() { $!p.h(self.path) }
}
is R.new.go, 'P.h(file)', 'an outer call keeps its invocant across the resumed callee';

my @seen;
sub noted() {
    CATCH { default { @seen.push: .message; .resume } }
    f();
    'done'
}
is noted(), 'done', 'the handler runs ...';
is-deeply @seen, ['boom'], '... once, with the Failure\'s exception';
