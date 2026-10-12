use Test;

# When `await` rethrows a broken Promise, the exception's gist must
# contain BOTH where the Promise's code died (the throwing location) and where
# it was awaited (the re-throw / await-site location) — matching Raku's
# X::Await::Died.

plan 4;

sub death-bar { die "golly!" }
sub make-it { start { death-bar() } }

sub waiting-for-it {
    await make-it();
    CATCH {
        default {
            like .gist, /golly/,           'gist contains the original message';
            like .gist, /death\-bar/,      'gist contains the throwing location';
            like .gist, /waiting\-for\-it/, 'gist contains the await-site location';
        }
    }
}
waiting-for-it;

# The await site lives in the X::Await::Died role's gist header; the
# exception's own backtrace stays where its code died (as in rakudo).
{
    sub inner { die "boom" }
    sub spawner { start { inner() } }
    sub awaiter {
        await spawner();
        CATCH { default { unlike .backtrace.Str, /awaiter/, 'backtrace keeps the throw location only' } }
    }
    awaiter;
}
