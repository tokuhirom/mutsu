use Test;

plan 9;

# `.die` on an Exception (or a Failure) throws it, exactly like `.throw`.
# mutsu had no `.die` method at all ("No such method 'die'"), and `die
# $failure` printed the Failure's type repr `Failure()` instead of the
# wrapped exception's message (#9777).

# .die on a plain X:: exception instance.
{
    my $caught;
    {
        X::AdHoc.new(payload => "x").die;
        CATCH { default { $caught = .message } }
    }
    is $caught, 'x', 'X::AdHoc.new(...).die throws with the payload as message';
}

# .die on a HANDLED Failure still throws the wrapped exception (unlike
# `.fail`'s "only when sunk" semantics, `.die` always throws).
{
    my $caught;
    {
        my $f = (sub { fail "boom" })();
        $f.defined;
        $f.die;
        CATCH { default { $caught = .message } }
    }
    is $caught, 'boom', 'a handled Failure.die throws the wrapped exception';
}

# .die on an UNHANDLED Failure (the generic "unhandled Failure explosion"
# already covers this, but must still give the right message).
{
    my $caught;
    {
        my $f = (sub { fail "boom-unhandled" })();
        $f.die;
        CATCH { default { $caught = .message } }
    }
    is $caught, 'boom-unhandled', 'an unhandled Failure.die throws the wrapped exception';
}

# `die $failure` (the sub form) re-throws the wrapped exception's message,
# not the Failure's type repr.
{
    my $caught;
    {
        my $g = (sub { fail "boom2" })();
        die $g;
        CATCH { default { $caught = .message } }
    }
    is $caught, 'boom2', 'die $failure throws with the wrapped exception message';
}

# `orelse .die` idiom (Language/perl-nutshell.rakudoc).
{
    my $caught;
    {
        (sub { fail "no such file" })() orelse .die;
        CATCH { default { $caught = .message } }
    }
    is $caught, 'no such file', 'orelse .die throws the wrapped exception';
}

# .die on a type object is a concreteness error, not "no such method".
throws-like 'X::AdHoc.die', Exception, '.die on a type object dies (concreteness)';

# .die still works on a user-defined Exception subclass.
{
    my $caught;
    class MyErr is Exception {
        has $.msg;
        method message { $.msg }
    }
    {
        MyErr.new(:msg('user-die')).die;
        CATCH { default { $caught = .message } }
    }
    is $caught, 'user-die', '.die on a user-defined Exception subclass';
}

# A Failure's `.gist` shows a `(HANDLED)` prefix once it has been marked
# handled, both directly and via the raw fallback used inside a container
# (e.g. an Array), which used to render the type repr `Failure()`.
{
    my $f = (sub { fail "gisted" })();
    $f.defined;
    is $f.gist, '(HANDLED) gisted', 'a handled Failure gists with a HANDLED prefix';
    is [$f].gist, '[(HANDLED) gisted]', 'a handled Failure inside an Array gists correctly';
}
