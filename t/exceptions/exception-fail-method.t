use Test;

plan 6;

# `$exception.fail` is `fail $exception`: it returns a Failure carrying the
# exception (which throws only when the Failure is sunk/used unhandled), rather
# than throwing immediately like `.throw`. mutsu previously had no `.fail`
# method on Exception instances ("No such method 'fail'"). This is used by
# DBIish: `X::DBIish::DriverNotFound.new(...).fail`.

class MyErr is Exception {
    has $.msg;
    method message { $.msg }
}

# .fail inside a sub, caught by CATCH.
{
    my $caught;
    sub boom() { MyErr.new(:msg('bang')).fail }
    {
        boom();
        CATCH { default { $caught = .message } }
    }
    is $caught, 'bang', '.fail throws the exception when sunk';
}

# .fail returns a Failure (not an immediate throw).
{
    sub mk() { return MyErr.new(:msg('x')).fail }
    my $f = mk();
    nok $f.defined, '.fail returns an undefined Failure';
    ok $f.WHAT === Failure, '.fail returns a Failure';
}

# A user X::-namespaced exception works too.
{
    my $caught;
    {
        X::AdHoc.new(:payload('adhoc!')).fail;
        CATCH { default { $caught = .message } }
    }
    is $caught, 'adhoc!', '.fail on an X:: exception';
}

# .fail on a type object is still a concreteness error, not "no such method".
throws-like 'MyErr.fail', Exception, '.fail on a type object dies (concreteness)';

# .throw still throws immediately.
{
    my $caught;
    {
        MyErr.new(:msg('thrown')).throw;
        CATCH { default { $caught = .message } }
    }
    is $caught, 'thrown', '.throw still works';
}
