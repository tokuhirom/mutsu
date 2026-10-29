use Test;
use nqp;

plan 7;

# `nqp::throw` of a CONTROL_WARN exception whose payload is a user CX::Warn
# object (Red's X::Red::Transaction::AsyncResult) is delivered to
# `CONTROL { when TheClass {...} }` as that object, and `.resume` continues.
class MyWarn is CX::Warn {
    method message { 'my pending warn' }
    method warn {
        my $exception := nqp::newexception();
        nqp::setmessage($exception, self.message);
        nqp::setextype($exception, nqp::const::CONTROL_WARN);
        nqp::setpayload($exception, self);
        nqp::throw($exception);
    }
}
sub doit { MyWarn.new.warn; "done" }

my Int $warnings = 0;
my $result;
{
    CONTROL {
        when MyWarn {
            $warnings++;
            is .message, 'my pending warn', 'the handler gets the object itself';
            .resume;
        }
    }
    $result = doit();
}
is $result, 'done', 'a resumed warning continues after the throw';
is $warnings, 1, 'the handler ran once';

my $generic = 0;
{
    CONTROL { when CX::Warn { $generic++; .resume } }
    doit();
}
is $generic, 1, 'a CX::Warn handler also sees the subclass';

# No handler (or none matching): the default handler prints and resumes.
my $plain = '';
{
    CONTROL { when X::AdHoc { $plain = 'wrong' } }
    $plain = doit();
}
is $plain, 'done', 'a non-matching CONTROL handler declines; the warning resumes';
is doit(), 'done', 'with no handler at all the warning is printed and the call continues';
{
    CONTROL { when MyWarn { .resume } }
    my @r = (doit(), doit());
    is-deeply @r, ['done', 'done'], 'repeated resumes';
}
