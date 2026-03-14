use Test;

plan 3;

{
    die "oops";
    CATCH { default { .resume } }
}
pass 'bare .resume resumes after die in enclosing block';

my $resumed = 0;
{
    die "oops";
    $resumed = 1;
    CATCH { default { .resume } }
}
is $resumed, 1, '.resume continues from the statement after die';

my $caught = 0;
try {
    die "oops";
    CATCH {
        default {
            my $ex = $_;
            $caught = $ex.^name eq "Exception";
            $ex.resume;
        }
    }
}
ok $caught, 'exception object can be resumed explicitly via $_';
