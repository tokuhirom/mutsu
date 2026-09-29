use Test;

plan 6;

sub make-failure { fail 'not done' }

my $failure = make-failure();
is $failure.exception.gist.lines.elems, 1,
    'the exception wrapped by an unthrown Failure has no backtrace lines';
nok $failure.exception.backtrace.defined,
    'the Failure exception has no Backtrace before it is thrown';
nok X::AdHoc.new(payload => 'x').backtrace.defined,
    'a newly constructed exception has no Backtrace';

try { $failure.exception.throw }
ok $!.backtrace.defined,
    'throwing the Failure exception attaches a Backtrace';

my $pending = make-failure();
try { sink $pending }
ok $!.backtrace.Str.contains('in sub make-failure'),
    'sinking a Failure attaches its original fail-site frames';
is $!.gist.lines.elems, 3,
    'the sunk Failure gist includes its message and origin frames';
