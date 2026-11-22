use Test;

# #18604: a handled Failure is an ordinary value, so `try` answers it as the
# block's value with `$!` Nil; only a live (unhandled) Failure is caught.

plan 9;

my $g = Failure.new("g");
$g.so;    # handled

my $a = try { $g };
isa-ok $a, Failure, 'my $x = try { $handled } keeps the Failure';
ok $a === $g, '... the same Failure';
ok $! === Nil, '... and $! is Nil';

my $e = try $g;
isa-ok $e, Failure, 'the statement form keeps it too';
ok $! === Nil, '... with $! Nil';

is (try { $g }).^name, 'Failure', 'try as an expression answers it';

# A live Failure is still caught.
my $l = try { my $f = Failure.new("live") };
nok $l.defined, 'an unhandled Failure is caught';
isa-ok $!, X::AdHoc, '... into $!';

my $u = try %*UNDECLARED;
isa-ok $!, X::Dynamic::NotFound, 'a missing dynamic variable is caught';
