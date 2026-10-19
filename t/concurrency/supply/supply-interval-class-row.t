use Test;

# `Supply.interval` is a class-method row, a `Systemic` class's type-object
# rows (`Compiler.id`) are reached by owner while its attributes need an
# instance, and `X::AdHoc.payload` is its row (ADR-11276 §9.196; checked
# against Rakudo).

plan 7;

my @ticks;
my $tap = Supply.interval(0.05).tap({ @ticks.push($_) });
sleep 0.4;
$tap.close;
is @ticks[^3], (0, 1, 2), 'Supply.interval ticks 0, 1, 2, ...';
throws-like { Supply.new.interval(1) }, Exception, 'an instance is not its invocant';

ok Compiler.id.chars > 0, 'Compiler.id answers the type object';
ok $*RAKU.compiler.id.chars > 0, 'and an instance';
throws-like { Compiler.name }, Exception,
    message => /'Cannot look up attributes in a Compiler type object'/,
    'an attribute needs an instance';

try { die "boom" };
is $!.payload, 'boom', 'X::AdHoc.payload of die "..."';
try { die 42 };
is $!.payload.^name, 'Int', 'X::AdHoc.payload keeps the value';
