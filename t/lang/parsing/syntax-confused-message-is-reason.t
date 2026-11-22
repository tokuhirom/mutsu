use Test;

# rakudo's `X::Syntax::Confused.message` is its `.reason` ("Two terms in a
# row"), and the compile error prints that, not "Confused. Two terms in a row".

plan 5;

try EVAL q[say "a" rescue "x"];
isa-ok $!, X::Syntax::Confused, 'two terms in a row is X::Syntax::Confused';
is $!.message, 'Two terms in a row', '.message is the reason';
is $!.reason, 'Two terms in a row', '.reason';

my $p = run $*EXECUTABLE, '-e', 'say "a" rescue "x"', :err;
my $err = $p.err.slurp(:close);
like $err, /^^ 'Two terms in a row' $$/, 'the compile error prints the reason';
unlike $err, /'Confused. Two terms'/, 'without the "Confused." prefix';
