use experimental :rakuast;
use Test;

# A bare `die` / `fail` throws `$!` again. The parser supplies the `$!`, and
# RakuAST keeps the call without arguments (#14880, the `.DEPARSE` corpus's
# S17-scheduler/at.t).

plan 9;

sub deparse($src) { $src.AST.statements[*-1].DEPARSE.subst("\n", ' ', :g).subst(/\s+/, ' ', :g).trim }

is deparse(Q{die}), 'die', 'a bare die';
is deparse(Q{fail}), 'fail', 'a bare fail';
is deparse(Q{sub f { die }}), 'sub f { die }', 'in a routine body';
is deparse(Q{die if 1}), 'die if 1', 'under a modifier';
is deparse(Q{die $!}), 'die $!', 'an explicit $! stays';
is Q{die}.AST.statements[0].expression.args.args.elems, 0, 'the call has no arguments';

try { die 'first' };
my $again = try { die };
is $!.message, 'first', 'a bare die throws $! again';
sub g { fail }
is g().exception.message, 'Failed', 'a bare fail without $! fails with Failed';
try { die 'second' };
try EVAL Q{die}.AST;
is $!.message, 'second', 'EVAL of the .AST throws $! again too';
