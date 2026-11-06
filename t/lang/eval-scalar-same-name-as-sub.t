use Test;
use nqp;

# From Text::CodeProcessing (REPLSandbox): the first REPL unit declares
# `my $Out = []; sub Out { $Out }`; the next unit reads `$Out`. A sub of the
# same name does not hide the variable.
plan 6;

sub session(*@codes) {
    my $repl = REPL.new(nqp::getcomp("Raku"), {});
    my $ctx;
    my @out;
    for @codes -> $c {
        my $*CTXSAVE = $repl;
        my $*MAIN_CTX;
        my $exception;
        my $o = $repl.repl-eval($c, $exception, :outer_ctx($ctx), :interactive(1));
        @out.push: $exception.defined ?? 'died' !! $o.raku;
        $ctx := $*MAIN_CTX if $*MAIN_CTX;
    }
    @out
}

is session('my $Out = []; sub Out { $Out };', '$Out.push(1); $Out.elems')[1], '1', 'sub declared last';
is session('my $Out = []; sub Out { 5 }; 1', '$Out.elems')[1], '0', 'sub not using the variable';
is session('sub Out { 5 }; my $Out = [];', '$Out.elems')[1], '0', 'sub declared first';
is session('my $Out = [];', '$Out.elems')[1], '0', 'no sub (control)';

my $v = 3;
sub v { 4 }
is EVAL('$v'), 3, 'EVAL reads the scalar of the same name as a sub';
my $Q = 9;
sub Q { 1 }
is EVAL('$Q + Q()'), 10, 'and both';
