use v6.e.PREVIEW;
use lib 't/lib';
use Test;
use nqp;

# A REPL session keeps a line's slang the way rakudo's REPL and
# Jupyter::Kernel do: the line hands its `$?LANG` on, the session sets
# `$compiler.parsegrammar($lang.slang_grammar('MAIN'))`, and the next
# `$compiler.eval` is parsed with that grammar (#17087). Setting the grammar
# imports nothing.

plan 6;

class Handler {
    has Mu $.lang = $?LANG;
    method set-lang(Mu $lang) { $!lang := $lang }
}

my $compiler := nqp::getcomp('Raku');
my $repl = REPL.new($compiler, {});
my $handler = Handler.new;
my $ctx;

sub run(Str $code) {
    my $*CTXSAVE = $repl;
    my $*MAIN_CTX;
    my $*HANDLER = $handler;
    my $exception;
    my $output = $repl.repl-eval($code, $exception, :outer_ctx($ctx), :interactive(1));
    $ctx := $*MAIN_CTX if $*MAIN_CTX;
    $compiler.parsegrammar($handler.lang.slang_grammar('MAIN'));
    with $exception { return 'EXC: ' ~ .message }
    $output.gist
}

like run('answer'), /^ 'EXC: '/, 'before the slang, its term is not there';
is run('use TermSlang; $*HANDLER.set-lang($?LANG); answer'), '42', 'the line that uses the slang';
is run('answer + 1'), '43', 'a later line is parsed with the slang';
is run('roman:XIV'), '14', 'all of its productions';
is run('my $x = $( use TermSlang; answer ); $*HANDLER.set-lang($?LANG); $x'), '42',
    'a $( ) group does not end the slang';
is run('answer'), '42', 'and the line after that one still has it';
