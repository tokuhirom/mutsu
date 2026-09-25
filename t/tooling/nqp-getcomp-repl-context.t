use Test;
use nqp;

# nqp::getcomp("Raku") and the `$*CTXSAVE` / `:outer_ctx` protocol that
# rakudo's REPL and the ecosystem sandboxes built on it (CodeUnit,
# Jupyter::Kernel, Text::CodeProcessing) use to keep one session's
# declarations alive from one `.eval` to the next (ADR-0122, #9349).

plan 19;

my $compiler = nqp::getcomp("Raku");
is $compiler.^name, 'Perl6::Compiler', 'nqp::getcomp("Raku") is the compiler object';
ok nqp::isnull(nqp::getcomp('NoSuchLang')), 'an unknown language has no compiler';
my $host-lexical = 1;
dies-ok { $compiler.eval('$host-lexical') }, '.eval without a context sees the setting only';
like $compiler.version_string, /Raku/, '.version_string names the language';

class Saver {
    method ctxsave(--> Nil) {
        $*MAIN_CTX := nqp::ctxcaller(nqp::ctx());
        $*CTXSAVE := 0;
    }
}

my $ctx := Mu;
sub ev($code) {
    my $*CTXSAVE := Saver.new;
    my $*MAIN_CTX := $ctx;
    my $value := $compiler.eval($code, :outer_ctx($ctx), :interactive(1));
    $ctx := $*MAIN_CTX;
    $value
}

is ev('my $a = 42'), 42, 'a declaration evaluates to its value';
is $ctx.^name, 'BOOTContext', 'ctxsave handed the unit context back';
is ev('$a'), 42, 'the lexical survives into the next unit';
is ev('$a = $a + 1'), 43, 'and can be assigned there';
is ev('$a'), 43, 'the assignment is kept for the unit after';
is ev('sub double($x) { $x * 2 }; double(5)'), 10, 'a sub is callable in its own unit';
is ev('double($a)'), 86, 'and in the next one';
is ev('sub infix:<foo>($, $) { "foo" }').name, 'infix:<foo>', 'an operator declaration';
# rakudo 2026.07 itself still fails this one ("Two terms in a row"); it is
# what CodeUnit's own test expects of a fixed rakudo.
is ev('42 foo 666'), 'foo', 'the operator parses in the next unit';
is ev('class Pt { has $.x }; Pt.new(x => 3).x'), 3, 'a class in its own unit';
is ev('Pt.new(x => $a).x'), 43, 'the class and the lexical in the next one';
ok nqp::ctxlexpad($ctx)<$a>:exists, 'nqp::ctxlexpad lists the context lexicals';

# Neither the unit's lexicals nor its lexical subs leak into the host.
throws-like { EVAL 'double(1)' }, Exception, 'the REPL sub stays in the session';

# rakudo's core REPL class drives the same protocol.
my $repl = REPL.new($compiler, {}, True);
my $*CTXSAVE = $repl;
my $*MAIN_CTX;
my $exception;
my $value = $repl.repl-eval('my $r = 7; $r * 6', $exception);
is $value, 42, 'REPL.repl-eval evaluates';
$repl.repl-eval('die "boom"', $exception);
is $exception.message, 'boom', 'REPL.repl-eval hands the exception back';
