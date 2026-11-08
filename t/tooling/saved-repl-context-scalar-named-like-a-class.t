use Test;
use nqp;

plan 5;

# A saved REPL context keeps `my $Out`; a caller-scope `class Out` of the same
# spelling must not hide it from a later unit (#17087). Identical under rakudo.
class Out { }
class Zed { }

class Sandbox {
    has $.save_ctx;
    has $.repl;
    method TWEAK() {
        my $compiler := nqp::getcomp('Raku') || nqp::getcomp('perl6');
        $!repl = REPL.new($compiler, {});
        self.run('my $Out = []; my $Zed = 41;');
    }
    method run(Str $code) {
        my $*CTXSAVE = $!repl;
        my $*MAIN_CTX;
        my $exception;
        my $output = $!repl.repl-eval($code, $exception, :outer_ctx($!save_ctx), :interactive(1));
        if $*MAIN_CTX { $!save_ctx := $*MAIN_CTX }
        with $exception { return "EXC: " ~ $_.message }
        $output.gist
    }
}

my $sb = Sandbox.new;
is $sb.run('$Out.elems'), '0', 'a scalar named like a caller class is visible to a later unit';
is $sb.run('$Out[1] := 123; $Out[1]'), '123', 'and it can be written';
is $sb.run('$Out.elems'), '2', 'the write persists in the saved context';
is $sb.run('$Zed + 1'), '42', 'a second scalar named like a class';
like $sb.run('$Nope'), /'Variable \'$Nope\' is not declared'/, 'a variable never declared is still undeclared';
