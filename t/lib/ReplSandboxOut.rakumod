# Fixture for t/tooling/repl-compiler-unit-scope.t (#17087): a module that
# declares a class and runs a REPL session from its own methods, as
# Jupyter::Kernel::Sandbox does with its `class Out`.
use nqp;

class Out { }

class ReplSandbox is export {
    has $.save_ctx;
    has $.repl;
    method TWEAK() {
        my $compiler := nqp::getcomp('Raku');
        $!repl = REPL.new($compiler, {});
        self.run('my $Out = [1, 2]; sub Out { $Out }; 1');
    }
    method run(Str $code) {
        my $*CTXSAVE = $!repl;
        my $*MAIN_CTX;
        my $exception;
        my $output = $!repl.repl-eval($code, $exception, :outer_ctx($!save_ctx), :interactive(1));
        $!save_ctx := $*MAIN_CTX if $*MAIN_CTX;
        with $exception { return 'EXC: ' ~ .message }
        $output.gist
    }
}
