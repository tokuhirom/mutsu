use Test;

# rakudo compiles an EVAL unit under its name joined onto `$*CWD`, while the
# unit's own `$?FILE` stays the bare `EVAL_<N>`. A compile error's `.filename`
# is that absolute name, and an uncaught one renders under it, with a snippet
# of the EVAL'd source rather than of the calling program.

plan 6;

try EVAL '1 +';
isa-ok $!, X::Comp, 'a syntax error in EVAL is X::Comp';
like $!.filename, /^ $($*CWD) '/EVAL_' \d+ $/, '.filename is the EVAL unit under $*CWD';
like EVAL(q[$?FILE]), /^ 'EVAL_' \d+ $/, '$?FILE stays the bare unit name';

my $p = run $*EXECUTABLE, '-e', 'my $x = 42; EVAL "1 +"', :err;
my $err = $p.err.slurp(:close);
like $err, /^ '===SORRY!=== Error while compiling ' \N* 'EVAL_0' \n/, 'rendered under the EVAL unit';
like $err, /'EVAL_0:1' \n/, 'at its own line';
unlike $err, /'my $x = 42'/, 'with no snippet of the calling program';
