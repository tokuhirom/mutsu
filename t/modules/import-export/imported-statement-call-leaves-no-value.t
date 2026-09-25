use v6;
use Test;

# A bare statement call to an imported routine (`f 1;`) used to compile to a
# dedicated `ExecCall` opcode instead of `CallFunc` + `SinkPop`. Two of that
# opcode's branches -- a wrapped routine and a NativeCall sub -- pushed the
# call's value although the opcode was documented and emitted as "no push".
# The stray value then became the enclosing block's value, so
# `.map({ f 1; $_ * 2 })` answered `(42 42 42)` (#9448).
#
# Whether `f 1;` took that path depended on the parser knowing `f` was
# imported, which it did only when the module was compiled from source in the
# same process -- so the bug showed on a cold precompilation cache and not on
# a warm one. Each snippet below runs in a fresh subprocess against its own
# module directory, so the first run is always the cold one.

plan 5;

my $dir = $*TMPDIR.child("mutsu-stmt-call-{$*PID}");
$dir.mkdir;
END { try { run('rm', '-rf', $dir.absolute) } }

$dir.child('StmtCallM.rakumod').spurt(q:to/END/);
unit module StmtCallM;
sub f($x) is export { 42 }
END

sub run-snippet($name, $source) {
    my $file = $dir.child($name);
    $file.spurt($source);
    my $proc = run($*EXECUTABLE, '-I', $dir.absolute, $file.absolute, :out, :err);
    my $out = $proc.out.slurp(:close);
    $proc.err.slurp(:close);
    $out.trim
}

my $wrapped = q:to/END/;
use StmtCallM;
&f.wrap(-> |c { callsame });
say (1, 2, 3).map({ f 1; $_ * 2 });
sub g { f 1; f 2; 7 }
say g();
say (1, 2).map({ f 1; f 2; $_ });
END

is run-snippet('cold.raku', $wrapped), "(2 4 6)\n7\n(1 2)",
    'a wrapped imported routine called as a statement leaves no value behind (cold cache)';
is run-snippet('warm.raku', $wrapped), "(2 4 6)\n7\n(1 2)",
    'the same program gives the same answer once the module is precompiled';

is run-snippet('plain.raku', q:to/END/), "(2 4 6)",
use StmtCallM;
say (1, 2, 3).map({ f 1; $_ * 2 });
END
    'an unwrapped imported statement call leaves no value behind either';

# The statement call still sinks its value: an unhandled Failure it returns
# throws, as it does for a sunk call in value position.
$dir.child('StmtCallFail.rakumod').spurt(q:to/END/);
unit module StmtCallFail;
sub bad($x) is export { fail "sunk failure $x" }
END
is run-snippet('sink.raku', q:to/END/), "caught: sunk failure 1",
use StmtCallFail;
try { bad 1; say "not reached"; CATCH { default { say "caught: ", .message } } }
END
    'a sunk statement call still throws the Failure it returned';

# And the statement form still returns nothing into a surrounding list.
is run-snippet('list.raku', q:to/END/), "[5 6]",
use StmtCallM;
my @a = do { f 1; 5 }, 6;
say @a;
END
    'a statement call inside a do block does not add an element';
