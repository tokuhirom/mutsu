use Test;

# rakudo's CHECK reports every undeclared routine at once ("Undeclared
# routines:", sorted by name, each at its last use), and the compile error
# prints the list as is: each name keeps its "used at line N", with no source
# snippet.

plan 4;

sub compile-error(Str $code) {
    my $file = $*TMPDIR.add("undeclared-routines-{$*PID}-{(^1_000_000).pick}.raku");
    $file.spurt($code);
    my $p = run $*EXECUTABLE, $file, :err;
    my $err = $p.err.slurp(:close);
    $file.unlink;
    $err.lines.skip(1).join("\n")
}

is compile-error("say 1;\nnosuch();\nfoo-bar 3;\n"),
    "Undeclared routines:\n    foo-bar used at line 3\n    nosuch used at line 2. Did you mean 'cosech'?\n",
    'every undeclared routine, sorted by name';
is compile-error("nosuch();\nsay 1;\nnosuch();\nnosuch();\n"),
    "Undeclared routine:\n    nosuch used at line 4. Did you mean 'cosech'?\n",
    'one name, at its last use';
is compile-error("say 1;\nnosuch();\n"),
    "Undeclared routine:\n    nosuch used at line 2. Did you mean 'cosech'?\n",
    'a single name keeps its line';

try EVAL "nosuch();\nzz();";
like $!.message, /'Undeclared routines:'/, 'EVAL lists them too';
