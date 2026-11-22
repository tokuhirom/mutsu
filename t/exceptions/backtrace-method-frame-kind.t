use Test;

# A backtrace names each routine frame by its kind, as rakudo does: `in method
# m`, `in submethod s`, `in sub f`. A parameter binding failure is reported
# inside the method it was raised for, at the method's declaration line.

plan 9;

sub stderr-of(Str $code) {
    my $file = $*TMPDIR.add("bt-kind-{$*PID}-{(^1_000_000).pick}.raku");
    $file.spurt($code);
    my $p = run $*EXECUTABLE, $file, :err;
    my $err = $p.err.slurp(:close);
    $file.unlink;
    $err
}

like stderr-of("class B \{\n  method m() \{ die 'boom' }\n}\nB.m;\n"),
    /^^ '  in method m at ' \N* ' line 2' $$/, 'a die in a method';
like stderr-of("class B \{\n  submethod s() \{ die 'boom' }\n}\nB.s;\n"),
    /^^ '  in submethod s at ' \N* ' line 2' $$/, 'a die in a submethod';
like stderr-of("sub f \{\n  die 'boom'\n}\nf;\n"),
    /^^ '  in sub f at ' \N* ' line 2' $$/, 'a die in a sub';
like stderr-of("class B \{\n  method m() \{ my \@a; \@a.nosuch }\n}\nB.m;\n"),
    /^^ '  in method m at ' \N* ' line 2' $$/, 'a run-time error in a method';

my $bind = stderr-of("class B \{\n  method m(Int \$x) \{ }\n}\nB.m('a');\n");
like $bind, /'Type check failed in binding to parameter'/, 'a binding failure';
like $bind, /^^ '  in method m at ' \N* ' line 2' $$/, '...is reported inside the method';
like $bind, /^^ '  in block <unit> at ' \N* ' line 4' $$/, '...called from the mainline';

class C { method m() { die 'x' } }
try C.m;
like $!.backtrace.nice, /^ '  in method m at '/, 'Backtrace.nice names the method';
try die 'y';
is $!.backtrace[0].Str.words.head(3).join(' '), 'in method throw', 'the setting throw frame is a method';
