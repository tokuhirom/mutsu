use Test;

# Event::Emitter::Channel: `submethod BUILD { $!channel := Channel.new; ...;
# END { $!channel.close; await $!promise } }`. An END declared in a method
# closes over its invocant; it runs at exit, when no method frame is left, and
# mutsu died with "Variable $!channel used where no 'self' is available".

plan 4;

sub run-code(Str $code) {
    my $p = run $*EXECUTABLE, '-e', $code, :out, :err;
    ($p.out.slurp(:close), $p.err.slurp(:close), $p.exitcode)
}

my ($out, $err, $rc) = run-code(q:to/CODE/);
    class C {
        has $!x;
        submethod BUILD { $!x := 42; END { say "END sees $!x" } }
    }
    C.new;
    say "main done";
    CODE
is $out, "main done\nEND sees 42\n", 'an END in a submethod reads a private attribute of its invocant';
is $rc, 0, 'and exits cleanly';

($out, $err, $rc) = run-code(q:to/CODE/);
    class C {
        has $.x = 5;
        method m { END { say "self: ", self.WHAT.raku; say "attr: ", $!x } }
    }
    C.new.m;
    class D { has $!y = 7; method n { my $v = 3; END { say "v=$v"; say "y=$!y" } } }
    D.new.n;
    CODE
is $out, "v=3\ny=7\nself: C\nattr: 5\n", 'self and attributes in an END of a method, in reverse install order';

($out, $err, $rc) = run-code(q:to/CODE/);
    END { say "plain END" }
    class E { method k { END { say "no self needed" } } }
    CODE
is $out, "no self needed\nplain END\n", 'an END of a method that never ran, and a plain END, are unchanged';
