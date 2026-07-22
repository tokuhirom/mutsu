use v6;
use Test;

plan 5;

# Proc has no stringifier of its own in Rakudo, so its gist is the default
# instance repr — NOT the exitcode number the old native Str/gist arm
# rendered (`Proc.new.gist` was `-1`).

like Proc.new.gist, /^ 'Proc.new'/, 'fresh Proc gist is the instance form';
like Proc.new.raku, /^ 'Proc.new'/, 'fresh Proc raku likewise';

my $p = run $*EXECUTABLE, '-e', '', :out;
# (Rakudo renders a run Proc with a cyclic backref binding, `(my \Proc_... =
# Proc.new(...))`; mutsu renders the plain instance form for now.)
like $p.gist, /^ [ 'Proc.new' | '(my \Proc_' ]/, 'a run Proc gists as an instance, not a number';
is $p.exitcode, 0, 'exitcode accessor unaffected';
is (run $*EXECUTABLE, '-e', 'exit 3', :out).exitcode, 3, 'nonzero exitcode unaffected';
