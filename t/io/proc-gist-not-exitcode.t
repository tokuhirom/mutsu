use v6;
use Test;

plan 12;

# Proc has no stringifier of its own in Rakudo, so its gist is the default
# instance repr — NOT the exitcode number the old native Str/gist arm
# rendered (`Proc.new.gist` was `-1`).

# Proc's public attributes are registered so `.raku`/`.gist` render the full
# Rakudo form with type-object defaults, not the bare `Proc.new`.
my $full = 'Proc.new(in => IO::Pipe, out => IO::Pipe, err => IO::Pipe, ' ~
    'os-error => Str, exitcode => Nil, signal => Any, pid => Nil, command => [])';
is Proc.new.raku, $full, 'fresh Proc raku is the full attribute form';
is Proc.new.gist, $full, 'fresh Proc gist matches raku';

# Registering the attributes must NOT disturb the native getters, whose
# computed fallbacks differ from the raw seeded defaults.
is Proc.new.signal, 0, 'signal getter reports 0 (not the Any default) for an un-run proc';
is Proc.new.exitcode, Nil, 'exitcode getter is Nil for an un-run proc';
is Proc.new.pid, Nil, 'pid getter is Nil for an un-run proc';
is Proc.new.Int, -1, 'Int coercion is -1 for an un-run proc';
is Proc.new.in.^name, 'IO::Pipe', 'in default is the IO::Pipe type object';

my $p = run $*EXECUTABLE, '-e', '', :out;
# (Rakudo renders a run Proc with a cyclic backref binding, `(my \Proc_... =
# Proc.new(...))`; mutsu renders the plain instance form for now.)
like $p.gist, /^ [ 'Proc.new' | '(my \Proc_' ]/, 'a run Proc gists as an instance, not a number';
is $p.exitcode, 0, 'exitcode accessor unaffected';
is $p.signal, 0, 'signal accessor unaffected on a run proc';
is $p.command.raku, ($*EXECUTABLE.Str, '-e', '').raku, 'command accessor unaffected';
is (run $*EXECUTABLE, '-e', 'exit 3', :out).exitcode, 3, 'nonzero exitcode unaffected';
