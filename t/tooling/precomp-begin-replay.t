use Test;

# ADR-18012 S1: the first load of a precompiled module records what its
# value-position BEGIN expressions produced; a warm load replays that instead
# of running the body again. A body is recorded only when it is replay-safe by
# construction, so each rejected class below must still run on the warm load,
# and every warm run must print exactly what its cold run printed.

plan 28;

my $root = $*TMPDIR.add("mutsu-begin-replay-{$*PID}");
my $lib = $root.add('lib');
$lib.mkdir;
my $mark = $root.add('mark.txt');

# One module per class. Each `BEGIN` sets a unit lexical and binds an `@` or
# `%` variable, which keeps it out of the unit's BEGIN prologue (ADR-0134): it
# stays a `BeginOnceExpr` in the mainline, which is what S1 records. `begin-ran` is printed on stdout every time a body
# runs, so "printed on the warm run" means "not replayed".
my %modules =
    # Replayed: a Map over native int arrays, plus a unit lexical the body set.
    BRTable => q:to/M/,
        use nqp;
        my $words;
        my %table := BEGIN {
            my int32 @odd;
            my $h := nqp::hash('odd', @odd);
            for 1..4 -> int32 $i {
                nqp::push_i(@odd, $i * 2 - 1);
                nqp::push_i(nqp::ifnull(nqp::atkey($h, ~($i * 2)), nqp::bindkey($h, ~($i * 2), my int32 @)), $i * 4);
            }
            $words = 'w' ~ nqp::elems($h);
            nqp::p6bindattrinvres(nqp::create(Map), Map, '$!storage', $h)
        }
        sub table() is export { %table }
        sub words() is export { $words }
        M
    # A built-in name whose receiver is user code: the method runs. (The type
    # comes through a lexical: a bare type name is a lookup the scan refuses.)
    BRUserMethod => q:to/M/,
        class Shout { method comb(|) { say 'begin-ran user-method'; <a b> } }
        my $class;
        my @type := BEGIN { $class := Shout; () };
        my @v := BEGIN { $class.comb };
        sub user-method() is export { @v.join(',') }
        M
    # A slow-path dispatch into user code: `.Str` on a user type.
    BRSlowPath => q:to/M/,
        class Named { method Str() { say 'begin-ran slow-path'; 'named' } }
        use nqp;
        my $class;
        my @type := BEGIN { $class := Named; () };
        my @v := BEGIN { nqp::list($class.Str) };
        sub slow-path() is export { @v[0] }
        M
    # A routine call (file write): the scan refuses it.
    BRFileWrite => qq:to/M/,
        use nqp;
        my \$touch;
        my \@v := BEGIN \{ \$touch = 1; '{$mark}'.IO.spurt('written'); nqp::list('done') };
        sub file-write() is export \{ \@v[0] }
        M
    # A process-specific value read through a dynamic variable.
    BRProcess => q:to/M/,
        use nqp;
        my $touch;
        my @v := BEGIN { $touch = 1; nqp::list($*PID) };
        sub process-static() is export { @v[0] == $*PID }
        M
    # An overwrite of a unit lexical that keeps its element count. (The first
    # body rebinds `$h`, which the scan refuses: only the second is replayed,
    # on top of the hash the first builds again.)
    BROverwrite => q:to/M/,
        use nqp;
        my $h;
        my @one := BEGIN { $h := nqp::hash('a', 1); nqp::list('one') };
        my @two := BEGIN { nqp::bindkey($h, 'a', 2); nqp::list('set') };
        sub overwrite() is export { nqp::atkey($h, 'a') ~ @two[0] }
        M
    # A recorded body whose input an earlier, unrecorded body sets differently
    # on each load: its recorded result is stale, so it runs again.
    BRInputs => q:to/M/,
        use nqp;
        my $h;
        my @pid := BEGIN { $h := nqp::hash('pid', $*PID); () };
        my @x := BEGIN { nqp::bindkey($h, 'x', 1); nqp::list() };
        sub inputs() is export { nqp::atkey($h, 'pid') == $*PID && nqp::atkey($h, 'x') == 1 }
        M
    # One container reached twice: a decode would build it twice.
    BRShared => q:to/M/,
        use nqp;
        my $touch;
        my %v := BEGIN { $touch = 1; my $l := nqp::list(); nqp::hash('a', $l, 'b', $l) };
        sub shared() is export { nqp::push(nqp::atkey(%v, 'a'), 1); nqp::elems(nqp::atkey(%v, 'b')) }
        M
    ;
for %modules.kv -> $name, $src {
    $lib.add("$name.rakumod").spurt: $src;
}

my $script = $root.add('probe.raku');
$script.spurt: qq:to/PROBE/;
    use lib '$lib';
    use BRTable; use BRUserMethod; use BRSlowPath; use BRFileWrite;
    use BRProcess; use BROverwrite; use BRShared; use BRInputs;
    say table().sort.map(\{ .key ~ '=' ~ .value.join(',') }).join(' ');
    say table().^name, ' ', table()<odd>.^name, ' ', words();
    say user-method();
    say slow-path();
    say file-write();
    say process-static();
    say overwrite();
    say shared();
    say inputs();
    PROBE

%*ENV<XDG_CACHE_HOME> = $root.add('cache').Str;
%*ENV<MUTSU_PRECOMP_TRACE> = '1';

sub run-probe(*%env) {
    temp %*ENV;
    %*ENV{.key} = .value for %env;
    my $proc = run($*EXECUTABLE, $script.Str, :out, :err);
    my $out = $proc.out.slurp;
    my @begin = $proc.err.slurp.lines.grep(*.starts-with('precomp begin:'));
    %( :$out, :@begin )
}

sub replayed(%run) { %run<begin>.grep(*.contains('replayed')).elems }

my %cold = run-probe();
ok $mark.e, 'cold: the file-writing BEGIN ran';
$mark.unlink;
my %warm = run-probe();

is %warm<out>, %cold<out>, 'warm output is identical to the cold output';
is replayed(%cold), 0, 'cold: nothing is replayed';
is replayed(%warm), 2, 'warm: exactly the two replay-safe bodies are replayed';

my @cold = %cold<out>.lines.grep(!*.starts-with('begin-ran'));
is @cold[0], '2=4 4=8 6=12 8=16 odd=1,3,5,7', 'the table is right';
is @cold[1], 'Map array[int32] w5', 'its Map and native-array types and the lexical are right';
my @warm = %warm<out>.lines.grep(!*.starts-with('begin-ran'));
is @warm[1], 'Map array[int32] w5', 'warm: the replayed table is still a Map of int32 arrays';

for <user-method slow-path> -> $class {
    ok %warm<out>.contains("begin-ran $class"), "warm: a body reaching user code ($class) runs again";
}
ok $mark.e, 'warm: the file-writing body runs again';
is @warm[5], 'True', 'warm: the process-specific body sees this process';
is @warm[6], '2set', 'warm: the overwritten lexical keeps the new value';
is @warm[7], '1', 'warm: a container reached twice is still one container';
is @warm[8], 'True', 'warm: a body whose recorded inputs differ runs again';
is %warm<begin>.grep(*.contains('inputs differ')).elems, 1,
    'warm: the replay checked the inputs';

ok %cold<begin>.grep(*.contains('a BEGIN body is not recorded')).elems >= 2,
    'cold: the scan refuses the routine call and the dynamic read';
ok %cold<begin>.grep(*.contains('ran other compiled code')).elems >= 2,
    'cold: the watch refuses both bodies that reached user code';
ok %cold<begin>.grep(*.contains('reached twice')).elems == 1,
    'cold: the shared container is refused';

# A changed source is a new compiled entry: its old log is never replayed.
$lib.add('BROverwrite.rakumod').spurt:
    $lib.add('BROverwrite.rakumod').slurp.subst("'a', 2", "'a', 3");
my %edited = run-probe();
ok %edited<out>.contains('3set'), 'an edited module records afresh';
is replayed(%edited), 1, 'an edited module does not replay its stale log';
my %edited-warm = run-probe();
is %edited-warm<out>, %edited<out>, 'the edited module replays its new log';
is replayed(%edited-warm), 2, 'both replay-safe bodies replay again';

# The kill switch: every body runs, nothing is recorded or replayed.
my %off = run-probe(MUTSU_BEGIN_REPLAY => '0');
is %off<out>, %edited<out>, 'MUTSU_BEGIN_REPLAY=0: same output';
is replayed(%off), 0, 'MUTSU_BEGIN_REPLAY=0: nothing is replayed';
is %off<begin>.grep(*.contains('recorded')).elems, 0, 'MUTSU_BEGIN_REPLAY=0: nothing is recorded';

# A damaged log is a miss, never trusted in part.
my @logs = $root.add('cache').&find-logs;
ok @logs.elems >= 1, 'the logs are stored in the cache';
for @logs -> $log {
    my $buf = $log.slurp(:bin);
    $buf[$buf.elems - 1] +^= 1;
    $log.spurt($buf);
}
my %damaged = run-probe();
is %damaged<out>, %edited<out>, 'a damaged log: same output';
is replayed(%damaged), 0, 'a damaged log is not replayed';

sub find-logs(IO::Path $d) {
    gather for $d.dir -> $e {
        if $e.d { .take for find-logs($e) }
        elsif $e.basename.ends-with('.begin') { take $e }
    }
}

END {
    try $root.&rmtree if $root.e;
}

sub rmtree(IO::Path $d) {
    for $d.dir -> $e {
        $e.d ?? rmtree($e) !! $e.unlink;
    }
    $d.rmdir;
}
