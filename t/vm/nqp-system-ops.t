use Test;
use nqp;

# The process, system-introspection and time nqp:: ops (#11501), checked
# against rakudo and against the Raku-level API that shares each routine.

plan 24;

is nqp::getpid(), $*PID, 'getpid is $*PID';
ok nqp::getppid() > 0, 'getppid';
is nqp::execname(), $*EXECUTABLE.Str, 'execname is $*EXECUTABLE';

is nqp::cpucores(), $*KERNEL.cpu-cores, 'cpucores is Kernel.cpu-cores';
ok nqp::totalmem() > 0, 'totalmem';
ok nqp::freemem() > 0, 'freemem';
is $*KERNEL.total-memory, nqp::totalmem(), 'Kernel.total-memory';
ok $*KERNEL.free-memory > 0, 'Kernel.free-memory';

{
    my $u := nqp::uname();
    is nqp::elems($u), 4, 'uname has four fields';
    is (nqp::const::UNAME_SYSNAME, nqp::const::UNAME_RELEASE, nqp::const::UNAME_VERSION,
        nqp::const::UNAME_MACHINE), (0, 1, 2, 3), 'the UNAME_* constants';
    is nqp::atpos_s($u, nqp::const::UNAME_RELEASE), $*KERNEL.release, 'uname release';
    is nqp::atpos_s($u, nqp::const::UNAME_MACHINE), $*KERNEL.hardware, 'uname machine';
}

{
    my $s := nqp::getsignals();
    is nqp::elems($s), 70, 'getsignals lists 35 name/number pairs';
    is (nqp::atpos($s, 0), nqp::atpos($s, 1)), ('SIGHUP', 1), 'getsignals starts with SIGHUP';
    my %sig = (^35).map({ nqp::atpos($s, 2 * $_) => nqp::atpos($s, 2 * $_ + 1) });
    is %sig<SIGINT SIGKILL SIGTERM>, (2, 9, 15), 'getsignals numbers';
    is Signal.enums<SIGINT>, %sig<SIGINT>, 'the Signal enum agrees';
    is $*KERNEL.signal('SIGTERM'), %sig<SIGTERM>, 'Kernel.signal agrees';
}

is nqp::atkey(nqp::getenvhash(), 'PATH'), %*ENV<PATH>, 'getenvhash holds the environment';
ok nqp::elems(nqp::backendconfig()) > 0, 'backendconfig is $*VM.config';

{
    my $epoch = 1_700_000_000 - $*TZ;
    my $d := nqp::decodelocaltime($epoch);
    is (^9).map({ nqp::atpos_i($d, $_) }).head(8), (20, 13, 22, 14, 11, 2023, 2, 317),
        'decodelocaltime: sec min hour mday month year wday yday';
}

{
    my $t = now;
    is nqp::sleep(0.1e0), 0.1e0, 'sleep answers its seconds';
    ok now - $t >= 0.09, 'sleep sleeps';
}

# nqp::exit ends the process at once: unlike `exit`, no END phaser runs.
{
    my $p = run $*EXECUTABLE, '-e', 'use nqp; END { say "end" }; say "a"; nqp::exit(3); say "b"', :out;
    is $p.out.slurp(:close), "a\n", 'nqp::exit skips END phasers and the rest';
    is $p.exitcode, 3, 'nqp::exit sets the exit status';
}
