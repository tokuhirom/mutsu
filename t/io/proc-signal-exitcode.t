use v6;
use Test;

# A process killed by a signal has no exit status at all: waitpid reports it
# as signalled, not exited. Rakudo normalises that to `exitcode = 0` and keeps
# the information in `.signal`; mutsu used to pass through what Rust's
# `ExitStatus::code()` gives for a signal death (`None`) as -1.
#
# The whole point is that this is NOT "always report 0": a child that genuinely
# exits non-zero must still report that code, and everything that treats a Proc
# as unsuccessful (sink context, Bool) must keep doing so on a signal death even
# though its exitcode is now 0.

plan 16;

# --- run ---------------------------------------------------------------

my $killed = run '/bin/sh', '-c', 'kill -TERM $$', :out, :err;
is $killed.exitcode, 0, 'run: a signal-killed child reports exitcode 0';
is $killed.signal, 15, 'run: ... and carries the signal in .signal';
nok $killed.Bool, 'run: a signal-killed Proc is still falsy';

my $failed = run '/bin/sh', '-c', 'exit 3', :out, :err;
is $failed.exitcode, 3, 'run: a genuine non-zero exit still reports its code';
is $failed.signal, 0, 'run: ... with no signal';
nok $failed.Bool, 'run: a non-zero-exit Proc is falsy';

my $clean = run '/bin/sh', '-c', 'exit 0', :out, :err;
is $clean.exitcode, 0, 'run: a clean exit reports 0';
is $clean.signal, 0, 'run: ... with no signal';
ok $clean.Bool, 'run: a clean Proc is truthy';

# --- shell -------------------------------------------------------------

my $sh = shell 'kill -TERM $$', :out, :err;
is $sh.exitcode, 0, 'shell: a signal-killed child reports exitcode 0';
is $sh.signal, 15, 'shell: ... and carries the signal in .signal';

# --- Proc::Async -------------------------------------------------------

{
    my $p = Proc::Async.new: 'sleep', '60';
    my $started = $p.start;
    await $p.ready;
    $p.kill: SIGTERM;
    my $proc = await $started;
    is $proc.exitcode, 0, 'Proc::Async: a killed child reports exitcode 0';
    is $proc.signal, 15, 'Proc::Async: ... and carries the signal in .signal';
}

# --- sink context ------------------------------------------------------

# Sinking an unsuccessful Proc throws X::Proc::Unsuccessful. A signal death now
# reports exitcode 0, so the signal is the only evidence left that it failed.
{
    my $threw = '';
    my $message = '';
    {
        run '/bin/sh', '-c', 'kill -TERM $$', :out, :err;
        CATCH {
            default {
                $threw = .^name;
                $message = .message;
            }
        }
    }
    is $threw, 'X::Proc::Unsuccessful',
        'sinking a signal-killed Proc still throws X::Proc::Unsuccessful';
    like $message, /'signal: 15'/, '... and the message reports the killing signal';
}

{
    my $threw = '';
    {
        run '/bin/sh', '-c', 'exit 0', :out, :err;
        CATCH { default { $threw = .^name } }
    }
    is $threw, '', 'sinking a successful Proc does not throw';
}
