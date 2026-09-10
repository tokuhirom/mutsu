use Test;

# Log::Async (+ its Terminal::ANSI dependency) runs VERBATIM as a bundled
# battery (modules/Log-Async, modules/Terminal-ANSI): no `-I` and no
# `mzef install` — a plain `use Log::Async` resolves against the shipped
# modules/ tree. This file pins the observable behavior of the general
# logging slot; the exhaustive check is the release-time gate running the
# full upstream suite (scripts/battery-testsuite.sh).
#
# NOTE ON ORDERING: `Log::Async`'s `send-msg` emits each message from its own
# thread (`(start $.source.emit($m))`, modules/Log-Async/lib/Log/Async.rakumod),
# so the ORDER in which a tap sees them is not guaranteed — it is whichever
# `start` the scheduler runs first. Assert on the message->level mapping, never
# on `@seen[N]`. Indexing positionally made this file fail roughly 2 runs in 25
# under CPU contention (`scripts/flake-repro.sh -n 25 -l 4`), with a WARNING
# where an INFO was expected.

plan 8;

use Log::Async;

ok logger.defined, 'the bundled Log::Async loads with no -I and installs a logger';

# A custom sink: `add-tap` hands each message to a callback as a Hash carrying
# the text, the severity, and a timestamp.
my @seen;
logger.untapped-ok = True;
my $tap = logger.add-tap: -> $m { @seen.push: $m };

trace   'a trace line';
debug   'a debug line';
info    'an info line';
warning 'a warning line';
error   'an error line';

logger.done;

my %level-of = @seen.map({ .<msg> => .<level> });

is @seen.elems, 5, 'all five severity levels reached the sink';
is @seen.map(*.<msg>).sort.join('|'),
    'a debug line|a trace line|a warning line|an error line|an info line',
    'every message carries its own text';
is %level-of{'a trace line'}, TRACE, 'trace maps to the TRACE level';
is %level-of{'an info line'}, INFO, 'info maps to the INFO level';
is %level-of{'an error line'}, ERROR, 'error maps to the ERROR level';
is @seen.grep({ .<when> ~~ DateTime }).elems, 5, 'each message is timestamped';

# The severity enum is exported and ordered, which is what level filtering
# (`use Log::Async <trace>`) relies on.
ok TRACE < INFO < ERROR, 'the severity enum is exported and ordered';

$tap.close;
