use Test;

# Log::Async (+ its Terminal::ANSI dependency) runs VERBATIM as a bundled
# battery (modules/Log-Async, modules/Terminal-ANSI): no `-I` and no
# `mzef install` — a plain `use Log::Async` resolves against the shipped
# modules/ tree. This file pins the observable behavior of the general
# logging slot; the exhaustive check is the release-time gate running the
# full upstream suite (scripts/battery-testsuite.sh).

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

is @seen.elems, 5, 'all five severity levels reached the sink';
is @seen[0]<msg>, 'a trace line', 'the first message carries its text';
is @seen[0]<level>, TRACE, 'trace maps to the TRACE level';
is @seen[2]<level>, INFO, 'info maps to the INFO level';
is @seen[4]<level>, ERROR, 'error maps to the ERROR level';
ok @seen[0]<when> ~~ DateTime, 'each message is timestamped';

# The severity enum is exported and ordered, which is what level filtering
# (`use Log::Async <trace>`) relies on.
ok TRACE < INFO < ERROR, 'the severity enum is exported and ordered';

$tap.close;
