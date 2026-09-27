# P5unlink can recover the caller's topic

mutsu now implements the chained `CALLER::LEXICAL::` pseudo-stash used by
P5unlink's no-argument `unlink` routine. The routine can once again read the
caller's implicit topic and remove the requested path, bringing P5unlink's
baseline test file to parity.
