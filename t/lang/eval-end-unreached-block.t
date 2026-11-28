use Test;
use MONKEY-SEE-NO-EVAL;

plan 1;

EVAL q[if False { END { ok True, 'an unreached END runs at exit' } }];
