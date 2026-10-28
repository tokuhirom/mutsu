use v6;
use lib 't/lib';
use Test;

# Needle::Compile (#15087): a multi candidate's parameter type is a
# module-private `my constant StrType = Str but Type`. The caller imports only
# `kind` and `Type`, so the candidate must be ranked with its type resolved in
# the module's own scope, ahead of the plain `Str:D` candidate.

use PrivateConstantMulti <kind Type>;

plan 3;

is kind("abc" but Type("code")), 'mixed:code', 'the narrower module-private candidate wins';
is kind("abc"), 'plain', 'a plain Str still takes the plain candidate';
is kind("xyz" but Type("t2")), 'mixed:t2', 'and again for another value';
