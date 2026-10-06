use lib 't/lib';
use Test;
use SwjKl;
use SwjHolder;

# A `where` predicate naming types the declaring module imported
# (`where SwjKl|SwjItf`) must resolve them in the declaring scope. The caller
# imports `SwjKl` but not `SwjItf`, so the bare `SwjItf` only resolves there.
# Seen in Java::Generate's CompUnit (`my subset Unit where Class|Interface`).

plan 1;

is SwjHolder.new(type => SwjKl.new).type.^name, 'SwjKl', 'junction of imported types accepted';
