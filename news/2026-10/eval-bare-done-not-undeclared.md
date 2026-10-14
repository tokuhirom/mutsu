# A bare `done` in an EVAL string is not an undeclared name

The undeclared-name walk run on an `EVAL` string after the RakuAST round trip
saw the `done` of `react { whenever ... { ... }; done }` as an unknown bare
word (the round trip keeps it as a bare word for the compiler to resolve, the
parser's own tree has a dedicated statement). `done` is a core routine, so it
is now on the walk's list of terms that name no symbol; outside `react` and
`supply` it still fails at run time with "done without supply or react".
