unit class CrlfConst;
use CrlfDerived;
# A sigilless `constant` whose bare name is the same `CRLF` key. This is the
# value that leaked into CrlfDerived's method reads before the fix.
constant CRLF = "const-crlf";
