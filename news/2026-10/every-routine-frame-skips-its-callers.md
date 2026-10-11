# Every routine declared at program scope resolves lexicals past its callers

ADR-12529's static link, which so far only routines using `EVAL` or a
symbolic lookup had, now covers every routine declared at program scope: a
lexical that misses the routine's frame continues at the program scope, never
at the frames that called it. A closure called from such a routine still walks
on into its own enclosing block, since only the frame a lookup starts in
decides where its lexicals continue.

Getting there needed no change to the `is rw` calling convention after all:
the writeback broke only because marking a frame had stopped the call path
from chaining past an empty caller frame, and it no longer does (#12641).
