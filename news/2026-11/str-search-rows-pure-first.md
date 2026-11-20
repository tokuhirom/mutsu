# Str's search and slice methods skip the interpreter in the call-site lane

`contains`, `index`, `rindex`, `starts-with`, `ends-with`, `substr`,
`substr-eq`, `comb` and `wordcase` on `Str` and `Cool` were interpreter rows
of the method table whose routine first tried a pure candidate, because a
`Regex` needle, an adverb, a `Range` or a `WhateverCode` position needs the
interpreter. The VM's call-site lane cannot tell the two apart from the row
kind, so every `$s.substr($i, 1)` took the interpreter branch: a source-line
sync, copies of the receiver and the arguments, the in-method-call scope and
the full dispatch wrapper, only for the routine to find the pure answer.

These rows are now `Handler::InterpPureFirst`: a pure half and an
interpreter half. The lane runs the pure half on a `Str` receiver's
arguments where they sit on the stack, and falls back to the interpreter half
when it declines. Every other entry treats the row as an interpreter row, as
before (`Handler::needs_interp`), and debug builds cross-check the lane's
answer against the full path like any pure row's.

`$s.substr($i, 1)` in a loop went from about 3,260 to 2,680 instructions per
call (callgrind slopes, profiling build), and MCP::Server::Tool::Web's
html-to-text from 4.220G to 4.180G instructions per 0.05 of the fixture
(-0.9%), where `substr`, `index`, `starts-with` and `contains` are about
100,000 calls per 0.05.
