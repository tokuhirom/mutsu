# A module routine's `for` parameter shadows the module's lexical; faster hole reads

A pointy `for` parameter in a module routine is now read as the innermost
binding of its name. Before, `for 1, 2 -> $x { $x }` in a routine of a module
that also declares `my $x` read the module's variable. The parameter has no
local slot, so its reads go by name, and the by-name chain asked the
compunit's file-scope lexicals, package-block lexicals and `our` mirrors
before the `for` loop's own binding. A name bound by an active loop parameter
of the running call now reads that binding first. Those reads also get the
env fast path outside `GLOBAL`, which makes a loop in a `unit module` read its
parameter as fast as one in a script. A closure that captures the parameter,
and a write through `<->`, still read the module's variable (#17466).

The `$d[$i] = ...` / `$d[$i] := ...` store lane now also covers a store
through such a loop parameter (`for @tables -> $t { $t[$i] := ... }`).

The element-read fast path now covers more of `$a[$i]`:

- a hole, or an index past the end, of a real Array (both read as `Any`);
- a `:=`-bound element cell;
- an itemized Array (the one a `$` scalar or a `for` parameter holds).

Each of these used to take the general path, at about 3,200 instructions per
read. An array's `initialized` set now uses the Fx hasher.

`make-ansi-parser` (Terminal::ANSIParser, #17429) went from 47M to 39M
instructions per call. x100 now takes about 0.85 s (rakudo 0.30 s).
