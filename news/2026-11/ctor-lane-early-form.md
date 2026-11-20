# `Class.new(named...)` skips the argument stretch once the class is in the ctor lane

`Token.new(kind => 'text', :$text)` takes the ctor lane (ADR-0121 D3) once a
call of it has walked the whole `CallMethodMut` probe chain into the native
default constructor. The lane's gate, though, sat after the argument stretch
at the top of that chain, so every call still did the following before
reaching it:

- moved its arguments off the stack and decoded them;
- gathered and hllized them;
- scanned them for a consumed `Seq` and for a deferred `.map`/`.grep`;
- offered the receiver to two rewrites.

The lane now has an early form at the top of `CallMethodMut`, the
constructor twin of the plain-method lane's early form. It answers a hit
before the stretch. It refuses every call whose arguments the stretch would
change: a `|` spread, a `VarRef`, a value `hllize_for_binding` rewrites, or a
gather or consumed `Seq`. It reproduces the stretch's two remaining effects:
it publishes the argument sources, and it reifies a deferred `.map`/`.grep`
argument when one may be present.

Callgrind slopes on a profiling build:

- `Token.new(kind => 'text', :$text)` went from 21.7k to 18.9k instructions;
- MCP::Server::Tool::Web's html-to-text, which builds one per token, went
  from 3.877G to 3.828G instructions per 0.05 of the fixture (−1.3%).
