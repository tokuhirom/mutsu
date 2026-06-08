use Test;

# The simple `substr` slice (non-negative Int start, optional non-negative Int
# length) is a single pure helper in builtins::substr, shared by the four native
# call sites (the .substr method 1-/2-arg, and the substr() function 2-/3-arg).
# Negative / WhateverCode / Range starts and lengths, and out-of-range starts
# (which return a Failure), stay in the interpreter's dispatch_substr.

plan 16;

# --- native fast path: method form ---
is "hello world".substr(6), 'world', '.substr(start)';
is "hello world".substr(0, 5), 'hello', '.substr(start, len)';
is "hello".substr(2), 'llo', '.substr(start) mid';
is "hello".substr(1, 2), 'el', '.substr(start, len) mid';
is "hello".substr(5), '', '.substr(len) at end is empty';
is "hello".substr(2, 100), 'llo', '.substr length past end clamps';

# --- native fast path: function form ---
is substr("hello world", 6), 'world', 'substr(str, start)';
is substr("hello world", 0, 5), 'hello', 'substr(str, start, len)';

# --- interpreter-coupled cases still work ---
is "hello".substr(*-3), 'llo', '.substr(*-3) WhateverCode start';
is "hello".substr(1, *-1), 'ell', '.substr(1, *-1) WhateverCode length';
is "hello".substr(2..4), 'llo', '.substr(Range)';
is "hello".substr(-2), 'lo', '.substr(negative start)';
nok "abc".substr(10).defined, '.substr past end returns undefined (Failure)';

# --- EVAL carrier (interpreter evaluates the call) ---
is EVAL(q{"hello world".substr(6)}), 'world', 'simple substr via EVAL';
is EVAL(q{"hello".substr(*-3)}), 'llo', 'WhateverCode substr via EVAL';

# --- Unicode: char (not byte) indexing ---
is "héllo".substr(1, 1), 'é', '.substr indexes by character';
