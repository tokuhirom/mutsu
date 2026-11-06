# "Required named parameter" names the parameter without its sigil

A missing `:&some!`, `:@x!` or `:%y!` reported `'&some'` / `'@x'` / `'%y'`;
rakudo words it `'some'` / `'x'` / `'y'`. Monad's `omitting :some is a hard
error` tests match on that text.
