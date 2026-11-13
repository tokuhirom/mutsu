# Pod::To::HTML: Positional:D Seq failover, lexical-type defaults, andthen and delegated protos

Running Pod::To::HTML's own test suite exposed four general bugs, now fixed:

- A method parameter typed `Positional:D` (or `Positional:_`) rejected a Seq
  instead of caching it into a List through PositionalBindFailover; only a
  bare `Positional` constraint did. Sub and method binding now share one
  predicate, `binds_through_positional_failover`.
- An omitted optional parameter typed by a lexical class with a qualified
  name (`my class TOC::Calc`, `TOC::Calc :$toc`) bound a type object built
  from the bare spelling, so `$toc.new` died with "Could not find symbol
  '&Calc'". It now binds the lexical class's type object, also when the
  routine belongs to a module.
- `andthen` / `orelse` called any Callable right-hand side with the topic.
  Like rakudo, they now call it only when it is a block literal (bare,
  pointy or placeholder block, possibly parenthesized); `%h andthen .<code>`
  yields the stored Sub.
- A `handles` delegation into a proto method with a body skipped the body
  when the delegate attribute held a container (after `:$!logger` was passed
  as a named argument), so Template::Mustache's log-level filter was bypassed
  and every Debug line was printed.
