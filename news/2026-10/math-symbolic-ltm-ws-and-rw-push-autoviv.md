# Math::Symbolic: LTM whitespace after a zero-width atom, and push through an rw-returned entry

Taking the `Math::Symbolic` distribution (`t/01-basics.t`, 11 assertions) from a parse failure to
all 11 passing under mutsu fixed two interpreter gaps.

- **Declarative-prefix measurement.** In `rule expression { \s* [...] }` the implicit whitespace
  follows the `\s*`, so it is not the rule's leading whitespace even though `\s*` matched nothing.
  The measurement treated any whitespace at subject position 0 as leading and therefore
  transparent, so `expression` out-ranked `equation` in `token TOP { <equation> | <expression> }`
  and `y=m*x` stopped parsing after `y`. The NFA's `WsLead` node now carries whether the whitespace
  can still be leading (nothing but literals before it in its pattern), as Rakudo ends the prefix there.
- **Autovivifying push through an `is rw` return.** `sub f($k) is rw { %h{$k} }; f('x').push: 5`
  (and the same through a variable `:=`-bound to the result, or an `is rw` method returning an
  attribute hash entry) left the hash unchanged. A push-family call on a deferred hash-entry token
  whose entry is missing now stores a fresh Array into the entry and mutates that node, as
  `Any:U` autovivification does in Rakudo.
