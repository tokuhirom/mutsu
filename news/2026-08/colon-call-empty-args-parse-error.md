# A colon call with an empty argument list is now a plain zero-argument call

`say 4.log:   ;` is legal Raku — a colon call whose argument list happens to be
empty means exactly the same thing as `say 4.log;`. mutsu rejected it with
`===SORRY!=== ... Confused. expected statement`, which the doc-diff harness
found while checking `raku-doc/doc/Language/objects.rakudoc`.

## Root cause

`src/parser/expr/postfix/loop_.rs` handled the no-space `.method:` colon-listop
form by unconditionally parsing one argument expression after the colon. The one
exception was a literal `}` immediately after it, added earlier for CSV::Table's
`$w = $r.rwid:` followed by a closing brace; an accompanying comment claimed
raku "still demands a colon-pair" before `;` / `)` / `]` / EOF. That claim was
simply wrong. Checking every context against rakudo shows the zero-argument form
is accepted everywhere the argument list can legally be empty:

```raku
my $sem      = 4.log:   ;      # before `;`
my $paren    = (4.log: );      # before `)`
my $block    = do { 4.log: };  # before `}`
my @brackets = [ 4.log: ];     # before `]`
say 4.log:                     # at end of input
```

The same one-argument assumption existed in the `.= method:` form in
`src/parser/expr/postfix/dot_assign.rs`, so `$s .= uc: ;` failed too.

## Fix

Both colon-listop argument parsers now treat the list as empty whenever the next
token cannot start a term — end of input, a statement terminator, or the close of
the enclosing block/group (`;` `}` `)` `]`) — and emit a plain zero-argument
`MethodCall`. Anything else, including a leading infix such as `,`, still falls
through to the term parser so `say 4.log: ,` remains a parse error exactly as it
is in raku.

The parser unit test that asserted the old behaviour
(`parse_postfix_colon_args_require_expression`) pinned the bug rather than the
spec; it was replaced by a pair of tests that pin the empty list parsing as a
zero-argument call and the leading-infix case still erroring.

## Tests

`t/colon-call-argument-parsing.t` covers every context above (plus the `.=`
form), and is written so it passes verbatim under both `raku` and `mutsu`.
