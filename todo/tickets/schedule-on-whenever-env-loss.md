# schedule-on loses the whenever body's lexical env (enum values unresolvable)

When a supply is tapped through `.schedule-on($*SCHEDULER)`, the `whenever`
body executes WITHOUT the supply block's lexicals in env: a `my enum`
declared in the supply block is not visible, so a bareword enum value dies
with `X::Undeclared::Symbols: Undeclared name` (raised from
`exec_get_bare_word_op`, on the main VM thread). The same code without
`.schedule-on` works.

Repro (with any module that makes `Header` a suppressed name, e.g.
`t/suppressed-type-vs-local-decl-lib/SuppMod.rakumod` — without a
suppressed-name collision the bareword still resolves via the registry, so
the env loss is masked):

```raku
use SuppMod;
my $in = Supplier.new;
my $out = supply {
  my enum E <A Header B>;
  whenever $in -> $v { emit Header.Int }
};
$out.schedule-on($*SCHEDULER).tap: -> $x { say "got $x" },
    quit => -> $ex { say "QUIT: {$ex.gist}" };
$in.emit(1);
sleep 1;
```

Prints `QUIT: X::Undeclared::Symbols: ... Header used at line 1` instead of
`got 1`.

This blocks `tmp/cro-http/t/http-response-parser.rakutest`'s `parses` helper
(it taps `transformer(...)` through `.schedule-on($*SCHEDULER)`;
Cro::HTTP::ResponseParser declares `my enum Expecting <StatusLine Header
Body>` in its supply block), leaving ~25 failures + duplicate-run TAP
("planned 72 ran 154") from re-taps. The non-schedule-on path was fixed by
PR #5628's suppressed-name/local-declaration resolution; this ticket is only
about the schedule-on env loss.

Where to look: the `schedule-on` tap wiring (supply_classify /
native_supply_mut_methods) — the whenever body callback appears to be
re-dispatched with a base env instead of its captured closure env.
