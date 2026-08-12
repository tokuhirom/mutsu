# Text::CSV runtime sweep round 4: `.can` multi dispatcher, listop word-logical precedence, expression-assign copy semantics

Three general interpreter fixes found by running Text::CSV's upstream test
suite (github.com/Tux/CSV, 33 files), which moved the suite from 11/33 to
27/33 fully-passing files.

1. **`.can` / `^find_method` on a multi method returns ONE dispatcher, and
   invoking it re-dispatches.** `collect_can_methods` returned one Sub per
   candidate (raku: one dispatcher Method per MRO class), and the
   dispatcher-shaped Sub that `^lookup`/`^find_method` builds carried only
   the FIRST candidate's signature/body, so `$meth($invocant, $arg)` threw
   "Too many positionals" whenever candidate 0 had a different arity. Both
   value-call paths (`call_sub_value`, `vm_call_on_value`) now detect the
   dispatcher shape (`__mutsu_lookup_class`/`__mutsu_lookup_method` env
   markers without a candidate idx) and re-dispatch with the first argument
   as invocant. Text::CSV's `BUILD → !set-attributes` does exactly
   `.(self, %init{$attr}) for self.can(lc $attr)` for every constructor
   attribute, so `:quote-empty` etc. never took effect (12_acc / 15_flags /
   21_combine / 22_print). Pin: `t/can-multi-dispatcher.t`.

2. **Statement-level listop arguments stop at the loose word-logicals.**
   `defined $f and $cf.add(...)` parsed as `defined($f and ...)` — the
   no-paren statement-call argument parser (`parse_remaining_call_args`)
   and the IO listop expression list (`say`/`print`/`put`/`note`) both
   parsed each argument with the full `expression` parser, swallowing
   `and`/`or`/`andthen`/`orelse`/`xor` into the last argument. They now
   parse at list-prefix precedence (`ExprMode::ListopArg`, the same mode
   the expression-position listop path already used), and an IO listop
   followed by a word-logical bails to the general call parser so the
   operator binds at statement level (`say 0 or die` is `(say 0) or die`).
   Text::CSV's `combine` guards every field add with `defined $f and
   $cf.add ($f.Str)`, so `""` fields were silently dropped. Pin:
   `t/listop-word-logical.t`.

3. **Expression-position container assignment gets Raku `=` copy
   semantics.** `$!io and @ch = @!ahead` (assignment as `and`-RHS) adopted
   the RHS array's backing `Gc` instead of copying — the statement-form
   `SetLocal` had the in-place-reassign + `detach_shared_container` logic,
   but the two expression-position paths (`AssignExprLocal` /
   name-based `AssignExpr`) did not. A later `@ch.append` then leaked the
   parsed chunks back into `@!ahead`, so after a `skip_empty_rows` empty-
   line recursion every subsequent `getline` re-emitted the previous row
   (45_eol.t tests 1513–1520). Both paths now mirror SetLocal: copy into
   the existing same-kind container (identity-preserving) or detach to a
   distinct container. Pin: `t/assign-expr-container-copy.t`.

Suite status after this round: 45_eol.t joins the green set (1520/1520);
remaining failures are 65_allow (13/1022), 85_util, 90_csv (csv() header
semantics), 91_csv_cb, 92_csv_encoding, and 99_meta (needs external
Test::META).
