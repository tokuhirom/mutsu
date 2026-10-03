use Test;

# An `rx` value whose source tree holds a node the direct tree plan does not
# execute (here `\N*` and `<[a]>*`) falls back to the string parser, which
# interpolates the scalar's current value into the plan. That plan must not be
# cached under the source-tree fingerprint, or every later call of the same
# literal matches against the first call's value.

plan 6;

sub any-then($e) { so "x42" ~~ rx/x \N* $e/ }
nok any-then('for'), 'first call: a non-matching value fails';
ok  any-then('42'),  'second call: the new value is interpolated';

sub class-then($e) { so "x42" ~~ rx/x <[a]>* $e/ }
nok class-then('for'), 'enumerated class: a non-matching value fails';
ok  class-then('42'),  'enumerated class: the new value is interpolated';

sub echo($e) { so "------>42 if 23" ~~ rx/'------>' \N* $e/ }
nok echo('for'),      'a value absent from the line does not match';
ok  echo('42 if 23'), 'a later value with spaces is matched literally';
