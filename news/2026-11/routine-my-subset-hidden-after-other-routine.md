# A routine's `my subset` stays hidden from its callee's EVAL after another routine used the name

```raku
sub callee { (try EVAL q[my Small $x = 5; $x]) // 'not visible' }
sub first  { my subset Small of Int where * < 10; 1 }
sub second { my subset Small of Int where * < 10; callee() }
first(); say second();
```

rakudo prints `not visible`; mutsu printed `5` (#16099). A routine-local
`my subset` is taken out of the registry while the `EVAL` of a sub it calls
runs. That only works when the bare registry key is this declaration's own.
Since #18712, a `my subset` left the key to whichever subset already held it,
so that an outer `subset S` keeps answering after a block. The rule also kept
an earlier routine's `my subset` of the same name, and the EVAL then
recognised neither one.

A subset definition now records whether it is a declaration-site `my subset`.
A later `my subset` replaces such an entry under the bare key, and still
leaves a package subset's entry alone.
