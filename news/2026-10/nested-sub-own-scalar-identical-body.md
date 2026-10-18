# A nested sub keeps to its own routine's scalars, and leaves the program's alone

Suppose a program-scope `sub sv { $s }` and a routine's nested `sub sv { $s }`
have identical bodies. Two things went wrong (#13900):

- A call of the nested `sv` made before its declaration statement ran read the
  program's `$s`, where it should read the routine's own `$s`.
- Once the routine had run, the program-scope `sv` read the routine's `$s`
  instead of its own. The same happened for `@`/`%` variables.

```raku
my $s = 'main-s';
sub sv { $s }
sub mk2 { my $s = 'mk2-s'; my $r = sv(); sub sv { $s }; $r }
say mk2();   # mk2-s (was main-s)
say sv();    # main-s (was mk2-s)
```

There are two fixes:

- **The hoisted registration seeds a scalar's cell.** It now seeds the cell
  of a routine's own scalar, as it already did for `@`/`%`. The declaration
  adopts that cell and marks it as an untyped scalar. That way an integer
  atomic reaching it through an `is rw` parameter still refuses it
  (`t/nativecall/atomic-int-native-target.t`). Before, the adopted cell
  carried no such mark, so `⚛++` ran on a plain `my $x`.
- **The routine-nested sub's latest-activation cells (mutsu#9111) also record
  which compiled body they belong to.** Previously they knew only the sub's
  package and file, so the program-scope `sv` — same name, same package,
  same file — answered from them too.
