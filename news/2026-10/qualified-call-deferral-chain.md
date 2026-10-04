# A qualified method call defers along its qualifier's chain

```raku
role R { method m() { say self.^name; nextsame } }
class Base { method m() { say "base" } }
class C is Base does R { method m() { self.R::m() } }
C.m;   # C
```

mutsu used to print `C` and then `base`. The `nextsame` in a method called
through a qualified call (`self.R::m`) read the caller's dispatch frame and
went on to the receiver's next MRO candidate (#11592).

A qualified call now runs its callee under a deferral frame of its own,
built from the qualifier:

- **A class qualifier** (`self.Q::m`) gets the frame Q's own MRO gives, so a
  `callsame` reaches Q's parent.
- **A role qualifier** (`self.R::m`, `self.R::new(|%a)`) gets a frame with no
  next candidate and no native base behind it. A deferral in it answers Nil,
  as in rakudo.

An unqualified call on a punned role keeps its native base: `nextsame` in a
punned role's own `new` still constructs.
