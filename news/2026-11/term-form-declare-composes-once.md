# A term-form EXPORTHOW-declared class composes once

A class declared as a term (`given stately Term is tagged("t") { }`) is registered
by a compile-time shell and again in place, and since the in-place pass keeps the
shell's metaobject it queued the user `compose` a second time on the same
instance, so a HOW that accumulates state in `compose` saw the class composed
twice. A hoisted shell no longer queues the user `compose`; the in-place pass, which re-registers
the class, composes it once.
