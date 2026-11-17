# A missed method on a user class builds its "Did you mean" tip 20x cheaper

`X::Method::NotFound` carries rakudo's "Did you mean ...?" tip, which compares the missing
name with every method the invocant answers to. The comparison was memoized for built-in
types, but not for a user class: such a class adds its own method names to the candidates,
and the memo is keyed by the built-in catalog alone. So each miss paid the full edit-distance
walk over ~250 candidates (~1.3M instructions). XML::Class probes `self.parent.?xml-namespace-prefix`
once per node, and in Audio::Hydrogen's `t/030-song.t` that walk was 38% of the run
(#17693).

The tip itself is unchanged; computing it is cheaper:

- A candidate whose length differs from the name's by more than the distance bound, and
  which cannot contain the name either, is skipped before any distance is computed. Edit
  distance is at least the length difference, so the skipped candidates could never match.
- The distance stops at the first table row whose minimum exceeds the bound.
- The built-in candidate table is built once per catalog MRO instead of once per miss.

`Audio::Hydrogen::Song.from-xml` went from 25.7G to 16.7G instructions, and `t/030-song.t`
from ~4.5 s to ~3.6 s on a release build (rakudo: ~0.66 s; the rest is the generic method-call floor).
