# A candidate family's declared-type index is remembered for user classes

Ranking a multi-method family of 16 or more candidates builds an O(n^2) declared-type index
(`declared_index::build`), and it was only remembered when every conformance answer was static, so
Red's ~370-candidate families were re-ranked on every `^create`. The index is now also remembered
under the registry stamp, the per-name lexical-env checks the fast path answered from, and the
pairs the fast path declined (re-asked through the general walk on each reuse), so a hit is exact
by construction. Red `Bla.^create` (callgrind slope N=5 -> 15, profiling build, warm): 570M -> 482M
Ir per create (-15.5%). `t/38-equal.rakutest` is not re-measured; the 120 s goal is not met.
