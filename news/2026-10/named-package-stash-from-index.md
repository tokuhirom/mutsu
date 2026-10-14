# A named package's stash no longer scans the whole program

`P::`, `P::.keys`, an unsigiled stash key and `.WHO` built a named
package's stash by walking the whole env, the whole `our` store and the
whole routine registry, then keeping the keys that named `P`. With many
lexicals or package variables elsewhere in the program, each such read paid
for all of them (#9171).

The members of a named package are qualified names that mention it, so the
build now visits only the names the package index lists for it
(`qualified_tail_index::names_under_package`, the index the package-existence
check already used). The enum-key namespace's spelling of those names is
included as well. Each candidate is still judged by the same member rules, so
the stash comes out the same. `GLOBAL::` and the pseudo-packages, whose
members are bare names, still scan everything.

300 reads of `P::.keys` (release build) with 1000 / 2000 lexicals and package
variables elsewhere in the program went from 0.43 / 0.76 s to 0.07 / 0.11 s.
For a package with no routine member the time no longer depends on them at
all (0.07 / 0.04 s).

A routine member's code value is still built per read, and that build
captures the env the read runs in (#9170).
