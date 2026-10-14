# Reading a Seq one subscript at a time is linear again

`my $s = (1..$n).map(* + 1); $t += $s[$_] for ^$n` was quadratic: 0.91 s
for 20,000 elements and 3.51 s for 40,000 on a release build (#13023). It had
two causes.

- Every subscript copied the whole Seq into a List before it indexed it. A
  single non-negative position is now read in place, which answers the same
  thing: a bound cell's value, and `Nil` past the end.
- Every prefix pull (`pull_prefix`, `extend_from_iterator`,
  `extend_map_grep_prefix`, and the first full read of a `Str` cursor) copied
  the prefix already pulled into a new generation, and the graveyard kept all
  of them. A generation is now an append-only buffer. A pull writes the new
  elements past the length readers can see and then publishes the longer
  length, and it starts a new buffer of twice the capacity only when the
  current one is full. Earlier borrows still see their own prefix, untouched.

The same loop now takes 0.04 s for 20,000 elements, 0.05 s for 40,000 and
0.22 s for 160,000. Its peak RSS (42.7 MB) matches a run that reifies the Seq
first (43.2 MB). How far a subscript pulls a `.map` Seq (all of it, where
rakudo pulls only up to the index) is tracked separately as #13166.
