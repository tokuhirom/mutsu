# A regex scan skips starts inside a failed leading run, and enters the engine once

An unanchored match such as `/ \w+ \s \d ** 6 /` over a long subject used to
enter the compiled regex engine once per start position, give back one
character of `\w+` at a time to retry `\s` against a word character, and
re-test every start inside a word that had already failed from its first
character. Three changes remove that work without changing any match:

- **One scan entry.** `rx_scan_first` loops the candidate start positions
  inside one engine entry: the LTM/ignoremark guards, the program lookup and
  the scratch state are taken once per scan instead of once per position.
- **Atom runs in one loop, futile give-backs pruned.** `X+` looks up the atom's
  ASCII set once per run, and a run directly followed by a one-grapheme atom
  does not give back to positions that atom provably rejects (the grapheme
  there is one printable ASCII character outside its set).
- **Failed leading runs are skipped.** For a program shaped `X{min,} tail`
  (`min >= 1`, no upper bound, `tail` only one-grapheme atoms and the end of
  the subject), a failed start makes every start at an iteration boundary of
  its run fail too, so the scan does not try them.

Measured with callgrind on the release-optimised `profiling` build, second
run, over a 40,937-character subject (`tmp/scan.raku`, scan cost = run with
the match minus run without it): 52.64M instructions before, 16.33M after
(1,286 to 399 per subject character). The issue's close condition is 250 per
character, so #10315 stays open; what remains is the prefilter's per-position
iteration and the engine's per-entry setup.
