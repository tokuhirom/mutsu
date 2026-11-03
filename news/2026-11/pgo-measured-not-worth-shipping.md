# PGO measured on the Mooish loop: at most -15%, not shipped

ADR-15737 D3 (profile-guided optimization) was the one lever aimed at the instruction-speed factor
of the AttrX::Mooish gap (#15688). Built with `-Cprofile-generate`, trained on the benchmarks and a
sample of `t/`, and rebuilt with `-Cprofile-use`, the loop was unchanged when held out of the
training set and 15% faster when it was in it. That is below the ADR's "ships only on a measured
gain" bar, so the release pipeline is untouched. The setup, numbers and caveats are in
`docs/pgo-experiment.md` so the experiment is not repeated.
