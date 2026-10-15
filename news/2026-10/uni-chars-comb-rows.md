# `NFC.chars`, `NFC.comb` and `NFC.perl` answer from the `Uni` rows

The normalization-form strings (`NFC`, `NFD`, `NFKC`, `NFKD`) answered `.chars`,
`.comb` and `.perl` through name-matching arms of the zero-argument cascade. The
first two are now rows of `Uni` (roast pins them, though current Rakudo no
longer declares them), and `.perl` renders through the `Uni.raku` row
(ADR-11276 §9.116).
