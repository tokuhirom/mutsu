# A backslash inside a `｢...｣` regex term no longer unterminates the regex

`｢...｣` is a raw `Q` string, so `｢\｣` is one literal backslash and `｣` always closes it.
The regex delimiter scanners and the quote-region tracker treated `\｣` as an escaped
closer, which ran the term to the end of the pattern (`Regex not terminated`). They now
scan `｢...｣` without escape handling, as the atom parser already did (#11569).
