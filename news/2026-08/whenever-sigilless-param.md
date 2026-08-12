# whenever accepts a sigilless pointy param (Text::CSV runtime sweep round 8)

`whenever $source -> \row { ... }` never delivered a single event: the
`whenever` statement parser accepted only sigiled pointy params (`-> $x`,
with an optional type constraint) and type-only blocks (`-> Int { }`). A
sigilless param made the whole statement fail to parse, so it silently
fragmented into a bare `whenever` word plus a standalone pointy-block
expression statement — the subscription never registered, and the
enclosing `react` completed with zero events (no error anywhere).

Text::CSV's Supply and Channel in-format loops are exactly this shape:

    react {
        whenever $in -> \row {
            @in.push: row ~~ Str ?? $[ self.getline (row) ] !! row;
            LAST { done; }
            }
        }

so `csv(in => $channel)` and `csv(in => $supply)` returned empty row sets
(90_csv.t tests 34-36). The parser now binds `\name` as the whenever
parameter — the same bare-name env key a sigilless read resolves — and the
existing react machinery (including the direct-Channel subscription source
with its drain-until-closed polling) does the rest.

90_csv.t: the whole first (Class) scope now passes, 36/36 up from 33/36.
The file still aborts at the second (Method) scope — a separate,
pre-existing env-map leak: after the Channel iteration, a closure reading
the test's file-scope `@in` by name sees the CSV method's same-named
internal `@in` rows while the file-scope slot itself stays correct
(dual-store divergence; the next slice).

Pin: `t/whenever-sigilless-param.t` (4 assertions, raku-verified).
