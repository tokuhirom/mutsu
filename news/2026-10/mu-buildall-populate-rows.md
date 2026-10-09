# Mu.BUILDALL and Mu.POPULATE are method-table rows

The base candidate a user `BUILDALL`/`POPULATE` override reaches through `callsame`/`nextsame`/`nextwith` is now a row on
`Mu`, as in Rakudo, instead of an inline match arm (ADR-11276 §9.47, part of #12423).
