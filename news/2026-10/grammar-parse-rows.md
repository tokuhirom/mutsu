# Grammar.parse, subparse and parsefile are method-table rows

`Grammar` joins the Rakudo method-table oracle snapshot and owns rows for the three parse entry points. A user grammar's
`method parse` that defers with `callsame`/`nextsame`/`nextwith` reaches the row, which shares `dispatch_instance_parse` with
the direct call (ADR-11276 §9.46, part of #12423).
