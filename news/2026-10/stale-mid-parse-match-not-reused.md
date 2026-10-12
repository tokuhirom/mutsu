# A Match kept in mid-parse no longer breaks the grammar's actions

Since one lazy Match per capture node (ADR-11404), the finished parse tree reused a Match that
user code had read and kept during the parse -- `{ @keep.push: $<num> }` in a token, or a
`.wrap`ped token's wrapper keeping its `callsame` result. That Match was materialized before the
parse stamped its grammar class on the run, so in the action walk it read as neither a plain
`Match` nor a grammar cursor: the action's `$/` printed as `G.new` and `make` died with "The make
function expects $/ to contain a Match". The memo now rejects a node forced under a cursor class
other than the run's current one (`MatchNode::is_current`), and the finished tree builds a fresh
Match for it. Stamping the class at parse start, which would let that node be shared as in
rakudo, is #12943.
