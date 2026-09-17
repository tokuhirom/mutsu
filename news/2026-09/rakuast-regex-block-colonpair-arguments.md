# Block-valued regex colonpairs keep their callable value

Argumented regex subrules now preserve an expression-only block-valued
colonpair as `RakuAST::ColonPair::Value` with a direct `RakuAST::Block` value.
The matcher also keeps that block callable while the subrule's code assertion
runs, including its captured outer lexicals.

Hash-composer bodies and other complex block forms remain explicit boundaries.
