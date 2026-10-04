# `nqp::iscoderef` recognizes direct code bodies

`nqp::iscoderef` now distinguishes a high-level `Sub` from its executable
`Code.$!do` body, matching Rakudo. Direct code identity is stored on the code
object and shared with wrap-chain dispatch, replacing the former environment
marker. The remaining representation-dependent NQP ops are still tracked by
#11553.
