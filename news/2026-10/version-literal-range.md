# `v1..v2` is a Range of Versions, and a Version smartmatches it by Version order

The version-literal scanner ate `v1..v2` as one malformed version, and a Range smartmatch always
compared its endpoints numerically, so `v0.1 ~~ v1..v2` was True. A version literal now ends where
`..` begins, and a Version value matched against a Range with Version endpoints is ordered with the
`Version` comparison (open ends and exclusive ends included). Pakku::Spec's `:ver(v1..v2)` rows
depend on it (mutsu#14870).
