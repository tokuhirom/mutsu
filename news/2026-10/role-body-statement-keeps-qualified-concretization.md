# A run-time statement in a parametric role body no longer changes `self.Role::method`

`self.P1::of-type` resolved to the `T` of `does P2[Num]` instead of `does P1[Int]` as soon as `P2`'s
body held a bare run-time statement. The role-body composition rework (#12740) made both spellings
resolve to `P1[Int]`; `t/oo/role/role-body-statement-keeps-qualified-concretization.t` now pins that
(#10679).
