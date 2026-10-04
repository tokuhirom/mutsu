# A missing dynamic variable read in a TRIR routine is a Failure

`sub f { $*x }; say f().WHAT` printed `(Nil)` whenever the routine body was
lowered to TRIR (for example under the RakuAST frontend, which drops the
`SetLine` statements that otherwise keep a sub on the VM path). `TrOp::LoadDynamic`
now answers the same lazy `X::Dynamic::NotFound` Failure as the VM's `GetGlobal`
read when the name is declared nowhere in the dynamic scope, and also consults
the `PROCESS::` dynamics before giving up. Builtin dynamic variables still read
as before. Regression test: `t/vm/scope/dynamic-var-missing-failure.t` (#11728).
