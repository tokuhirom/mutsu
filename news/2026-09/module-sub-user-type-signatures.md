# Module sub signatures keep their package-local user types

Ordinary subs declared inside a module now qualify bare user-defined parameter
and return types while the declaring package is still active.  An exported
`sub make-result(Str:D $value --> Result:D)` previously stored `Result` as a
global-looking name, so a call from the importing compilation unit rejected the
valid `ModuleSubUserType::Result` instance.  Both parameter binding and return
checking now preserve the module-qualified type identity.

Pinned by `t/modules/module-sub-user-type-signatures.t`, based on
`Pinterest::URL::Normalizer` 0.1.0.
