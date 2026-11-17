# The implicit MAIN dispatch runs a `proto MAIN` body

mutsu's command-line dispatch picked the matching `MAIN` candidate and called it directly, so a
`proto MAIN` with a body of its own never ran. Sway::Config's `bin/sway-config` sets its `$config` in
that body before `{*}`, and `sway-config configs -i` died "Cannot look up attributes in a
Sway::Config type object". The dispatch now runs a non-trivial proto through the VM
(`vm_try_run_nontrivial_proto_body`, which closes over the module's lexicals the way an ordinary call
does), and its `{*}` reaches the candidate, as rakudo's `RUN-MAIN` calling `&MAIN` does.
Sway::Config's `t/02-sway-config-cli.rakutest` "Prints config using STDIN" now passes as on rakudo.
Closes #17849.
