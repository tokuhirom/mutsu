# The fast method-call path masks a spawning callee's aggregates on return

`call_compiled_method_fast` had no `ThreadParamMaskGuard`, so a method that declared
`my %pars` / `my @pars` and spawned a thread left its entry in the shared bare-name lane
answering the caller's same-named variable after the call (`K.new.m; %pars<z> = 1`
gave the callee's keys). The fast path now installs the same guard as the general method
path and the sub paths (#15156, #16345). Part of #15020.
