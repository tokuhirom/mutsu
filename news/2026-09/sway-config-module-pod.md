# Sway::Config modules receive their own Pod document

`Sway::Config::CLI` uses `Pod::Contents` on `$=pod` while the module is being
loaded. mutsu populated Pod variables for the main program but not for source
modules, so the CLI failed at load time when `$=pod` was `Nil`.

Loaded modules now establish Pod variables from their own source before their
mainline runs. The importer’s Pod document is restored afterward, including
through nested module loads.

Pinned by `t/modules/module-pod-variable.t`, with the fixture in
`t/lib/EcosystemSwayConfigPod.rakumod`.

`Sway::Config` 0.2.2 was locked on
[#7884](https://github.com/tokuhirom/mutsu/issues/7884) and moves from
`blocked_load` to `green`: all five provided modules load and its only
Rakudo-baseline file passes 1/1 under mutsu. The other two distribution tests
remain `no_baseline` because Rakudo does not pass them.
