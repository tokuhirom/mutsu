# `copy(%?RESOURCES<file>, $dest)` copies the resource's file

The routine form of `copy` stringified its arguments, so a `Distribution::Resource`
became "Distribution::Resource<N>" and the copy failed. It now reads IO::Path-like
arguments (a resource included) for the file they name, as `slurp` does.
XDG::GuaranteedResources' `guarantee-resource` relies on it; its `t/01-basic` matches rakudo.
