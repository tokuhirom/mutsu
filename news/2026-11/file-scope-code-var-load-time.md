# A module's file-scope `my &f` keeps a load-time assignment

A package-less module file that declared `my &f;` and filled it from a routine
its body calls at load time (the JSON::Native shape) left `&f` undefined: the
declaration was never boxed into a shared cell for `&` names, so the routine's
write died with its call frame. `&` code variables are now boxed like scalars
when a nested named sub writes them (#16824).
