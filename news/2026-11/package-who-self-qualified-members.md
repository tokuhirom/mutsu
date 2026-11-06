# `class M::c2` inside `module M` is the member `c2` of M's stash

Slice of #16819. A declaration qualified with the enclosing package's own name was listed as a
sub-package `M` (and, for an export, as `M::c3`) in `M.WHO` / `M::EXPORT::ALL.WHO`; rakudo lists the
plain member `c2` / `c3`. The stash builders now strip a head equal to the package's last component.
