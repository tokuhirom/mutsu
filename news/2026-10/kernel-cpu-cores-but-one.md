# `Kernel.cpu-cores-but-one`

Added the undocumented rakudo `is-implementation-detail` method
`Kernel.cpu-cores-but-one` (`max(1, cpu-cores - 1)`), which the `ParaSeq`
distribution calls at load time, so `use ParaSeq` no longer dies with "No such
method 'cpu-cores-but-one'". This is a per-issue maintainer approval (#15315),
not a general rule for adding undocumented rakudo methods.
