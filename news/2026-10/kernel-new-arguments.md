# `Kernel.new(...)` honours its constructor arguments

`Kernel.new(:name<k>, :release<r>, ...)` ignored every argument and answered the running
kernel. As `Distro.new` does since #14171, it now takes its arguments over the attributes the
process gives it (#14552). The attribute order that `Kernel.raku` renders still differs from
Rakudo's; that is tracked on the issue.
