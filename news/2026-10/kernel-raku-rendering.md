# `Kernel.raku` renders the instance's attributes in Rakudo's order

`Kernel.raku` was a fixed format string that ignored every attribute but `name`
(`Kernel.new(release => Str, ...)`), so `Kernel.new(:release<r>).raku` still showed `Str`. The
row is gone: `Kernel` is rendered like any other instance, from its declared attributes
(`name, version, hardware, arch, bits, release, desc, signature`, Rakudo's order), and an unset
`desc` is the `Str` type object as in Rakudo (#14552).
