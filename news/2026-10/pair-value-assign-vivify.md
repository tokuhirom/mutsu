# A Pair over a not-yet-existing element sees its own `.value =` write

`my $p = thread => @threads[$++]; $p.value = 5` created `@threads[0]` but the
Pair kept reading the deferred vivification token as `Any`, because the write
stored a plain value where the token only reads through a `ContainerRef` cell.
The first write through a deferred token now materializes the slot as a shared
cell (the same primitive `my $r := @a[5]; $r = 1` uses), so the Pair and the
container stay connected. This is what the `MoarVM::Remote` thread-lifecycle
suite needed (#11890).
