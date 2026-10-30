# `emit` in a sub a `supply` block calls works for `Supply.interval` and `watch` sources

`supply { sub add($d) { whenever $d.watch { emit $_ } }; add($dir) }` died with "emit without supply or
react" on a worker thread (and `Supply.interval` likewise): the parser rewrites `emit` to the block's emitter
only where it is written directly in the body, and a nested sub finds the emitter dynamically, through the
stamp `whenever` puts on its callback. A Supplier source dispatched through `call_supply_tap`, which makes the
stamped emitter active; the act loop that runs native sources' callbacks called the body directly. It now
makes the stamped emitter active too (`call_sub_value_with_stamped_emitter`).

cro's `Cro::Tools::Services.services` (`watch-recursive`) is built this way, so a service directory created
after startup is now delivered.
