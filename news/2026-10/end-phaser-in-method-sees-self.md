# An `END` declared in a method sees the method's `self`

Event::Emitter::Channel has `submethod BUILD { $!channel := Channel.new; ...; END { $!channel.close;
await $!promise } }`. The `END` closes over the invocant and runs at exit, when no method frame is
left, so mutsu died with "Variable $!channel used where no 'self' is available" for every use of the
module's threaded emitter. `self` is the frame record's alone (ADR-14843), not an env entry, so
capturing the env did not carry it. An `END` phaser now keeps the declaring frame's record and the exit
run installs it around the body, as a helper that runs code outside any frame does.
