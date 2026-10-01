# From Email::MessageID: `:$user = generate-key, :$header` must call the
# module-private sub, and the 6.e `nano` term must exist under `use v6.*`.
use Test;
use lib 't/lib';
use NoArgDefaultSub;

plan 4;

is make-id, "KEY@h", "no-arg private sub default followed by a comma is called";
is make-id(:header), "<KEY@h>", "default still applies with another named arg";
isa-ok stamp(), Int, "nano is an Int under use v6.*";
ok stamp() > 1_000_000_000_000_000_000, "nano is in nanoseconds since the epoch";
