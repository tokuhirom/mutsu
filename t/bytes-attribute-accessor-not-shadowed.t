use v6;
use Test;

# `.bytes` is a Cool-only builtin (ADR-0051 P4): a plain Any-derived class
# does not resolve it at all in real Rakudo. Before this fix mutsu instead
# had a target-agnostic native `.bytes` fast path (methods_0arg dispatch)
# that intercepted the call *before* checking whether the receiver's own
# class declares a public `has $.bytes` accessor, silently shadowing it
# with `target.to_string_value().len()` -- discovered while bundling the
# UUID module (github:retupmoca), whose `class UUID { has $.bytes; ... }`
# came away with `$.bytes` always reading as the byte-length of the
# object's default gist instead of the buf8 it was constructed with.

plan 6;

class Plain { }
throws-like { Plain.new.bytes }, Exception, message => /bytes/,
    'a plain class with no `bytes` accessor does not resolve .bytes (matches raku)';

class HasBytes {
    has $.bytes;
}
my $h = HasBytes.new(:bytes(42));
is $h.bytes.WHAT, Int, 'a user accessor named `bytes` wins: .WHAT is Int, not shadowed';
is $h.bytes, 42, 'a user accessor named `bytes` reads back the value it was constructed with';

my $buf = buf8.new(1, 2, 3, 4, 5);
class HoldsBuf {
    has $.bytes;
    method new(:$bytes) { self.bless(:$bytes) }
}
my $u = HoldsBuf.new(:bytes($buf));
isa-ok $u.bytes, Buf, 'a `bytes` accessor holding a real Buf still reads back the Buf';
is $u.bytes.elems, 5, 'the Buf value is intact, not collapsed to a byte count';

# Real Buf/Blob .bytes (the native fast path this bug's fix must not break)
# still resolves natively.
is buf8.new(1, 2, 3).bytes, 3, 'buf8.bytes (the real native method) is unaffected';
