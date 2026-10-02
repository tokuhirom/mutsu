# Strict force of an infinite closure sequence or triangle reduce throws

`(1, {$_ + 1} ... *).eager.elems` answered `33` and `([\+] 1..*).eager.elems`
answered `200000`: a strict force of an endpoint-less closure sequence handed
back whatever prefix had been generated so far, and a strict force of a scan
stopped at a 200000-element cap, both as if that were the whole list (#10861).

Both now answer `X::Cannot::Lazy`, the verdict an infinite map/grep pipe and an
infinite sequence spec (#10846) already reach. A closure sequence still forces
completely when its generator ends it (`1, { last if $_ >= 5; $_ + 1 } ... *`),
within the same one-million-element attempt the pipe force makes, and a scan
over a finite lazy source scans that source to its end.

The front mutators keep a lazy `@`-array over either shape lazy, through the
prefix-stitching path #10846 introduced for sequence specs: `my @a = 1, {$_+1}
... *; @a.shift` leaves `@a.is-lazy` True and `@a.elems` throwing, as in
Rakudo. A closure sequence's generator keeps reading its own true history
rather than the mutated array, so `my @f = 1, 1, * + * ... *; @f.shift;
@f.unshift(100)` still continues `2, 3, 5, 8, ...`. To allow that, the closure
sequence's cache and its generator history no longer have to be the same
length, and a scan walks its source from its own position rather than from the
cache length.
