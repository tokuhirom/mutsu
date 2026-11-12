# Every Hash view of a Capture subclass's `%!hash` shares one store

EEL 0.3's `Event is Capture` writes its captures through a fresh `Hash`
built over the event's `%!hash` each time
(`my $hash := nqp::create(Hash); nqp::bindattr(nqp::decont($hash), Map, '$!storage', nqp::getattr(self, Capture, '%!hash'))`).
A fresh wrapper already adopted a storage held in a variable as its own node,
but the inline `nqp::getattr` form still copied the storage, so a Hash built
by an earlier call kept an old copy and missed writes made through a later
one. The fresh-wrapper rule now takes any storage expression. Every view, and
the capture, name the one store, as rakudo's shared VMHash does
(ADR-12666 §11, #17413).
