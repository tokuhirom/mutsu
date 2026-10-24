# `Rakudo::Internals::JSON.to-json` takes a Pair held in a variable as its subject

The native `to-json` / `from-json` argument scan read the positional-Pair flavour (`ValuePair`)
as a named option, so `to-json($pair, :!pretty)` returned `null` and a forwarded call died with
"Unexpected named argument 'pretty'". Only the call-site `Pair` flavour is an option now
(ADR-0021). Pakku's `config` suite writes its options through exactly that call (mutsu#14870).
