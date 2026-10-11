# A wrap no longer leaks to a same-named routine

Wrapping `Pkg::f` runs the wrapper only for that routine. An unrelated routine
with the same bare name, in `GLOBAL`, another package or another lexical scope,
runs unwrapped, as in Rakudo (#11659). The behaviour came from the callee check
added with #11350; this pins it with a regression test.
