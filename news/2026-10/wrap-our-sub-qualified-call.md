# A package-qualified call runs a wrapped routine's wrapper

`our sub c() { "c" }; &c.wrap(-> | { "wc" }); GLOBAL::c()` answered `c`. A
routine wrapped by a trait in a module (`O::marked()`) ran its unwrapped
body too (#11350). In Raku the lexical `&c` and the package entry are the
same `Routine`, and `.wrap` changes it in place, so every way of calling it
runs the wrapper.

The wrap is recorded under the routine's bare name. A call or term spelled
with a package qualifier now finds the wrap under the bare name when the
wrapped routine is declared in that package (`GLOBAL::` stands for the
top-level package).
