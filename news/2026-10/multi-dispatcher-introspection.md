# A multi dispatcher introspects as its proto

`&f.signature`, `.arity` and `.count` on a multi sub's dispatcher used to
answer its candidates' signatures as an `any(...)` Junction (or the first
candidate's arity); they now answer the proto's -- the declared one, or the
generated `(;; Mu |)` of a protoless multi -- exactly as `.raku` already did.
A package-qualified `&Pkg::proto` handle now finds its candidates through
`.cando` and reports `Pkg` as its `.package`, and two dispatchers whose
`.raku` agree (`&A::p eqv &B::p` for two `proto sub p(|)`) are `eqv`, following
Rakudo's `Any:D eqv Any:D` rule. A typed capture parameter (`Mu |c`) now shows
its type in a signature's `.raku` (#10707).
