use Test;

plan 6;

multi sub choose(Int $x) { "int $x" }
multi sub choose(Str $x) { "str $x" }

is choose(42), "int 42", "multi dispatch prefers Int candidate";
is choose("hi"), "str hi", "multi dispatch prefers Str candidate";

sub single(Int $x;; Str $y) { $x ~ ":" ~ $y }
is single(7, "days"), "7:days", "single subs accept signatures with ;;";

multi sub classify(Int $tag;; Str $payload) { "str payload" }
multi sub classify(Int $tag;; Int $payload) { "int payload" }
is classify(1, 99), "int payload", "types after ;; still filter applicable candidates";

multi sub ambiguous-after-sep(;; Any $value) { "any" }
multi sub ambiguous-after-sep(;; Int $value) { "int" }
dies-ok { ambiguous-after-sep(42) }, "types after ;; do not make one candidate narrower";

proto sub describe(|) {*}
multi sub describe(Int $value) { $value + 1 }
multi sub describe(Str $value) { $value ~ "!" }
is describe("ok"), "ok!", "bodyless proto defaults to {*} dispatch";
