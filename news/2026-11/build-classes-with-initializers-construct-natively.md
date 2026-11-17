# A class with BUILD and attribute initializers is constructed natively

The native default constructor (`build_native_default_instance`) used to give
up on any class that has a `submethod BUILD` and an attribute initializer the
caller did not supply. Raku applies such an initializer only after BUILD, and
only when BUILD left the attribute alone, and the native path did not
implement that order. So `class C { has $.id; has $!l = 5; submethod BUILD(:$id) { $!id = $id } }`
fell through to the full `dispatch_new` cascade on every `.new`: some 30
probes for role puns, native bases and IO/Date/Range/Match subclasses before
it reached the ordinary user-class path.

The native path now seeds those attributes, runs BUILD under the write
frame, and applies the deferred initializers through the same
`apply_post_build_attr_defaults` the full constructor uses. The pre-BUILD
`where`/smiley checks skip deferred attributes in both paths through one
shared helper (`enforce_pre_build_attr_constraints`).

Two memos keep the deferral cheap. A seed default (the type object the parser
gives `has Str $.s`) is read from the plan's `seed_defaults` after BUILD
instead of being evaluated again, and when the memo equals the seed the
attribute is not deferred at all. The pre-BUILD seed of a `$` attribute is
memoized per class shape in `NativeCtorPlan::deferred_seeds`.

Measured with callgrind on a profiling build (second run of each binary,
slope between 300 and 600 constructions):

| | main | this change |
| --- | ---: | ---: |
| `C.new(id => 1)` above, Ir per construction | 134,600 | 45,100 |
| LLM::Chat `Response.new` + `emit` + `done` + `is-done`, Ir per round | 1,571,000 | 1,396,000 |

The issue script of #17361 (1000 rounds of `Response.new` + `start`) went from
0.95 s to 0.67 s locally; rakudo takes 0.20 s, so #17361 stays open.
