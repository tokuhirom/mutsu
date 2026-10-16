# A built-in iterator's protocol methods are rows of Iterator and PredictiveIterator

`pull-one`, `push-exactly`, `push-at-least`, `push-all`, `push-until-lazy`, `sink-all`, `skip-one`, `skip-at-least`,
`skip-at-least-pull-one`, `count-only` and `bool-only` of a built-in iterator are now rows of the `Iterator` and
`PredictiveIterator` roles (ADR-11276 §9.126), with one implementation instead of three name-matching copies (a
variable receiver, an array element or temporary, and the VM's fast path). The iterator's state lives in its shared
attribute cell, so the rows need no receiver place. `.can` answers what the iterator's kind does, as in Rakudo: a
`.map` stream, a `squish` scan with callbacks, a `gather` and an unending range are not `PredictiveIterator`s. A
`squish` scan held in an array element now steps instead of answering `IterationEnd`.
