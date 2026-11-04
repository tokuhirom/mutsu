# `augment` freezes subclass dispatchers by declaration order

An `augment` froze the dispatcher of every user subclass that declared its own
`multi` candidates of the method and existed when the statement ran, which
included a subclass declared textually AFTER the augment: mutsu registers a
hoisted shell for every class before any statement runs, so the later class was
already there. Rakudo composes in source order, so that subclass sees the
augmented candidate ([#16314](https://github.com/tokuhirom/mutsu/issues/16314),
found fixing #16049).

The class's real declaration registers it again at its source position. That
registration now drops the freeze an earlier augment recorded against the shell
(`Registry::frozen_dispatchers`), so only a subclass composed before the augment
stays frozen. `class E is A { multi method m(Rat $x) {...} }` declared after
`augment class A { multi method m(Str $x) {...} }` answers `A-Str` for
`E.new.m("s")`, as `raku` does, while a subclass declared before it still
answers `A-Any`.
Regression test: `t/oo/class/augment-multi-subclass-dispatcher.t`.
