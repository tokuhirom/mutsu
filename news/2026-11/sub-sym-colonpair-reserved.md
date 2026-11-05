# `sub meow:sym<bar> {}` is X::Syntax::Reserved from 6.d on

The `:sym<>` colonpair of a plain (non-operator) subroutine name is reserved for
the grammar multi-dispatch convention from language version 6.d on, and rakudo
refuses it with `X::Syntax::Reserved` ("The :sym<> colonpair is reserved").
mutsu accepted it silently. A `sub` declaration whose name carries `:sym<...>` or
`:sym«...»` (in any position among its colonpairs) now raises it; operator
categories (`infix:sym<foo>`), grammar tokens and other colonpairs are unchanged.
This is the first of `roast/MISC/misc.t`'s four failing subtests (#16412).
