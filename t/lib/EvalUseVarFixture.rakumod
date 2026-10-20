# An exported variable and constant, for t/vm/scope/eval-toplevel-use-binds-lexically.t.
unit module EvalUseVarFixture;
our $vx is export = 'vx-val';
constant vk is export = 'vk-val';
