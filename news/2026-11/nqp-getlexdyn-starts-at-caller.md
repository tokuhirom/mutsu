# nqp::getlexdyn starts at the caller again

`nqp::getlexdyn('$*x')` searches from the caller of the running frame, as
MoarVM's op does. Since a scope's `my $*x` is bound from the scope's entry
(#19174), a routine that read a caller's flag through `nqp::getlexdyn` before
declaring its own `$*x` saw its own unset declaration instead. EEL's slang
overrides `arglist` exactly that way, so `<name: args>` calls in a `pattern`
stopped ending at `>` and EEL 0.3 regressed from green to 31/36 files (#19184).
When the running frame binds the name, the op now resumes the search above the
frame.
