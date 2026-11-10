# `try` yields Any for a Failure it caught

`my $x = try %*UNDECLARED;` (and `try { $*UNDECLARED }`) left `$x` holding the
lookup's handled Failure, so `$x.DEFINITE` was True. A real `try` now yields an
undefined `Any` for a Failure result, as it does for a caught exception. Found via the
Red distribution (`Red::Compiler::Template.path`, `t/093-template-reuse.rakutest`).
