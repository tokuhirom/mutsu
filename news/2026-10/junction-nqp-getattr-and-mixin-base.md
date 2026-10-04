# `nqp::getattr` on a Junction, and `.^mixin_base`

`MUGS::Games` (blocked_load: every `MUGS::Server::Game::*` module died with "Cannot unbox a type
object to str") validates config forms through `MUGS::Util::StructureValidator`, which reads a
Junction's `$!type` and `$!eigenstates` with `nqp::getattr` and delegates `Optional.ACCEPTS` to
`self.^mixin_base`. mutsu now answers both Junction attributes (also through a `but Role` mixin) and
implements the `.^mixin_base` meta-method. All 27 provided modules load and `t/00-use.rakutest`
passes.
