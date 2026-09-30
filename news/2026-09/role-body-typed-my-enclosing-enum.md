# Role body `my Level $x` resolves types through the role's package

A typed `my` statement in a role body (`my Level $level` with `enum Level` declared in the
class that encloses the role) was rejected at composition time with "Type 'Level' is not declared",
because plain role-body statements run under the composing class's package. The check now also
resolves the short name through the role's own package chain. Found via `App::RakuCron` (which
loads `Lumberjack::Logger`); pinned by `t/oo/role/role-body-enclosing-enum-type.t`.
