# Class-scoped enum members no longer poison each other

Enums declared in separate class bodies may use the same member spelling. Mutsu
now keeps each class body's enum-alias scope separate and resolves a method's
bare member through its declaring class, matching Rakudo. This was found while
measuring `Lumberjack::Dispatcher::Syslog`.

The distribution's `t/020-dispatcher.t` proceeds past its former poisoned
`Error` alias failure. Its remaining role-method lexical-type failure is tracked
in #8565.
