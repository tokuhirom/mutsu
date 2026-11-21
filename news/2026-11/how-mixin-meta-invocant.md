# `.^meth` keeps the instance after a role is mixed into a custom HOW

A user-defined metaclass receives the object itself from `$obj.^meth`
(`$obj.^save` reaches `save($obj)` with the instance), while the built-in
metaclasses get the type object. The check that tells the two apart looked
only at a plain HOW instance, so once a trait mixed a role into the
metaclass (`$model.HOW does MetamodelX::Red::GlobalNotifiable`, which turns
the HOW into a `Mixin`) every `.^meth` call on an instance passed the type
object instead. Red 0.2.16's `is global-notifiable` models died in
`^create` with "Cannot look up attributes in a GlobalItem type object"; the
check now looks through the mixin to the metaclass inside, and
`t/191-global-events.rakutest` passes 18/18 as on rakudo (#18408).
