use Test;

plan 10;

# An attribute initializer that can NEVER satisfy the constraint — a *defined*
# value of the wrong type, or any defined value for a `:U` — is rejected when
# the class is declared, as X::TypeCheck::Attribute::Default.

throws-like 'class AD1 { has Int $.n = "str" }', X::TypeCheck::Attribute::Default,
    message => q{Can never assign default value Str ("str") to attribute '$!n', it expects: Int},
    'a defined default of the wrong type is rejected at declaration';

throws-like 'class AD2 { has Int:D $.n = "str" }', X::TypeCheck::Attribute::Default,
    message => q{Can never assign default value Str ("str") to attribute '$!n', it expects: Int:D},
    'the smiley is part of the expected type';

throws-like 'class AD3 { has Int:U $.n = 5 }', X::TypeCheck::Attribute::Default,
    message => q{Can never assign default value Int (5) to attribute '$!n', it expects: Int:U},
    'a defined default can never satisfy :U';

# A default that is a TYPE OBJECT is a runtime assignment failure instead:
# rakudo cannot rule it out at declaration time, so `.new` raises
# X::TypeCheck::Assignment.

class BD1 { has Int:D $.n = Int }
throws-like { BD1.new }, X::TypeCheck::Assignment,
    message => 'Type check failed in assignment to $!n; expected Int:D but got Int (Int) (perhaps Nil was assigned to a :D which had no default?)',
    'a type-object default under :D fails at construction, with the "perhaps Nil" hint';

class BD2 { has Str:D $.n = Int }
throws-like { BD2.new }, X::TypeCheck::Assignment,
    message => 'Type check failed in assignment to $!n; expected Str:D but got Int (Int)',
    'a type object of an unrelated type fails without the hint';

# `Nil` assigned to a typed attribute resets it to the type object, exactly as
# it does for a typed variable — so the reported `got` is the declared type.
class BD3 { has Str:D $.n = Nil }
throws-like { BD3.new }, X::TypeCheck::Assignment,
    message => 'Type check failed in assignment to $!n; expected Str:D but got Str (Str) (perhaps Nil was assigned to a :D which had no default?)',
    'a Nil default under :D reports the type object it reset to';

class BD4 { has Str $.n = Nil }
is BD4.new.n.^name, 'Str', 'a Nil default on a plain typed attribute reads as the type object';

class BD5 { has $.m = Nil }
is BD5.new.m.^name, 'Any', 'and as Any when the attribute is untyped';

# Defaults that DO satisfy their constraint keep working.
class OK1 { has Int:D $.n = 5 }
is OK1.new.n, 5, 'a satisfying :D default is untouched';

class OK2 { has Int:U $.n = Int }
is OK2.new.n.^name, 'Int', 'a type-object default satisfies :U';
