# A role body can capture the native method of the value it is mixed into

`my &AT-KEY := ::?CLASS.^find_method('AT-KEY')` in a role body, with the role
mixed into a hash (`my %h does R`), captured the role's own `AT-KEY`, and
calling it from the override recursed forever. `::?CLASS` in the body of a
role applied to a value is now that value's base type, and a method object of
a core type called on a mixed-in invocant runs the native base instead of
re-dispatching through the override. WriteOnceHash 0.0.9 now passes 35 of 37
tests (#15403).
