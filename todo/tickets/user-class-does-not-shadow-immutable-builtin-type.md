# A user class named after an immutable builtin type is treated as that builtin

Declaring `class Map is Hash { }` (or `class Set is Hash { }`, `class Bag …`,
…) gives a class whose *name* collides with a builtin type. In Raku the
declaration shadows the builtin for the rest of the lexical scope, so instances
behave like the user's class. Under mutsu the immutability checks still consult
the builtin by name, so an element assignment through such an instance throws.

## Repro

```raku
class Map is Hash { }
my $m = Map.new;
$m<a> = 1;                 # raku: ${:a(1)}
say $m.raku;               # mutsu: "Cannot modify an immutable Map (Map)"
```

`class Set is Hash { }` fails the same way ("Cannot modify an immutable value
(Set)"). A user class whose name collides with a *mutable* builtin is fine —
`class Pair { has $.x }` works — so the trigger is specifically the
"is this value an immutable builtin?" check, which is name-based and does not
ask whether the name currently resolves to a user-declared class.

## Where to look

The RO/immutability guard on element assignment. Start from the message text
("Cannot modify an immutable ...") and make the check consult the resolved
declaration for the name (a user class registered in the current package/lexical
scope wins) rather than matching the class-name string against the builtin
QuantHash/Map set.

## How it was found

Writing `t/attr-subscript-assignment.t`. The Template::Mustache `Logger` shape it
pins declares `class LoggersMap is Hash does Associative[…] { }`; naming the test
class `Map` instead — the natural short name — made the whole file abort. The
test uses `LoggerMap` to stay on topic, so nothing currently pins this.
