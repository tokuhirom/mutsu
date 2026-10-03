# `.^get_attribute_for_usage` and `self.Role::new` keep the class invocant

ClassX::StrictConstructor makes a class's constructor reject unknown named
arguments. It needed two fixes, and all five of its test files now pass.

- **`.^get_attribute_for_usage($name)`** is implemented. It returns the type's
  own `Attribute` of that full name (`$!a`, `@!list`), and dies with
  "No $name attribute in Type" for anything else, parents' attributes
  included, as Rakudo's `Metamodel::AttributeContainer` does.
  `.^attribute_table` now shares the same lookup.
- **A qualified call to a role's `new`** (`self.R::new(|%attrs)` from a
  class's own `new`) runs that method with the class as the invocant. mutsu
  used to construct the punned role instead, so `self` inside it was the
  role, and a `nextsame` there re-entered the class's `new` until the stack
  overflowed.

A remaining divergence, `nextsame` inside a qualified call continuing to the
next MRO candidate where Rakudo returns `Nil`, is tracked in #11592.
