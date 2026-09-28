# Preserve type objects in parameterized role method calls

Calling a method on a parameterized role now dispatches through its punned
class as a type object, even when the role does not define `new`. This lets
methods with a `::?CLASS:U:` invocant run and keeps `self.defined` false, as in
Rakudo's BinaryTree example.
