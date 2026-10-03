# `.are` walks a user iterator; `$obj[$i] = $v` yields ASSIGN-POS's result

Two fixes from Functional::LinkedList (an immutable linked list built on
ValueClass):

- `.are` judged a class with its own `iterator` (a `does Positional` linked
  list) as a single item, so ValueClass's `data.are !~~ $attr.type.of` check
  rejected every list node: "Value (Functional::LinkedList<...>) does not
  pass the type constraint". It now walks the iterator, as rakudo's
  `Any.are` does.
- The value of `$obj[$i] = $v` / `$obj{$k} = $v` on a class with its own
  `ASSIGN-POS` / `ASSIGN-KEY` is now what that method returns, so
  `my ($new-list, $value) = $list[0] = 1` receives the method's pair.

The distribution's test file goes from dying at test 3 to 55 of 60; the rest
need a bare block's implicit `$_` to alias its caller's container on reads
(#11298).
