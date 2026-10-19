# String operators read an `Array`/`Hash` subclass's elements

`~`, `eq`, `lt` and `leg` on an instance of a user subclass of `Array`,
`Hash` or a QuantHash rendered the instance itself (`A<19>`) instead of its
elements, although `.Str` and prefix `~` answered correctly
(`class A is Array {}; my $a = A.new; $a.push(1); $a eq "1"` was `False`). The
string operators' operand coercion called `.Str` only for a class that
declares one; an instance backed by array, hash or QuantHash storage now
coerces through the `Str` its built-in ancestor declares (#14016).
