# `temp (@a)[0] = v` temporizes the element

`temp` followed by a parenthesized container and a subscript, such as
`temp (@a)[0] = 99` or `temp (%h)<k> = 5`, used to fall out of the `temp`
statement parser and come back as a call of an unknown function `temp`. The
parser now treats parentheses around the container as transparent and lowers
the statement through the same compound-element path that
`temp $s[1]<k>[1] = v` uses, so the element is saved and restored at scope
exit as in rakudo. The bare form with no assignment (`temp (@a)[0];`,
`temp @n[0][1];`) is accepted too; the multi-level spelling of it used to be
a parse error (#10581).
