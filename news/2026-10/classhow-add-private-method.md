# Metamodel::ClassHOW.add_private_method

`$type.^add_private_method($name, $code)` now has a row in the ClassHOW method table. It installs
the code in the private namespace (`self!name`, `.^private_method_table`) and keeps the public
methods of the same name, the mirror image of `add_method`. Found running `AttrX::Mooish`
(#13262).
