# `my %h := :{}` keeps object keys on element assignment

An object-hash literal bound with `:=` (`my %h := :{}`, `our %h := :{ ... }`,
`my %*H := :{}`) is an object hash, but its declaration carries no `{Mu}`
constraint. The element-assignment path (`%h{$obj} = ...`) took the key type only
from the declaration for a `%` variable, so the key was stringified (`AST<20>`)
where rakudo keeps the object. It now reads the key type from the hash the variable
holds, as it already did for a `$` alias of an object hash.

Red's `what-does-it-do` keeps its branch conditions in `my %*VALS := :{}` /
`:{ $ast => ... }`; once `CX::Red::Bool` became resumable they reached
`hash-to-cond(%val)` with `Str` keys and died with "expected Red::AST but got Str".
