# An object-hash literal files its keys under a user-declared `WHICH`

`:{ $a => 1 }` built its entries with the pure keying layer before any user-declared `WHICH`
had run, so a class whose instances compare by value (Red's `Red::AST::*`) got identity keys
in the literal while element assignment (`%h{$b} = 2`) used the user's answer: two entries
where rakudo has one. Red's `what-does-it-do` builds `:{ $ast.clone => $value }` and stores
under the same AST afterwards, which turned every two-branch `grep` filter into a wrong
condition tree (Red `t/05-better-map`, `t/51-pre-create-or-update`, `t/24-grep-no-AND-OR`).
The literal now runs each key's `WHICH` first, like the other object-hash paths.
