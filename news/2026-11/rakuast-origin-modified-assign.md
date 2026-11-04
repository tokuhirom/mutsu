# RakuAST: origins of a statement-modified assignment

In `$n = 5 if $n` the assigned variable was found by looking for a range of an
equal term in the statement, and took the condition's `$n`. A plain scalar,
array or hash target is now built without that lookup, so the target, the `=`
and the condition each have rakudo's range (#15780). A condition whose call
reads the same variable as a self-assignment's source
(`$n = $n.abs if $n.WHAT`) is still ambiguous to the lookup and open.
