# List meta-operators consume longer user infix operators

`X+%`, `Z+%` and `R+%` now recognize a declared `infix:<+%>` as one operator instead of parsing its built-in `+` prefix first. The same longest-token rule applies when a user infix extends other built-in symbolic operators (#13591).
