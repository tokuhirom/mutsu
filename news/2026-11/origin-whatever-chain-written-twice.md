# `.origin` of a whatever-curried chain written twice in one statement

In `f $x.map(*.head), $x.map(*.head)` the marker the whatever pass puts around
`*.head` and the body it curries fingerprint alike, so the first occurrence's
marker and body each took a noted range, the body's one outside the marker's
and dropped, and the second occurrence found none left. A range a term
rejects as not inside its enclosing term now stays for the next term of its
shape, so both occurrences keep their origins (#15780).
