# Cool routines called as subs throw an unhandled Failure argument

`chars $f`, `uc $f`, `abs $f`, `floor $f`, `sqrt $f`, ... with an unhandled
`Failure` answered a result computed from the Failure's message text (`chars`
returned the message length), or died with "Unknown function: abs", where
rakudo throws the Failure's exception (the routine form is `$f.chars`). The
method forms already threw; the Cool routine forms now do too.
