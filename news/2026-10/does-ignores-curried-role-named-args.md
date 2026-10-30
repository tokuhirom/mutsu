# `.does` of a curried role ignores its named arguments

`$w.does(NT[Int, :coerce(&cf)])` answered False for an instance of that very role. The role value is
now named by its positional arguments only, as the smartmatch already does (#15715).
