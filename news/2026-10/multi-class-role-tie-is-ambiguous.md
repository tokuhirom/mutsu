# A parent class and a composed role now tie in multi dispatch

`multi f(N:D $)` next to `multi f(R:D $)`, called with an instance of `class B is N does R`,
used to run the class candidate. The two declared types are incomparable (neither conforms
to the other), so rakudo orders them as a tie and reports `X::Multi::Ambiguous`. The tie
logic treated candidates whose parameter *names* differ as never tied; it now also treats
candidates whose shapes differ only in user-defined, mutually incomparable types as tied,
leaving every comparable pair (a class and its own role, a role and the role it does) on
the distance ranking (#14846).
