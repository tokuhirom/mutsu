# A wrapped built-in class method reaches calls on the type object

`DateTime.^lookup('now').wrap(...)` registered the wrapper, but `DateTime.now`
still ran the original: the wrap lookup named a type object's class
`Package` instead of the type it stands for, treated the runtime-registered
`DateTime` as a class of the program (whose wraps run elsewhere), and the
mutating call op (a call on a variable or a type name) never asked for a wrap
chain at all. All three are fixed, so a wrapper of `DateTime.now` or
`Date.today` answers, and its `callsame` reaches the original (#13626).
BDD::Behave's clock-freezing helpers (`t/timing/time-helpers.rakutest`,
`t/timing/time-mocking.rakutest`) now pass in full.
