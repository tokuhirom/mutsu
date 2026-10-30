# A sunk call of a Block does not sink the container its last statement returns

`my &c = { $z = f() }; c();` threw the `Failure` (or the unsuccessful `Proc`) that
`$z` now holds. A Block hands back the container its last statement evaluated
to, and sinking a container sinks nothing inside it; only a Routine
decontainerizes its implicit result (`sub s { $z = f() }; s()` does throw).

`SinkPopCall` now asks the called code variable: a Block whose body ends in a
scalar assignment, a scalar declaration or a bare scalar variable
(`ast::result_decont::block_result_is_container`) skips the Failure and
unsuccessful-`Proc` explosion. Found by App::RaCoCo `t/02-ext-proc.rakutest`
(`silently { $actual = RunProc.new.run(...) }`).
