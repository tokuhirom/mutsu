# Method calls skip the Exception/Failure control-row lookup for other names

Every method call went through `exception_control_by_mro`, which interned the method
name and probed both control owners (`Exception`, `Failure`) for a row before it could
decline. Only `throw`, `rethrow`, `die`, `fail`, `resume` and `Failure` have such rows,
so any other name now returns at once; a unit test pins the name list to the rows.
