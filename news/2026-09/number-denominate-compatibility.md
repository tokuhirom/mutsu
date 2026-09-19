# Number::Denominate now runs under mutsu

The ecosystem roulette found that `Number::Denominate` could not run under
mutsu because fat arrows were parsed outside loose `and`/`or` expressions,
compound-assignment declarations were reinitialized by postfix `for`, and
`is copy` default container parameters could alias their source. Interpolated
postcircumfix chains such as `@units[*-1]<plural>` also left the final lookup
as literal text.

Fixed these parser, binding, and interpolation paths generally. The complete
`Number::Denominate` test suite now passes under both Rakudo and mutsu,
including repeated calls using the module's default unit table.
