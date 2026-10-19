# A variable used before the unit's top level declares it is a compile-time error

`sub f { $zz = 42 }; my $zz = 1; f(); say $zz` fails to compile in rakudo with
`Variable '$zz' is not declared` (X::Undeclared), and so does `say $x; my $x`.
mutsu ran both programs. Worse, it resolved `$zz` at run time through the
call chain, so the routine silently wrote its *caller's* same-named lexical
(#12800).

The CHECK-time pass that already rejects undeclared routines and barewords now
also reports this shape. It reuses the undeclared-variable scan `EVAL` runs on
its snippets, over the whole compunit, in a narrow mode. A use is reported
only when nothing in scope at that point declares the name and the unit's top
level declares it later in the source. Every other undeclared use is still
left to run time, because the full check would need to know every declaration
form the scan does not model yet.

After a `use` of a module, the variables the parse-time scan knows it exports
count as declared. If the scan does not know what a module exports, nothing
after that `use` is reported. All 8,000 files of `t/` pass under the new pass.
