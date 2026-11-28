# `.parse` on a grammar goes straight to the native row

`$grammar.parse($text)` and `.subparse`, on a grammar type object or
instance whose class declares neither method, are answered by the native
`Grammar` row. Before, the call reached that row only after the whole general
`CallMethod` path had run: every receiver probe of `exec_call_method_op_impl`,
then the compiled-method lookup, `call_method_with_values` and
`dispatch_method_by_name_1`. That was about 30k instructions of dispatch
before the parse started.

A route at the call site (`vm_grammar_parse_site`) now answers the call
directly. It applies the same test `dispatch_method_by_name_1`'s grammar arm
applies: the receiver is a grammar and the grammar has no method of that name
in its MRO or a composed role. It also requires that no type declares a
`^find_method`, that no method is wrapped, and that the arguments are plain.
A named argument, a `Junction`, a `|` spread or a wrapped `parse` still takes
the full path, so a grammar's own `parse` (YAMLish's `nextwith`
override, for instance) is called exactly as before.

YAMLish makes one such call per plain scalar it concretizes
(`$schema.new.parse($!value)`, 60 per `load-yaml` of
`bench-yaml-parse-big`'s document) (#12917).
