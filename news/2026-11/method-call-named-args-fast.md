# A method call with scalar named arguments takes the fast binder

A method call with a named argument such as `$r.emit('x', :space)` against
`method emit(Str:D $t, Bool:D :$space = False)` never reached the
compiled-method fast binder. The fast path refused every named parameter
carrying a `:D`/`:U` smiley outright: the general binder had to be the one to
raise the concreteness error. So each such call paid for
`bind_function_args_values`, about 20,000 instructions.

The refusal is now per value. The smiley becomes part of the named parameter's
fast check: a supplied value, or the constant default, that passes both the
type and the definiteness test binds on the fast path. Any other value fails
`accepts`, and the call keeps the general binder and its error. The sub light
path's named-call plan binds without that check, so it still refuses a
smiley-typed named parameter.

The method-call plan (ADR-15737 D1) also admits these calls now. It accepts a
string-keyed named argument whose value is scalar-like; the lane key already
holds the argument's name and the type of its value.

Callgrind slope (profiling build, 2,000 to 4,000 iterations, second run) for
`$r.emit('x', :space)`: 52,301 to 25,572 instructions per call.
