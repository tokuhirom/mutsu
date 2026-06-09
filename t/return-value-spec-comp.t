use Test;

# A routine whose signature pins the return value (`--> Nil`, `--> 42`,
# `--> "foo"`) may not also `return` an argument. Raku rejects this at compile
# time with an X::Comp carrying the offending value in its `payload`.

plan 5;

throws-like 'sub f(--> Nil) { return 5 }; f', X::Comp, 'Nil return spec', payload => /Nil/;
throws-like 'sub f(--> 42) { return 43 }; f', X::Comp, 'int return spec', payload => /42/;
throws-like 'sub f(--> 42) { return 42 }; f', X::Comp,
    "same value still not allowed", payload => /42/;
throws-like 'sub f(--> "foo") { return () }; f', X::Comp, 'str return spec',
    payload => /'"foo"'/;

# A bare `return` (no arguments) is fine with a definite return spec.
lives-ok { my sub g(--> 7) { return }; }, 'bare return is allowed';
