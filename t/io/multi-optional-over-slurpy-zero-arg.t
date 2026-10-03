use Test;

plan 6;

# A zero-arg call: an optional-positional candidate is narrower than a
# slurpy one, whichever is declared first (#11173).
multi g(*@a) { "slurpy" }
multi g($x?) { "opt" }
is g(), "opt", 'sub: optional positional beats slurpy for a zero-arg call';

class E {
    multi method u($x?) { "opt" }
    multi method u(*@a) { "S" }
}
is E.u(), "opt", 'method: optional first, type-object invocant';
is E.new.u(), "opt", 'method: optional first, instance invocant';

class F {
    multi method u(*@a) { "S" }
    multi method u($x?) { "opt" }
}
is F.new.u(), "opt", 'method: slurpy declared first';
is F.new.u(1), "opt", 'method: one arg still picks the optional candidate';
is F.new.u(1, 2), "S", 'method: two args only fit the slurpy candidate';
