use Test;

# Regression pin: a block containing a `CATCH` yielded the *topic* instead of
# its last statement's value.
#
# A body with a `CATCH`/`CONTROL` phaser compiles to an implicit `try` wrapping
# every statement; the compiler then emitted `Pop`, discarding that try's value.
# With nothing left on the stack the block's value fell back to
# `last_topic_value` — i.e. `$_`. Any block used for its value broke as soon as
# it grew a `CATCH`: `.map` yielded the topic rather than the computed value,
# and `.first`/`.grep` saw a truthy topic and matched the first element.
#
# Zef's `Zef::Client!find-prereq-candidates` resolves `any(...)` dependency
# alternatives with `$needed.specs.first({ CATCH {...}; ...; @candidates })`,
# so it always picked the first (unsatisfiable) alternative.

plan 8;

is (<a b>).first({ CATCH { default { } }; $_ eq 'b' }), 'b',
    '.first uses the block value, not the topic, when the block has a CATCH';
is-deeply (<a b>).grep({ CATCH { default { } }; $_ eq 'b' }).List, ('b',),
    '.grep uses the block value when the block has a CATCH';
is-deeply (<a b>).map({ CATCH { default { } }; $_ eq 'b' }).List, (False, True),
    '.map uses the block value when the block has a CATCH';

# An empty list is a falsy block value, which is what the `any(...)` resolution
# above relies on to move past an unsatisfiable alternative.
my @seen;
my $hit = (<a b>).first({
    CATCH { default { } }
    @seen = $_ eq 'b' ?? ('C',) !! ();
    @seen
});
is $hit, 'b', 'an empty-list block value is falsy and the search continues';
is-deeply @seen, ['C'], 'the matching iteration left its state behind';

# The control cases must be unchanged.
is (<a b>).first({ $_ eq 'b' }), 'b', '.first without a CATCH still works';
is ({ CATCH { default { } }; 42 })(), 42,
    'a plain block with a CATCH still returns its last value';

# A CATCH that actually fires still swallows the exception.
my $caught = 0;
my $r = ({ CATCH { default { $caught = 1 } }; die 'boom'; 7 })();
is $caught, 1, 'a firing CATCH still handles the exception';
