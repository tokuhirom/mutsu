use Test;

plan 4;

# A `:&name` sub-signature parameter reads the `name` key, without the sigil
# (Red::Driver::Mock's `% (:$times!, :$counter! is rw, :&run!)`).

my %when = a => { :times(2), :counter(0), :run({ 42 }) };
with %when<a> -> % (:$times!, :$counter! is rw, :&run!) {
    $counter++;
    is $times, 2, 'required scalar named sub-parameter';
    is $counter, 1, 'is rw counter';
    is run(), 42, 'required callable named sub-parameter is bound';
}

with { run => { 7 } } -> % (:&run) {
    is run(), 7, 'optional callable named sub-parameter';
}
