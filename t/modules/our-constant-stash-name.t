use lib 't/lib';
use Test;
use OurGlobalsBase;

# A sigil-less `our constant` is a term: its stash member is `answer`, never a
# `$`-scalar `$answer` (#11877). A real `our $answer` keeps its `$`.

plan 7;

is OurGlobalsBase::.keys.sort.join(','), '$greeting,%config,@items,answer',
    'the constant is listed by its bare name next to the sigilled variables';
ok OurGlobalsBase::.EXISTS-KEY('answer'), 'the stash has the term key';
nok OurGlobalsBase::.EXISTS-KEY('$answer'), 'and no scalar key for it';
is OurGlobalsBase::<answer>, 42, 'the term member holds the value';

module Local {
    our constant limit = 7;
    our $level = 1;
}
is Local::.keys.sort.join(','), '$level,limit',
    'in-file: the constant is bare, the scalar keeps its sigil';
is Local::<limit>, 7, 'the term member is the constant';
is Local::<$level>, 1, 'the sigilled member is the scalar';
