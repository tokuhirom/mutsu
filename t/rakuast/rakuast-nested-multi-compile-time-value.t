use experimental :rakuast;
use lib 't/lib';
use Test;
use RakuastCheckWalker;
# A `sub EXPORT` hook hands the unit a family of `+` too, as `use Red` does.
use HookOperatorFamily;

# The resolution of a `+` inside a block that declares its own
# `multi infix:<+>` is that declaration; asked at CHECK time, before the block
# runs, its `compile-time-value` is the declared candidate, whose
# `.candidates` is not the setting's (Red t/091, "Custom operators retain their
# implementation": Red's CHECK compiler falls back to the user operator when a
# candidate comes from outside the setting). It was the setting's first `+`
# candidate, or the whole setting dispatcher when the candidate's body called
# a routine the unit imported and the walk ran in another module's code, or
# the imported family when a hook had handed the unit one.

plan 6;

my @files;
my $answer;
my @walked;
CHECK {
    sub walk($n) {
        if $n ~~ RakuAST::Infix && $n.operator eq '+' && $n.is-resolved {
            my $code = $n.resolution.compile-time-value;
            @files = ($code.^can('candidates') ?? $code.candidates !! ($code,)).map(*.file);
            $answer = $code('a', 1) if $code ~~ Code;
        }
        $n.visit-children(&walk);
    }
    walk($*CU);
    @walked = plus-candidate-files();
}

{
    multi infix:<+>(Str $left, Int $right) { helper-value(21) }
    is "a" + 1, 42, 'the block candidate answers at run time';
}
ok @files.elems, 'the CHECK phase found the operator';
nok @files.all.starts-with('SETTING::'), 'not every candidate is the setting\'s';
is $answer, 42, 'the CHECK-time value runs the declared body';
ok @walked.elems, 'a module walking the unit found it too';
nok @walked[0].all.starts-with('SETTING::'),
    'and its candidate is not the setting\'s either';
