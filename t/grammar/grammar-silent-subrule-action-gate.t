use Test;

plan 4;

# A childless silent call (`<.sp>`) dispatches its action when the actions
# class declares one -- Rakudo reduces every subrule call, captured or not.
grammar Spaced {
    token TOP { <.sp> 'x' <.gap> 'y' <.sp> }
    token sp  { ' '* }
    token gap { ' ' }
}

class CountSp {
    has $.n = 0;
    method sp($/) { $!n++ }
}

my $counter = CountSp.new;
ok Spaced.parse('  x y ', :actions($counter)), 'parse with a silent-call action';
is $counter.n, 2, 'a childless silent subrule runs its action once per call';

# An actions class without a method for the silent rule still parses, and
# the silent call leaves nothing visible in the match.
class NoSp {
    method TOP($/) { make 'top' }
}
my $m = Spaced.parse('  x y ', :actions(NoSp));
is $m.made, 'top', 'silent calls without an action method do not disturb TOP';
is $m.hash.keys.elems, 0, 'silent calls stay out of .hash';
