use v6;
use Test;

plan 5;

# A sigilless declaration initializer has a special binding grammar. In that
# context an assignment in the else branch of a ternary is valid Raku syntax;
# this is the minimized shape from Terminal::Widgets::Widget's `sgr` cache.
my %cache;
my \value = False ?? 1 !! %cache<answer> //= 2;
is value, 2, 'sigilless ternary initializer evaluates the else assignment';
is %cache<answer>, 2, 'the indexed //= writes through the hash element';

my \kept = True ?? 3 !! %cache<untouched> //= 4;
is kept, 3, 'the selected ternary branch remains the sigilless value';
ok !%cache<untouched>.defined, 'the unselected short-circuit branch is untouched';

class Renderer {
    method render(@line) {
        my %colors;
        my \sgr = False ?? 'hot' !! %colors<default> //= 'cold';
        my @seen;
        for @line {
            @seen.push("$_:$sgr");
        }
        @seen.join(' ')
    }
}

is Renderer.new.render(<one two>), 'one:cold two:cold',
    'a following for block parses after the sigilless ternary initializer';

done-testing;
