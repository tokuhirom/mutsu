use lib 't/lib';
use Test;

# rakudo's `infixstopper` (#16888, EEL 0.3's `<name: args>` calls): a slang's
# `arglist` override that sets `$*IN_REGEX_ASSERTION` makes a `>` end the
# arguments instead of starting a comparison, while its `circumfix:sym<( )>`
# override resets it inside parentheses.

plan 4;

my @seen;
sub record(*@args, *%named) { @seen.push: (@args.join(','), %named.sort.map({ .key ~ '=' ~ .value }).join(',')).join('|') }

use AngleCallSlang;

call <record: 4>;
is @seen[0], '4|', 'a positional argument ends at the >';
call <record: 1, 2 >;
is @seen[1], '1,2|', 'a list ends at the > after a space';
call <record: :k(3 > 1)>;
is @seen[2], '|k=True', 'a comparison in parentheses keeps its >';
call <record: 5, :v(1 > 2)>;
is @seen[3], '5|v=False', 'both in one call';
