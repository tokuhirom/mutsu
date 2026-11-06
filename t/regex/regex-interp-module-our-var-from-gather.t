use Test;
use lib 't/lib';
use RxOurUser;

plan 3;

# A module routine interpolates its own file-scope `our` regex variable. The
# routine is called from a `gather` body (which runs over a snapshot of its
# creator's env): the variable must still be found.
is rx-plain(), 'word:abc', 'called from a plain sub';
is-deeply rx-gathered().List, ('word:abc', 'other:123'), 'called from a lazy gather body';
is-deeply rx-assigned(), ['word:xyz'], 'called from a gather assigned to an array';
