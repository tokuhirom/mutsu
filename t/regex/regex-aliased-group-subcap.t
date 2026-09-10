use Test;

# A named capture alias on a group that contains a subrule must preserve the
# inner subrule capture as a nested subcapture of the alias, so
# `$<outer><inner>` is reachable. See roast/S05-capture/alias.t test 20.

plan 5;

ok "Jon Lee" ~~ m:s/$<first>=(<.ident>) $<family>=(<ident>)/,
    'aliased group with subrule matches';
is ~$/<first>,  'Jon', '$<first> captured';
is ~$/<family>, 'Lee', '$<family> captured';
is ~$/<family><ident>, 'Lee',
    'inner <ident> preserved as nested subcap of $<family>';
is $/<family>.hash.keys.sort.join(','), 'ident',
    '$<family> exposes exactly the nested ident key';
