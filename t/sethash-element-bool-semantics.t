use Test;

# A SetHash element is Bool-valued: `$sh<k>` is True/False by existence, and
# ++/--/assignment control that existence, evaluating to a Bool (the OLD value
# for postfix ++/--, the NEW value for prefix and for assignment). mutsu treated
# these numerically (0/1/-1) and returned the assigned RHS.

plan 12;

# postfix ++ / -- return the OLD Bool
{ my SetHash $s; is-deeply $s<a>++, Bool::False, 'postfix ++ returns old False'; is-deeply $s<a>, Bool::True, '...and adds the key'; }
{ my SetHash $s; is-deeply $s<a>--, Bool::False, 'postfix -- returns old False'; is-deeply $s<a>, Bool::False, '...stays absent'; }

# prefix ++ / -- return the NEW Bool
{ my SetHash $s; is-deeply ++$s<a>, Bool::True, 'prefix ++ returns new True'; }
{ my SetHash $s; is-deeply --$s<a>, Bool::False, 'prefix -- returns new False'; }

# postfix -- on a present key returns old True and removes it
{ my SetHash $s = <a>.SetHash; is-deeply $s<a>--, Bool::True, 'postfix -- on present returns True'; is-deeply $s<a>, Bool::False, '...and removes it'; }

# assignment evaluates to the Bool existence
{ my SetHash $s; is-deeply ($s<a> = 2), Bool::True, 'truthy assign is True'; }
{ my SetHash $s; is-deeply ($s<a> = 0), Bool::False, 'falsy assign is False'; }

# BagHash / MixHash keep numeric semantics
{ my BagHash $b; is $b<a>++, 0, 'BagHash ++ is numeric'; is $b<a>, 1, 'BagHash count'; }
