use Test;

plan 6;

is-deeply (gather { (1,2,3)».&take }).list, (1, 2, 3), '».&take takes each element';

my @a = 4, 5, 6;
is-deeply (gather { @a».&take }).map({ $_ }).list, (4, 5, 6), 'lazily read gather';

is (gather { (1,2,3).map(&take) }).elems, 3, '.map(&take)';

is (gather { my &f = &take; f(5) }).elems, 1, 'a stored &take is callable';

is &take.WHAT.gist, '(Sub)', '&take is a Sub';

try { (1, 2)».&take; CATCH { default { is .message, 'take without gather', 'outside gather' } } }

done-testing;
