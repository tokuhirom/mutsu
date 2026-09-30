use Test;

plan 4;

is "résumé resume".comb(/ :m resume /).elems, 2, '.comb with :m finds marked and unmarked';
is "résumé resume".match(/ :m resume /, :g).elems, 2, '.match(:g) with :m agrees';
is "résumé resume".comb(/ :m resume /).join("|"), "résumé|resume", '.comb :m returns original text';
is "résumé resume".comb(/ :m resume /, 1).elems, 1, '.comb :m honours limit';
