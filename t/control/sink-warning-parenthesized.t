use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

# A parenthesized expression in sink context is not reported, whatever it
# holds; only an empty `()` still is, and a comma list without parentheses
# still distributes the sink to each element. Expected values from rakudo
# (found while comparing MCP::Server::Tool::Web's `(@x.map(...), 5);`).

plan 8;

is_run '(5); say "ok"', { :0status, :out("ok\n"), :err('') },
    'a parenthesized constant is silent';
is_run '(1, 2); say "ok"', { :0status, :out("ok\n"), :err('') },
    'a parenthesized list is silent';
is_run '(1 + 2); say "ok"', { :0status, :out("ok\n"), :err('') },
    'a parenthesized operator expression is silent';
is_run 'my @x = 1, 2; (@x.map({ $_ }), 5); say "ok"', { :0status, :out("ok\n"), :err('') },
    'a parenthesized list holding a map and a constant is silent';
is_run '((5)); say "ok"', { :0status, :out("ok\n"), :err('') },
    'nested parentheses are silent';
is_run '()', { :0status, :err(/"Useless use of () in sink context"/) },
    'an empty () still warns';
is_run '(1, 2), 3', { :0status, :err({ .comb('Useless use').elems == 1 && .contains('integer 3') }) },
    'outside the parentheses only the bare element warns';
is_run '1, 2', { :0status, :err(/"Useless use" .* "Useless use"/) },
    'a bare comma list still distributes the sink';
