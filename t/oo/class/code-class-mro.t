use Test;

# A code value's methods are found through its class's MRO (ADR-14477):
# `.^can` and the dispatch agree for every built-in code class.

my @methods = <multi dispatcher is_dispatcher package candidates yada
               is-implementation-detail arity count of onlystar soft>;

my $wc = * + 1;
my @values =
    '{;}'       => {;},
    'pointy'    => -> $x { $x },
    'whatever'  => $wc,
    'sub'       => sub { 1 },
    'method'    => method { 1 },
    'regex'     => /a/,
    '&say'      => &say;

plan @values * @methods + 4;

for @values -> $pair {
    my $v = $pair.value;
    for @methods -> $m {
        my $can = so $v.^can($m);
        my $not-found = False;
        try {
            $v."$m"();
            CATCH { when X::Method::NotFound { $not-found = True }; default { } }
        }
        is !$not-found, $can, "{$pair.key}.$m: dispatch agrees with .^can";
    }
}

throws-like { {;}.multi }, X::Method::NotFound, 'a Block has no Routine.multi';
lives-ok { /a/.multi }, 'a Regex answers Routine.multi';
is sub { 1 }.cando(\()).elems, 1, 'Code.cando on a Sub';
is-deeply { 1 }.arity, 0, 'a Block answers Code.arity';
