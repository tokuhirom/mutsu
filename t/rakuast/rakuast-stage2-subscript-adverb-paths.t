use Test;

# ADR-15334 section 3.1 item 7: the same snippets as a file, as an EVAL string
# and as EVAL of the string's .AST must mean the same thing. Family: the
# adverbs written after a subscript. The parser keeps the colonpairs as the
# postcircumfix's own; one lowering expands them.

my @cases =
    'exists',          'my @a = 1,2,3; @a[1]:exists',                                                                  'True',
    'not exists',      'my %h = a => 1; %h<b>:!exists',                                                                'True',
    'exists arg',      'my %h = a => 1; my $f = 0; %h<a>:exists($f)',                                                  'False',
    'delete',          'my %h = a => 1, b => 2; my $r = %h<a>:delete; $r ~ "/" ~ %h.keys.join(",")',                   '1/b',
    'delete cond',     'my %h = a => 1, b => 2; my $r = %h<a>:delete(0); $r ~ "/" ~ %h.keys.sort.join(",")',            '1/a,b',
    'dynamic delete',  'my %h = a => 1, b => 2; my $delete = 1; my $r = %h<a>:$delete; $r ~ "/" ~ %h.keys.join(",")',  '1/b',
    'k',               'my @a = 5,6,7; @a[1]:k',                                                                       '1',
    'kv',              'my %h = a => 1; %h<a>:kv.join(",")',                                                           'a,1',
    'p',               'my %h = a => 1; %h<a>:p.gist',                                                                 'a => 1',
    'v',               'my @a = 5,6,7; @a[0,2]:v.join(",")',                                                           '5,7',
    'not k',           'my @a = 5,6,7; @a[1,5]:!k.join(",")',                                                          '1,5',
    'k cond',          'my @a = 5,6,7; my $ok = 1; @a[1,5]:k($ok).join(",")',                                          '1',
    'delete and v',    'my @a = 5,6,7; my $r = @a[1]:delete:v; $r ~ "/" ~ @a.elems',                                   '6/3',
    'exists delete',   'my %h = a => 1; my $r = %h<a>:exists:delete; $r ~ "/" ~ %h.elems',                             'True/0',
    'delete exists',   'my %h = a => 1; my $r = %h<a>:delete:exists; $r ~ "/" ~ %h.elems',                             'True/0',
    'exists kv',       'my %h = a => 1; (%h<a>:exists:kv).join(",")',                                                  'a,True',
    'multidim exists', 'my @a = [1, 2], [3, 4]; @a[1;0]:exists',                                                       'True',
    'multidim k',      'my @a = [1, 2], [3, 4]; (@a[1;0]:k).join(",")',                                                '1,0',
    'zen exists',      'my @a = 1,2; (@a[]:exists).join(",")', 'True,True',
    'zen kv',          'my @a = 7,8; (@a[]:kv).join(",")',                                                             '0,7,1,8',
    'whatever v',      'my %h = a => 1, b => 2; (%h{*}:v).sort.join(",")',                                             '1,2',
    'conflict',        'my @a = 1; my $r = "none"; try { @a[0]:k:v; CATCH { default { $r = .^name } } }; $r',          'X::Adverb',
    'unknown adverb',  'my @a = 1; my $r = "none"; try { @a[0]:foo; CATCH { default { $r = .^name } } }; $r',          'X::Multi::NoMatch',
    'in a ternary',    'my %h = a => 1; %h<a>:exists ?? "yes" !! "no"',                                                'yes',
    'angle spaced',    'my %h = a => 1; %h<a> :exists',                                                                'True';

sub as-file(Str $code) {
    my $proc = run $*EXECUTABLE, '-e', "say do \{ $code }", :out, :err;
    $proc.out.slurp(:close).trim;
}

for @cases -> $name, $code, $expected {
    is as-file($code), $expected, "$name: as a file";
    is EVAL($code).Str, $expected, "$name: as an EVAL string";
    is EVAL($code.AST).Str, $expected, "$name: as EVAL of the string's .AST";
}

# The read direction: the adverbs are the postcircumfix's colonpairs, in source order.
{
    my $pc = Q[my %h; %h<a>:delete:exists].AST.statements[1].expression.postfix;
    is $pc.colonpairs.map(*.key).join(","), 'delete,exists', 'colonpairs keep their order';
    isa-ok $pc.colonpairs[0], RakuAST::ColonPair::True, 'a bare adverb is a true colonpair';
    my $no = Q[my @a; @a[0]:!exists].AST.statements[1].expression.postfix;
    isa-ok $no.colonpairs[0], RakuAST::ColonPair::False, 'a negated adverb is a false colonpair';
}

done-testing;
