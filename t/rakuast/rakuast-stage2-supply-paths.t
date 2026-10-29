use Test;

# ADR-15334 section 3.1 item 7: the same snippets as a file, as an EVAL string
# and as EVAL of the string's .AST must mean the same thing. Family: a
# `supply { ... }` block, whose one lowering rewrites `emit`, `done` and CLOSE
# phasers around the on-demand emitter.

my @cases =
    'emit',          'supply { emit 1; emit 2 }.list.join(",")',                                  '1,2',
    'emit topic',    'supply { for 1..3 { .emit } }.list.join(",")',                              '1,2,3',
    'done stops',    'supply { emit 1; done; emit 2 }.list.join(",")',                            '1',
    'done in if',    'supply { for 1..5 { done if $_ > 2; emit $_ } }.list.join(",")',            '1,2',
    'nested supply', 'supply { emit 1; emit 2 }.map(* * 10).list.join(",")',                      '10,20',
    'close phaser',  'my @l; supply { emit 1; CLOSE { @l.push("closed") } }.tap(-> $v { @l.push($v) }); @l.join(",")', '1,closed',
    'emit call',     'supply { emit(7) }.list.join(",")',                                         '7',
    'user emit',     'sub emit($x) { "mine$x" }; emit(3)',                                        'mine3';

sub as-file(Str $code) {
    my $proc = run $*EXECUTABLE, '-e', "say do \{ $code }", :out, :err;
    $proc.out.slurp(:close).trim;
}

for @cases -> $name, $code, $expected {
    is as-file($code), $expected, "$name: as a file";
    is EVAL($code).Str, $expected, "$name: as an EVAL string";
    is EVAL($code.AST).Str, $expected, "$name: as EVAL of the string's .AST";
}

done-testing;
