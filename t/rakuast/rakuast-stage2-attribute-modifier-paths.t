use Test;

# ADR-15334 section 3.1 item 7: the same snippets as a file, as an EVAL string
# and as EVAL of the string's .AST must mean the same thing. Family: a class
# attribute under a postfix statement modifier (`has $.x if COND`, `for LIST`,
# both). The modifier cannot gate or repeat the declaration (it installs when
# the class is composed); it only evaluates its operand, once.

my @cases =
    'if',            'my @log; class PATH { has $.x = 5 if @log.push(1); }; PATH.new.x ~ "/" ~ @log.elems',                 '5/1',
    'if false',      'my @log; class PATH { has $.x = 5 if 0 && @log.push(1); }; PATH.new.x ~ "/" ~ @log.elems',            '5/0',
    'unless',        'class PATH { has $.x = 6 unless 0; }; PATH.new.x',                                                      '6',
    'with',          'my @log; class PATH { has $.x = 7 with @log.push(1); }; PATH.new.x ~ "/" ~ @log.elems',               '7/1',
    'without',       'class PATH { has $.x = 8 without Any; }; PATH.new.x',                                                   '8',
    'when',          'class PATH { has $.x = 9 when 1; }; PATH.new.x',                                                        '9',
    'for',           'my @log; class PATH { has $.x = 1 for @log.push(1), @log.push(2); }; PATH.new.x ~ "/" ~ @log.elems',  '1/2',
    'for names',     'class PATH { has $.x for 1, 2; has $.y; }; PATH.^attributes.map(*.name).join(",")',                     '$!x,$!y',
    'while false',   'class PATH { has $.x = 2 while 0; }; PATH.new.x',                                                       '2',
    'if and for',    'my @log; class PATH { has $.x = 3 if 1 for @log.push(1), @log.push(2); }; PATH.new.x ~ "/" ~ @log.elems', '3/2',
    'private',       'class PATH { has $!h = 4 if 1; method h { $!h } }; PATH.new.h',                                         '4';

sub as-file(Str $code) {
    my $proc = run $*EXECUTABLE, '-e', "say do \{ $code }", :out, :err;
    $proc.out.slurp(:close).trim;
}

# A class is installed in its unit's package, so each path names its own.
sub named(Str $code, Str $path) { $code.subst(:g, 'PATH', $path) }

my $n = 0;
for @cases -> $name, $code, $expected {
    $n++;
    is as-file(named($code, "F$n")), $expected, "$name: as a file";
    is EVAL(named($code, "S$n")).Str, $expected, "$name: as an EVAL string";
    is EVAL(named($code, "A$n").AST).Str, $expected, "$name: as EVAL of the string's .AST";
}

# The read direction: the declaration with its modifier nodes, as rakudo has it.
{
    my $ast = 'class C { has $.x for 1, 2; }'.AST;
    like $ast.gist, /'loop-modifier' .* 'StatementModifier::For'/, 'the for modifier is a loop-modifier node';
    my $ast2 = 'class D { has $.x = 1 if 1 for 1, 2; }'.AST;
    like $ast2.gist, /'condition-modifier' .* 'StatementModifier::If' .* 'loop-modifier' .* 'StatementModifier::For'/, 'both modifiers are kept';
}

done-testing;
