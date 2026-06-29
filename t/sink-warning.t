use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

plan 30;

# A pure value evaluated in sink (void) context warns.
is_run 'say "hi"; 42', { :0status, :out("hi\n"), :err(/"Useless use" .* 42/) },
    'bare constant integer in sink warns';
is_run '"foo"', { :0status, :err(/"Useless use" .* 'foo'/) },
    'bare string in sink warns';
is_run 'my $x; $x', { :0status, :err(/"Useless use" .* '$x'/) },
    'bare variable in sink warns';
is_run '1+2', { :0status, :err(/"Useless use" .* '1+2'/) },
    'bare operator expression in sink warns';
is_run 'foo => 42', { :0status, :err(/"Useless use" .* 'foo => 42'/) },
    'bare fatarrow in sink warns';

# A comma list distributes the sink to each element.
is_run '1;2', { :0status, :err(/"Useless use" .* "Useless use"/) },
    'statement list produces two sink warnings';
is_run '1,2', { :0status, :err(/"Useless use" .* "Useless use"/) },
    'comma list produces two sink warnings';

# Bare blocks stay in sink context.
is_run '{ 1,2 }', { :0status, :err(/"Useless use" .* "Useless use"/) },
    'bare block distributes sink to its statements';

# Statement modifiers suggest Nil.
is_run '1 while 0', { :0status, :err(/"Useless use" .* 'Nil'/) },
    'while modifier suggests Nil';
is_run '1 until 1', { :0status, :err(/"Useless use" .* 'Nil'/) },
    'until modifier suggests Nil';
is_run '"x" for 1,2', { :0status, :err(/"Useless use" .* 'Nil'/) },
    'for modifier suggests Nil';
is_run '1.0 given 1,2', { :0status, :err(/"Useless use" .* 'Nil'/) },
    'given modifier suggests Nil';

# Conditionals warn but do not suggest Nil.
is_run '5 if 1', { :0status, :err(/"Useless use"/) },
    'if modifier warns in sink';

# Side-effecting expressions never warn.
is_run 'sub f { 5 }; f()', { :0status, :err('') },
    'sub call with discarded value does not warn';
is_run 'my $y; $y = 10', { :0status, :err('') },
    'assignment in sink does not warn';
is_run 'say 1+2', { :0status, :out("3\n"), :err('') },
    'value consumed by say does not warn';

# A `gather` body is in sink context wherever the gather appears.
is_run 'my @x = gather 43', { :0status, :err(/"Useless use" .* 43/) },
    'gather body as initializer warns its useless statements';
is_run 'sub f { my @x = gather 43 }; f()', { :0status, :err(/"Useless use" .* 43/) },
    'gather body inside a sub warns';
is_run 'my @x = gather { take 1 }; say @x.elems', { :0status, :out("1\n"), :err('') },
    'gather body with only a take does not warn';

# A bare anonymous `$` is reported as "unnamed $ variable", not its internal name.
is_run '$; my $b;', { :0status, :err(/"Useless use of unnamed " '$' " variable"/) },
    'bare anonymous $ in sink warns as unnamed variable';

# Pure prefix operators on useless operands warn, naming the operator.
is_run '-5', { :0status, :err(/"Useless use" .* '"-"' .* '-5'/) },
    'prefix minus on constant in sink warns';
is_run '!5', { :0status, :err(/"Useless use" .* '"!"' .* '!5'/) },
    'prefix bang on constant in sink warns';
is_run '?5', { :0status, :err(/"Useless use" .* '"?"' .* '?5'/) },
    'prefix question on constant in sink warns';
is_run 'my $x = 1; -$x', { :0status, :err(/"Useless use" .* '"-"' .* '-$x'/) },
    'prefix minus on variable in sink warns';

# A prefix operator on a side-effecting operand never warns.
is_run 'sub f { 5 }; -f()', { :0status, :err('') },
    'prefix minus on sub call does not warn';

# A sink warning preserves the source format the user wrote (radix prefix,
# scientific notation, Unicode infinity), not the canonical stringification.
is_run '0xFF', { :0status, :err(/"Useless use" .* 'integer' .* '0xFF'/) },
    'sink warning keeps hex literal format';
is_run '0b1010', { :0status, :err(/"Useless use" .* 'integer' .* '0b1010'/) },
    'sink warning keeps binary literal format';
is_run '6.02e23', { :0status, :err(/"Useless use" .* 'number' .* '6.02e23'/) },
    'sink warning keeps scientific notation';
is_run '∞', { :0status, :err(/"Useless use" .* '∞'/) },
    'sink warning keeps Unicode infinity glyph';

# Double statement modifier ejects with X::Syntax::Confused carrying pre/post.
throws-like 'say 1 if 2 if 3 { say 3 }', X::Syntax::Confused,
    reason => { m/'Missing semicolon'/ },
    pre    => { m/'1 if 2'/ },
    post   => { m/'3 { say 3 }'/ };
