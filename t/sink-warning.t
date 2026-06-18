use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

plan 19;

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
