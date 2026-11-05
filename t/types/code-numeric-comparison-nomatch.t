use Test;
plan 14;

sub msg(&c) { try { c(); CATCH { default { return .message } } }; 'no error' }

my $n = { 1 };
sub g() { 1 }
my $tail = "; none of these signatures matches:\n    (Mu:U \\v:: *\%_)";

is msg({ 2 == $n }),  "Cannot resolve caller Numeric(Block:D: )$tail", '== on a Block';
is msg({ 2 != $n }),  "Cannot resolve caller Numeric(Block:D: )$tail", '!= on a Block';
is msg({ 2 == &g }),  "Cannot resolve caller Numeric(Sub:D: )$tail",   '== on a Sub';
is msg({ 2 < $n }),   "Cannot resolve caller Real(Block:D: )$tail",    '< on a Block coerces through Real';
is msg({ $n >= 2 }),  "Cannot resolve caller Real(Block:D: )$tail",    '>= on a Block';
is msg({ 2 <=> $n }), "Cannot resolve caller Real(Block:D: )$tail",    '<=> on a Block';
is msg({ $n.Numeric }), "Cannot resolve caller Numeric(Block:D: )$tail", '.Numeric on a Block';
is msg({ &g.Numeric }), "Cannot resolve caller Numeric(Sub:D: )$tail",   '.Numeric on a Sub';
is msg({ $n.Int }),   "Cannot resolve caller Int(Block:D: )$tail",     '.Int on a Block';
is msg({ $n.Real }),  "Cannot resolve caller Real(Block:D: )$tail",    '.Real on a Block';
is msg({ Whatever.new.Numeric }), "Cannot resolve caller Numeric(Whatever:D: )$tail", '.Numeric on a Whatever';
is msg({ class A {}; A.new.Real }), "Cannot resolve caller Real(A:D: )$tail", '.Real on a plain object';
is msg({ 2 == 2 }), 'no error', 'plain numbers still compare';
is msg({ &g === &g }), 'no error', 'identity on code is unaffected';
