use Test;

plan 6;

# A lowercase bareword naming nothing is a CHECK-time error in any
# expression position, not just as a whole statement (#12108).
for 'say bar;', 'my $x = bar; say $x;', 'say(bar);' -> $code {
    throws-like { EVAL $code }, X::Undeclared::Symbols, "'$code' is undeclared";
}

is EVAL('sub bar { 5 }; my $x = bar; $x'), 5, 'a declared sub is fine';
is EVAL('my %h = a => 1; %h<a>'), 1, 'a pair key is not a routine call';
ok EVAL('time > 0'), 'a core term is fine';

done-testing;
