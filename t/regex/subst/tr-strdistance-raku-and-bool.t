use Test;

# The StrDistance `tr///` returns renders its `before` / `after`, and is false
# when the transliteration changed nothing (#18903).

plan 8;

my $t = "hello";
my $d = ($t ~~ tr/l/L/);
is $d.raku, 'StrDistance.new(before => "hello", after => "heLLo")', '.raku';
is $d.gist, 'StrDistance.new(before => "hello", after => "heLLo")', '.gist';
is +$d, 2, 'the distance';
is ~$d, 'heLLo', 'stringifies to the result';
ok $d.Bool, 'a change is true';
my $u = "hello";
my $same = ($u ~~ tr/z/Z/);
nok $same.so, 'no change is false';
is +$same, 0, 'and its distance is 0';
nok StrDistance.new(before => "a", after => "a").Bool, 'a constructed one follows the same rule';
