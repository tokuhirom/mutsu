use Test;

# `q => ...` (and qq/Q) is the pair key `q`, not a `q`-quote whose delimiter is
# `=`. rakudo accepts `=` as a quote delimiter (`q=foo=`), but `=>` is a fat
# arrow. Regression guard: previously `q=>10,k=>20` mis-parsed `q=...=` as a
# quote of ">10,k".

plan 6;

my %a = q=>10, k=>20;
is-deeply %a, {q => 10, k => 20}, 'q => is an autoquoted pair key';

my %b = qq=>1, z=>2;
is-deeply %b, {qq => 1, z => 2}, 'qq => is an autoquoted pair key';

my %c = Q=>3, y=>4;
is-deeply %c, {Q => 3, y => 4}, 'Q => is an autoquoted pair key';

# `=` remains a usable quote delimiter when it is not a fat arrow.
is q=foo=, 'foo', 'q=foo= still quotes with the = delimiter';

# The generic comparator cmp still orders strings as strings.
is ('b' cmp 'a'), More, 'cmp orders strings lexically';
is-deeply <b a>.sort(&[cmp], :k), (1, 0), 'sort with &[cmp] :k orders strings';
