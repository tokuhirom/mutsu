use Test;

# A block returning an `rx/.../` literal (`.grep({ rx/.../ })`) answers with
# Regex.Bool, i.e. whether the regex matches the topic -- whatever the
# pattern holds (`\w`, `.`, character classes, interpolated variables).

plan 6;

my @lines = 'root:x:0:0:root:/root:/bin/bash', 'daemon:x:1:1:daemon:/usr/sbin:/usr/sbin/nologin';
my $uid = 0;
is-deeply @lines.grep({ rx/ ^ \w+ ':' <-[:]>+ ':' $uid ':' .* $ / }).List,
    ('root:x:0:0:root:/root:/bin/bash',), 'the File::Utils uid lookup shape';
is-deeply <ab cd>.grep({ rx/ \w 'zz' / }).List, (), '\w with no match';
is-deeply <ab cd>.grep({ rx/ . 'd' / }).List, ('cd',), '. with a match';
is-deeply <ab cd>.grep({ rx/ \s / }).List, (), '\s';
is-deeply <ab cd>.map({ rx/ \w 'b' /.Bool }).List, (True, False), 'Regex.Bool in a map';
is-deeply <ab cd>.grep({ rx/ \d 'zz' / }).List, (), '\d (a statically parsed pattern)';
