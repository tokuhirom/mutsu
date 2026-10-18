use Test;

plan 5;

# A colon followed by digits after an interpolated scalar is literal text, not an
# adverb suffix of the variable name ("$port:8123" is `$port` then ":8123").
my $v = 7;
is "$v:8123", '7:8123', 'lexical followed by :digits';
is "$v: x", '7: x', 'colon and space stay literal';
is "x$v:1:2", 'x7:1:2', 'several colon-digit groups';

class A {
    has $!port = 5;
    has $.name = 'n';
    method published { "$!port:8123" }
    method named { "$.name:80" }
}
is A.new.published, '5:8123', 'private attribute followed by :digits';
is A.new.named, 'n:80', 'public accessor followed by :digits';
