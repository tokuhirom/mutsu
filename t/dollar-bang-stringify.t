use Test;

plan 2;

try { die "qwerty" };
ok ~$! ~~ /qwerty/, 'stringify $! works for smartmatch';
ok ~($!) ~~ /qwerty/, 'parenthesized $! does not misparse as custom prefix op';
