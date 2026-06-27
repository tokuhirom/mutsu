use Test;

plan 5;

# The X::Adverb error raised by a conflicting (`:k:v`) subscript-adverb combo
# reports `.what` describing the subscript shape.

my @a = <a b c d>;

throws-like '@a[1]:k:v',     X::Adverb, what => 'element access', 'single-element → element access';
throws-like '@a[1,2]:k:v',   X::Adverb, what => 'slice',          'multi-element literal → slice';
throws-like '@a[1..2]:k:v',  X::Adverb, what => 'slice',          'range → slice';
throws-like 'my @i = 1,2; @a[@i]:k:v', X::Adverb, what => 'slice', 'array-var index → slice';
throws-like '@a[*]:k:v',     X::Adverb, what => 'whatever slice', 'whatever → whatever slice';
