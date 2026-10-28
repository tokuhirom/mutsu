use lib 't/lib';
use Test;

plan 1;

# Cookie::Jar (ecosystem): a `my class Pkg::Item { trusts Pkg; method !m }` in a
# module file, called as `$item!Pkg::Item::m` from `class Pkg`, was refused as
# untrusted: the owner resolved to its plain name, the `trusts` list is keyed
# by the mangled lexical storage name.
use LexicalClassTrusts;

is LexicalClassTrusts.reveal, 'x-item', 'a trusted class reaches the lexical class\'s private method';
