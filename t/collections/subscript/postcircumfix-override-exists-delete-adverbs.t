use lib 't/lib';
use Test;

plan 10;

class Local does Associative {
    has %.h = a => 1;
    method AT-KEY(\k) { %!h{k} }
}
multi sub postcircumfix:<{ }>(Local:D $c, \key, *%adv) { "user key={key.raku} adv={%adv.raku}" }

my %m is Local;
is %m{"a"}:exists, 'user key="a" adv={:exists}', "a user postcircumfix:<{ }> takes :exists";
is %m{"a"}:!exists, 'user key="a" adv={:!exists}', "... :!exists passes a False";
is %m{"a"}:delete, 'user key="a" adv={:delete}', "... and :delete";
is %m{"a"}, 'user key="a" adv={}', "... as it does the plain subscript";
is %m{"a"}:k, 'user key="a" adv={:k}', "... and a value adverb";

{
    use PcmExistsDelete;
    my %i is PCM;
    is %i{"a"}:exists, 'import key="a" adv={:exists}', "an imported postcircumfix:<{ }> takes :exists";
    is %i{"a"}:delete, 'import key="a" adv={:delete}', "... and :delete";
}

my %plain = a => 1, b => 2;
ok %plain<a>:exists, "a plain hash still answers :exists";
is %plain<b>:delete, 2, "... and :delete";
nok %plain<b>:exists, "... which removed the key";
