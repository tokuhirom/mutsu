use Test;

plan 5;

is "abcdefg".trans("\t" => '  ', :g), "abcdefg", 'an unknown named argument (:g) is not a mapping';
is "a\tbcdefg".trans("\t" => ' ', :g), "a bcdefg", 'the real mapping still applies';
is "hello".trans("l" => "L", :global), "heLLo", 'another unknown named argument is ignored';
is "aabb".trans("ab" => "x", :s), "x", ':s still squashes';
is "abc".trans("a" => "", :d), "bc", ':d still deletes';
