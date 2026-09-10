use Test;
plan 6;

is (1, 2, 3).fmt("%02d", "-"), "01-02-03", "list fmt joins formatted values";
is ("a", "b", "c").fmt("%s", ","), "a,b,c", "list fmt supports string format";
dies-ok { 42.fmt("%d", ",") }, "non-list fmt with separator dies";
is 10.fmt("%08b"), "00001010", "fmt supports binary conversion";
is (2, 5, 10).fmt("%04b", ""), "001001011010", "list fmt works with binary conversion";
is (1 +< 65).fmt("%b"), "100000000000000000000000000000000000000000000000000000000000000000", "fmt handles big integers";
