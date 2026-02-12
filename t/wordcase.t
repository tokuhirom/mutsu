use Test;

plan 12;

# Basic wordcase function
is wordcase(""), "", "wordcase('') works";
is wordcase("puGS Is cOOl!"), "Pugs Is Cool!", "wordcase('...') works";

# Method form
is "puGS Is cOOl!".wordcase, "Pugs Is Cool!", "'...'.wordcase works";

# Apostrophes and dashes
is "don't sit under the apple tree".wordcase, "Don't Sit Under The Apple Tree",
    "wordcase works properly with apostrophes";
is "tir-na nog'th".wordcase, "Tir-na Nog'th",
    "wordcase works properly with apostrophes and dashes";

# Non-destructive
my $a = "puGS Is cOOl!";
is wordcase($a), "Pugs Is Cool!", "wordcase string works";
is $a, "puGS Is cOOl!", "original string not touched";
is $a.wordcase, "Pugs Is Cool!", "method form works";
is $a, "puGS Is cOOl!", "original string not touched after method";

# Ordinary string
is "ab cD Ef".wordcase, "Ab Cd Ef", "works on ordinary string";

# Non-ASCII
is wordcase("äöü abcä"), "Äöü Abcä", "wordcase() works on non-ASCII chars";

# Int
is ~(0.wordcase), ~0, ".wordcase on Int";
