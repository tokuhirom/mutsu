use Test;

plan 3;

is EVAL("1"), 1, "EVAL default language is Raku";
is EVAL("1", :lang<Raku>), 1, "EVAL accepts :lang<Raku>";
nok so try EVAL("1", :lang<Perl5>), "EVAL with unsupported :lang fails under try";
