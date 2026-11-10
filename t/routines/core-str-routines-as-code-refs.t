use Test;

plan 13;

# Core string routines that are real subs are first-class `&name` values.
ok defined(&fc), '&fc is defined';
ok defined(&tclc), '&tclc is defined';
ok defined(&wordcase), '&wordcase is defined';
ok defined(&samecase), '&samecase is defined';
ok defined(&ords), '&ords is defined';
ok defined(&index) && defined(&rindex) && defined(&indices), '&index, &rindex and &indices are defined';
ok defined(&trim-leading) && defined(&trim-trailing) && defined(&samemark) && defined(&split),
    '&trim-leading, &trim-trailing, &samemark and &split are defined';

is &fc("aA"), "aa", 'calling &fc';
is <aA bB>.map(&fc).join(","), "aa,bb", '&fc as a map callback';
is <aA bB>.map(&tclc).join(","), "Aa,Bb", '&tclc as a map callback';

# A named `&`-parameter bound to such a routine is callable by its own name.
sub apply($x, :&as) { as($x) }
is "aaAaA".&apply(:as(&fc)), "aaaaa", 'method-call form with :as(&fc)';
is apply("aA", :as(&fc)), "aa", 'plain call with :as(&fc)';
is-deeply &split(",", "a,b").list, ("a", "b"), 'calling &split';

done-testing;
