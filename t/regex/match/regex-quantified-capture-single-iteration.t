use Test;

plan 10;

# Raku listifies EVERY quantified capture, even when the quantifier ran exactly
# once: `(a)+`, `(a)*`, `(a)**1` and `(a)**0..1` all bind `$0` to a list. A bare
# `?` is the sole exception — it binds the Match itself.
#
# `Cro::Uri`'s grammar actions walk `@$0` (`method pchars($/) { … for @$0 }`),
# so an unlistified single iteration made every parsed URI's path come out as
# `/`, and every Cro client request went to the wrong target.

'a' ~~ /(a)+/;
nok $0 ~~ Match, 'a single-iteration + capture is not a bare Match';
is $0.elems, 1, '... it is a one-element list';
is ~$0[0], 'a', '... holding the iteration match';

'a' ~~ /(a)*/;
nok $0 ~~ Match, 'a single-iteration * capture is a list';

'a' ~~ /(a)**1/;
nok $0 ~~ Match, 'a **1 capture is a list';

'a' ~~ /(a)**0..1/;
nok $0 ~~ Match, 'a **0..1 capture is a list';

'a' ~~ /(a)?/;
ok $0 ~~ Match, 'a bare ? capture stays a Match';
is ~$0, 'a', '... which is the matched text';

'ab' ~~ /(\w)+/;
is $0.elems, 2, 'a multi-iteration capture still lists every iteration';

'ab' ~~ /(a)(b)/;
ok $0 ~~ Match, 'an unquantified capture stays a Match';
