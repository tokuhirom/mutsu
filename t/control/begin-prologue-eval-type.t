use Test;

plan 6;

# A nested BEGIN whose `EVAL` text is a literal is lifted from a scope that
# declares a type when the text, parsed on its own, does not change a type
# through its metaobject (ADR-0134, #13096). None of the enclosing routines is
# called before its value is checked, so each value was stored by the lifted
# BEGIN alone.

my ($name, $method-form, $reads-scope, $nested, $after);
sub evals-type-name { my class K { }; BEGIN $name = EVAL q[K.^name] }
is $name, 'K', 'an EVAL naming a type of the scope';

sub evals-method-form { my class KM { }; BEGIN $method-form = q[KM.^name].EVAL }
is $method-form, 'KM', 'the .EVAL method form';

sub evals-scope-names {
    my $x; BEGIN $x = 5;
    sub h { 'h' }
    my class KS { }
    BEGIN $reads-scope = EVAL q[KS.^name ~ $x ~ h()];
}
is $reads-scope, 'KS5h', 'the EVAL text still reaches the variables and routines of the scope';

sub evals-nested { my class KN { }; BEGIN $nested = EVAL q[EVAL q{KN.^name}] }
is $nested, 'KN', 'an EVAL nested in the literal text';

sub after-eval { BEGIN $after = 'lifted' }
is $after, 'lifted', 'a literal EVAL no longer keeps later BEGINs in place';

# A text that changes the type keeps the BEGIN in place, since the scope's own
# declaration would register the type afresh and lose the change; that halts
# every later nested BEGIN, so it comes last.
sub adds-method {
    my class KA { }
    BEGIN EVAL q[KA.^add_method('hi', method { 'hi' })];
    KA.hi
}
is adds-method(), 'hi', 'a literal EVAL changing the type changes the real one';
