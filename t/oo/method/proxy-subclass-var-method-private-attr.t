use Test;

# A method a Proxy subclass declares, called on its `.VAR`, when a *private*
# attribute shares the method's name (AttrX::Mooish's `AttrProxy`:
# `has Bool $!is-set` next to `method is-set`). The method answers; a public
# attribute's accessor of that name still answers from the attribute.

plan 4;

my class P is Proxy {
    has Bool $!is-set is default(False);
    has $.label;
    method is-set { $!is-set }
    method mark { $!is-set = True; self }
    method !SET-SELF($label) { $!label := $label; self }
    method new(:$FETCH, :$STORE, :$label) is raw {
        callwith(:$FETCH, :$STORE).VAR!SET-SELF($label)
    }
}

my $p := P.new(FETCH => -> $ { 42 }, STORE => -> $, $ { }, :label<public>);
is $p, 42, 'the Proxy fetches';
is $p.VAR.is-set, False, 'the method answers, not the private attribute';
$p.VAR.mark;
is $p.VAR.is-set, True, 'and reads the state the other method set';
is $p.VAR.label, 'public', 'a public attribute keeps its accessor';
