use Test;
use nqp;

plan 3;

# A Proxy subclass's public accessor answers the attribute's value, not its
# cell: a later store into the cell must not change what the caller already took
# (#14266, AttrX::Mooish's `:old-value`).
class Z { has $.x; }
my Mu $aviv := Z.^attributes[0].auto_viv_container;

my class P is Proxy {
    has Mu $.val;
    has Bool $!is-set is default(False);
    method new(:$FETCH, :$STORE) is raw { callwith(:$FETCH, :$STORE) }
    method store-value(Mu $value is raw, Mu $aviv is raw) is raw {
        unless ⚛$!is-set {
            $!val := nqp::isconcrete_nd($aviv) ?? nqp::clone_nd($aviv) !! $aviv.new;
        }
        nqp::if(nqp::iscont($!val), ($!val = $value), ($!val.STORE($value)));
        $!is-set ⚛= True;
        $!val
    }
}

my $p := P.new(FETCH => -> $ { 1 }, STORE => -> $, $v { });
$p.VAR.store-value("first", $aviv);
my $params = \( :old-value( nqp::clone($p.VAR.val) ) );
my $taken = $p.VAR.val;
$p.VAR.store-value("second", $aviv);
is $params<old-value>, 'first', 'a captured old value does not follow the store';
is $taken, 'first', 'neither does a plain read';
is $p.VAR.val, 'second', 'the accessor answers the current value';
