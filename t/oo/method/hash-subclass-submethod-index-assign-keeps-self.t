use Test;

# PDF::COS::Dict: `self{k} = v` inside a submethod, reached from an `is rw`
# accessor whose closure captured an Attribute as `self` (COSAttrHOW.compose),
# overwrote the invocant's local slot with that stale Attribute, so the
# `self.required-entries` right after died "No such method ... Attribute+{...}".

plan 4;

class Dict {...}
my @seen;
class COSAttr {
    has $.type;
    multi method tie($lval is rw, :$check) is rw { $lval }
}
role Tie {
    has Attribute %.entries;
    has Attribute $.of-att is rw;
    method lvalue($v) { $v ~~ Hash ?? Dict.new(:d($v)) !! $v }
    method ASSIGN-KEY($key, $val, :$check) {
        my $lval = self.lvalue($val);
        my Attribute \att = %!entries{$key} // $.of-att;
        .tie($lval, :$check) with att;
        self.BIND-KEY($key, $lval)
    }
    method AT-KEY($key, :$check) is rw {
        my $val := callsame;
        with %!entries{$key} // $.of-att { .tie($val, :$check) } else { $val }
    }
    method rw-accessor(Attribute $att, Str :$key!) is rw {
        my $val;
        sub FETCH($) { $val }
        sub STORE($, \v) { $val := self.ASSIGN-KEY($key, v, :check) }
        Proxy.new: :&FETCH, :&STORE;
    }
}
role AttrHow {
    has COSAttr $.cos is rw handles<tie>;
    method compose($package) {
        my $key = self.name.subst(/^\$'!'/, '');
        my &accessor = sub (\obj) is rw { obj.rw-accessor(self, :$key) };
        &accessor.set_name($key);
        $package.^add_method($key, &accessor);
    }
}
class Dict is Hash does Tie {
    method new(:$d) { self.bless(:$d) }
    submethod TWEAK(:$d) {
        my %entries := self.entries;
        for $d.pairs.sort { self{.key} = .value }
        @seen.push(self.^name);
    }
}
class Top is Dict { has $!plain }
my $att = Top.^attributes.first(*.name eq '$!plain');
$att does AttrHow;
$att.cos .= new(:type(Hash));
$att.compose(Top);
my $t = Top.new(:d{});
$t.plain = {Name => 'Fred'};


is @seen.join(','), 'Top,Dict', 'self stays the invocant after an index-assign in TWEAK';
is $t.^name, 'Top', 'the object is the class instance';
ok $t.entries ~~ Hash, 'a method call after the assignment still finds the invocant';
lives-ok { $t.plain = {Name => 'Fred2'} }, 'a second assignment works too';
