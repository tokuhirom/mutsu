use Test;

# From AttrX::Mooish: `self.^get_attribute_for_usage('$!lazy').get_value(self) = $v`
# on an Attribute object a role was mixed into with `does`.

plan 8;

class C { has $.x is rw; has $!y }

{
    my $o = C.new(x => 1);
    C.^get_attribute_for_usage('$!x').get_value($o) = 5;
    is $o.x, 5, 'assigning to get_value stores into the object';
    C.^get_attribute_for_usage('$!y').get_value($o) = 7;
    is C.^get_attribute_for_usage('$!y').get_value($o), 7, 'also a private attribute';
}

role R {
    has $!lazy;
    has $.base-name is rw;
    method set-it($v) { self.^get_attribute_for_usage('$!lazy').get_value(self) = $v }
    method get-it { $!lazy }
}

{
    my $attr = C.^attributes[0];
    $attr does R;
    ok $attr.^attribute_table<$!lazy>.defined, 'a mixed-in role attribute is in the attribute table';
    is $attr.^get_attribute_for_usage('$!lazy').name, '$!lazy', 'and found for usage';
    $attr.set-it(42);
    is $attr.get-it, 42, 'get_value(self) = $v writes the mixin role cell';
    $attr.base-name = 'b';
    is $attr.base-name, 'b', 'public role attribute still works';
}

{
    my class D { has $!d }
    my $o = D.new but R;
    is $o.^get_attribute_for_usage('$!lazy').name, '$!lazy', 'an object mixin finds the role attribute too';
    throws-like { D.^get_attribute_for_usage('$!lazy') }, Exception, 'a class without the role does not';
}
