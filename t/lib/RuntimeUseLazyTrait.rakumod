unit module RuntimeUseLazyTrait;

# A minimal reimplementation of the AttrX::Lazy shape (#8806's real trigger):
# a custom `trait_mod:<is>` that mixes ONE role into the Attribute meta-object
# (`$attr does AttrRole`) and, separately, mixes ANOTHER role into the
# composing class's HOW (`$class.HOW does HowRole`) so its `compose` hook can
# install a lazy accessor. Two mixins from the SAME handler call is the shape
# that used to corrupt mutsu's `trait_mod:<is>` writeback slot.

my role AttrRole {
    has $.base-name = self.name.substr(2);
}

my role HowRole {
    method compose(Mu \type) {
        for type.^attributes.grep(AttrRole) -> $attr {
            my $accessor = $attr.base-name;
            type.^add_method($accessor, method (Mu:D:) {
                my $val = $attr.get_value(self);
                unless $val.defined {
                    $val = self."build-{$accessor}"();
                    $attr.set_value(self, $val);
                }
                return $val;
            });
        }
        callsame;
    }
}

multi trait_mod:<is>(Attribute:D $attr, :$runtime-use-lazy!) is export {
    my $class := $attr.package;
    $attr does AttrRole;
    unless $class.HOW ~~ HowRole {
        $class.HOW does HowRole;
    }
}
