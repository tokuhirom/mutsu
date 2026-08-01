# A monitor-shaped EXPORTHOW::DECLARE HOW exercising the full registration
# protocol mutsu drives for a DECLARE'd class: `new_type` (callsame resolves
# to the registered type) + `add_attribute` through the HOW, `add_method`
# wrapping every method and re-adding it via the fully-qualified native
# `self.Metamodel::ClassHOW::add_method`, and `compose` installing a
# BUILDALL/POPULATE pair via `anon method` (whose `callsame` resolves to the
# built instance). Mirrors OO::Monitors' MetamodelX::MonitorHOW.
class MetamodelX::TraceHOW is Metamodel::ClassHOW {
    has $!trace-attr;

    method new_type(|) {
        my \type = callsame();
        type.HOW.setup-trace(type);
        type
    }

    method setup-trace(Mu \type) {
        $!trace-attr = Attribute.new(
            name => '$!TRACE-log',
            type => Str,
            package => type
        );
        self.add_attribute(type, $!trace-attr);
    }

    method add_method(Mu \type, $name, $meth) {
        unless $name eq 'BUILDALL' | 'POPULATE' | 'clone' {
            $meth.wrap: -> \SELF, | {
                if SELF.DEFINITE {
                    my $log = $!trace-attr.get_value(SELF);
                    $!trace-attr.set_value(SELF, $log ~ $name ~ ";");
                    LEAVE $!trace-attr.set_value(SELF, $!trace-attr.get_value(SELF) ~ "/" ~ $name ~ ";");
                    callsame
                }
                else {
                    callsame
                }
            }
        }
        self.Metamodel::ClassHOW::add_method(type, $name, $meth);
    }

    method compose(Mu \type) {
        my %methods := self.method_table(type);
        my $trace-attr := $!trace-attr;
        unless %methods<POPULATE>:exists or %methods<BUILDALL>:exists {
            my $method := anon method POPULATE(Mu \SELF: |) {
                $trace-attr.set_value(SELF, "init;");
                callsame;
            }
            self.add_method(type, 'BUILDALL', $method);
            self.add_method(type, 'POPULATE', $method);
        }
        self.Metamodel::ClassHOW::compose(type);
    }
}

my package EXPORTHOW {
    package DECLARE {
        constant traced = MetamodelX::TraceHOW;
    }
}
