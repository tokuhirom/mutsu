class MetamodelX::MonitorHOW is Metamodel::ClassHOW {
    has $!lock-attr;

    method new_type(|) {
        my \type = callsame();
        type.HOW.setup_monitor(type);
        type
    }

    method setup_monitor(Mu \type) {
        $!lock-attr = Attribute.new(
            name => '$!MONITR-lock',
            type => Lock,
            package => type
        );
        self.add_attribute(type, $!lock-attr);
    }

    # Make sure that .^attributes will hide the monitor lock attribute
    # the the right environment variable is set
    method attributes(|) {
        if $*EXCLUDE-MONITOR-LOCK {
            my @attributes;

            # The standard .^attributes method returns an NQPArray, which
            # doesn't iterate in Raku land.  However, the standard argument
            # handling will HLLize any NQP land objects, so it will return
            # an iterable List when given an NQPArray object.
            my sub hllize(Mu $a) { $a<> }

            for hllize(callsame) -> $attr {
                @attributes.push($attr) unless $attr.name eq '$!MONITR-lock';
            }
            @attributes
        }
        else {
            nextsame
        }
    }

    method add_method(Mu \type, $name, $meth) {
        unless $name eq 'BUILDALL' | 'POPULATE' | 'clone' {
            $meth.wrap: -> \SELF, | {
                if SELF.DEFINITE {
                    # Instance method call; acquire lock.
                    my $*MONITOR := SELF;
                    my $lock = $!lock-attr.get_value(SELF);
                    $lock.lock();
                    LEAVE $lock.unlock();
                    callsame
                }
                else {
                    # Type object method call; delegate (presumably
                    # .new or some such).
                    callsame
                }
            }
        }
        self.Metamodel::ClassHOW::add_method(type, $name, $meth);
    }

    method compose(Mu \type) {
        my %methods   := self.method_table(type);
        my $lock-attr := $!lock-attr;

        if %methods<POPULATE>:exists {
            %methods<POPULATE>.wrap: -> \SELF, | {
                $!lock-attr.set_value(SELF, Lock.new);
                callsame();
            };
        }
        elsif %methods<BUILDALL>:exists {
            %methods<BUILDALL>.wrap: -> \SELF, | {
                $!lock-attr.set_value(SELF, Lock.new);
                callsame();
            };
        }
        else {
            my $method := anon method POPULATE(Mu \SELF: |) {
                $lock-attr.set_value(SELF, Lock.new);
                callsame;
            }
            self.add_method(type, 'BUILDALL', $method);
            self.add_method(type, 'POPULATE', $method);
        }

        # Add a .clone method if there is none already.  This sets a
        # dynamic variable to make .^attributes hide the monitor lock
        # attribute when cloning and potentially twiddling (which is
        # what Mu.clone calls to find out which attributes to clone).
        # If there is a .clone method, it is assumed it knows what
        # it's doing.
        unless %methods<clone>:exists {
            self.add_method: type, 'clone', anon method clone(Mu \SELF: |) {
                my $*EXCLUDE-MONITOR-LOCK := True;
                my $cloned := callsame;
                $lock-attr.set_value(SELF, Lock.new);
                $cloned
            }
        }

        self.Metamodel::ClassHOW::compose(type);
    }
}

my package EXPORTHOW {
    package DECLARE {
        constant monitor = MetamodelX::MonitorHOW;
    }
}

# vim: expandtab shiftwidth=4
