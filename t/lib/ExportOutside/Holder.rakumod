use ExportOutside::Thing;
unit class ExportOutside::Holder is export;
has Thing $.thing = Thing.new;
method hi { $!thing.hi }
