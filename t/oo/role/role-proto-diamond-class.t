use Test;

plan 1;

role ProtoDiamondClass::Base {
    proto rule filler {*}
}
role ProtoDiamondClass::Left does ProtoDiamondClass::Base { }
role ProtoDiamondClass::Right does ProtoDiamondClass::Base { }
class ProtoDiamondClass::Consumer
    does ProtoDiamondClass::Left
    does ProtoDiamondClass::Right
{ }

lives-ok { ProtoDiamondClass::Consumer.new },
    'a class can compose a shared proto rule through a diamond';
