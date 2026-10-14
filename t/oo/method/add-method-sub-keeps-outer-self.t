use Test;

plan 3;

# A `sub` installed with `^add_method` gets the receiver as its first
# positional parameter; `self` inside it is still the `self` of the method
# that created it (PDF::COS::Tie's entry accessors pass their Attribute).
class Holder {
    has $.name = 'holder';
    method make { sub (\obj) { "{self.name}/{obj.name}" } }
    method make-block { -> $o { "{self.name}/{$o.name}" } }
}
class Tgt { has $.name = 'tgt' }
Tgt.^add_method('acc', Holder.new.make);
Tgt.^add_method('blk', Holder.new.make-block);
Tgt.^add_method('meth', method { self.name });
is Tgt.new.acc, 'holder/tgt', 'sub: self is the creator, the parameter is the receiver';
is Tgt.new.blk, 'holder/tgt', 'pointy block: the same';
is Tgt.new.meth, 'tgt', 'a method literal still gets the receiver as self';
