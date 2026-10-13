use Test;

plan 3;

# A method called on a role type object runs on the role's pun, so
# `self.^is_pun` is true there and `self.^pun_source` is the role
# (PDF::COS::Tie's `induce`, which mixes the role into the coerced object).
role R {
    method probe { (self.^is_pun, self.^pun_source.^name) }
    method induce($obj) { $obj.^mixin(self.^pun_source) if self.^is_pun; $obj }
}
my ($is-pun, $source) = R.probe;
ok $is-pun, '.^is_pun on the pun';
is $source, 'R', '.^pun_source names the role';
class C { }
ok R.induce(C.new) ~~ R, 'the pun source can be mixed in';
