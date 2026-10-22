unit module ModuleUnitBind::Vars;

our $mub-value is export = 'initial';
our @mub-list is export = <a b>;

class MubClass is export {
    method hi() { 'hi' }
}
