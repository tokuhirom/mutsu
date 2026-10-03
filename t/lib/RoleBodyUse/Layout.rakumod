# Fixture for t/oo/role/role-body-use-at-declaration.t: a unit role composing
# a `my role` declared earlier in its own body (PDF::Attributes::Layout).
unit role RoleBodyUse::Layout;

my role Common { method common { 'common' } }
also does Common;

my enum Fit « :FitWindow<Fit> :FitHoriz<FitH> »;
multi method construct(FitWindow) { 'window' }
multi method construct(FitHoriz)  { 'horiz' }
method fit-horiz { self.construct(FitHoriz) }
