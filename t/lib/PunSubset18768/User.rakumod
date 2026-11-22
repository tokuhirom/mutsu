# Fixture for t/oo/role/role-pun-my-subset-keeps-role-visible.t: a class of
# another compunit that calls a method on the role's type object.
use PunSubset18768::Units;
class PunSubset18768::User {
    method run { PunSubset18768::Units.value(3, 'pt').unit }
}
