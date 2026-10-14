# Naming a role type object no longer runs its body

`R.^name`, `R.WHAT`, `R.HOW` and the other identity meta-methods on a role type object
used to pun the role first, so a `say` in the role body fired. The body now runs only when
the role is composed or punned (#13103).
