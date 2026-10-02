# `bless` leaves BUILD-owned named args to BUILD

`self.bless(:address(...))` on a class whose own `submethod BUILD` declares the parameter used to
store the named argument into the attribute first, so a typed `has Int @.address` rejected a value
BUILD would have processed (Net::Netmask `::ffff:12.34.56.78`). Like `.new`, `bless` now honours the
BUILD-owned attribute set (`build_owning_attr_names`). Net::Netmask goes partial -> green.
