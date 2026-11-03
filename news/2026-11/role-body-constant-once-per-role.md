# A role's `my constant` is initialised once for all composing classes

Two classes composing the same non-parametric role now read one binding of the
role body's `my constant`; the initializer no longer runs again for each
composition, as on Rakudo. A body `my @x` is still computed per composing class,
and a parametric role keeps one evaluation per specialization. (#16255)
