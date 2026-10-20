# `also does R` composes R's attributes after R was punned

A role called as a type object (`R.method`) is punned to a class of the same name. When another role was then (re-)registered with `also does R`, the parent walk found the class first and recorded a class parent instead of composing the role, so `True but Other` lost `R`'s attributes. This showed up as `require ::($pkg)` inside a role's own method, which reloads the module that composes that role (PDF's `indobj-*.t`). A name that is both a role and a class is now taken as the role.
