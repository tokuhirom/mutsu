# Same-named nested `my role` in two roles no longer collide

Two roles that each declare a nested `my role W` now keep their own `W` once both are
composed: the nested role's bare name is part of the role body's lexicals, so a method of
the first role no longer resolves `W` to the role composed last (#12773).
