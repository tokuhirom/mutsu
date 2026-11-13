# `nqp::unipropcode` knows the General_Category aggregates and value names

`nqp::unipropcode('P')` (and `L`, `LC`, `M`, `N`, `S`, `Z`, `C` and their
long names) answers MoarVM's code for that aggregate, a binary property over
the categories it groups, and a General_Category, Script or Block value name
(`Lu`, `Greek`, `ASCII`) answers its property's code, as MoarVM's name table
does. Red's regex compiler resolves `<:P>` and friends through them; its
t/109 died on `<graph>` before.
