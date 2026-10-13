# A compound-named role no longer shadows a core type in its prefix package

A role declared with a compound name (`unit role PDF::COS::Bool`) was not
recorded as compound-declared, as a compound-named class is, so the outward
type lookup treated `PDF::COS` as a scope that owned a `Bool`. Code under
`PDF::COS` (`has Bool $.is-required` in `PDF::COS::Tie`) then type-checked
against the role and rejected `True`. Role declarations now record their
compound name the same way, which lets PDF's `t/cos-coerce.t` load (#12907).
