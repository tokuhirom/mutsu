# `is export` classes of a `unit module X:ver<..>:auth<..>` reach the importer

`use Qt::QtWidgets` loads `unit module QtWidgets:ver<0.0.7>:auth<zef:yguillemot>:api<2>;`,
whose file is named differently from its module. With the version adverbs the unit declaration
lowers to a synthetic block (metadata call plus the package), which `detect_unit_package_name`
did not look into, so the module's declared name was unknown and its exported classes and roles were
never aliased under their short names (`class QSize is QtObject` died with an unknown parent).
The detection now sees through the synthetic block.
