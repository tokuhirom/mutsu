# Exported subsets are known types in the importing file

The importer's static module scan recorded exported classes, roles and enums but ignored `subset`
declarations, so `when Base64Binary { ... }` (a `subset ... is export` from another module) was
rejected as a routine call that gobbled its block. The scan now registers subsets too. This takes
the FHIR distribution's `FHIR::JsonSerdes`, `FHIR::DomainModel` and `FHIR::Store` from a parse
failure to loading.
