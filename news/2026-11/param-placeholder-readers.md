# Two more parameter-placeholder readers use `ParamDef.target`

The sub-signature variable collector (`param_list.rs`) and the parametric-role type-parameter
filter (`registration_role.rs`) ask the `ParamDef` for its form (`is_parens`, `is_bare_type_capture`)
instead of comparing the placeholder name (ADR-15761). No behaviour change.
