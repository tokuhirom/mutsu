//! Tests of the instances group (ADR-11276 §10, slice 3D).

#[test]
#[ignore = "input for scripts/gen-rakuast-accessor-rows.py"]
fn dump_rakuast_accessor_schema() {
    for (class, measured, names) in crate::rakuast::accessor_schema() {
        println!("MODEL_CLASS\t{class}\t{measured}");
        for name in names {
            println!("MODEL_ACCESSOR\t{class}\t{measured}\t{name}");
        }
    }
    for row in super::rows().filter(|row| {
        row.owner.starts_with("RakuAST::")
            && row.arity == 0
            && !super::rakuast_accessors::declares_generated(row.owner, row.name)
    }) {
        println!("REGISTERED_ACCESSOR\t{}\t{}", row.owner, row.name);
    }
}
