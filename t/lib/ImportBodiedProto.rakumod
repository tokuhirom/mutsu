unit module ImportBodiedProto;

# Candidate-less protos, for t/modules/import-export/imported-candidate-less-proto.t
proto ibp-answer($x) is export { "IBP answer $x" }
proto ibp-star(|) is export {*}
