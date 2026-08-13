use Cro::Transform;

role Cro::Connector {
    method consumes() { ... }
    method produces() { ... }

    method connect(*%options --> Promise) { ... }

    method establish(Supply $incoming, *%options --> Supply) {
        return supply {
            my Promise $connection = self.connect(|%options);
            whenever $connection -> Cro::Transform $transform {
                whenever $transform.transformer($incoming) -> $msg {
                    emit $msg;
                }
            }
        }
    }
}
