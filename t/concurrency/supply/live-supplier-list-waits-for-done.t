use Test;

plan 2;

{
    my $supplier = Supplier.new;
    my $supply = $supplier.Supply;
    start {
        sleep 0.1;
        $supplier.emit(1);
        $supplier.emit(2);
        $supplier.done;
    }
    is-deeply $supply.list, (1, 2), 'list waits for a live Supplier to finish';
}

{
    my $supplier = Supplier.new;
    $supplier.emit(0);
    my $supply = $supplier.Supply;
    start {
        sleep 0.1;
        $supplier.emit(1);
        $supplier.done;
    }
    is-deeply $supply.list, (1,), 'a live Supply does not replay an earlier emission';
}
