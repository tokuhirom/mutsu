use SpawnA;
unit class SpawnB;
method run() {
    my Int $n = 0;
    for <x y> { SpawnA.new.h; $n = $n + 1 }
    $n
}
