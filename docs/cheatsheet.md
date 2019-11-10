## Operators ##

### `<<<` - depends on targets (run sequentially)

> `"main" <<< ["restore"; "build-debug"; "unit-test"]`

### `<==` - depends on targets that are allowed to run in parallel
> `"main" <<< ["build-debug"; "build-release"]`

