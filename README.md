# Elm-style Json Decoding Package for Roc

This uses [lukewilliamboswell](https://github.com/lukewilliamboswell)'s [roc-json](https://github.com/lukewilliamboswell/roc-json) library to do the actual serialization and deserialization.
The API is basically a port of the Elm Json.Decode package.

## Example

```sh
roc run examples/simple1.roc
```

## Documentation

[Documentation](https://enkidatron.github.io/roc-json-value/)

You can also generate the documentation locally using `roc docs package/main.roc` and then serving the html files using your favorite local web server.

## Notes about floating point numbers

This library uses the `Dec` type when decoding numbers. This is because the floating point types do not implement the `Eq` ability, which the `Value` type needs to have. `Value`s need to have `Eq` because they are included in the `Error` type, and so the overall Results becomes uncomparable otherwise.

This also means that this library does not have recipes for floating points numbers, because the Roc compiler does not yet support converting `Dec` values into floating point numbers.

## TODO
- Write readme DONE
- more number recipes DONE
- more number encoding functions DONE
- documentation for all exposed functions DONE enough
- implementations DONE
- tests DONE
- push to github DONE
- publish documentation DONE
- create release
- automate publishing documentation
- automate tests
- error_to_str
