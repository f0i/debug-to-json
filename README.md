# Debug to JSON

Get a JSON structure from a string generated by [Debug.toString](https://package.elm-lang.org/packages/elm/core/latest/Debug#toString).

## Install

```
elm install f0i/debug-to-json
```

## Examples

```elm
import DebugToJson

formated =
    { asdf = "qwer" }
        |> Debug.toString
        |> DebugToJson.ppp
```

`formated` will now contain the following String:

```
{
    "asdf": "qwer"
}
```

A complete list of the helper functions can be found in the
[DebugToJson module docs](DebugToJson).
