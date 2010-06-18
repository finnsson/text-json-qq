## Text.JSON.QQ ##

JSON quasiquatation for Haskell.

This package expose the function `jsonQQ` that compile time converts json code into a `Text.JSON.JSValue`.
`jsonQQ` got the signature

    jsonQQ :: QuasiQuoter

and is used like

    myCode = [$jsonQQ| {age: 23, name: "Pelle", likes: ["mac","Haskell"] } |]

where it is important that

* you got no space in `[$jsonQQ|` and
* no additional code after `|]`.

The quasiquatation can also bind to variables like

    myCode = [$jsonQQ | {age: <<age>>, name: <<name>>} |]
     where age = 34 :: Integer
           name = "Pelle"

where a function `toJsonOrId` (that is also exported by `Text.JSON.QQ`) will be called on `age` and `name` runtime.
