module HelloWorld exposing (helloWorld)

-- It's good style to include any types at the top level of your modules.


helloWorld : Maybe String -> String
helloWorld name =
    -- Debug.crash "Please implement this function"
    "Hello, " ++ (Maybe.withDefault "World" name) ++ "!"