open Mastermind

[<EntryPoint>]
let main argv =
    let secret = { p0 = G; p1 = B; p2 = BL; p3 = WH }
    printfn "secret: %A" secret
    let guesses = autosolve (evaluateGuess secret)
    printfn "Number of guesses: %d" <| List.length guesses
    let printGuess pair = printfn "guess: %A score: %A" (fst pair) (snd pair)
    List.map printGuess guesses |> ignore
    0
