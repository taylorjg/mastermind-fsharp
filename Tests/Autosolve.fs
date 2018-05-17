module AutosolveTests

open Expecto
open Mastermind

[<Tests>]
let tests =
  testList "autosolve" [

    testCase "fixed secret" <| fun _ ->
      let secret = { p0 = G; p1 = B; p2 = BL; p3 = WH }
      let guesses = autosolve (evaluateGuess secret)
      Expect.isGreaterThanOrEqual (List.length guesses) 1 "at least 1 guess"
      Expect.isLessThanOrEqual (List.length guesses) 5 "no more than 5 guesses"
      Expect.equal (List.last guesses |> fst) secret "last guess matches secret"
      Expect.equal (List.last guesses |> snd) { blacks = 4; whites = 0 } "last score is 4 blacks"
  ]
