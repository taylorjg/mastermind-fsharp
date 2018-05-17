module EvaluateGuessTests

open Expecto
open Mastermind

[<Tests>]
let tests =
  testList "evaluateGuess" [

    testCase "no overlap" <| fun _ ->
      let secret = { p0 = R; p1 = R; p2 = G; p3 = G }
      let guess = { p0 = BL; p1 = BL; p2 = WH; p3 = WH }
      let score = evaluateGuess secret guess
      Expect.equal score.blacks 0 "score.blacks"
      Expect.equal score.whites 0 "score.whites"

    testCase "exact match" <| fun _ ->
      let secret = { p0 = R; p1 = R; p2 = G; p3 = G }
      let guess = { p0 = R; p1 = R; p2 = G; p3 = G }
      let score = evaluateGuess secret guess
      Expect.equal score.blacks 4 "score.blacks"
      Expect.equal score.whites 0 "score.whites"

    testCase "correct colours in wrong positions" <| fun _ ->
      let secret = { p0 = R; p1 = G; p2 = B; p3 = Y }
      let guess = { p0 = Y; p1 = B; p2 = G; p3 = R }
      let score = evaluateGuess secret guess
      Expect.equal score.blacks 0 "score.blacks"
      Expect.equal score.whites 4 "score.whites"
  ]
