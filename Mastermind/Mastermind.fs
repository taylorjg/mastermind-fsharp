module Mastermind

type Peg = R | G | B | Y | BL | WH
type Code = { p0: Peg; p1: Peg; p2: Peg; p3: Peg }
type Score = { blacks: int; whites: int }

let private allPegs = [R; G; B; Y; BL; WH]

let allCodes =
    seq {
        for p0 in allPegs do
        for p1 in allPegs do
        for p2 in allPegs do
        for p3 in allPegs do
        yield { p0 = p0; p1 = p1; p2 = p2; p3 = p3 }
    }    

let private allScores =
    seq {
        for blacks in 0..4 do
        for whites in 0..4 - blacks do
        if not (blacks = 3 && whites = 1) then
            yield { blacks = blacks; whites = whites }
    }

let private pegs code = [code.p0; code.p1; code.p2; code.p3]

let evaluateGuess secret guess =
    let spegs = pegs secret
    let gpegs = pegs guess
    let countMatchingPegs p1 pegs = List.filter (fun p2 -> p1 = p2) pegs |> List.length
    let minPegs p = min (countMatchingPegs p spegs) (countMatchingPegs p gpegs)
    let sumOfMins = List.sumBy minPegs allPegs
    let blacks = List.zip spegs gpegs |> List.filter (fun (a, b) -> a = b) |> List.length
    let whites = sumOfMins - blacks
    { blacks = blacks; whites = whites }

let initialGuess = { p0 = R; p1 = R; p2 = G; p3 = G }    

let private autosolve' attempt set acc =
    let guesses = [] : (Code * Score) list
    guesses

let autosolve attempt =
    autosolve' attempt [] []
