namespace RandomGenerator

open System
open Random

module Text =

    let randomChar max startingChar =
        choose 0 max |>> (+) (int startingChar) |>> char
        
    let randomDigit =
        randomChar 10 '0'
        
    let randomLetter =
        randomChar 26 'a'
        
    let randomFromChar = function
        | '#' -> randomDigit
        | '%' -> randomLetter
        | c -> constant c
        
    let generateString input =
        Seq.toList input
        |> List.map randomFromChar
        |> sequence
        |>> (Array.ofList >> String)