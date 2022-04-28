namespace RandomGenerator

open System.Security.Cryptography

module Generator =
    
    type Generator = Generator of (int -> int -> int)
    
    let devGenerator seed =
        let gen = System.Random(seed)
        let innerFn minBound maxBound =
            gen.Next(minBound, maxBound)
            
        Generator innerFn
        
    let prodGenerator =
        let innerFn minBound maxBound =
            RandomNumberGenerator.GetInt32(maxBound - minBound) + minBound
        
        Generator innerFn