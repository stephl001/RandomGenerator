namespace RandomGenerator

open System
open Generator

module Random =
    
    type Random<'a> = Random of (Generator -> 'a)
    
    let choose minBound maxBound =
        let innerFn (Generator gen) =
            gen minBound maxBound
        
        Random innerFn
        
    let sample gen (Random rdnFn) =
        rdnFn gen
        
    let samples count random gen =
        List.init count (fun _ -> sample random gen)
        
    let constant constVal =
        Random (fun _ -> constVal)
        
    let twoR r1 r2 =
        let innerFn gen =
            (sample gen r1, sample gen r2)
            
        Random innerFn
        
    let (.>>.) = twoR
    
    let bindR f r =
        let innerFn gen =
            sample gen r |> f |> sample gen
            
        Random innerFn
        
    let ( >>= ) r f = bindR f r
    
    let mapR f =
        bindR (f >> constant)
        
    let ( <!> ) = mapR
    let ( |>> ) r f = mapR f r
    
    let applyR fR r =
        fR >>= (fun f ->
            r >>= (fun x ->
                f x |> constant))
        
    let ( <*> ) = applyR
    
    let map2 f r1 r2 =
        constant f <*> r1 <*> r2
        
    let map3 f r1 r2 r3 =
        constant f <*> r1 <*> r2 <*> r3
        
    let rec sequence randoms =
        let cons head tail = head::tail
        let consR = map2 cons
        
        match randoms with
        | [] -> constant []
        | head::tail -> consR head (sequence tail)
        
    let oneOf (values:'a[]) =
        choose 0 values.Length |>> Array.get values
        
    let filter predicate r =
        let rec innerFn gen =
            let res = sample gen r
            if predicate res then res else innerFn gen
            
        Random innerFn
        
    let shuffle items =
        let arrayCopy = Array.copy items
        let innerFn gen =
            let swap x y (arr:'a[]) =
                let tmp = arr[x]
                arr[x] <- arr[y]
                arr[y] <- tmp
            let swapWithRandom i _ =
                let randomIndex = choose 0 arrayCopy.Length |> sample gen
                arrayCopy |> swap i randomIndex
            arrayCopy |> Array.iteri swapWithRandom
            arrayCopy
        
        Random innerFn
            