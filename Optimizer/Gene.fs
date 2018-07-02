namespace OptimizationCore
namespace GA

open System.Collections
open MathNet.Numerics


type Gene(minlevel:float,maxlevel:float,nbit:int,genval:float option)=
    
    let mutable bits:bool array = Array.zeroCreate nbit 
    let mxint = pown 2 nbit
    let rnd = new Random.MersenneTwister(42)
    let dx = maxlevel-minlevel
    
    let setNumber(x:float)=        
        let y = if x > maxlevel then 1.0
                else if x < minlevel then 0.0
                else (x-minlevel)/dx
        
        let n =  int(y* (float)mxint)
        let xbits = System.Convert.ToString(n,2).ToCharArray() |> Array.map(fun c -> c='1') |> Array.rev
        bits <- Array.zeroCreate nbit
        xbits |> Array.iteri(fun idx v -> bits.[idx]<-v)

    do if genval.IsSome then setNumber(genval.Value)
    
    new (minlevel:float,maxlevel:float,nbit:int)= Gene(minlevel,maxlevel,nbit,None)

    member self.SetNumber = setNumber

    member self.SetBit(arr) = 
                                bits <- arr

    member self.SpawnAnotherGene()=
        new Gene(minlevel,maxlevel,nbit,Some(minlevel+ rnd.NextDouble()*dx))

    member self.GetFloat()=
        let x = bits |> Array.mapi(fun i x -> if x then (pown 2 i) else 0 ) |> Array.sum |> float
        minlevel+ x/float(mxint)*dx
    
    member self.Bits
        with get()=bits

    member self.BitSize
        with get()=bits.Length