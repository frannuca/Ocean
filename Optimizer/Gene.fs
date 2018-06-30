namespace OptimizationCore

open System.Collections
open MathNet.Numerics

type Gene(minlevel:float,maxlevel:float,nbit:int)=
    
    let mutable bits:bool array = Array.zeroCreate nbit 
    let mxint = pown 2 nbit

    let dx = maxlevel-minlevel
    member self.SetNumber(x:float)=        
        let y = if x > maxlevel then 1.0
                else if x < minlevel then 0.0
                else (x-minlevel)/dx
        
        let n =  int(y* (float)mxint)
        bits <- System.Convert.ToString(n,2).ToCharArray() |> Array.map(fun c -> c='1') |> Array.rev
        self

    member self.GetFloat()=
        let x = bits |> Array.mapi(fun i x -> if x then (pown 2 i) else 0 ) |> Array.sum |> float
        minlevel+ x/float(mxint)*dx