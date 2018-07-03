
open GA



// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    
    let chromo = (new ChromosomeBuilder()).WithParameter(-5.12,5.12,12,None)
                                          .WithParameter(-5.12,5.12,12,None)
                                          .WithParameter(-5.12,5.12,12,None)                                       
                                          .Build()

    let fitness (x:float[]) = 10.0 + ([0 .. x.Length-1] |> Seq.map(fun i -> x.[i]**2.0- 10.0 * System.Math.Cos(2.0* System.Math.PI*x.[i])) |> Seq.sum)
    
    let treeGA = new TreeGA(100,10,0.01,fitness,5000.0,0.97,0.5,chromo)

    let fitnessvec = [|0 .. 100|] |> Array.map(fun n -> treeGA.Next()) |> Array.ofSeq

    printfn "%A" (treeGA.GetHistoryBest())
       

        
    0 // return an integer exit code
