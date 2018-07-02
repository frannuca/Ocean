open GA

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    
    let chromo = (new ChromosomeBuilder()).WithParameter(0.0,100.0,10,None)
                                          .WithParameter(0.0,100.0,10,None)
                                          .WithParameter(0.0,100.0,10,None)                                       
                                          .Build()

    let fitness (x:float[]) = ((x.[0]-12.5)**2.0+(x.[1]-25.5)**2.0+(x.[2]-67.5)**2.0+1e-6)
    let treeGA = new TreeGA(200,10,0.01,fitness,5000.0,0.99,1.0,chromo)

    [0 .. 10000 ] |> Seq.iter(fun n -> printfn "%d" n; treeGA.Next())

        
    0 // return an integer exit code
