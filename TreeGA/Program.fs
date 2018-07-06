
open GA



// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    let chromo = (new ChromosomeBuilder()).WithParameter(-5.12,5.12,14,None)
                                          .WithParameter(-5.12,5.12,14,None)
                                          .WithParameter(-5.12,5.12,14,None)                                       
                                          .WithParameter(-5.12,5.12,14,None)                                       
                                          .WithParameter(-5.12,5.12,14,None)                                       
                                          .WithParameter(-5.12,5.12,14,None)                                       
                                          .WithParameter(-5.12,5.12,14,None)                                       
                                          .WithParameter(-5.12,5.12,14,None)                                       
                                          .WithParameter(-5.12,5.12,14,None)                                       
                                          .Build()

    let fitness (x:float[]) = 10.0 + ([0 .. x.Length-1] |> Seq.map(fun i -> x.[i]**2.0- 10.0 * System.Math.Cos(2.0* System.Math.PI*x.[i])) |> Seq.sum)
    let popsize = 100
    let elitism = 10
    let pmutation = 0.05
    let Cinit = 5000.0
    let preduction = 0.97
    let incTree = 0.5
    let treeGA = new TreeGA(popsize,elitism,pmutation,fitness,Cinit,preduction,incTree,chromo)

    let counter = ref 0
    let convergencereached = ref false
    while !counter < 1000 && not(!convergencereached) do
        treeGA.Next()
        counter := !counter + 1
        let h = treeGA.NDiff
        convergencereached := System.Math.Abs(h)< 0.01*(float)popsize



    printfn "Finished  number of iter %A \n %A " !counter  (treeGA.GetBest())
       

        
    0 // return an integer exit code
