open System
open HierarchyAllocation
open GA

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    
    //building the company tree
    let root = new Node<PDATA>("Monsters Inc.",
                    {
                     PDATA.RETURNMODEL= fun x -> 0.07*x;
                     PDATA.EXPENSEMODEL=fun x -> 0.01*x;
                     PDATA.RISK=10.0;
                     PDATA.RWA = fun _ -> 0.08
                    }
                 )

    let N10 = new Node<PDATA>("South America",
                        {
                         PDATA.RETURNMODEL= fun x -> 0.07*x;
                         PDATA.EXPENSEMODEL=fun x -> 0.01*x;
                         PDATA.RISK=10.0;
                         PDATA.RWA = fun _ -> 0.08
                        }
                      )
                      
    let N11 = new Node<PDATA>("North America",
                               {
                                PDATA.RETURNMODEL= fun x -> 0.07*x;
                                PDATA.EXPENSEMODEL=fun x -> 0.01*x;
                                PDATA.RISK=10.0;
                                PDATA.RWA = fun _ -> 0.08
                               }
                            )


    let N12 = new Node<PDATA>("Asia",
                               {
                                PDATA.RETURNMODEL= fun x -> 0.07*x;
                                PDATA.EXPENSEMODEL=fun x -> 0.01*x;
                                PDATA.RISK=10.0;
                                PDATA.RWA = fun _ -> 0.08
                               }
                            )

    let N13 = new Node<PDATA>("Australia",
                               {
                                PDATA.RETURNMODEL= fun x -> 0.07*x;
                                PDATA.EXPENSEMODEL=fun x -> 0.01*x;
                                PDATA.RISK=10.0;
                                PDATA.RWA = fun _ -> 0.08
                               }
                            )
        
    let divisions = [N10;N11;N12;N13]
    divisions |> Seq.iter(fun d -> d.Parent <- Some(root))
    root.Children.AddRange(divisions)
    
    let listofnode = root.ListOfNodes
    
    let computeTree(tree:Node<PDATA>)=
       let x = tree.ListOfNodes |> Seq.map(fun s -> s.Data.RISK)|>Array.ofSeq
       10.0 + ([0 .. x.Length-1] |> Seq.map(fun i -> x.[i]**2.0- 10.0 * System.Math.Cos(2.0* System.Math.PI*x.[i])) |> Seq.sum)
       
    //define the portolio global rule:
    let chromo =  (new ChromosomeBuilder()).WithParameter(-5.12,5.12,14,None) //root
                                           .WithParameter(-5.12,5.12,14,None) //D10
                                           .WithParameter(-5.12,5.12,14,None) //D11                                      
                                           .WithParameter(-5.12,5.12,14,None) //D12                                      
                                           .WithParameter(-5.12,5.12,14,None) //D13                                      
                                           .Build()

    let fitness (x:float[]) =
       x |> Array.iteri(fun i y -> listofnode.[i].Data <- {listofnode.[i].Data with RISK=y})
       computeTree(root)
           
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
