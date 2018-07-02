namespace Optimization
namespace GA

open OptimizationCore
open GA


type TreeGA(popSize:int,elitism:int,pmutation:float,fitness:float array->float,
            Cinit:float,preduction:float,incTree:float,xchromo:Chromosome)=

     
    let nbitstotal = xchromo.BitSize
    let deplink = Array2D.init nbitstotal nbitstotal (fun _ _ -> new LinkProb(Cinit,preduction,incTree))
    let chromo = xchromo
    let rnd = new MathNet.Numerics.Random.MersenneTwister(639)
    
    let mutable population = new Population(popSize,chromo)
            
    let bestchromo:PopulationItem option = None

    let FillTree()=
        
        let mutable bitsoutofTree_deps=        
                [0 .. chromo.BitSize-2]
                |>List.map(fun i -> [i .. chromo.BitSize-1]
                                    |>List.map(fun j -> (i,j),deplink.[i,j].Info)
                                    )
                |> List.collect(id)
                |> List.filter(fun ((i,j),_) -> i<j)
                |> List.sortByDescending(fun (_,info) -> info)
                
        let bitsoutofTree = new System.Collections.Generic.List<int>([0 .. chromo.BitSize-1]) 
        let bitsIntheTree = new System.Collections.Generic.List<int>()

        let (i0,j0),_ = bitsoutofTree_deps.Head        

        let tree = new Node(i0)
        tree.Parent<-None
        tree.Children.Add(new Node(j0))

        bitsIntheTree.AddRange(tree.ListOfNodes |> Seq.map(fun x -> x.ID))
        bitsIntheTree |> Seq.iter(fun k -> bitsoutofTree.Remove(k)|>ignore)

        while bitsoutofTree.Count>0 do            
            let ((i,j),_) = bitsoutofTree_deps
                            |> List.filter(fun ((xi,xj),_)-> (bitsIntheTree.Contains(xi) && bitsoutofTree.Contains(xj)) ||
                                                             (bitsIntheTree.Contains(xj) && bitsoutofTree.Contains(xi)) )
                            |> List.maxBy(fun (_,f) -> f)

            let n = tree.ListOfNodes |> Seq.filter(fun n-> n.ID=i || n.ID=j) |> List.ofSeq |> List.head
            
            let ntoadd = if n.ID=i then j else i
            let xx = new Node(ntoadd)
            xx.Parent <- Some(n)
            n.Children.Add(xx)

            bitsIntheTree.Add(ntoadd)
            bitsIntheTree |> Seq.iter(fun k -> bitsoutofTree.Remove(k)|>ignore)
        tree

    member self.PopSize = popSize
    member self.Elitism = elitism
    member self.Pmut = pmutation
    member self.Fitness = fitness
   
    
    member self.Next()=
        population.EvalPopulation(fitness)
               
        //update tree of probabilities
        [|0 .. elitism|]
        |>Array.iter(fun i -> 
                    let {CHROMOSOME=chromo;FITNESS=fitval} = population.[i]
                    let bits = chromo.Bits
                    for i in 0 .. bits.Length-1 do
                        let bit_i = bits.[i]
                        for j in i .. bits.Length-1 do
                            let bit_j = bits.[j]
                            deplink.[i,j].Update(bit_i,bit_j)                                                       
            )
        |>ignore
        
        //build dependency tree:
        let tree = FillTree()

        
        //
        //generate new population
        let oldpop = population
        population <- new Population(popSize,chromo)

        for i in 0 .. population.Size-1 do
            let sschromo = population.[i]
            let vbit = sschromo.CHROMOSOME.Bits |> Array.ofSeq
            vbit.[tree.ID] <- if rnd.NextDouble() < deplink.[tree.ID,0].P00+deplink.[tree.ID,0].P01 then
                                false
                              else 
                                true

            tree.ListOfNodes
                     |> Seq.iter(fun node -> 
                                        let i = node.ID
                                        node.Children 
                                        |> Seq.iter(fun s -> 
                                                        vbit.[s.ID] <-
                                                                    if rnd.NextDouble() <  deplink.[i,s.ID].P(true,vbit.[i]) then
                                                                        true
                                                                    else 
                                                                        false
                                                        )
                                                
                                        ) 
        
            sschromo.CHROMOSOME.SetBits(vbit)
    //re-insert elite chromosomes
        [0 .. elitism-1]
        |> Seq.iter(fun i -> population.[i].CHROMOSOME.SetBits(oldpop.[i].CHROMOSOME.Bits))

        printfn "%s" (System.String.Join(",",population.[0].CHROMOSOME.GetVector()))
        

            


        

