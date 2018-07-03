namespace Optimization
namespace GA

open OptimizationCore
open GA
open System.Runtime.Serialization


type TreeGA(popSize:int,elitism:int,pmutation:float,fitness:float array->float,
            Cinit:float,preduction:float,incTree:float,xchromo:Chromosome)=

     
    let nbitstotal = xchromo.BitSize
    let fitnessvector= new System.Collections.Generic.List<PopulationItem>()

    let linkage = new LinkProbObject(xchromo.BitSize,Cinit,preduction,incTree)

    let chromo = xchromo
    let rnd = new MathNet.Numerics.Random.MersenneTwister(639)
    
    let mutable population = new Population(popSize,chromo)
            
    let bestchromo:PopulationItem option = None
    let FillTree()=
            let allnodes = [0 .. chromo.BitSize - 1] |> Seq.map(fun i -> new Node(i)) |> Array.ofSeq
            let root = allnodes.[0]
            MaxSpanningTree.ComputeBestMatch(root,allnodes,linkage)
            root

    let GenerateBit0(parent:Node)=
                        if rnd.NextDouble()< linkage.GetLinkageFor(parent.ID,parent.ID).P1_ then
                            true
                        else
                            false
    
    
    let GenerateBit(parent:Node,node:Node,bit_parent:bool)=
                    let link = linkage.GetLinkageFor(node.ID,parent.ID)
                    if rnd.NextDouble() < link.P(true,bit_parent) then
                        true
                    else 
                        false


    member self.PopSize = popSize
    member self.Elitism = elitism
    member self.Pmut = pmutation
    member self.Fitness = fitness
   
    member self.GetHistoryBest()=
        fitnessvector |> Seq.map(fun f -> f.CHROMOSOME.GetVector(),f.FITNESS) |> Array.ofSeq

    member self.Next()=
        population.EvalPopulation(fitness)
        fitnessvector.Add(population.[0])
        //build dependency tree:Fill
        let tree = FillTree()
        printfn "%A" population
        printfn "--------------------------------------------------------"       

        //update tree of probabilities
        [|0 .. elitism|]
        |>Array.iter(fun i -> 
                    let {CHROMOSOME=chromo;FITNESS=fitval} = population.[i]
                    let bits = chromo.Bits
                    for i in 0 .. bits.Length-1 do
                        let bit_i = bits.[i]
                        for j in 0 .. bits.Length-1 do
                            let bit_j = bits.[j]
                            linkage.Update(i,j,bit_i,bit_j)
            )
        |>ignore
                               
        //
        //generate new population
        let oldpop = population
        population <- new Population(popSize,chromo)

        for i in 0 .. population.Size-1 do

            let sschromo = population.[i]
            let vbit = sschromo.CHROMOSOME.Bits |> Array.ofSeq |> Array.map(fun _ -> false)
            
            tree.ListOfNodes
                     |> Seq.iter(fun node -> 
                                        if node.Parent.IsNone then
                                            vbit.[node.ID] <- GenerateBit0(node)                                        
                                        else 
                                            vbit.[node.ID] <- GenerateBit(node.Parent.Value,node,vbit.[node.Parent.Value.ID])                                        
                                                
                                        ) 
        
            sschromo.CHROMOSOME.SetBits(vbit)
    //re-insert elite chromosomes
        [0 .. elitism-1]
        |> Seq.iter(fun i -> population.[i].CHROMOSOME.SetBits(oldpop.[i].CHROMOSOME.Bits))
    
    
        
            


        

