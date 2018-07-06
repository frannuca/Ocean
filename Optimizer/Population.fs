namespace OptimizationCore
namespace GA
open System
open Deedle

type PopulationItem = {CHROMOSOME:Chromosome;FITNESS:float}

type Population(popsize:int,chromoprototype:Chromosome)=
    let mutable population = [|0 .. popsize - 1|]
                                 |> Array.map(fun i -> {CHROMOSOME=chromoprototype.SpawnAnotherChromo();FITNESS=System.Double.MinValue})

    member this.Size= popsize                                    
    member this.Item
      with get(index:int) = population.[index]
    
    member this.EvalPopulation(pfunc:float array -> float)=
        let newpop = population 
                     |> Array.Parallel.map(fun chromo -> 
                                        {chromo with FITNESS= chromo.CHROMOSOME.GetVector() |> pfunc}
                                    )
        population <- (newpop |> Array.sortBy(fun c -> c.FITNESS))

    override this.ToString()=
        System.String.Join("\n",population|>Array.map(fun item -> item.CHROMOSOME.ToString()+"|"+item.FITNESS.ToString()))

    member this.Stdev()=   
        population         
        |> Array.map(fun x -> x.FITNESS)
        |> Series.ofValues
        |> Stats.stdDev

    member this.NChromoDifferent()=   
        let pairs = population |> Array.allPairs population
        let sumall =
                    pairs 
                    |> Array.map(fun (a,b) -> a.CHROMOSOME <> b.CHROMOSOME)
                    |> Array.sumBy(fun k -> if k then 1.0 else 0.0)
        sumall/2.0