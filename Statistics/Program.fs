
open System.Data
open Accord.Math
open Accord.Math.Differentiation
open Accord.Math.Optimization
open Accord.Statistics.Distributions
open Accord.Statistics.Distributions.Univariate
open Ocean.Statitics
open Ocean.Statitics
open LogisticFitter

[<EntryPoint>]
let main argv = 
    let Nsamples = 1000
    let ldtest = new LogisticDistribution(-3.175,5.75)
    let samples = [|0 ..  Nsamples-1|] |> Array.map(fun _ -> ldtest.Generate())
    
    match  LogisticDistribution(0.0,1.0,-100.0,100.0,100.0,5000,samples) with 
    |RETURNSOLUTION.SUCCESS(x) -> printfn "location = %f, scale=%f" (x.[0]) (x.[1])
                                  let loc2 = match LogisticDistributionLocationBracketting(x.[0],x.[1],x.[0]*0.1,x.[0]*2.0,5000,samples) with
                                              |RETURNSOLUTION.SUCCESS(z) -> z.[0]
                                              | _ -> x.[0]                                    
                                  let scale2 = match LogisticDistributionScaleBracketting(loc2,x.[1],x.[1]*3.0,5000,samples) with
                                                |RETURNSOLUTION.SUCCESS(z) -> z.[0]
                                                |_ -> x.[1]
                                                
                                  printfn "location = %f, scale=%f" (loc2) (scale2)
    |ERROR(msg) -> printfn "%A" msg
    
    
    0 // return an integer exit code
