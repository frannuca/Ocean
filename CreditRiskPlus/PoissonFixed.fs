﻿namespace CreditRiskPlus

open System
open Deedle
open MathNet.Numerics
open FSharp.Charting
open FSharp.Charting
open FSharp.Charting.ChartTypes
open System.Drawing
open System.Windows.Forms
open Accord.Math
open MathNet.Numerics.IntegralTransforms
open Commons
open Data

module APoisson=
    type PoissonFixed(portfolio:Frame<int,string>)=
   
        let bands = portfolio?Vn |> Series.values |>Seq.distinct
        let gframe:Frame<int*int,string> = portfolio |> Frame.groupRowsBy(PortfolioColumns.Vn.ToString())
        let qL = (portfolio.GetColumn<float>(PortfolioColumns.Exposure.ToString()) / portfolio.GetColumn<float>(PortfolioColumns.E.ToString()) ) 
                  |> Stats.mean

    
        member self.Compute(ndefaults:int,samples:int,quantiles:float[])=
            let nsamples = int(System.Math.Pow(2.0,System.Math.Ceiling(System.Math.Log10(float(samples))/System.Math.Log10(2.0))))

            let Glm = bands |> Seq.map(fun v ->                                                 
                                                        let losses = Array.zeroCreate<float> nsamples 
                                                        let frame = gframe.Rows.[int(v),*]
                                                        let lambda=frame.GetColumn<float>(PortfolioColumns.Lambda.ToString()) |> Stats.sum
                                              
                                                        [|0 .. ndefaults|] 
                                                        |>Array.iter(fun t -> losses.[t*int(v)] <-MathNet.Numerics.Distributions.Poisson.PMF(lambda,t))
                                                        losses
                                                        )
            let Glm2 = Glm |> Seq.map(fun s -> s |> fft)
            let convL = Glm2 
                        |> Seq.reduce(fun a b -> a |> Array.mapi(fun i x -> b.[i]*x )) 
                        |> ifft
                                               
            let density = convL |> Array.mapi(fun n x -> float(n)*qL,Complex.realPart(x))                                                    
            let summ = density |> Seq.mapi(fun n (ll,_) -> ll, (density.[0 .. n] |>Array.sumBy(fun (_,b) -> b)))
        
            let vars = quantiles |> Array.map(fun q -> summ |> Seq.map(fun (n,v) -> n,q,Math.Abs(v-q)) |> Seq.minBy(fun (_,_,x) -> x)) |> Seq.map(fun (a,b,_) -> (a,b))

            {Commons.VaRResult.DENSITY=density; Commons.VaRResult.ValueAtRisk=vars|> Array.ofSeq}
    
    
    let run(data:PortfolioData,qL:float)=
     
           
            let portfolio= GenerateFrame(data)(qL)

                         
            let poissonfixed = new PoissonFixed(portfolio)
            let qs = [|0.99;0.999;0.9997|]
            let {DENSITY=density;ValueAtRisk=VaR} = poissonfixed.Compute(50,1024,qs)
   
            VaR |> Array.iter(fun (l,q) -> printfn "loss %f for quantile %f" l q)

            let chartsforquantiles =
                                        VaR |> Array.map(fun (l,q) ->
                                                                    Chart.Line(Array.init(5)(fun k -> l, 0.001*float(k)),Name=q.ToString()+"F")
                                                                    )

            Array.concat([[|Chart.Line(density.[0 .. ] ,Name="Income_fixed")|];chartsforquantiles])
            