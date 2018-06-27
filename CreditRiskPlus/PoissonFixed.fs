namespace CreditRiskPlus

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
module APoisson=
    type PoissonFixed(portfolio:Frame<int,string>)=
   
        let bands = portfolio?Vn |> Series.values |>Seq.distinct
        let gframe:Frame<int*int,string> = portfolio |> Frame.groupRowsBy("Vn")
        let qL = portfolio?AEaD / portfolio?qE |> Stats.mean

    
        member self.Compute(ndefaults:int,samples:int,quantiles:float[])=
            let nsamples = int(System.Math.Pow(2.0,System.Math.Ceiling(System.Math.Log10(float(samples))/System.Math.Log10(2.0))))

            let Glm = bands |> Seq.map(fun v ->                                                 
                                                        let losses = Array.zeroCreate<float> nsamples 
                                                        let frame = gframe.Rows.[int(v),*]
                                                        let lambda=frame?lambda |> Stats.sum
                                              
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
    
    
    let run()=
            let PD(lambda)(n) = MathNet.Numerics.Distributions.Poisson.PMF(lambda,n)
            let rng = new MathNet.Numerics.Random.MersenneTwister(42);
            //definition of a portolio as a frame wiht the following columns:
            (**
                Name|PD|Exposure|LGD
            *)
            let getPD()=
                    0.08
            let getAeaD()=
                15.0*rng.NextDouble()+1.0

            let nobligors = 150
            let nameseries= series [for n in 0 .. nobligors do yield n => n.ToString() ]
            let PDseries = series  [for n in 0 .. nobligors do yield n => getPD()]
            let AEaDseries = series[for n in 0 .. nobligors do yield n =>  getAeaD()]
            let LGDseries = series [for n in 0 .. nobligors do yield n => 1.0]
    
            let portfolio= Frame(["Name";"PD";"E";"LGD"],[nameseries;PDseries;AEaDseries;LGDseries])

            let AEaD = portfolio?E * portfolio?LGD
            portfolio.AddColumn("AEaD",AEaD)
    
            let qL = 2.0
    
            let qE = portfolio?AEaD / qL             
    
            portfolio.AddColumn("qE",qE)
    
            //computing the lambda per obligor
            let lambda = portfolio?PD |> Series.mapValues(fun x -> x)
    
            portfolio.AddColumn("lambda",lambda)
    
            //building map of loss rate per band:
            portfolio.AddColumn("Vn", portfolio?qE |> Series.mapValues(System.Convert.ToInt32))

    
    
            let poissonfixed = new PoissonFixed(portfolio)
            let qs = [|0.99;0.999;0.9997|]
            let {DENSITY=density;ValueAtRisk=VaR} = poissonfixed.Compute(50,1024,qs)
   
            VaR |> Array.iter(fun (l,q) -> printfn "loss %f for quantile %f" l q)

            let chartsforquantiles =
                                        VaR |> Array.map(fun (l,q) ->
                                                                    Chart.Line(Array.init(5)(fun k -> l, 0.001*float(k)),Name=q.ToString()+"F")
                                                                    )

            Array.concat([[|Chart.Line(density.[0 .. 256 ] ,Name="Income_fixed")|];chartsforquantiles])
            