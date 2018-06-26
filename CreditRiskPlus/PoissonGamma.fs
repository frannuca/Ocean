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

module PoissonGamma=
    type PoissonMixture(portfolio:Frame<int,string>)=
   
        let bands = portfolio?Vn |> Series.values |>Seq.distinct
        let gframe:Frame<int*int,string> = portfolio |> Frame.groupRowsBy("Vn")
        let qL = portfolio?AEaD / portfolio?qE |> Stats.mean
        let lambda_p = portfolio?lambda |> Stats.sum    
        let sigma_tot = portfolio?sigma |> Stats.sum
        let mu_tot = portfolio?lambda |> Stats.sum
        let alpha = mu_tot**2.0/sigma_tot**2.0
        let beta = sigma_tot**2.0/mu_tot

        let Gn(t:complex)=
            MathNet.Numerics.ComplexExtensions.Power(tocomplex(1.0) - tocomplex(beta)*(t - tocomplex(1.0)),complex(-alpha)(0.0))

        member self.Compute(ndefaults:int,samples:int,quantiles:float[])=
            let nsamples = int(System.Math.Pow(2.0,System.Math.Ceiling(System.Math.Log10(float(samples))/System.Math.Log10(2.0))))
            let aa = Gn(complex(0.999959)(0.008496))
            let fp = Array.zeroCreate 1024 
            bands |> Seq.iter(fun v ->                                                                               
                                        let frame = gframe.Rows.[int(v),*]
                                        let lambda=frame?lambda |> Stats.sum
                                        fp.[int(v)] <- lambda/lambda_p                                                       
                                        )
            

            let fp_fourier = fp |> fft

            let Glm = fp_fourier |> Array.map(Gn)
            let convL = Glm |> ifft 
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
           
            
            //let nameseries= series [1 => "1";2=>"2"]
            //let PDseries = series  [1 => 0.08;2=>0.05]
            //let AEaDseries = series[1 => 1.0;2=>2.0]
            //let LGDseries = series [1 => 1.0;2=>1.0]
            //let sigmas_vol = series [1 => 0.04;2=>0.025]
            let getPD()=
                    0.08
            let getSigma()=
                    0.05
            let getAeaD()=
                15.0*rng.NextDouble()+1.0
            

            let nobligors = 150
            let nameseries= series [for n in 0 .. nobligors do yield n => n.ToString() ]
            let PDseries = series  [for n in 0 .. nobligors do yield n => getPD()]
            let AEaDseries = series[for n in 0 .. nobligors do yield n =>  getAeaD()]
            let LGDseries = series [for n in 0 .. nobligors do yield n => 1.0]
            let sigmas_vol = series[for n in 0 .. nobligors do yield n => getSigma()]
            let portfolio= Frame(["Name";"PD";"E";"LGD";"sigma"],[nameseries;PDseries;AEaDseries;LGDseries;sigmas_vol])

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

    
    
            let poissonfixed = new PoissonMixture(portfolio)
            let qs = [|0.99;0.999;0.9997|]
            let {DENSITY=density;ValueAtRisk=VaR} = poissonfixed.Compute(50,1024,qs)
   
            VaR |> Array.iter(fun (l,q) -> printfn "loss %f for quantile %f" l q)

            let chartsforquantiles =
                                        VaR |> Array.map(fun (l,q) ->
                                                                    Chart.Line(Array.init(5)(fun k -> l, 0.001*float(k)),Name=q.ToString())
                                                                    )

            Array.concat([[|Chart.Line(density.[0 ..  ] ,Name="Income")|];chartsforquantiles])
            

