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
open Data
module PoissonGamma=
    type PoissonMixture(portfolio:Frame<int,string>)=
   
        let bands = portfolio.GetColumn<float>(PortfolioColumns.Vn.ToString()) |> Series.values |>Seq.distinct
        let gframe:Frame<int*int,string> = portfolio |> Frame.groupRowsBy(PortfolioColumns.Vn.ToString())
        let qL = (portfolio.GetColumn<float>(PortfolioColumns.Exposure.ToString()) / portfolio.GetColumn<float>(PortfolioColumns.E.ToString()) ) 
                          |> Stats.mean
        
        let lambda_p = portfolio.GetColumn<float>(PortfolioColumns.Lambda.ToString()) |> Stats.sum    
        let sigma_tot = portfolio.GetColumn<float>(PortfolioColumns.Sigma.ToString()) |> Stats.sum
        
        let alpha = lambda_p**2.0/sigma_tot**2.0
        let beta = sigma_tot**2.0/lambda_p

        let Gn(t:complex)=
            MathNet.Numerics.ComplexExtensions.Power(tocomplex(1.0) - tocomplex(beta)*(t - tocomplex(1.0)),complex(-alpha)(0.0))

        member self.Compute(samples:int,quantiles:float[])=
            let nsamples = int(System.Math.Pow(2.0,System.Math.Ceiling(System.Math.Log10(float(samples))/System.Math.Log10(2.0))))
            let fp = Array.zeroCreate nsamples  
            bands |> Seq.iter(fun v ->                                                                               
                                        let frame = gframe.Rows.[int(v),*]
                                        let lambda=frame.GetColumn<float>(PortfolioColumns.Lambda.ToString()) |> Stats.sum
                                        fp.[int(v)] <- lambda/lambda_p
                                        )
            

            let fp_fourier = fp |> fft

            let Glm = fp_fourier |> Array.map(Gn)
            let convL = Glm |> ifft 
            let density = convL |> Array.mapi(fun n x -> float(n)*qL,Complex.realPart(x))                                                    
            let summ = density |> Seq.mapi(fun n (ll,_) -> ll, (density.[0 .. n] |>Array.sumBy(fun (_,b) -> b)))
        
            
            let vars = quantiles |> Array.map(fun q -> summ |> Seq.map(fun (n,v) -> n,q,Math.Abs(v-q)) |> Seq.minBy(fun (_,_,x) -> x)) |> Seq.map(fun (a,b,_) -> (a,b))

            {Commons.VaRResult.DENSITY=density; Commons.VaRResult.ValueAtRisk=vars|> Array.ofSeq}
    
    
    let run(data:PortfolioData,qL:float,nsamples:int)=
                
                      
            let portfolio= GenerateFrame(data)(qL)
           
                
            let poissonfixed = new PoissonMixture(portfolio)
            let qs = [|0.99;0.999;0.9997|]
            let {DENSITY=density;ValueAtRisk=VaR} = poissonfixed.Compute(nsamples,qs)
   
            VaR |> Array.iter(fun (l,q) -> printfn "loss %f for quantile %f" l q)

            let chartsforquantiles =
                                        VaR |> Array.map(fun (l,q) ->
                                                                    Chart.Line(Array.init(5)(fun k -> l, 0.001*float(k)),Name=q.ToString())
                                                                    )

            Array.concat([[|Chart.Line(density.[0 ..   ] ,Name="Income")|];chartsforquantiles])
            

