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

type PoissonVariablePD(portfolio:Frame<int,string>,Gn: complex -> complex)=
    
    let bands = portfolio?Vn |> Series.values |>Seq.distinct
    let gframe:Frame<int*int,string> = portfolio |> Frame.groupRowsBy("Vn")
    let qL = portfolio?AEaD / portfolio?qE |> Stats.mean
    let lambda_p = portfolio?lambda |> Stats.sum
    

    member self.Compute(ndefaults:int,samples:int,quantiles:float[])=
        let nsamples = int(System.Math.Pow(2.0,System.Math.Ceiling(System.Math.Log10(float(samples))/System.Math.Log10(2.0))))


        let fn = Array.create nsamples 0.0 
        bands |> Seq.iter(fun u -> 
                                
                                let frame = gframe.Rows.[int(u),*]
                                let lambda=frame?lambda |> Stats.sum
                                fn.[int(u)] <- lambda/lambda_p)

        let fnfour = fn |> fft
        let fzfour = fnfour |> Array.map(fun s -> Gn(s))
        let fz = fzfour |> ifft

        let density = fz |> Array.mapi(fun n x -> float(n)*qL,Complex.realPart(x))                                                    
        let summ = density |> Seq.mapi(fun n (ll,_) -> ll, (density.[0 .. n] |>Array.sumBy(fun (_,b) -> b)))
        let summtotal = density |> Seq.map(fun (_,a) -> a) |> Seq.sum
        let vars = quantiles |> Array.map(fun q -> summ |> Seq.map(fun (n,v) -> n,q,Math.Abs(v-q)) |> Seq.minBy(fun (_,_,x) -> x)) |> Seq.map(fun (a,b,_) -> (a,b))

        {Commons.VaRResult.DENSITY=density; Commons.VaRResult.ValueAtRisk=vars|> Array.ofSeq}
          



    

