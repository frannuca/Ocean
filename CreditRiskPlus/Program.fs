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
open Deedle.Frame
open System.Windows.Forms.DataVisualization.Charting
open CreditRiskPlus
open Commons
[<EntryPoint>]
let main argv = 
   
   
    let PD(lambda)(n) = MathNet.Numerics.Distributions.Poisson.PMF(lambda,n)
    let rng = new MathNet.Numerics.Random.MersenneTwister(42);
    //definition of a portolio as a frame wiht the following columns:
    (**
        Name|PD|Exposure|LGD
    *)
    let getPD()=
            0.1
    let getAeaD()=
        1500.0*rng.NextDouble()+1000.0
    let nobligors = 150
    let nameseries= series [for n in 0 .. nobligors do yield n => n.ToString() ]
    let PDseries = series  [for n in 0 .. nobligors do yield n => getPD()]
    let AEaDseries = series[for n in 0 .. nobligors do yield n =>  getAeaD()]
    let LGDseries = series [for n in 0 .. nobligors do yield n => 1.0]
    
    let portfolio= Frame(["Name";"PD";"E";"LGD"],[nameseries;PDseries;AEaDseries;LGDseries])

    let AEaD = portfolio?E * portfolio?LGD
    portfolio.AddColumn("AEaD",AEaD)
    
    let qL = 150.0
    
    let qE = portfolio?AEaD / qL             
    
    portfolio.AddColumn("qE",qE)
    
    //computing the lambda per obligor
    let lambda = portfolio?PD |> Series.mapValues(fun x -> -Math.Log(1.0-x))
    
    portfolio.AddColumn("lambda",lambda)
    
    //building map of loss rate per band:
    portfolio.AddColumn("Vn", portfolio?qE |> Series.mapValues(System.Convert.ToInt32))

    
    
    let poissonfixed = new PoissonFixed(portfolio)
    let qs = [|0.99;0.999;0.9997|]
    let {DENSITY=density;ValueAtRisk=VaR} = poissonfixed.Compute(50,1024,qs)
   
    VaR |> Array.iter(fun (l,q) -> printfn "loss %f for quantile %f" l q)

    let chartsforquantiles =
                                VaR |> Array.map(fun (l,q) ->
                                                            Chart.Line(Array.init(5)(fun k -> l, 0.001*float(k)),Name=q.ToString())
                                                            )

    let allcharts = Array.concat([[|Chart.Line(density.[0 .. ] ,Name="Income")|];chartsforquantiles])
    let plot = Chart.Combine(allcharts)
    
    let myChartControl = new ChartControl(plot, Dock=DockStyle.Fill)
    let form = new Form(Visible = true, TopMost = true, Width = 700, Height = 500)
    
    form.Controls.Add(myChartControl)
    do Application.Run(form) |> ignore
    
    0 // return an integer exit code
