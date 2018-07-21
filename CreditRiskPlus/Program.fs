open System
open Deedle
open MathNet.Numerics
//open FSharp.Charting
//open FSharp.Charting.ChartTypes
//open System.Drawing
//open System.Windows.Forms
open Accord.Math
open MathNet.Numerics.IntegralTransforms
open Deedle.Frame
//open System.Windows.Forms.DataVisualization.Charting
open CreditRiskPlus
open Commons
open Data

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let rng = new MathNet.Numerics.Random.MersenneTwister(42);
    let getPD()=
           rng.NextDouble()*0.15+0.001
    let getAeaD()=
       100.0*rng.NextDouble()+1.0

    let nobligors = 1500
    let nameseries= [|for n in 0 .. nobligors do yield  n.ToString() |]
    let PDseries =  [|for _ in 0 .. nobligors do yield  getPD()|]
    let AEaDseries =[|for _ in 0 .. nobligors do yield  getAeaD()|]
    let LGDseries = [|for _ in 0 .. nobligors do yield  1.0|]
    let sigmas = [|for _ in 0 .. nobligors do yield  rng.NextDouble()*0.02|]

    let data = {PortfolioData.Id=nameseries;LGD=LGDseries;E=AEaDseries;PD=PDseries;Sigma=Some(sigmas)}
    let qL = 5.0
   
    let {DENSITY=density1;ValueAtRisk=_} = CreditRiskPlus.APoisson.run(data,qL,int(Math.Pow(2.0,12.0)))
    let {DENSITY=density2;ValueAtRisk=_} = CreditRiskPlus.PoissonGamma.run(data,qL,int(Math.Pow(2.0,12.0)))
//    let plot = Chart.Combine(Array.concat([|charts1;charts2|]))
//    
//    let myChartControl = new ChartControl(plot, Dock=DockStyle.Fill)
//    let form = new Form(Visible = true, TopMost = true, Width = 700, Height = 500)
//    
//    form.Controls.Add(myChartControl)
//    do Application.Run(form) |> ignore    
    let frame1 = density1 |> Array.map(fun (x,y)-> {POINT.LOSS=x;POINT.PROBD=y} ) |> Frame.ofRecords
    let frame2 = density2 |> Array.map(fun (x,y)-> {POINT.LOSS=x;POINT.PROBD=y} ) |> Frame.ofRecords
    frame1.SaveCsv("/home/fran/tmp/fixedPoison")
    frame2.SaveCsv("/home/fran/tmp/GammaPoison")
    

 
    0 // return an integer exit code
