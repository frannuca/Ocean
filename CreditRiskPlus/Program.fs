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
   
   
    let charts1 = CreditRiskPlus.APoisson.run()
    let charts2 = CreditRiskPlus.PoissonGamma.run()
    let plot = Chart.Combine(Array.concat([|charts1;charts2|]))
    
    let myChartControl = new ChartControl(plot, Dock=DockStyle.Fill)
    let form = new Form(Visible = true, TopMost = true, Width = 700, Height = 500)
    
    form.Controls.Add(myChartControl)
    do Application.Run(form) |> ignore


 
    0 // return an integer exit code
