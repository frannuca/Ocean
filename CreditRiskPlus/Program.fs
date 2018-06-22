﻿open System
open Deedle
open MathNet.Numerics
open FSharp.Charting
open FSharp.Charting
open FSharp.Charting.ChartTypes
open System.Drawing
open System.Windows.Forms
open Accord.Math
open MathNet.Numerics.IntegralTransforms

type TFA = float->float->float
    
[<EntryPoint>]
let main argv = 
    let tocomplex(a) = complex a 0.0
    let toimag(a) = complex 0.0 a

    let F(u)(z)= Math.Exp(u*(z-1.0)) 
    
    let PD(lambda)(n) = MathNet.Numerics.Distributions.Poisson.PMF(lambda,n)
    let rng = new MathNet.Numerics.Random.MersenneTwister(42);
    //definition of a portolio as a frame wiht the following columns:
    (**
        Name|PD|Exposure|LGD
    *)
    let getPD()=
            rng.NextDouble()*0.25+ 0.01
    let getAeaD()=
        rng.NextDouble()*1000.0+500.0
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
             |> Series.mapValues(fun x -> System.Convert.ToInt32(Math.Ceiling(x)))      
    
    portfolio.AddColumn("qE",qE)
    
    //computing the lambda per obligor
    let lambda = portfolio?PD |> Series.mapValues(fun x -> -Math.Log(1.0-x))
    
    portfolio.AddColumn("lambda",lambda)
    
    //building map of loss rate per band:
    portfolio.AddColumn("Vn", portfolio?qE |> Series.mapValues(System.Convert.ToInt32))

    let bands = portfolio?Vn |> Series.values |>Seq.distinct
    let gframe:Frame<int*int,string> = portfolio |> Frame.groupRowsBy("Vn")
    let muperband = bands |> Seq.map(fun v -> let frame = gframe.Rows.[int(v),*]
                                              int(v),frame?lambda |> Stats.sum
                                             )
                          |>dict                   
                                           
    
   
    let maxJ = (portfolio?Vn |> Series.values |> Seq.max |> System.Convert.ToInt32) + 256
    
    //Getting the distribution from FFT:
    let fft (x: float array) =
                               let y =  Array.init (x.Length)(fun _ -> complex 0.0 0.0)
                               x |> Seq.iteri(fun i s -> y.[i] <- tocomplex(s))
                               Accord.Math.Transforms.FourierTransform2.FFT(y,FourierTransform.Direction.Forward)
                               y
                                                   
    let ifft (x: MathNet.Numerics.complex array) = 
                                                   let y =  Array.init (x.Length)(fun _ -> complex 0.0 0.0)
                                                   x |> Seq.iteri(fun i s -> y.[i]<-s)
                                                   Accord.Math.Transforms.FourierTransform2.FFT(y,FourierTransform.Direction.Backward)
                                                   y

    
  
    let ndefaults = 30
    let nsamples = int(2.0**10.0)
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
                                               
    let density = convL |> Array.mapi(fun n x -> float(n),(Complex.realPart(x)))
    let arrr = density |> Seq.map(fun (a,b) ->  sprintf "%f %f" a b) |> Array.ofSeq
    let strr = System.String.Join("\n", arrr)
    //printfn "%s" strr

    let plot = Chart.Combine([ Chart.Line(density.[0 .. ],Name="Income")])
    
    let myChartControl = new ChartControl(plot, Dock=DockStyle.Fill)
    let form = new Form(Visible = true, TopMost = true, Width = 700, Height = 500)
    
    form.Controls.Add(myChartControl)
    do Application.Run(form) |> ignore
    0 // return an integer exit code
