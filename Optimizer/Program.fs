﻿// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open OptimizationCore
open GA
open Differenciation

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let f(x:float array) = 2.0*x.[0]**2.0+3.0*x.[1]**2.0
    let x0 = [|1.0;1.0|]
    let dx = [|1.0e-5;1.0e-5|]
    
    let a = Operators.Grad(x0,dx)(f)
    let b = Operators.Grad2(x0,dx)(f)
    let c = Operators.Hessian(x0,dx)(f)

    let gen = new Gene(0.0,100.0,5)
    let aa = gen.SetNumber(73.0)
    let bb = gen.GetFloat()

    let chromobld = new ChromosomeBuilder()
    let chromo=
                chromobld
                    .WithParameter(0.0,1200.0,30,Some(324.9))
                    .WithParameter(0.0,50.0,30,Some(25.0))
                    .WithParameter(-110.0,10.0,30,Some(-25.0))
                    .Build()
    let vchromo = chromo.GetVector()

    let pop = new Population(1000,chromo)

    for i in 0 .. 10000 do
        pop.EvalPopulation(fun x -> (x.[0]**2.0 + x.[1]**2.0 + x.[2]**2.0)**0.5)

    0 // return an integer exit code
