// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open OptimizationCore


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let f(x:float array) = 2.0*x.[0]**2.0+3.0*x.[1]**2.0
    let x0 = [|1.0;1.0|]
    let dx = [|1.0e-5;1.0e-5|]
    
    let a = Differentiation.Grad(x0,dx)(f)
    let b = Differentiation.Grad2(x0,dx)(f)
    let c = Differentiation.Hessian(x0,dx)(f)

    let gen = new Gene(0.0,100.0,5)
    let aa = gen.SetNumber(73.0)
    let bb = gen.GetFloat()
    0 // return an integer exit code
