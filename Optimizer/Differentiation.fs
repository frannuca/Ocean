namespace OptimizationCore

module Differentiation=
    open MathNet.Numerics.LinearAlgebra

    let Grad(x0:float array,dx:float array)(f:float[] -> float)=
    
        let x0 = Vector<float>.Build.DenseOfArray x0
        [|0 .. x0.Count - 1 |]
        |> Array.map(fun n -> 
                            let dp = Vector<float>.Build.Dense(x0.Count)
                            dp.[n] <- dx.[n]
                            
                            let xplus = (x0+dp).AsArray()
                            let v1 = f(xplus) 
                            let v2 = f(x0.AsArray())
                            let cc = (v1-v2)/(dx.[n])
                            cc
                          )    

    let Grad2(x0:float array,dx:float array)(f:float[] -> float)=
    
        let x0 = Vector<float>.Build.DenseOfArray x0
        [|0 .. x0.Count - 1 |]
        |> Array.map(fun n -> 
                            let dp = Vector<float>.Build.Dense(x0.Count)
                            dp.[n] <- dx.[n]
                            
                            let xplus = (x0+dp).AsArray()
                            let xminus = (x0-dp).AsArray()
                            (f(xplus)-f(xminus))/(2.0*dx.[n])
                          )    

    let Jacobian(x0:float array,dx:float array)(f:float[] -> float[])=
        let n = x0.Length
        let m = f(x0).Length

        [|0 .. m-1|]
        |>Array.map(fun i -> let fi a = f(a).[i]
                             Grad(x0,dx)(fi)
                           )


    let Hessian(x0:float array,dx:float array)(f:float[] -> float)=                
        Jacobian(x0,dx)(fun x -> Grad(x,dx)(f))
        