
open System.Data
open Accord.Math
open Accord.Math.Differentiation
open Accord.Math.Optimization
open Accord.Statistics.Distributions
open Accord.Statistics.Distributions.Univariate


[<EntryPoint>]
let main argv = 

    let ld = new  LogisticDistribution(-10.0, 6.0)
    let samples = [|0 .. 1000|] |> Array.Parallel.map(fun _ ->ld.Generate() )

   
    let mlh (l:float,s:float) =
        
        let ldx = new  LogisticDistribution(l, s)
        (samples |> Array.Parallel.map(fun s -> ldx.LogProbabilityDensityFunction(s))
                 |> Array.sum) 
        
    //let mlh = ld2.Fit(samples)
    printfn "Mean %A, scale %A" (ld.Location) (ld.Scale)    
    printfn "%A" argv
//    
    let N = 2
    let fitness = System.Func<float[],float>(fun x -> mlh(x.[0],x.[1]))
    let gfit = new FiniteDifferences(N,fitness,2,1e-4)
    let grad = System.Func<float[],float[]>(gfit.Compute)
    let f = new NonlinearObjectiveFunction(N,fitness,grad)
    let locationconstraints=
        [|
            new NonlinearConstraint(f,System.Func<float[],float>(fun x-> x.[0]),
                                    ConstraintType.GreaterThanOrEqualTo,
                                     -100.0,
                                     System.Func<float[],float[]>(fun x-> x|>Array.mapi(fun n v-> if n=0 then 1.0 else 0.0)),
                                     1e-12
                                     );
            new NonlinearConstraint(f,System.Func<float[],float>(fun x-> x.[0]),
                                    ConstraintType.LesserThanOrEqualTo,
                                     100.0,
                                     System.Func<float[],float[]>(fun x-> x|>Array.mapi(fun n v-> if n=0 then 1.0 else 0.0)),
                                     1e-12
                                     )                                     
                                     
        |]
    
    let scaleconstraints=
            [|
                new NonlinearConstraint(f,System.Func<float[],float>(fun x-> x.[1]),
                                        ConstraintType.GreaterThanOrEqualTo,
                                         0.25,
                                         System.Func<float[],float[]>(fun x-> x|>Array.mapi(fun n v-> if n=1 then 1.0 else 0.0)),
                                         1e-12
                                         );
                new NonlinearConstraint(f,System.Func<float[],float>(fun x-> x.[1]),
                                        ConstraintType.LesserThanOrEqualTo,
                                         100.0,
                                         System.Func<float[],float[]>(fun x-> x|>Array.mapi(fun n v-> if n=1 then 1.0 else 0.0)),
                                         1e-12
                                         )                                     
                                         
            |]
            
    let constraintsnl =   Array.concat [|locationconstraints;scaleconstraints|]     
    let constraints = constraintsnl |> Array.map(fun c -> c :> IConstraint) 
    //first solver:
    let solver_cobyla = new Cobyla(f,constraintsnl)
    solver_cobyla.MaxIterations <- 100000
    let okcobyla = solver_cobyla.Maximize([|0.0;1.0|])
    let cobyla_status = solver_cobyla.Status
    
    printf "Cobyla %A" (solver_cobyla.Solution)
//    let locationconstraints=
//            [|
//                new NonlinearConstraint(f,System.Func<float[],float>(fun x-> x.[0]),
//                                        ConstraintType.GreaterThanOrEqualTo,
//                                         solver_cobyla.Solution.[0]*0.5,
//                                         System.Func<float[],float[]>(fun x-> x|>Array.mapi(fun n v-> if n=0 then 1.0 else 0.0)),
//                                         1e-12
//                                         );
//                new NonlinearConstraint(f,System.Func<float[],float>(fun x-> x.[0]),
//                                        ConstraintType.LesserThanOrEqualTo,
//                                         solver_cobyla.Solution.[0]*2.0,
//                                         System.Func<float[],float[]>(fun x-> x|>Array.mapi(fun n v-> if n=0 then 1.0 else 0.0)),
//                                         1e-12
//                                         )                                     
//                                         
//            |]
//        
//    let scaleconstraints=
//                [|
//                    new NonlinearConstraint(f,System.Func<float[],float>(fun x-> x.[1]),
//                                            ConstraintType.GreaterThanOrEqualTo,
//                                             solver_cobyla.Solution.[1]*0.5,
//                                             System.Func<float[],float[]>(fun x-> x|>Array.mapi(fun n v-> if n=1 then 1.0 else 0.0)),
//                                             1e-12
//                                             );
//                    new NonlinearConstraint(f,System.Func<float[],float>(fun x-> x.[1]),
//                                            ConstraintType.LesserThanOrEqualTo,
//                                             solver_cobyla.Solution.[0]*2.0,
//                                             System.Func<float[],float[]>(fun x-> x|>Array.mapi(fun n v-> if n=1 then 1.0 else 0.0)),
//                                             1e-12
//                                             )                                     
//                                             
//                |]
//                
//                
//    let constraintsnl =   Array.concat [|locationconstraints;scaleconstraints|]     
//    let constraints = constraintsnl |> Array.map(fun c -> c :> IConstraint) 
//
//    let solver = new AugmentedLagrangian(f,constraints)
//    (solver.Optimizer :?> BroydenFletcherGoldfarbShanno).Delta <- 1e-6
//    (solver.Optimizer :?> BroydenFletcherGoldfarbShanno).FunctionTolerance <- 1e-6
//    (solver.Optimizer :?> BroydenFletcherGoldfarbShanno).Epsilon <- 1e-6
//    solver.MaxEvaluations <- 500
//    let oklang = solver.Maximize(solver_cobyla.Solution)
//    printf "Aulag %A" (solver.Solution)
    0 // return an integer exit code
