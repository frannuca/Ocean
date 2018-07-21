namespace Ocean.Statitics
open Accord.Math
open Accord.Statistics.Distributions.Univariate
open Accord.Math.Differentiation
open Accord.Math.Optimization

module LogisticFitter=

    
    type RETURNSOLUTION=
    |SUCCESS of float array
    |ERROR of string
    
    
    let private optimizer =
        0.0
        
    
        
    let LogisticDistribution(location:float, scale:float, minlocation:float,maxlocation:float,maxscale:float, maxiter:int ,samples:float array) =
            let mlh(samples:float array)(l:float,s:float) =                        
                                let ldx = new  LogisticDistribution(l, s)
                                (samples |> Array.Parallel.map(fun s -> ldx.ProbabilityDensityFunction(s))
                                         |> Array.Parallel.map(fun s -> if s<1e-12 then System.Math.Log(1e-12) else System.Math.Log(s))
                                         |> Array.sum)       / float(samples.Length)
            let mlh = mlh(samples)
                                         
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
                                             minlocation,
                                             System.Func<float[],float[]>(fun x-> x|>Array.mapi(fun n v-> if n=0 then 1.0 else 0.0)),
                                             1e-12
                                             );
                    new NonlinearConstraint(f,System.Func<float[],float>(fun x-> x.[0]),
                                            ConstraintType.LesserThanOrEqualTo,
                                             maxlocation,
                                             System.Func<float[],float[]>(fun x-> x|>Array.mapi(fun n v-> if n=0 then 1.0 else 0.0)),
                                             1e-12
                                             )                                     
                                             
                |]
            
            let scaleconstraints=
                    [|
                        new NonlinearConstraint(f,System.Func<float[],float>(fun x-> x.[1]),
                                                ConstraintType.GreaterThanOrEqualTo,
                                                 0.1,
                                                 System.Func<float[],float[]>(fun x-> x|>Array.mapi(fun n v-> if n=1 then 1.0 else 0.0)),
                                                 1e-12
                                                 );
                        new NonlinearConstraint(f,System.Func<float[],float>(fun x-> x.[1]),
                                                ConstraintType.LesserThanOrEqualTo,
                                                 maxscale,
                                                 System.Func<float[],float[]>(fun x-> x|>Array.mapi(fun n v-> if n=1 then 1.0 else 0.0)),
                                                 1e-12
                                                 )                                     
                                                 
                    |]
                    
            let constraintsnl =   Array.concat [|locationconstraints;scaleconstraints|]                  
            //first solver:
            let solver_cobyla = new Cobyla(f,constraintsnl)
            solver_cobyla.MaxIterations <- maxiter
            let okcobyla = solver_cobyla.Maximize([|location;scale|])
            match solver_cobyla.Status with
            |CobylaStatus.Success ->  SUCCESS(solver_cobyla.Solution)
            |CobylaStatus.MaxIterationsReached -> ERROR("Max. iteration reached")
            |CobylaStatus.NoPossibleSolution -> ERROR("The provided optimization domain does not hold a solution")
            |CobylaStatus.DivergingRoundingErrors -> ERROR("Divergence due to rouning errors")
            
            
    let LogisticDistributionLocationBracketting(location:float, scale:float, minlocation:float,maxlocation:float, maxiter:int ,samples:float array) =
            let mlh(samples:float array)(l:float,s:float) =                        
                                let ldx = new  LogisticDistribution(l, s)
                                (samples |> Array.Parallel.map(fun s -> ldx.ProbabilityDensityFunction(s))
                                         |> Array.Parallel.map(fun s -> if s<1e-12 then System.Math.Log(1e-12) else System.Math.Log(s))
                                         |> Array.sum)  / float(samples.Length)     
            let mlh = mlh(samples)       
            
                                         
        //    
            let N = 2
            let fitness = System.Func<float,float>(fun x -> mlh(x,scale))
            
            
            
            let solver_cobyla = new BrentSearch(fitness,minlocation,maxlocation,1e-6,maxiter)
            
            let okcobyla = solver_cobyla.Maximize()
            match solver_cobyla.Status with
            |BrentSearchStatus.Success ->  SUCCESS([|solver_cobyla.Solution|])
            |BrentSearchStatus.MaxIterationsReached -> ERROR("Max. iteration reached")
            

    let LogisticDistributionScaleBracketting(location:float, scale:float, maxscale:float, maxiter:int ,samples:float array) =
            let mlh(samples:float array)(l:float,s:float) =                        
                                let ldx = new  LogisticDistribution(l, s)
                                (samples |> Array.Parallel.map(fun s -> ldx.ProbabilityDensityFunction(s))
                                         |> Array.Parallel.map(fun s -> if s<1e-12 then System.Math.Log(1e-12) else System.Math.Log(s))
                                         |> Array.sum)  / float(samples.Length)     
            let mlh = mlh(samples)       
            
                                         
        //    
            let N = 2
            let fitness = System.Func<float,float>(fun x -> mlh(location,x))
            
            
            
            let solver_cobyla = new BrentSearch(fitness,0.1,maxscale,1e-6,maxiter)
            
            let okcobyla = solver_cobyla.Maximize()
            match solver_cobyla.Status with
            |BrentSearchStatus.Success ->  SUCCESS([|solver_cobyla.Solution|])
            |BrentSearchStatus.MaxIterationsReached -> ERROR("Max. iteration reached")
            
                                                
            

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
            