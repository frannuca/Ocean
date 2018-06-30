namespace OptimizationCore

type IConstraint<'a>=
    abstract member evaluate:'a -> float

type IOptimizer<'Tinput,'Tparam>=
    abstract member evaluate:'Tparam -> 'Tinput-> float
    abstract member WithContraint:IConstraint<'Tinput> array
 

