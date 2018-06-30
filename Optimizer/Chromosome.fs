namespace OptimizationCore

type Chromosome(chr:Gene seq)=
        let chromo = chr |> Array.ofSeq

        member self.GetVector()=
            chromo |> Array.map(fun x -> x.GetFloat())

        member self.SetVector(x:float  array)=
            x |> Array.iteri(fun idx v -> chromo.[idx].SetNumber(v))

type ChromosomeBuilder()=
    let mutable chromo: Gene list = []

    member self.WithParameter(minlevel:float,maxlevel:float,nbits:int,x:float option)=
        let gen = new Gene(minlevel,maxlevel,nbits)
        if x.IsSome then gen.SetNumber(x.Value)

        chromo <- chromo @ [gen]
        self

    member self.Build()=
        new Chromosome(chromo)
    

