namespace OptimizationCore
namespace GA


type Chromosome(chr:Gene seq)=
        let chromo = chr |> Array.ofSeq

        member self.GetVector()=
            chromo |> Array.map(fun x -> x.GetFloat())
      
        member self.SetVector(x:float  array)=
            x |> Array.iteri(fun idx v -> chromo.[idx].SetNumber(v))

        member self.SetBits(x:bool  array)=
            let mutable offset = 0
            chromo |> Array.iteri(fun idx ss ->
                                                let chunk = x.[offset .. offset+ss.BitSize-1]
                                                offset <- offset + ss.BitSize
                                                ss.SetBit(chunk))

        member self.SpawnAnotherChromo()=
            new Chromosome(chromo |> Array.map(fun gen -> gen.SpawnAnotherGene())) 
        
        member self.Bits
            with get() = chromo |> Array.map(fun g -> g.Bits) |> Array.concat
        
        member self.BitSize
            with get() = chromo |> Array.map(fun g -> g.BitSize) |> Array.sum

type ChromosomeBuilder()=
    let mutable chromo: Gene list = []

    member self.WithParameter(minlevel:float,maxlevel:float,nbits:int,x:float option)=
        let gen = new Gene(minlevel,maxlevel,nbits)
        if x.IsSome then gen.SetNumber(x.Value)

        chromo <- chromo @ [gen]
        self

    member self.Build()=
        new Chromosome(chromo)
        
        
