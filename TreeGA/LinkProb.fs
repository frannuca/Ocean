namespace Optimization
namespace GA

open System

type LinkProb(c:float,preduction:float,inctree:float)=

    let mutable  counter00 = c
    let mutable  counter01 = c
    let mutable  counter10 = c
    let mutable  counter11 = c
   

    member self.P00 
        with  get() = let a = counter00/(counter00+counter01+counter10+counter11)
                      a

    member self.P01
        with get() = let a = counter01/(counter00+counter01+counter10+counter11)
                     a

    member self.P10
        with get() = let a = counter10/(counter00+counter01+counter10+counter11)
                     a

    member self.P11
        with get() = let a = counter11/(counter00+counter01+counter10+counter11)
                     a

    member self.P0_
        with  get() =  self.P00 + self.P01

    member self.P1_
        with  get() =  self.P10 + self.P11

    member self.P_0
        with  get() =  self.P00 + self.P10

    member self.P_1
        with  get() =  self.P01 + self.P11
    
    member self.Psum
        with get() = self.P00 + self.P01 + self.P10 + self.P11

    member self.P(bit1:bool,conditionBit2:bool)=
        match bit1,conditionBit2 with
        |(false,false) -> let a = self.P00/(self.P_0)
                          a
        |(true,false)  -> let a = self.P10/(self.P_0)
                          a
        |(false,true)  -> let a = self.P01/(self.P_1)
                          a
        |(true,true)  -> let a = self.P11/(self.P_1)
                         a

    member self.Info
        with get()= let a  = self.P00 * Math.Log(self.P00/(self.P0_*self.P_0))+
                             self.P01 * Math.Log(self.P01/(self.P0_*self.P_1)) +
                             self.P10 * Math.Log(self.P10/(self.P1_*self.P_0)) +
                             self.P11 * Math.Log(self.P11/(self.P1_*self.P_1))
                    a
    
    member self.Update(x0:bool,x1:bool)=
        counter00 <- counter00 * preduction
        counter01 <- counter01 * preduction
        counter10 <- counter10 * preduction
        counter11 <- counter11 * preduction

        match x0,x1 with
        |(false,false) ->  counter00 <- counter00 + inctree
        |(false,true)  ->  counter01 <- counter01 + inctree
        |(true,false)  ->  counter10 <- counter10 + inctree
        |(true,true)   ->  counter11 <- counter11 + inctree


type LinkProbObject(nbits:int,c:float,preduction:float,inctree:float)=
    let container = Array.init(nbits)(fun i -> Array.init(nbits)(fun j -> (i,j),new LinkProb(c,preduction,inctree))) |> Array.collect(id) 
                    |> dict
    

    member self.GetLinkageFor(i,j)=
       match container.TryGetValue((i,j)) with
       |(true,x) -> x
       |(false,_) -> failwith "key not found"

    member self.Update(i,j,biti,bitj)=        
        self.GetLinkageFor(i,j).Update(biti,bitj)                                                       

    