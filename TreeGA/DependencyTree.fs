namespace Optimization
namespace GA

open System

type Node(id:int)=
    let children = new System.Collections.Generic.List<Node>()
    let mutable parent:Node option = None
    member self.ID = id
    member self.Children = children
    member self.Parent 
        with get()= parent
        and set(value)= parent <- value
    
    member self.ListOfNodes =
        let queue = new System.Collections.Generic.Queue<Node>()
        let listofnodes = new System.Collections.Generic.List<Node>()
        queue.Enqueue(self)
        while(queue.Count>0) do
            let n = queue.Dequeue()
            listofnodes.Add(n)
            n.Children |> Seq.iter(fun c -> queue.Enqueue(c))
        listofnodes

    
type LinkProb(c:float,preduction:float,inctree:float)=

    let mutable  counter00 = c
    let mutable  counter01 = c
    let mutable  counter10 = c
    let mutable  counter11 = c

    member self.P00 
        with  get() = counter00/(counter00+counter01+counter10+counter11)

    member self.P01
        with get() = counter01/(counter00+counter01+counter10+counter11)

    member self.P10
        with get() = counter10/(counter00+counter01+counter10+counter11)

    member self.P11
        with get() = counter11/(counter00+counter01+counter10+counter11)

    
    member self.P(bit1:bool,conditionBit2:bool)=
        match bit1,conditionBit2 with
        |(false,false) -> let a = self.P00/(self.P00+self.P10)
                          a
        |(true,false)  -> let a = self.P10/(self.P00+self.P10)
                          a
        |(false,true)  -> let a = self.P01/(self.P01+self.P11)
                          a
        |(true,true)  -> let a = self.P11/(self.P11+self.P11)
                         a

    member self.Info
        with get()= self.P00 * Math.Log(self.P00/((self.P00+self.P01)*(self.P00+self.P10)))+
                    self.P01 * Math.Log(self.P01/((self.P00+self.P01)*(self.P01+self.P11)))+
                    self.P10 * Math.Log(self.P10/((self.P10+self.P11)*(self.P00+self.P10)))+
                    self.P11 * Math.Log(self.P11/((self.P10+self.P11)*(self.P01+self.P11)))

    
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
