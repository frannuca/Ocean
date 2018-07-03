namespace Optimization
namespace GA

open System

type Node(id:int)=
    let children = new System.Collections.Generic.List<Node>()
    let mutable parent:Node option = None
    let mutable bestMatch:(Node*float) option = None
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
    member self.BestMatch
        with get() = bestMatch
        and set(value) = bestMatch<-value

    
module Tree=
    let InsertParentChild(root:Node)(parent:Node,child:Node)=
        
        let nodes = root.ListOfNodes |>List.ofSeq
        let iparent = parent.ID
        let ichild = child.ID
        
        match nodes |> List.filter(fun n -> n.ID=ichild) with
        |[] -> 
               parent.Children.Add(child)
               child.Parent<-Some(parent)

        |[x] -> let oldparent = x.Parent
                            
                if oldparent.IsSome then oldparent.Value.Children.Remove(x) |>ignore
                x.Parent <- Some(parent)
                parent.Children.Add(x)

        |_ -> failwith "tree node duplication"

    let GetNode(root:Node)(id:int)=
        match root.ListOfNodes |> Seq.filter(fun n -> n.ID = id) |> List.ofSeq with
        |[x] -> Some(x)
        |[] -> None
        | _ -> failwith "duplicated node"