namespace HierarchyAllocation
open System


type Node<'a>(name:string,initialdata:'a)=    
    let _name=name
    let mutable _parent:Node<'a> option = None
    let _children = new CSList<'a>()
    let mutable _data = initialdata
    
    member self.Children
        with get()= _children
    
    member self.Parent
        with get()=_parent    
        and set(p)= _parent <- p
    
    member self.Depth=
        let rec rdpeth(x:Node<'a> option)=
            match x with 
            |None -> 0
            |Some(n) -> 1 + rdpeth(n.Parent)

        rdpeth(Some(self))
        
    member self.ListOfNodes =
        let queue = new System.Collections.Generic.Queue<Node<_>>()
        let listofnodes = new System.Collections.Generic.List<Node<_>>()
        queue.Enqueue(self)
        while(queue.Count>0) do
            let n = queue.Dequeue()
            listofnodes.Add(n)
            n.Children |> Seq.iter(fun c -> queue.Enqueue(c))
        listofnodes        

    member self.DepthFirstSearch =
        let stack = new System.Collections.Generic.Stack<Node<_>>()
        let listofnodes = new System.Collections.Generic.List<Node<_>>()
        stack.Push(self)
        while(stack.Count>0) do
            let n = stack.Pop()
            listofnodes.Add(n)
            n.Children |> Seq.iter(fun c -> stack.Push(c))
        listofnodes        
        
    member self.Data
        with get()= _data
        and set(value) = _data <- value
               
and  CSList<'a> = System.Collections.Generic.List<Node<'a>>