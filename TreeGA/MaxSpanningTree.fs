namespace Optimization
namespace GA

open System

module MaxSpanningTree=
    open System.Collections
    open System.Collections.Generic
    
    type BestMatch={NODE:int;BMATCH:int}



    let ComputeBestMatch(tree:Node,nodes:Node[],linkage:LinkProbObject)=
        
        let mutable nodesout = nodes
        let mutable nodesint = [tree.ID]
        nodesout |> Seq.iter(fun node -> node.BestMatch <- Some(tree,linkage.GetLinkageFor(node.ID,tree.ID).Info))
        
        nodesout <- nodesout |> Array.filter(fun n -> n.ID <> tree.ID)

        while nodesout.Length>0 do
           nodesout |> Seq.iter(fun node ->                                             
                                            tree.ListOfNodes 
                                            |> Seq.iter(fun innode ->  
                                                                        let vb = linkage.GetLinkageFor(innode.ID,node.ID).Info
                                                                        if vb > (node.BestMatch.Value |> snd) then
                                                                            node.BestMatch <- Some(innode,vb)
                                                            
                                            )
                                )

            //select the best node to add
           let bestnodepout = nodesout |> Seq.maxBy(fun n -> n.BestMatch.Value |> snd)
           
           
           Tree.InsertParentChild(tree)((bestnodepout.BestMatch.Value |> fst),bestnodepout)
           nodesout <- nodesout |> Array.filter(fun n -> n.ID <> bestnodepout.ID)
           ()                                                 
                                            
           
                                            

