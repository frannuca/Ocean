namespace CreditRiskPlus

module Data=
    open Deedle
    open MathNet.Numerics.Distributions
    type POINT = {LOSS:float;PROBD:float}
        
    type PortfolioData={Id:string array;PD:float array;E:float array;LGD:float array;Sigma:float array option}
    
    type PortfolioColumns=
        |Id
        |PD
        |Exposure
        |LGD
        |Sigma
        |E
        |Vn
        |Lambda
        with 
        override self.ToString()=
            match self with 
            |Id -> "Id"
            |PD -> "PD"
            |Exposure -> "Exposure"
            |LGD -> "LGD"
            |Sigma -> "Sigma"
            |E -> "E"
            |Vn -> "Vn"
            |Lambda -> "Lambda"
    
    let private GeneratePortfolioFrame(data:PortfolioData)=
        let names = data.Id |> Series.ofValues
        let Pds = data.PD |> Series.ofValues
        let Exposures = data.E |> Series.ofValues
        let LGDs = data.LGD |> Series.ofValues
        
        let frame = frame []
        frame.AddColumn(PortfolioColumns.Id.ToString(),names)
        frame.AddColumn(PortfolioColumns.PD.ToString(),Pds)
        frame.AddColumn(PortfolioColumns.Exposure.ToString(),Exposures)
        frame.AddColumn(PortfolioColumns.LGD.ToString(),LGDs)
        
        frame.AddColumn(PortfolioColumns.Sigma.ToString(), match data.Sigma with 
                                                            |Some(s) -> s 
                                                            |None -> Array.zeroCreate data.Id.Length
                                                            
                                                            |> Series.ofValues
                                                            ) 
                            
        frame
    
    let private QuantifyPortolio(qL:float)(frame:Frame<int,string>)=
        frame.AddColumn(PortfolioColumns.E.ToString(),frame.GetColumn<float>(PortfolioColumns.Exposure.ToString())/qL)
        frame.AddColumn(PortfolioColumns.Vn.ToString(),frame.GetColumn<float>(PortfolioColumns.E.ToString()) |> Series.mapValues(fun x -> System.Convert.ToInt32(System.Math.Ceiling(x))))
        frame

    let private ProcessRateOfDefault(frame:Frame<int,string>)=
        let aux = frame.GetColumn<float>(PortfolioColumns.PD.ToString())
        let aux2 = aux  |> Series.mapValues(fun x -> -System.Math.Log(1.0-x))
        frame.AddColumn(PortfolioColumns.Lambda.ToString(),aux2)
        frame 
    
    
    let GenerateFrame(data:PortfolioData)(qL:float) =
        data 
        |> GeneratePortfolioFrame
        |> QuantifyPortolio(qL)
        |> ProcessRateOfDefault