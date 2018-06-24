namespace CreditRiskPlus

module Commons=
    open MathNet.Numerics
    open Accord.Math
    
    let tocomplex(a) = complex a 0.0
    let toimag(a) = complex 0.0 a
    let fft (x: float array) =
                            let y =  Array.init (x.Length)(fun _ -> complex 0.0 0.0)
                            x |> Seq.iteri(fun i s -> y.[i] <- tocomplex(s))
                            Accord.Math.Transforms.FourierTransform2.FFT(y,FourierTransform.Direction.Forward)
                            y
                                                   
    let ifft (x: MathNet.Numerics.complex array) = 
                                                   let y =  Array.init (x.Length)(fun _ -> complex 0.0 0.0)
                                                   x |> Seq.iteri(fun i s -> y.[i]<-s)
                                                   Accord.Math.Transforms.FourierTransform2.FFT(y,FourierTransform.Direction.Backward)
                                                   y

    

    type VaRResult={DENSITY:(float*float) array;ValueAtRisk:(float*float) array}