namespace Ocean.NumIntergration

module Methods=
    
    type IIntegration=    
        abstract Integrate:a:double*b:double->f:double->double->double
        
 