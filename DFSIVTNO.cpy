      * DATA AREA FOR TERMINAL INPUT                                    
                                                                        
       01  INPUT-MSG.                                                   
           02  IN-LL          PICTURE S9(3) COMP.                       
           02  IN-ZZ          PICTURE S9(3) COMP.                       
           02  IN-TRANCDE     PICTURE X(10).                             
           02  IN-COMMAND     PICTURE X(8).                             
           02  IN-LAST-NAME   PICTURE X(10).                            
           02  IN-FIRST-NAME  PICTURE X(10).                            
           02  IN-EXTENSION   PICTURE X(10).                             
           02  IN-ZIP-CODE    PICTURE X(7).                             
                                                                        
      * DATA AREA OUTPUT                                                
       01  OUTPUT-AREA.                                                 
           02  OUT-LL       PICTURE S9(3) COMP VALUE +95.               
           02  OUT-ZZ       PICTURE S9(3) COMP VALUE +0.                
           02  OUT-MESSAGE   PIC X(40).                             
           02  OUT-COMMAND   PIC X(8).                              
           02  OUT-LAST-NAME   PIC X(10).                       
           02  OUT-FIRST-NAME  PIC X(10).                       
           02  OUT-EXTENSION   PIC X(10).                       
           02  OUT-ZIP-CODE    PIC X(7).                        
           02  OUT-SEGMENT-NO  PICTURE X(4) VALUE '0001'.               