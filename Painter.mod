MODULE Painter
    !***********************************************************
    !
    ! Module:  Painter
    !
    ! Description:
    !   Painting module for the ABB IRB120 
    !
    ! Author: drongla, crookjj, doughezj, horvegc
    !
    ! Version: 0.5 - resizable canvas tools
    !
    !***********************************************************
    
    
  

    ! *** Constants ***
    ! Commonly Tweaked declarations
    ! I was here
    CONST num brushLength:=200;
    CONST num paintHeight:=5;
    !VAR signaldo gripper:= do10_16;
    PERS num canvasHeight:=-10; ! 5 for painting on the canvas. -10 for paper
    VAR bool paintMode := False;
    PERS num PAINT_MAX_DIST:=100;
    
    
    
    
    ! Of Relation to Colors:
    CONST num firstPaint{2}:=[276, -290];
    CONST num paintCupRadius:=50; ! The center-to-center spacings between paint cups (See: dixie cup :D )
    CONST num maxPaintX:=626;
    VAR num paintCupYOffset;
    VAR num cupIndex;     
    
    
    ! Canvas size Declarations
    ! * The largest usable square area the robot can draw in is 500mm wide by 150mm tall
    ! * This is a rectangular large canvas, about 19.6" by 9.8"
    PERS num canvasXmin:=400;
    PERS num canvasXmax:=640;
    PERS num canvasYmin:=-230;
    PERS num canvasYmax:=18;  
    
    ! Used in the conversion of pixels to mm on the canvas
    !CONST num XOffset:=260;! Unused - by default, corner of image goes to min y min x of canvas. 
    !CONST num YOffset:=-150; ! Unused - by default, corner of image goes to min y min x of canvas. 
    
    ! Scaling factor for when we load an image (Default 0.5)
    VAR num SF:=0.5;
    
    ! Orientation constants
    CONST orient ZeroZeroQuat:=[0.70427,0.02028,0.71115,0.01996];    
    VAR orient paintStrokeQuat; ! set to zero-zero in initializeProgram
    VAR orient paintCupQuat;    ! but these values are exposed for special cases where we may want them changed. 
    CONST orient paintcleanerQuat:=[0.51854, 0.50842, 0.49217, -0.47999]; 
    CONST orient paletteQuat:=[0.50, -0.50, 0.50, 0.50]; 
    CONST orient upsideDownQuat:=[0.03, 0.709, 0.03, 0.703]; 
    
    
    ! New locations using the palette
    ! it wouldn't be too hard to compute each, but this makes it a little easier to change quickly
    var robtarget safeMiddle:= [[500, 0, 150],ZeroZeroQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
    var robtarget upsideDownMiddle:= [[500, 0, 350],upsideDownQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
    var robtarget overPalette:= [[213, 412, 140],paletteQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
    ! [[palettePaint1{1},palettePaint1{2},paintHeight+70],paletteQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
    CONST num palettePaint1{2}:=[213, 412];
    VAR num targetCup := 0;
    
    ! Describes the paintbrush location. TODO: verify with metric calipers. 
    PERS tooldata paintBrush:=[TRUE,[[87,0,146],[1,0,0,0]],[0.2,[0,0,146],[0,0,1,0],0,0,0]];
    
    ! *** Variables ***  
    VAR iodev iodev1;
    VAR rawbytes rawMsgData;
    
    ! Store image size, in pixels
    VAR num sizeX;
    VAR num sizeY;
    VAR num XTGT:=0; ! X target (processed)
    VAR num YTGT:=0; ! Y Target (processed)
    VAR num lastX:=500;
    VAR num lastY:=0;
    ! Temporary values for processing coordinates
    VAR num tX;
    VAR num tY;
    VAR num tX1;
    VAR num tY1;
    VAR num tX2;
    VAR num tY2;
    VAR num tX3;
    VAR num tY3;
    VAR num tI;
    !
    ! Velocity values for processing coordinates. Note: Is not true velocity, just step-relative velocity.
    VAR num vX;
    VAR num vY;
    
    ! Used in distance calculation
    VAR num vectorMag;

    ! Locations of the painting targets. 
    VAR num bezierPointsX{101};
    VAR num bezierPointsY{101};
    ! Change these in procedure: initializeColors
    VAR robtarget overPaint;
    VAR robtarget inPaint;
    VAR robtarget approachPaint;
    
    VAR robtarget approachClean;
    VAR robtarget overClean;
    VAR robtarget clean;
    VAR robtarget overDryer;
    VAR robtarget dryer;
    VAR robtarget dryerL;
    VAR robtarget dryerH;
    !    
    VAR bool newStroke;
    VAR num distanceTravelled := 0;
    ! UI Variables/Constants
    
    
    ! First-loop flags
    VAR bool firstTimeRun := TRUE;
    VAR string currentColor:= "A";
    VAR string lastColor := "A";
    VAR num serialTimeout := 25;
    VAR bool doubleDip := FALSE ;
    VAR bool brushClean := FALSE; 
    VAR bool brushDirty := FALSE;
    VAR bool isRotated := FALSE;
    
    !***********************************************************
    !
    ! Procedure main
    !
    !   This is the entry point of your program
    !
    !***********************************************************
    PROC main()
        VAR btnres answer;
        VAR string currentSize;
        currentSize := "Canvas Dims: Z:"+ NumToStr(canvasHeight,0) + "mm, X:"+NumToStr(canvasXmax-canvasXmin,0)+"mm, Y:"+NumToStr(canvasYmax-canvasYmin,0)+"mm, Paint Dist:"+NumToStr(PAINT_MAX_DIST,0);
        ConfL\Off;
        initializeProgram;
        paintProgram;
        answer:= UIMessageBox(
            \Header:="Pre-Paint Com Checks"
            \MsgArray:=["Please check and verify the following:","- The serial cable is connected to COM1", "- The PC is connected to the serial cable","- BobRoss has opened the serial channel on the PC", currentSize]
            \BtnArray:=["Ready","Positioning", "Canvas Tools", "Park"]
            \Icon:=iconInfo);
        IF answer = 1 THEN
            ! Operator said ready
            paintProgram;
        ELSEIF answer = 2 THEN 
            
            CALIBRATIONTOOLS;
        ELSEIF answer = 3 THEN 
            
            CANVASTOOLS;
        ELSE 
            ! Parking spot for storage. 
            MoveL [[489,0,canvasHeight+100],ZeroZeroQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v500,z50,paintBrush;
            Stop;
        ENDIF      
       
        RETURN;
    ENDPROC
    
    PROC CALIBRATIONTOOLS()
        VAR num exitcode;
        VAR num canvasAnswer;
        VAR btnres answer1;
        
        exitcode:=0;
        
        WHILE exitcode = 0 DO 
            
            answer1:= UIMessageBox(
            \Header:="Equipment Position Helper"
            \MsgArray:=["Select an option from the selection below","", "","",""]
            \BtnArray:=["Clean","Super-Dry", "Touch","Home","Return"]
            \Icon:=iconInfo);
            
            IF answer1 = 1 THEN
                ! operator said clean
                MoveL approachClean, v100,z50,paintBrush;
                cleanCycle 1;
                MoveL approachClean, v100,z50,paintBrush;
                MoveL [[canvasXmin,canvasYmin-20,canvasHeight+100],ZeroZeroQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v100,z50,paintBrush;
            ELSEIF answer1 = 2 THEN 
                MoveL approachClean, v100,z50,paintBrush;
                MoveL overDryer,v100,z20,paintBrush;
                MoveL dryer,v100,fine,paintBrush;
                WaitTime 3;
                MoveL overDryer,v100,z50,paintBrush;
                MoveL approachClean, v100,z50,paintBrush;
                MoveL [[canvasXmin,canvasYmin-20,canvasHeight+100],ZeroZeroQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v100,z50,paintBrush;
            ELSEIF answer1 = 3 THEN 
                ! touches opposing corners of the canvas, pausing for a second and a half.  
                MoveL [[canvasXmin,canvasYmin,canvasHeight+100],ZeroZeroQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v100,z50,paintBrush;
                MoveL [[canvasXmin,canvasYmin,canvasHeight],ZeroZeroQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v100,z50,paintBrush;
                WaitTime 1.5;    
                MoveL [[canvasXmin,canvasYmin,canvasHeight+100],ZeroZeroQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v100,z50,paintBrush;
                MoveL [[canvasXmax,canvasYmax,canvasHeight+100],ZeroZeroQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v100,z50,paintBrush;
                MoveL [[canvasXmax,canvasYmax,canvasHeight],ZeroZeroQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v100,z50,paintBrush;
                WaitTime 1.5;
                MoveL [[canvasXmax,canvasYmax,canvasHeight+10],ZeroZeroQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v100,z50,paintBrush;
                
            ELSEIF answer1 = 4 THEN 
                MoveL [[canvasXmin,canvasYmin-20,canvasHeight+100],ZeroZeroQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v100,z50,paintBrush;
            else
                exitcode := 1;
            ENDIF 
        
        ENDWHILE 
        
    ENDPROC 
    
    PROC CANVASTOOLS()
        VAR num exitcode;
        VAR num canvasAnswer;
        VAR btnres answer2;
        VAR string currentSize;
        exitcode:=0;
        
        WHILE exitcode = 0 DO 
            
            currentSize := "Canvas Dims: Z:"+ NumToStr(canvasHeight,0) + "mm, X:"+NumToStr(canvasXmax-canvasXmin,0)+"mm, Y:"+NumToStr(canvasYmax-canvasYmin,0)+"mm, Paint Dist:"+NumToStr(PAINT_MAX_DIST,0);
            answer2:= UIMessageBox(
                \Header:="Canvas Tools Menu"
                \MsgArray:=["Welcome to Canvas Setup!","The values entered here are stored between controller restarts",  currentSize,"Note: Ensure the corner of the canvas is at ","x:400, y:-230"]
                \BtnArray:=["Z","X", "Y","L","Return"]
                \Icon:=iconWarning);
                
            IF answer2 = 1 THEN 
                canvasAnswer := UINumEntry(
                \Header:="Canvas Vertical Height Setup (Z-AXIS)"
                \MsgArray :=["What is the vertical height, in mm, of the canvas"," from the floor of the workcell?", "Current Dim: Z:"+ NumToStr(canvasHeight,0) ]
                \Icon:=iconInfo
                \InitValue:=canvasHeight
                \MinValue:=0
                \MaxValue:=100
                \AsInteger);
                canvasHeight := canvasAnswer;
            ELSEIF answer2 = 2 THEN 
                canvasAnswer := UINumEntry(
                \Header:="Canvas Height Setup (X-AXIS)"
                \MsgArray:=["What is the height, in mm, of the canvas?","Note: Use 250 for wide canvas due to mobility reasons", "Current Dim: X:"+NumToStr(canvasXmax-canvasXmin,0)]
                \Icon:=iconInfo
                \InitValue:=(canvasXmax-canvasXmin)
                \MinValue:=50
                \MaxValue:=500
                \AsInteger);
                canvasXmax := canvasXmin + canvasAnswer;
            ELSEIF answer2 = 3 THEN 
                canvasAnswer := UINumEntry(
                \Header:="Canvas Width Setup (Y-AXIS)"
                \MsgArray:=["What is the width, in mm, of the canvas?","Note: A wide canvas restricts X movement, and","can put parts of the image out of bounds", "Current Dim: Y:"+NumToStr(canvasYmax-canvasYmin,0)]
                \Icon:=iconInfo
                \InitValue:=(canvasYmax-canvasYmin)
                \MinValue:=50
                \MaxValue:=500
                \AsInteger);
                canvasYmax := canvasYmin + canvasAnswer;
            ELSEIF answer2 = 4 THEN 
                canvasAnswer := UINumEntry(
                \Header:="Brush Stroke Length (L)"
                \MsgArray:=["How long should the brush travel before getting more paint?","TRY: Using 50 for paper, 30 for canvas", "Current Paint Dist:"+NumToStr(PAINT_MAX_DIST,0)]
                \Icon:=iconInfo
                \InitValue:=PAINT_MAX_DIST
                \MinValue:=10
                \MaxValue:=80
                \AsInteger);
                PAINT_MAX_DIST := canvasAnswer;
            else 
                exitcode := 1;
            ENDIF 
        ENDWHILE 
    ENDPROC
    
    
    !***********************************************************
    !
    ! Procedure paintProgram
    !
    !   This initializes our program and immediately looks for SIZE to be set by serial commands. 
    !
    !***********************************************************    
    PROC paintProgram()
        VAR bool result;
        VAR string response;
        VAR num splitnum;
        VAR string directive;
        VAR string params;
        VAR num endTokenPos;
        
        Open "COM1:", iodev1 \Bin;
        ClearIOBuff iodev1;
        ClearRawBytes rawMsgData;
        WaitTime 0.1;                 
        WriteStrBin iodev1, "\05";
        response := readSerial();
        WHILE response = "" DO
            WriteStrBin iodev1, "\05";
            response := readSerial();
        ENDWHILE
            TPWrite response;
        
        ! Slice this up into directive and parameters
        endTokenPos:=StrFind(response, 1, ";");
        IF endTokenPos > StrLen(response) THEN
            throwError "semicolon", response;
        ELSE            
            response:=StrPart(response,1, endTokenPos-1); ! trim string to ignore end token.
        ENDIF
        splitNum := StrFind(response, 1, ":");
        ! note: StrPart( string, startIndexInclusive, length)
        directive := StrPart(response, 1, splitNum - 1); ! We don't care about the ':'
        params := StrPart(response, splitNum+1, Strlen(response) - (splitnum + 1) + 1);
        
        IF directive = "SIZE" THEN
            ! Expected 'response' to be SIZE:X400,Y200;
            result:=readSize(params);
            IF result = TRUE THEN
                ! do other stuff! WoohoooooooooO!OOO!O!O!O!OO!O!O!O
                WriteStrBin iodev1, "\06";
                paintLoop;  ! When this is called for the first time, it will be after obtaining the image size. 
            ELSE 
                WriteStrBin iodev1, "\15";
            ENDIF
        ELSE 
            WriteStrBin iodev1, "\15";
        ENDIF
        Close iodev1;
    ENDPROC

    !***********************************************************
    !
    ! Procedure initializeColors
    !
    !   This sets up all targets in the program. 
    !
    !***********************************************************
    PROC initializeProgram()
        
        paintStrokeQuat:=ZeroZeroQuat; 
        paintCupQuat:=ZeroZeroQuat;
        
        ! FirstTimeRun Flags
        
        firstTimeRun :=TRUE;
        currentColor:= "A";
        lastColor := "A";
        cupIndex := 0;
		paintCupYOffset :=0;
        overPaint := [[firstPaint{1} + (paintCupRadius * cupIndex),(-290 - (paintCupRadius*paintCupYOffset)),paintHeight+70],paintCupQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        inPaint:= [[firstPaint{1} + (paintCupRadius * cupIndex),(-290 - (paintCupRadius*paintCupYOffset)),paintHeight],paintCupQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        serialTimeout := 25;
        doubleDip := FALSE ;
        brushClean := FALSE; 
        brushDirty := FALSE;
        isRotated := FALSE;
        ! 

        
        approachPaint := [[376,-290,paintHeight+70],paintCupQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        
        
        approachClean:=[[455,-290,350],paintcleanerQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        overClean:=[[455,-472,175],paintcleanerQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        clean:=[[455,-472,100],paintcleanerQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        
        overDryer:=[[225.9,-464.4,350],paintcleanerQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        dryer:=[[225.9,-464.4,166.5],paintcleanerQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        dryerL:=[[225.9,-464.4,163.6],paintcleanerQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        dryerH:=[[225.9,-464.4,169.3],paintcleanerQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        firstTimeRun := TRUE;
        
        FOR i FROM  1 TO 101 DO
            bezierPointsX{i}:= 400.0;
            bezierPointsY{i}:= 10.0;
        ENDFOR 
        
    ENDPROC
    !***********************************************************
    !
    ! function processCoordinates(string)
    !           String: "x:123,y:456'
    !           Returns TRUE or FALSE if it passes or fails. 
    !
    !   This takes a string and puts the x, y into globals tX and tY
    !
    !***********************************************************
    FUNC bool processCoordinates(string parameters, bool contiguous)
        ! exampleParams are X:230,Y:170;
        ! note: it may or may not contain a semicolon
        ! Slice this up into directive and parameters
        
        VAR num splitNum;
        
        ! note: StrPart( string, startIndexInclusive, length)
        VAR bool ok;
        VAR bool result;
        VAR string dA;
        VAR string dB ;
        VAR num dataIndex ;
        VAR string dAtype;
        VAR string dBtype;
        VAR num dAval;
        VAR num dBval;
        VAR num specialCheckIndex;
        VAR string remainingMessage;
        VAR bool multiCoordMessage;
        
        result  := TRUE;
        
        multiCoordMessage := FALSE;
        splitNum:= StrFind(parameters, 1, ",");
        dA := StrPart(parameters, 1, splitNum - 1); ! Should be X:230 - We don't care about the ','
        dataIndex := StrFind(dA, 1, ":"); ! finding the ':' in X:230
        
        
        IF dataIndex < StrLen(dA) THEN
            ! continue processing
            dAtype := StrPart(dA, 1, 1); ! Should be X
            ok:=StrToVal(StrPart(dA, 3, StrLen(dA)-2), dAval); ! should trim the X: from X:230 and parse 230
            IF ok = TRUE THEN 
                IF dAtype = "X" OR dAtype = "x" THEN                
                    tX:=dAval;
                ELSE 
                    tY:=dAval;
                ENDIF 
            ELSE 
                ! throw error!
                throwError "coord", dA;
                
                result := FALSE;
            endif
            
            dB:= StrPart(parameters, splitNum + 1, StrLen(parameters) - (splitNum+ 1) + 1); ! should contain Y:170 
            dataIndex := StrFind(dB, 1, ":"); ! finding the ':' in Y:170
            specialCheckIndex:= StrFind(db, 1, ",");
            if (specialCheckIndex < StrLen(db)) THEN ! TODO: This does not currently work!! RUH_ROH
            ! Oh boy, We have a list of coords!
                dB:= StrPart(parameters, splitNum+1, specialCheckIndex  - (splitNum) );
                remainingMessage := StrPart(parameters, specialCheckIndex+1, StrLen(parameters) - (specialCheckIndex) + 1);
                TPWrite "Multi-coord message:";
                TPWrite remainingMessage;
                multiCoordMessage:=TRUE;
            endif 
            
            IF dataIndex < StrLen(dB) THEN 
                            ! continue processing
                dBtype := StrPart(dB, 1, 1); ! Should be X
                ok:=StrToVal(StrPart(dB, 3, StrLen(dB) - 3 + 1), dBval);
                IF ok = TRUE THEN 
                    IF dBtype = "X" OR dBtype = "x" THEN
                        IF dAtype = "Y" OR dAtype = "y" THEN 
                        tX:=dBval;
                        ok := TRUE;
                        ELSE
                            ok := FALSE;
                        ENDIF 
                    ELSE 
                        IF dAtype = "X" OR dAtype = "x" THEN 
                        tY:=dBval;
                        ok := TRUE;
                        ELSE 
                            ok:= FALSE;
                        ENDIF 
                    ENDIF 
                ELSE 
                    ! throw error!
                throwError "coord", dB;
                result := FALSE;
                ok:= TRUE;
                endif
                ! check and see if OK changed to false again.
                IF ok = FALSE THEN
                    throwError "multicoord", parameters;                    
                    result := FALSE;
                ENDIF 
            ELSE
                ! we have a problem. X or Y not found
                    throwError "missingcoord", parameters;  
                    result := FALSE;
            ENDIF 
        ELSE 
            ! we have a problem. Throw an error. 
            throwError "missingcoord", parameters;  
            
            result := FALSE;
        ENDIF
        
        ! NOTE: Recursion here! Warning: this may or may not be good. TODO: test this
        !IF multiCoordMessage = TRUE AND result = TRUE THEN 
        !    result := processCoordinates(remainingMessage, TRUE);
        !ENDIF 
        
        RETURN result;
    endfunc 
    
    
    FUNC bool processCoordinates2(string parameters, bool contiguous)
        ! exampleParams are X:230,Y:170;
        ! note: it may or may not contain a semicolon
        ! Slice this up into directive and parameters
        
        VAR num splitNum;
        
        ! note: StrPart( string, startIndexInclusive, length)
        VAR bool ok;
        VAR bool result;
        VAR string dA;
        VAR string dB ;
        VAR string dBpart;
        VAR string dC;
        VAR string dCpart;
        VAR num dataIndex ;
        VAR string dAtype;
        VAR string dBtype;
        VAR string dCtype;
        
        VAR num dAval;
        VAR num dBval;
        VAR num dCval;
        
        VAR num specialCheckIndex;
        VAR string remainingMessage;
        VAR bool multiCoordMessage;
        
        result  := TRUE;
        
        multiCoordMessage := FALSE;
        splitNum:= StrFind(parameters, 1, ",");
        dA := StrPart(parameters, 1, splitNum - 1); ! Should be X:230 - We don't care about the ','
        dataIndex := StrFind(dA, 1, ":"); ! finding the ':' in X:230
        
        
        IF dataIndex < StrLen(dA) THEN
            ! continue processing
            dAtype := StrPart(dA, 1, 1); ! Should be X
            ok:=StrToVal(StrPart(dA, 3, StrLen(dA)-2), dAval); ! should trim the X: from X:230 and parse 230
            IF ok = TRUE THEN 
                IF dAtype = "X" OR dAtype = "x" THEN                
                    tX:=dAval;
                ELSE 
                    tY:=dAval;
                ENDIF 
            ELSE 
                ! throw error!
                throwError "coord", dA;
                
                result := FALSE;
            endif
            
            dB:= StrPart(parameters, splitNum + 1, StrLen(parameters) - (splitNum+ 1) + 1); ! should contain Y:170 
            dataIndex := StrFind(dB, 1, ":"); ! finding the ':' in Y:170
            !specialCheckIndex:= StrFind(db, 1, ",");
            
            
            !if (specialCheckIndex < StrLen(db)) THEN ! TODO: This does not currently work!! RUH_ROH
            ! Oh boy, We have a list of coords!
            !    dB:= StrPart(parameters, splitNum+1, specialCheckIndex  - (splitNum) );
            !    remainingMessage := StrPart(parameters, specialCheckIndex+1, StrLen(parameters) - (specialCheckIndex) + 1);
            !    TPWrite "Multi-coord message:";
            !    TPWrite remainingMessage;
            !    multiCoordMessage:=TRUE;
            !endif 
            
            IF dataIndex < StrLen(dB) THEN 
                            ! continue processing
                dBtype := StrPart(dB, 1, 1); ! Should be Y
                specialCheckIndex:= StrFind(dB, 1, ",");
                dBpart:= StrPart(dB, 3, specialCheckIndex - 3);
                ok:=StrToVal(dBpart, dBval); !StrLen(dB) - 3 + 1
                IF ok = TRUE THEN 
                    IF dBtype = "Y" OR dBtype = "y" THEN
                        tY:=dCval;
                    ENDIF 
                ELSE 
                    ! throw error!
                    throwError "coord", dBpart;
                    result := FALSE;
                    ok:= TRUE;
                endif
                specialCheckIndex:= StrFind(dB, 1, ",");
                !TPWrite StrLen(parameters);
                dC:= StrPart(dB, specialCheckIndex + 1, StrLen(dB) - specialCheckIndex ); ! should contain I:170 
                dataIndex := StrFind(dC, 1, ":"); ! finding the ':' in I:170
                specialCheckIndex:= StrFind(dC, 1, ",");
                !TPWrite dC;
                
                IF dataIndex < StrLen(dB) THEN 
                                ! continue processing
                    dCtype := StrPart(dC, 1, 1); ! Should be I
                    dCpart := StrPart(dC, 3, StrLen(dC) - 3 + 1);
                    !TPWrite dCpart;
                    ok:=StrToVal(dCpart, dCval);
                    IF ok = TRUE THEN 
                        tI := dCval;
                        IF dBtype = "X" OR dBtype = "x" THEN
                            IF dAtype = "Y" OR dAtype = "y" THEN 
                            tX:=dBval;
                            ok := TRUE;
                            ELSE
                                ok := FALSE;
                            ENDIF 
                        ELSE 
                            IF dAtype = "X" OR dAtype = "x" THEN 
                            tY:=dBval;
                            ok := TRUE;
                            ELSE 
                                ok:= FALSE;
                            ENDIF 
                        ENDIF 
                    ELSE 
                        ! throw error!
                    throwError "coord", dC;
                    result := FALSE;
                    ok:= TRUE;
                    endif
                endif
                
                
                
                ! check and see if OK changed to false again.
                IF ok = FALSE THEN
                    throwError "multicoord", parameters;                    
                    result := FALSE;
                ENDIF 
            ELSE
                ! we have a problem. X or Y not found
                    throwError "missingcoord", parameters;  
                    result := FALSE;
            ENDIF 
        ELSE 
            ! we have a problem. Throw an error. 
            throwError "missingcoord", parameters;  
            
            result := FALSE;
        ENDIF
        
        ! NOTE: Recursion here! Warning: this may or may not be good. TODO: test this
        !IF multiCoordMessage = TRUE AND result = TRUE THEN 
        !    result := processCoordinates(remainingMessage, TRUE);
        !ENDIF 
        
        RETURN result;
    endfunc 
    
    
    
    FUNC bool processCoordinates3(string parameters, bool contiguous)
        ! exampleParams are X:230,Y:170#X:230,Y:170#X:230,Y:170;
        ! note: it may or may not contain a semicolon
        ! Slice this up into directive and parameters
        
        VAR num splitNum;
        VAR num splitNum2;
        
        
        ! note: StrPart( string, startIndexInclusive, length)
        VAR bool ok;
        VAR bool result;
        VAR string d1;
        VAR string d2 ;
        VAR string dBpart;
        VAR string d3;
        VAR string dA;
        VAR string dB;
        VAR string dC;
        VAR string dCpart;
        VAR num dataIndex ;
        VAR string dAtype;
        VAR string dBtype;
        VAR string dCtype;
        
        VAR num dAval;
        VAR num dBval;
        VAR num dCval;
        
        VAR num specialCheckIndex;
        VAR string remainingMessage;
        VAR bool multiCoordMessage;
        
        result  := TRUE;
        TPWrite parameters;
        
        multiCoordMessage := FALSE;
        splitNum:= StrFind(parameters, 1, "#");
        d1 := StrPart(parameters, 1, splitNum - 1); ! Should be X:230 - We don't care about the ','
        TPWrite d1;
        d2 := StrPart(parameters, splitNum +1 , StrLen(parameters) - splitNum ); 
        TPWrite d2;
        
        splitNum2 := StrFind(d2, 1, "#");
        d3 := StrPart(d2, splitNum2 + 1, StrLen(d2) - splitNum2);
        TPWrite d3;
        
        d2 := StrPart(d2, 1, splitNum2-1);
        TPWrite d2;

        !d2 := StrPart(parameters, splitNum +1 , ); 
        !dataIndex := StrFind(d1, 1, ":"); ! finding the ':' in X:230
        
        ok := processCoordinates(d1, FALSE);
        tX1 := tX;
        tY1 := tY;
        
        ok := processCoordinates(d2, FALSE);
        tX2 := tX;
        tY2 := tY;
        
        ok := processCoordinates(d3, FALSE);
        tX3 := tX;
        tY3 := tY;
        
        TPWrite "We did it!";
        
        RETURN ok;
        
        IF dataIndex < StrLen(dA) THEN
            ! continue processing
            dAtype := StrPart(dA, 1, 1); ! Should be X
            ok:=StrToVal(StrPart(dA, 3, StrLen(dA)-2), dAval); ! should trim the X: from X:230 and parse 230
            IF ok = TRUE THEN 
                IF dAtype = "X" OR dAtype = "x" THEN                
                    tX:=dAval;
                ELSE 
                    tY:=dAval;
                ENDIF 
            ELSE 
                ! throw error!
                throwError "coord", dA;
                
                result := FALSE;
            endif
            
            dB:= StrPart(parameters, splitNum + 1, StrLen(parameters) - (splitNum+ 1) + 1); ! should contain Y:170 
            dataIndex := StrFind(dB, 1, ":"); ! finding the ':' in Y:170
            !specialCheckIndex:= StrFind(db, 1, ",");
            
            
            !if (specialCheckIndex < StrLen(db)) THEN ! TODO: This does not currently work!! RUH_ROH
            ! Oh boy, We have a list of coords!
            !    dB:= StrPart(parameters, splitNum+1, specialCheckIndex  - (splitNum) );
            !    remainingMessage := StrPart(parameters, specialCheckIndex+1, StrLen(parameters) - (specialCheckIndex) + 1);
            !    TPWrite "Multi-coord message:";
            !    TPWrite remainingMessage;
            !    multiCoordMessage:=TRUE;
            !endif 
            
            IF dataIndex < StrLen(dB) THEN 
                            ! continue processing
                dBtype := StrPart(dB, 1, 1); ! Should be Y
                specialCheckIndex:= StrFind(dB, 1, ",");
                dBpart:= StrPart(dB, 3, specialCheckIndex - 3);
                ok:=StrToVal(dBpart, dBval); !StrLen(dB) - 3 + 1
                IF ok = TRUE THEN 
                    IF dBtype = "Y" OR dBtype = "y" THEN
                        tY:=dCval;
                    ENDIF 
                ELSE 
                    ! throw error!
                    throwError "coord", dBpart;
                    result := FALSE;
                    ok:= TRUE;
                endif
                specialCheckIndex:= StrFind(dB, 1, ",");
                !TPWrite StrLen(parameters);
                dC:= StrPart(dB, specialCheckIndex + 1, StrLen(dB) - specialCheckIndex ); ! should contain I:170 
                dataIndex := StrFind(dC, 1, ":"); ! finding the ':' in I:170
                specialCheckIndex:= StrFind(dC, 1, ",");
                !TPWrite dC;
                
                IF dataIndex < StrLen(dB) THEN 
                                ! continue processing
                    dCtype := StrPart(dC, 1, 1); ! Should be I
                    dCpart := StrPart(dC, 3, StrLen(dC) - 3 + 1);
                    !TPWrite dCpart;
                    ok:=StrToVal(dCpart, dCval);
                    IF ok = TRUE THEN 
                        tI := dCval;
                        IF dBtype = "X" OR dBtype = "x" THEN
                            IF dAtype = "Y" OR dAtype = "y" THEN 
                            tX:=dBval;
                            ok := TRUE;
                            ELSE
                                ok := FALSE;
                            ENDIF 
                        ELSE 
                            IF dAtype = "X" OR dAtype = "x" THEN 
                            tY:=dBval;
                            ok := TRUE;
                            ELSE 
                                ok:= FALSE;
                            ENDIF 
                        ENDIF 
                    ELSE 
                        ! throw error!
                    throwError "coord", dC;
                    result := FALSE;
                    ok:= TRUE;
                    endif
                endif
                
                
                
                ! check and see if OK changed to false again.
                IF ok = FALSE THEN
                    throwError "multicoord", parameters;                    
                    result := FALSE;
                ENDIF 
            ELSE
                ! we have a problem. X or Y not found
                    throwError "missingcoord", parameters;  
                    result := FALSE;
            ENDIF 
        ELSE 
            ! we have a problem. Throw an error. 
            throwError "missingcoord", parameters;  
            
            result := FALSE;
        ENDIF
        
        ! NOTE: Recursion here! Warning: this may or may not be good. TODO: test this
        !IF multiCoordMessage = TRUE AND result = TRUE THEN 
        !    result := processCoordinates(remainingMessage, TRUE);
        !ENDIF 
        
        RETURN result;
    endfunc 
    
    !***********************************************************
    !
    ! function result readSize(string)
    !           String: "x:123,y:456"
    !           Returns TRUE or FALSE if it passes or fails. 
    !
    !   Reads the size off the passed file
    !
    !***********************************************************    
    FUNC bool readSize(string parameters)   
        
        VAR bool result;
        result := processCoordinates(parameters, FALSE);
        IF result = TRUE THEN
            ! are we over the size constraints and in need of a scaling factor?
            IF tX > tY THEN 
                isRotated := TRUE;
                sizeY := tX;
                sizeX := tY;                
            else 
                sizeX := tX;
                sizeY := tY;   
            ENDIF

            
            IF sizeX <= 5 OR sizeY <= 5 THEN
                throwError "canvas", parameters + " ::SizeWayTooSmall";
            ENDIF 
            

            
            IF (sizeX > (canvasXmax-canvasXmin)) OR (sizeY > (canvasYmax-canvasYmin))THEN
                ! the Y proportion should be the scaling factor, as it was the smaller number
                IF ((canvasXmax-canvasXmin)/sizeX) > ((canvasYmax-canvasYmin)/sizeY) THEN
                    SF:=(canvasYmax-canvasYmin)/sizeY;
                ELSE
                    SF:=(canvasXmax-canvasXmin)/sizeX;
                ENDIF
                    
            ELSE
                SF:=1;
            ENDIF 
            
            
                
        ELSE
            throwError "canvas", parameters+ " ::CoordProcFail";  
            
        ENDIF
        ! Just in case of trouble later on.         
        XTGT:=(SF*tX)+canvasXmin;
        YTGT:=(SF*tY)+canvasYmin;
        RETURN result;
    ENDFUNC 
    !***********************************************************
    !
    ! function result readSerial()
    !           Returns string on completion. 
    !
    !   reads from the serial channel, returning commands sent. 
    ! Note: this effectively never times out. 
    !
    !***********************************************************        
    func string readSerial()
        VAR bool passed := TRUE;
        VAR string response;
        VAR rawBytes rawMsgData;
        ReadRawBytes iodev1, rawMsgData \Time:=serialTimeout;   
           
        ERROR
            IF ERRNO = ERR_DEV_MAXTIME THEN
                ! do something
                passed := FALSE;
                RETURN "";
                    
            ENDIF        
        IF passed = TRUE THEN 
            UnpackRawBytes rawMsgData, 1, response \ASCII:=(RawBytesLen(rawMsgData));
            ClearIOBuff iodev1; 
            
            RETURN response;
             
            
        ELSE 
            RETURN "";
        ENDIF 
    ENDFUNC
    
    !***********************************************************
    !
    ! function result paintLoop()
    !           Returns FALSE on completion
    !
    !   loops and reads from the serial channel, and passes the commands to subroutines.
    !
    !***********************************************************    
    proc paintLoop()
        ! TODO: Properly handle extra colons and Test this. Warining: Changed directives without parameters to not include the colon ':' character. Changed how this case was handled.
        VAR bool loop := TRUE;
        VAR string response;
        VAR num splitNum;
        VAR string directive;
        VAR string params;
        VAR num timeoutCounter:= 0;
        
        VAR num endTokenPos;
        WHILE loop = TRUE DO
            timeoutCounter := 0;
            response := readSerial();
            WHILE response = "" DO
                response := readSerial();
                timeoutCounter:= timeoutCounter + 1;
                IF timeoutCounter = -1 THEN
                    TPWrite "Aborting: Timeout";
                    moveToFinish;
                    Break;
                ENDIF
            ENDWHILE
            serialTimeout := 0.1;
            !TPWrite response; ! OH GOD UNCOMMENT THIS LATER
            
            endTokenPos:=StrFind(response, 1, ";");
            IF endTokenPos > StrLen(response) THEN
                throwError "semicolon", response;
            ELSE            
                response:=StrPart(response,1, endTokenPos-1); ! trim string to ignore end token.
            ENDIF
        
            ! Slice this up into directive and parameters
            splitNum := StrFind(response, 1, ":");
            ! note: StrPart( string, startIndexInclusive, length)
            IF splitNum > StrLen(response) THEN 
                
                loop:=directiveNoParams(response);
                
            ELSEIF splitNum < StrLen(response) THEN 
                directive := StrPart(response, 1, splitNum - 1); ! We don't care about the ':'
                params := StrPart(response, splitNum+1, Strlen(response) - (splitNum + 1) + 1);  
                
                loop:= directiveWithParams(directive, params);
            ELSE 
                WriteStrBin iodev1, "\15";
                ErrLog 4800, "Command Error", "Colon at end of message is extraneous",response,"-","-";
                TPWrite "Command Error: Extra colon at command termination";
                
                loop:= FALSE;
            ENDIF 
            
        ENDWHILE
        RETURN;
        
    endproc
    !***********************************************************
    !
    ! function result directiveNoParams(string)
    !           String: "NEXT" or "END"
    !           Returns TRUE or FALSE if it passes or fails. 
    !
    !   does the asssociated directive and echoes back what it has done.
    !
    !***********************************************************    
    FUNC bool directiveNoParams(string directive)
        ! TODO: Test this
        IF directive = "NEXT" THEN 
            WriteStrBin iodev1, "\06";
            ! Prevents the pogo-of-death where pogoing with 0-distance points makes the brush run out of paint            
            distanceTravelled := distanceTravelled + 2; 
            newStroke:=TRUE;
            
            RETURN TRUE;
            
        ELSEIF directive = "GRIP" THEN 
            WriteStrBin iodev1, "\06";
            ! Prevents the pogo-of-death where pogoing with 0-distance points makes the brush run out of paint            
            InvertDO do10_16;
            RETURN TRUE;
            
        ELSEIF directive = "GOPATH" THEN
            ! for loop
            FOR i FROM 1 TO 101 DO
                !TPWrite NumToStr(i,1);
                !TPWrite NumToStr(bezierPointsX{i},3);
                !TPWrite NumToStr(bezierPointsX{i},3);
                moveToXYcont bezierPointsX{i},bezierPointsY{i};
            ENDFOR 
            WaitRob \InPos;
            WriteStrBin iodev1, "\06";
            RETURN TRUE;
            
        ELSEIF directive = "MOVETOSAFE" THEN
            MoveL safeMiddle,v500,z50,paintBrush;
            WaitRob \InPos;
            WriteStrBin iodev1, "\06";
            RETURN TRUE;
            
        ELSEIF directive = "MOVEUPS" THEN
            MoveL upsideDownMiddle,v500,z50,paintBrush;
            WaitRob \InPos;
            WriteStrBin iodev1, "\06";
            RETURN TRUE;
            
        ELSEIF directive = "MOVEACLEAN" THEN
            MoveL approachClean,v500,z50,paintBrush;
            WaitRob \InPos;
            WriteStrBin iodev1, "\06";
            RETURN TRUE;
        
        ELSEIF directive = "MOVEOCLEAN" THEN
            MoveL overClean,v500,z50,paintBrush;
            WaitRob \InPos;
            WriteStrBin iodev1, "\06";
            RETURN TRUE;
            
        ELSEIF directive = "MOVECLEAN" THEN
            MoveL clean,v500,z50,paintBrush;
            WaitRob \InPos;
            WriteStrBin iodev1, "\06";
            RETURN TRUE;
            
        ELSEIF directive = "MOVEODRY" THEN
            MoveL overDryer,v500,z50,paintBrush;
            WaitRob \InPos;
            WriteStrBin iodev1, "\06";
            RETURN TRUE;
            
        ELSEIF directive = "MOVEDRY" THEN
            MoveL dryer,v500,z50,paintBrush;
            WaitRob \InPos;
            WriteStrBin iodev1, "\06";
            RETURN TRUE;
        
        ELSEIF directive = "RINSE" THEN
            rinse;
            WaitRob \InPos;
            WriteStrBin iodev1, "\06";
            RETURN TRUE;
        ELSEIF directive = "BRUSHUP" THEN
            paintMode := FALSE;
            moveToXYMarker tX, tY,0;
            WaitRob \InPos;
            WriteStrBin iodev1, "\06";
            RETURN TRUE;
        ELSEIF directive = "BRUSHDOWN" THEN
            paintMode:=TRUE;
            moveToXYMarker tX, tY,0;
            WaitRob \InPos;
            WriteStrBin iodev1, "\06";
            RETURN TRUE;
            
        ELSEIF directive = "END" THEN 
            WaitRob \InPos;
            WriteStrBin iodev1, "\06";
            moveToFinish;
            RETURN FALSE;
        ELSE 
            !throwError "unknown", directive;  
            WriteStrBin iodev1, "\02";
            RETURN TRUE;
        ENDIF 
            
    ENDFUNC
    !***********************************************************
    !
    ! function result directiveWithParams(directive, params)
    !           directive: "COORD" or "SWAP"
    !           params: "X:NUM,Y:NUM" or a character in range A-Z inclusive.
    !           Returns TRUE or FALSE if it passes or fails. 
    !
    !   does the asssociated directive and echoes OK or FAILED
    !
    !***********************************************************  
    FUNC bool directiveWithParams(string directive, string params)
        VAR bool result;
        VAR bool ok; 
        VAR bool testCheck;
        VAR num tempA;
        VAR num tempB;
        IF directive = "COORD" THEN 
            result := processCoordinates(params, FALSE);
            testCheck:=true;
            doubleDip := FALSE;

            
                
            !
            !   The image as given to the robot is in a mirrored state. 
            !   We must fix this. 
            !   If we need to rotate it, the easiest thing is to simply transpose it. 
            !   Otherwise, we need to flip (mirror) the image. 
            !
            IF isRotated THEN ! Transpose the image
                tempA := tX;
                tempB := tY;  
                tX:= tempB;
                tY:= tempA; 
            ELSE ! mirror the image across the X-axis
                tX := sizeX - tX;
            ENDIF 
            testCheck:=checkForBadPoints( tX,tY);
            XTGT:=(SF*tX)+canvasXmin;
            YTGT:=(SF*tY)+canvasYmin;
            moveToXY XTGT,YTGT;
            ! After moving, update our case. This ensures that we are starting new strokes
            ! correctly if NEXT was called before this
            newStroke:=FALSE;
            WriteStrBin iodev1, "\06";
            
            firstTimeRun := FALSE;
            
            IF testCheck = TRUE THEN 
            !WriteStrBin iodev1, "\06";
            
            
            RETURN TRUE;
            ELSE 
                WriteStrBin iodev1, "\15";
                RETURN FALSE;
            ENDIF
            
            
        ELSEIF directive = "COORDM" THEN 
            result := processCoordinates(params, FALSE);
            testCheck:=true;
            doubleDip := FALSE;
            !
            !   The image as given to the robot is in a mirrored state. 
            !   We must fix this. 
            !   If we need to rotate it, the easiest thing is to simply transpose it. 
            !   Otherwise, we need to flip (mirror) the image. 
            !
            IF isRotated THEN ! Transpose the image
                tempA := tX;
                tempB := tY;  
                tX:= tempB;
                tY:= tempA; 
            ELSE ! mirror the image across the X-axis
                tX := sizeX - tX;
            ENDIF 
            testCheck:=checkForBadPoints( tX,tY);
            XTGT:=(SF*tX)+canvasXmin;
            YTGT:=(SF*tY)+canvasYmin;
            
            moveToXYMarker XTGT,YTGT, (2530.0 - tX)/2530.0;
            ! After moving, update our case. This ensures that we are starting new strokes
            ! correctly if NEXT was called before this
            newStroke:=FALSE;
            WriteStrBin iodev1, "\06";
            
            firstTimeRun := FALSE;
            
            IF testCheck = TRUE THEN 
            !WriteStrBin iodev1, "\06";
            
            
            RETURN TRUE;
            ELSE 
                WriteStrBin iodev1, "\15";
                RETURN FALSE;
            ENDIF
        
            
        ELSEIF directive = "COORDP" THEN 
            result := processCoordinates2(params, FALSE);
            testCheck:=true;
            doubleDip := FALSE;

            !
            !   The image as given to the robot is in a mirrored state. 
            !   We must fix this. 
            !   If we need to rotate it, the easiest thing is to simply transpose it. 
            !   Otherwise, we need to flip (mirror) the image. 
            !
            IF isRotated THEN ! Transpose the image
                tempA := tX;
                tempB := tY;  
                tX:= tempB;
                tY:= tempA; 
            ELSE ! mirror the image across the X-axis
                tX := sizeX - tX;
            ENDIF 
            testCheck:=checkForBadPoints( tX,tY);
            XTGT:=(SF*tX)+canvasXmin;
            YTGT:=(SF*tY)+canvasYmin;
            
            
            !Store the coordinate in the array
            !TPWrite NumtoStr(tI,3);
            bezierPointsX{tI+1}:= XTGT;
            bezierPointsY{tI+1}:= YTGT;
            !TPWrite NumtoStr(bezierPointsX{tI+1},3);
            !TPWrite NumtoStr(bezierPointsY{tI+1},3);
            ! After moving, update our case. This ensures that we are starting new strokes
            ! correctly if NEXT was called before this
            newStroke:=FALSE;
            WriteStrBin iodev1, "\06";
            firstTimeRun := FALSE;
            
            IF testCheck = TRUE THEN 
            !WriteStrBin iodev1, "\06";
            
                RETURN TRUE;
            ELSE 
                WriteStrBin iodev1, "\15";
                RETURN FALSE;
            ENDIF
            
        ELSEIF directive = "COORDQ" THEN 
            result := processCoordinates3(params, FALSE);
            testCheck:=true;
            doubleDip := FALSE;
            TPWrite "here";

            !
            !   The image as given to the robot is in a mirrored state. 
            !   We must fix this. 
            !   If we need to rotate it, the easiest thing is to simply transpose it. 
            !   Otherwise, we need to flip (mirror) the image. 
            !
            IF isRotated THEN ! Transpose the image
                tempA := tX1;
                tempB := tY1;  
                tX1:= tempB;
                tY1:= tempA;
                
                tempA := tX2;
                tempB := tY2;  
                tX2:= tempB;
                tY2:= tempA; 
                
                tempA := tX2;
                tempB := tY2;  
                tX2:= tempB;
                tY2:= tempA; 
            ELSE ! mirror the image across the X-axis
                tX1 := sizeX - tX1;
                tX2 := sizeX - tX2;
                tX3 := sizeX - tX3;
                
            ENDIF 
            testCheck:=checkForBadPoints( tX1,tY1);
            testCheck:=checkForBadPoints( tX2,tY2);
            testCheck:=checkForBadPoints( tX3,tY3);
            
            XTGT:=(SF*tX)+canvasXmin;
            YTGT:=(SF*tY)+canvasYmin;
            
            TPWrite NumToStr(tX1, 3);
            TPWrite NumToStr(tY1, 3);
            TPWrite NumToStr(tX2, 3);
            TPWrite NumToStr(tY2, 3);
            TPWrite NumToStr(tX3, 3);
            TPWrite NumToStr(tY3, 3);
            
            
            TPWrite "here2";
            newStroke:=TRUE;
            distanceTravelled:=0;
            FOR t FROM 1 TO 101 do
                tI := t / 100.0;
                tX := (1 - tI) * (1 - tI) * tX1 + 2 * (1 - tI) * tI * tX2 + tI * tI * tX3;
                tY := (1 - tI) * (1 - tI) * tY1 + 2 * (1 - tI) * tI * tY2 + tI * tI * tY3;
                
                XTGT:=(SF*tX)+canvasXmin;
                YTGT:=(SF*tY)+canvasYmin;
                IF t = 1 THEN
                    lastX:=XTGT;
                    lastY:=YTGT;
                ENDIF
                !TPWrite NumToStr(XTGT, 3);
                !TPWrite NumToStr(YTGT, 3);
                bezierPointsX{t}:= XTGT;
                bezierPointsY{t}:= YTGT;
                moveToXYcont XTGT,YTGT;
                lastX:=XTGT;
                lastY:=YTGT;
            ENDFOR 
            TPWrite "here3";
            !Store the coordinate in the array
            !TPWrite NumtoStr(tI,3);
            
            !TPWrite NumtoStr(bezierPointsX{tI+1},3);
            !TPWrite NumtoStr(bezierPointsY{tI+1},3);
            ! After moving, update our case. This ensures that we are starting new strokes
            ! correctly if NEXT was called before this
            newStroke:=FALSE;
            WaitRob \InPos;
            WriteStrBin iodev1, "\06";
            firstTimeRun := FALSE;
            
            IF testCheck = TRUE THEN 
            !WriteStrBin iodev1, "\06";
            
                RETURN TRUE;
            ELSE 
                WriteStrBin iodev1, "\15";
                RETURN FALSE;
            ENDIF
            
        ELSEIF directive = "SWAP" THEN 
                newStroke := TRUE; 
               currentColor := params;
                ! TESTING: Send back OK before command execution if it passes. Should slow down serial lag. 
                WriteStrBin iodev1, "\06";
                ! END TESTING
               GotoPaint(currentColor);
               
               doubleDip := TRUE ;
               !WriteStrBin iodev1, "\06";
               RETURN TRUE; 
               
        ELSEIF directive = "LATHERUP" THEN 
                newStroke := TRUE; 
               
               ok:=StrToVal(params, targetCup); ! should trim the X: from X:230 and parse 230
               TPWrite NumToStr(targetCup,1);
               !targetCup := params;
                ! TESTING: Send back OK before command execution if it passes. Should slow down serial lag. 
                !WriteStrBin iodev1, "\06";
                ! END TESTING
               latherUp(targetCup); !latherUp
               
               doubleDip := TRUE ;
               WaitRob \InPos;
               WriteStrBin iodev1, "\06";
               RETURN TRUE; 
               
        ELSEIF directive = "MIXPAINT" THEN 
                newStroke := TRUE; 
               
               ok:=StrToVal(params, targetCup); ! should trim the X: from X:230 and parse 230
               TPWrite NumToStr(targetCup,1);
               !targetCup := params;
                ! TESTING: Send back OK before command execution if it passes. Should slow down serial lag. 
                !WriteStrBin iodev1, "\06";
                ! END TESTING
               mixPaint(targetCup); !latherUp
               
               doubleDip := TRUE ;
               WaitRob \InPos;
               WriteStrBin iodev1, "\06";
               RETURN TRUE; 
               
        
               
        ELSE 
            !throwError "unknown", directive;  
            WriteStrBin iodev1, "\02";
            RETURN TRUE; 
        ENDIF 
     RETURN FALSE;
    ENDFUNC
    
    !***********************************************************
    !
    ! function result checkForBadPoints(x, y)
    !           x, y: points within the canvas
    !           
    !           Returns TRUE or FALSE if it passes or fails. 
    !
    !   ensures that the point is within the defined canvas. 
    !
    !*********************************************************** 
    func bool checkForBadPoints(num Xcoord, num Ycoord)
        IF (Xcoord>sizeX) OR (Ycoord>sizeY) THEN
            WriteStrBin iodev1, "\15";
            ErrLog 4800, "Coord Error", "One of the coordinates is outside expected bounds","Coordinates larger than image size are not allowed","-","-";
            TPWrite "Bad coordinates in the file: outside expected bounds";
            moveToFinish;
            RETURN FALSE; 
        ENDIF
        IF  (Xcoord<0) OR (Ycoord<0) THEN
            WriteStrBin iodev1, "\15";
            ErrLog 4800, "Coord Error", "Negative Coordinates are not allowed.","-","-","-";
            TPWrite "Bad coordinates in the file: outside expected bounds";
            moveToFinish;
            RETURN FALSE;
        ENDIF
            RETURN TRUE;
    ENDFUNC 
    !***********************************************************
    !
    ! procedure  moveToXY(x, y)
    !           x, y: points within the canvas
    !           
    !
    !   Moves to points and goes to paint if the end of a stroke is reached
    !
    !***********************************************************
    PROC moveToXY(num XCoord,num YCoord)
        VAR string xcode;
        VAR string ycode;
        xcode:= NumToStr(XCoord,5);
        ycode:= NumToStr(YCoord,5);
        TPWrite xcode;
        TPWrite ycode;
        niceStroke;
        ConfL\Off;
        IF distanceTravelled>=PAINT_MAX_DIST OR newStroke=TRUE THEN
            !if we've gone maximum distance or we reach the end of a line.
            IF distanceTravelled > (PAINT_MAX_DIST) THEN 
                GotoPaint(currentColor);
                distanceTravelled:=0;
            ENDIF 
            IF newStroke AND (NOT firstTimeRun) THEN 
                MoveL [[lastX,lastY,canvasHeight+20],paintStrokeQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v500,z10,paintBrush; 
                MoveL [[XCoord,YCoord,canvasHeight+20],paintStrokeQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v500,z10,paintBrush; 
                vectorMag := 0;
            ENDIF 
        ENDIF
        IF (newStroke=FALSE) THEN
            vX:=XCoord-lastX;
            vY:=YCoord-lastY;
            vectorMag:=sqrt(vX*vX+vY*vY);
            distanceTravelled:=distanceTravelled+vectorMag;
        ENDIF
        
        IF brushClean THEN
            IF NOT newStroke THEN 
            MoveL [[lastX,lastY,canvasHeight+20],paintStrokeQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v500,z10,paintBrush;
            MoveL [[lastX,lastY,canvasHeight],paintStrokeQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v500,z10,paintBrush;
            ELSE
                MoveL [[XCoord,YCoord,canvasHeight+20],paintStrokeQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v500,z10,paintBrush;
                
            ENDIF 
            brushClean:= FALSE;
        endif
        
        MoveL [[XCoord,YCoord,canvasHeight],paintStrokeQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v100,z0,paintBrush;
        lastX:=XTGT;
        lastY:=YTGT;
        brushDirty:=TRUE;
        ! This moves to point at 100 mm/sec. 
    ENDPROC
    
    
    !***********************************************************
    !
    ! procedure  moveToXYcont(x, y)
    !           x, y: points within the canvas
    !           
    !
    !   Moves to points and does not return to paint, so that we can make nice curves and paths.
    !   goes to paint if the end of a stroke is reached
    !
    !***********************************************************
    PROC moveToXYcont(num XCoord,num YCoord)
        
        VAR string xcode;
        VAR string ycode;
        niceStroke;
        ConfL\Off;
        
        
        !IF distanceTravelled>=PAINT_MAX_DIST OR newStroke=TRUE THEN
        !    !if we've gone maximum distance or we reach the end of a line.
        !    IF distanceTravelled > (PAINT_MAX_DIST) THEN 
        !        !GotoPaint(currentColor);
        !        distanceTravelled:=0;
        !    ENDIF 
        !    IF newStroke AND (NOT firstTimeRun) THEN 
        !        MoveL [[lastX,lastY,canvasHeight+16],paintStrokeQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v100,z10,paintBrush; 
        !        MoveL [[XCoord,YCoord,canvasHeight+16],paintStrokeQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v100,z10,paintBrush; 
        !        vectorMag := 0;
        !    ENDIF 
        !ENDIF
        IF (newStroke=FALSE) THEN
            vX:=XCoord-lastX;
            vY:=YCoord-lastY;
            vectorMag:=sqrt(vX*vX+vY*vY);
            distanceTravelled:=distanceTravelled+vectorMag;
        ENDIF
        
        IF distanceTravelled>=PAINT_MAX_DIST and paintMode THEN
            TPWrite NumToStr(distanceTravelled, 3);
            MoveL [[XCoord,YCoord,canvasHeight],paintStrokeQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v100,fine,paintBrush;
            MoveL safeMiddle,v500,z50,paintBrush;
            latherUp(0);
            MoveL safeMiddle,v500,z50,paintBrush;
            distanceTravelled:=0;
            MoveL [[lastX,lastY,canvasHeight+30],paintStrokeQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v500,z10,paintBrush;
            MoveL [[lastX,lastY,canvasHeight],paintStrokeQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v500,z10,paintBrush;
        ENDIF
        
        !IF brushClean THEN
        !    IF NOT newStroke THEN 
        !    MoveL [[lastX,lastY,canvasHeight+20],paintStrokeQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v500,z10,paintBrush;
        !    MoveL [[lastX,lastY,canvasHeight],paintStrokeQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v500,z10,paintBrush;
        !    ELSE
        !        MoveL [[XCoord,YCoord,canvasHeight+20],paintStrokeQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v500,z10,paintBrush;
        !        
        !    ENDIF 
        !    brushClean:= FALSE;
        !endif
        IF newStroke THEN
            MoveL [[XCoord,YCoord,canvasHeight+30],paintStrokeQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v500,z10,paintBrush;
            
        ENDIF
        newStroke :=FALSE;
        
        
        xcode:= NumToStr(XCoord,5);
        ycode:= NumToStr(YCoord,5);
        !TPWrite xcode;
        !TPWrite ycode;
        lastX:=XCoord;
        lastY:=YCoord;
        MoveL [[XCoord,YCoord,canvasHeight],paintStrokeQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v100,z10,paintBrush;
        !WaitRob \InPos;
        ERROR
            TPWrite NumToStr(ERRNO,5);

        brushDirty:=TRUE;
        ! This moves to point at 100 mm/sec. 
    ENDPROC
    
    
    PROC moveToXYMarker(num XCoord,num YCoord, num modHeight)
        VAR string xcode;
        VAR string ycode;
        !VAR num modheight := XCoord;
        xcode:= NumToStr(XCoord,5);
        ycode:= NumToStr(YCoord,5);
        tX:=XCoord;
        tY:=YCoord;
        TPWrite xcode;
        TPWrite ycode;
        niceStroke;
        ConfL\Off;

        TPWrite NumToStr(modHeight, 3);
        IF paintMode then
            MoveL [[XCoord,YCoord,canvasHeight - modHeight*1.8],paintStrokeQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v500,z10,paintBrush;
        ELSE
            MoveL [[XCoord,YCoord,canvasHeight+20],paintStrokeQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v500,z10,paintBrush;
        endif
    ENDPROC
    
    !***********************************************************
    !
    ! procedure  moveToFinish()
    !          
    !      Moves to a photogenic finishing spot. 
    !
    !***********************************************************
    PROC moveToFinish()
        ! TODO: To be tested. We want to move to a nice parking spot when we are done. 
            MoveL approachClean, v500,z50,paintBrush;
            cleanCycle 2;
            MoveL approachClean, v500,z50,paintBrush;
        IF NOT firstTimeRun THEN 
        MoveL [[LastX,LastY,canvasHeight+100],ZeroZeroQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v500,z20,paintBrush;
        ENDIF 
        MoveL [[canvasXmin,canvasYmin-20,canvasHeight+100],ZeroZeroQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v500,z50,paintBrush;
        WriteStrBin iodev1, "\15";
        Close iodev1;        
        main;
    ENDPROC 
    !***********************************************************
    !
    ! procedure  GotoPaint(paintString)
    !       paintString: a character from A-Z inclusive
    !          
    !      Moves and gets paint. If the paint color has changed, clean the brush.
    !
    !***********************************************************
    PROC GotoPaint(string colorToPaint)
        ! Whack this color into shape!
        VAR byte part;
        VAR num maxPerRow;

        
        IF (StrLen(colorToPaint) > 1) THEN
            throwError "paint", colorToPaint;
        ENDIF 
        
                        
            ConfL\Off;
            
            IF (newStroke=TRUE) THEN
                lastX:=XTGT;
                lastY:=YTGT;
            ENDIF
            
            IF brushDirty THEN 
                MoveL [[lastX,lastY,canvasHeight+70],ZeroZeroQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]],v500,z20,paintBrush;             
                brushDirty:=FALSE;
            ENDIF 
                
            IF (NOT (colorToPaint=lastColor)) AND (NOT firstTimeRun ) THEN
                !NEED TO CLEAN
                MoveL approachClean, v500,z50,paintBrush;           
                cleanCycle 1;    
                MoveL approachClean, v500,z50,paintBrush;
            ENDIF 
        
           IF NOT (colorToPaint = lastColor) THEN 
                paintCupYOffset :=0;
                maxPerRow:= (maxPaintX - firstPaint{1})/50;
                part := StrToByte(colorToPaint\Char);
     
                IF (part >= 65) AND (part <=90) THEN 
                    TPWrite "Accepted and converted:"+colorToPaint;
                    ! Recall from earlier. 
                        !CONST num firstPaint{2}:=[276, -290];
                        !CONST num paintCupRadius:=50; ! The center-to-center spacings between paint cups (See: dixie cup :D )
                        !CONST num maxPaintX:=626;
                        !CONST num maxPaintY:=-340;
                    cupIndex := part - 65;
                    WHILE ((firstPaint{1} + (paintCupRadius * cupIndex)) > maxPaintX) DO
                        paintCupYOffset:= paintCupYOffset+1;
                        cupIndex := cupIndex - maxPerRow - 1; ! lol @ zero-based indexing in a 1-based indexing world. 
                        TPWrite "Overflow! Rolling back and travelling along Y-axis: index now " + NumToStr(cupIndex, 0);
                    ENDWHILE
                    
                    overPaint := [[firstPaint{1} + (paintCupRadius * cupIndex),(-290 - (paintCupRadius*paintCupYOffset)),paintHeight+70],paintCupQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
                    inPaint:= [[firstPaint{1} + (paintCupRadius * cupIndex),(-290 - (paintCupRadius*paintCupYOffset)),paintHeight],paintCupQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
                
                ELSE 
                    TPWrite "INVALID PAINT. DISQUALIFIED!";
                    throwError "paint", colorToPaint;
                ENDIF  
            ENDIF
            IF (cupIndex < 3) THEN
                !over paint
                MoveL approachPaint,v500,z50,paintBrush;
            ENDIF 
                MoveL overPaint,v500,z50,paintBrush;
                !into paint
                MoveL inPaint,v100,fine,paintBrush;
                !over paint
                MoveL overPaint,v500,z50,paintBrush;
            IF (cupIndex < 3) THEN
                !over paint
                MoveL approachPaint,v500,z50,paintBrush;
            ENDIF 

            lastColor:=colorToPaint;
            brushClean:=TRUE;
    
    
    ENDPROC
    
    !***********************************************************
    !
    ! procedure  GotoPaint(paintString)
    !       paintString: a character from A-Z inclusive
    !          
    !      Moves and gets paint. If the paint color has changed, clean the brush.
    !
    !***********************************************************
    PROC latherUp(num cupIndex)
        ! Whack this color into shape!
        VAR byte part;
        VAR num maxPerRow;
        CONST num radius := 20;
        CONST num innerradius := 8;
        CONST num brushHeight :=70;
        
        VAR robtarget inPaintCenter:= [[palettePaint1{1},palettePaint1{2},brushHeight],paletteQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        VAR robtarget inPaint1:= [[palettePaint1{1} + radius ,palettePaint1{2},brushHeight],paletteQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        VAR robtarget inPaint2:= [[palettePaint1{1},palettePaint1{2} + radius,brushHeight],paletteQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        VAR robtarget inPaint3:= [[palettePaint1{1} - radius, palettePaint1{2},brushHeight],paletteQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        VAR robtarget inPaint4:= [[palettePaint1{1},palettePaint1{2} - radius,brushHeight],paletteQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        VAR robtarget inPaintinner1:= [[palettePaint1{1} + innerradius ,palettePaint1{2},brushHeight],paletteQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        VAR robtarget inPaintinner2:= [[palettePaint1{1},palettePaint1{2} + innerradius,brushHeight],paletteQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        VAR robtarget inPaintinner3:= [[palettePaint1{1} - innerradius, palettePaint1{2},brushHeight],paletteQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        VAR robtarget inPaintinner4:= [[palettePaint1{1},palettePaint1{2} - innerradius,brushHeight],paletteQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        overPaint := [[palettePaint1{1},palettePaint1{2},120],paletteQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        
        ConfL\Off;
        
        IF (newStroke=TRUE) THEN
            lastX:=XTGT;
            lastY:=YTGT;
        ENDIF
        
        
        
        
        MoveL overPaint,v500,z50,paintBrush;
        MoveL inPaintCenter,v20,z50,paintBrush;
        MoveL inPaint1,v20,z50,paintBrush;
        FOR t FROM 1 TO 2 do
            MoveC inPaint2, inPaint3, v200, z50,paintBrush;
            MoveC inPaint4, inPaint1, v200, z50,paintBrush;
            MoveC inPaintinner2, inPaintinner3, v200, z50,paintBrush;
            MoveC inPaintinner4, inPaintinner1, v200, z50,paintBrush;
        ENDFOR
        MoveL overPaint,v20,z50,paintBrush;
               
        
        
    brushClean:=TRUE;
    
    ENDPROC
    
    PROC latherUp2(num cupIndex)
        ! Whack this color into shape!
        VAR byte part;
        VAR num maxPerRow;

        ConfL\Off;
        
        IF (newStroke=TRUE) THEN
            lastX:=XTGT;
            lastY:=YTGT;
        ENDIF
        
        
        overPaint := [[palettePaint1{1},palettePaint1{2},120],paletteQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        inPaint:= [[palettePaint1{1},palettePaint1{2},61],paletteQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];

        MoveL overPaint,v500,z50,paintBrush;
        MoveL inPaint,v20,z50,paintBrush;
        MoveL overPaint,v20,z50,paintBrush;
               
        
        
    brushClean:=TRUE;
    
    ENDPROC
    
    
    !***********************************************************
    !
    ! procedure  mixPaint(paintString)
    !       paintString: a character from A-Z inclusive
    !          
    !      Moves to the paint cup and spins the brush in order to mix two paint colors, takes ~ 15s
    !
    !***********************************************************
    PROC mixPaint(num cupIndex)
        ! Whack this color into shape!
        VAR byte part;
        VAR num maxPerRow;
        CONST num radius := 20;
        CONST num innerradius := 8;
        CONST num brushHeight :=66;
        
        VAR robtarget inPaintCenter:= [[palettePaint1{1},palettePaint1{2},brushHeight],paletteQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        VAR robtarget inPaint1:= [[palettePaint1{1} + radius ,palettePaint1{2},brushHeight],paletteQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        VAR robtarget inPaint2:= [[palettePaint1{1},palettePaint1{2} + radius,brushHeight],paletteQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        VAR robtarget inPaint3:= [[palettePaint1{1} - radius, palettePaint1{2},brushHeight],paletteQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        VAR robtarget inPaint4:= [[palettePaint1{1},palettePaint1{2} - radius,brushHeight],paletteQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        VAR robtarget inPaintinner1:= [[palettePaint1{1} + innerradius ,palettePaint1{2},brushHeight],paletteQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        VAR robtarget inPaintinner2:= [[palettePaint1{1},palettePaint1{2} + innerradius,brushHeight],paletteQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        VAR robtarget inPaintinner3:= [[palettePaint1{1} - innerradius, palettePaint1{2},brushHeight],paletteQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        VAR robtarget inPaintinner4:= [[palettePaint1{1},palettePaint1{2} - innerradius,brushHeight],paletteQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        overPaint := [[palettePaint1{1},palettePaint1{2},120],paletteQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        
        ConfL\Off;
        
        IF (newStroke=TRUE) THEN
            lastX:=XTGT;
            lastY:=YTGT;
        ENDIF
        
        
        
        
        MoveL overPaint,v500,z50,paintBrush;
        MoveL inPaintCenter,v20,z50,paintBrush;
        MoveL inPaint1,v20,z50,paintBrush;
        FOR t FROM 1 TO 15 do
            MoveC inPaint2, inPaint3, v200, z50,paintBrush;
            MoveC inPaint4, inPaint1, v200, z50,paintBrush;
            MoveC inPaintinner2, inPaintinner3, v200, z50,paintBrush;
            MoveC inPaintinner4, inPaintinner1, v200, z50,paintBrush;
        ENDFOR
        MoveL overPaint,v20,z50,paintBrush;
               
        
        
    brushClean:=TRUE;
    
    ENDPROC
    
    
        !***********************************************************
    !
    ! procedure  mixPaint(paintString)
    !       paintString: a character from A-Z inclusive
    !          
    !      Moves to the paint cup and spins the brush in order to mix two paint colors, takes ~ 15s
    !
    !***********************************************************
    PROC rinse()
        ! Whack this color into shape!
        VAR byte part;
        VAR num maxPerRow;
        CONST num radius := 35;!35
        CONST num innerradius := 20;!20
        CONST num brushHeight :=92;
        !CONST robtarget 
        const num rinsex := 455;!clean.trans.x;
        CONST num rinsey := -472;!clean.trans.y;
        
        
        VAR robtarget inPaintCenter:= [[rinsex,rinsey,brushHeight],paintcleanerQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        VAR robtarget inPaint1:= [[rinsex + radius ,rinsey,brushHeight],paintcleanerQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        VAR robtarget inPaint2:= [[rinsex,rinsey + radius,brushHeight],paintcleanerQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        VAR robtarget inPaint3:= [[rinsex - radius, rinsey,brushHeight],paintcleanerQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        VAR robtarget inPaint4:= [[rinsex,rinsey - radius,brushHeight],paintcleanerQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        VAR robtarget inPaintinner1:= [[rinsex + innerradius ,rinsey,brushHeight],paintcleanerQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        VAR robtarget inPaintinner2:= [[rinsex,rinsey + innerradius,brushHeight],paintcleanerQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        VAR robtarget inPaintinner3:= [[rinsex - innerradius, rinsey,brushHeight],paintcleanerQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        VAR robtarget inPaintinner4:= [[rinsex,rinsey - innerradius,brushHeight],paintcleanerQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        var robtarget overClean := [[rinsex,rinsey,100],paintcleanerQuat,[0,0,0,0],[9E9,9E9,9E9,9E9,9E9,9E9]];
        
        ConfL\Off;
        
        IF (newStroke=TRUE) THEN
            lastX:=XTGT;
            lastY:=YTGT;
        ENDIF
        
        
        
        
        MoveL overClean,v500,z50,paintBrush;
        MoveL inPaintCenter,v20,z50,paintBrush;
        MoveL inPaint1,v20,z50,paintBrush;
        FOR t FROM 1 TO 10 do
            MoveC inPaint2, inPaint3, v300, z50,paintBrush;
            MoveC inPaint4, inPaint1, v300, z50,paintBrush;
            MoveC inPaintinner2, inPaintinner3, v300, z50,paintBrush;
            MoveC inPaintinner4, inPaintinner1, v300, z50,paintBrush;
        ENDFOR
        MoveL overClean,v20,z50,paintBrush;
               
        
        
    brushClean:=TRUE;
    
    ENDPROC
    
    PROC cleanCycle(num count)
        VAR num counter:=1;
        WHILE counter <= count DO
            MoveL overClean,v500,z50,paintBrush;
            MoveL clean,v100,fine,paintBrush;
            WaitTime 0.5;
            MoveL overClean, v100, z0, paintBrush;
            MoveL overDryer,v500,z20,paintBrush;
            MoveL dryer,v100,fine,paintBrush;
            WaitTime 1;
            MoveL overDryer,v500,z50,paintBrush;
            counter := counter + 1;
        ENDWHILE
            MoveL overClean,v500,z50,paintBrush;
            MoveL clean,v100,fine,paintBrush;
            WaitTime 1;
            MoveL overClean, v100, z0, paintBrush;
            MoveL overDryer,v500,z20,paintBrush;
            MoveL dryerL,v100,fine,paintBrush;
            WaitTime 0.5;
            MoveL dryer,v5,fine,paintBrush;
            WaitTime 0.5;
            MoveL dryerH,v10,fine,paintBrush; 
            WaitTime 0.5;
            MoveL overDryer,v500,z50,paintBrush;
    ENDPROC 
    !***********************************************************
    !
    ! procedure  niceStroke()
    !       
    !          Access for modifying the quaternion in the future. 
    !
    !***********************************************************
    PROC niceStroke()
        ! Actual nice strokes nuked for now. FUTURE: Create actual nice quaternion strokes. 
        paintStrokeQuat:=ZeroZeroQuat;
    ENDPROC
    !***********************************************************
    !
    ! procedure  throwError(int errorType)
    !       
    !          Handles all error throwing and program halting. 
    !
    !***********************************************************    
    proc throwError(string errorType, string response)
        WriteStrBin iodev1, "\15";
        Close iodev1;
        IF errorType = "semicolon" THEN
            ErrLog 4800, "Command Error", "Missing ';' terminator in message",response,"-","-";
            TPWrite "Command Error: Missing semicolon to terminate command";
        ELSEIF errorType = "coord" THEN 
            ErrLog 4800, "Coord Error", "Mangled coordinates in pair",response,"-","-";
            TPWrite "Bad coordinates in the file: mangled in pair";
        ELSEIF errorType = "multicoord" THEN 
            ErrLog 4800, "Coord Error", "Multiple X or Y coords recieved","Expected to see coordinates of the format X:###,Y:###","Saw two declarations of X or Y",response;
            TPWrite "Bad coordinates in the file: duplicated";
        ELSEIF errorType = "missingcoord" THEN 
            ErrLog 4800, "Coord Error", "Missing X or Y coords","Expected to see coordinates of the format X:###,Y:### ","Saw missing declaration of X or Y in pair",response;
            TPWrite "Bad coordinates in the file: missing pair dec";
        ELSEIF errorType = "canvas" THEN 
            ErrLog 4800, "Data Error", "The canvas size data was malformed",response,"","You should have seen other errors before this.";
            TPWrite "Bad canvas size data";
        ELSEIF errorType = "paint" THEN 
           ErrLog 4800, "Paint Error", "Out of bounds paint",response,"-","-";
            TPWrite "Command Error: Paint Command is bad";
         
        ELSEIF errorType = "unknown" THEN 
           ErrLog 4800, "Command Error", "Unknown Command",response,"-","-";
            TPWrite "Command Error: Unknown Command";
        ELSE 
            ErrLog 4800, "Generic Error", "Generic Message",response,"-","-";
            TPWrite "Generic Error: Unknown";
        ENDIF 
        moveToFinish;
    ENDPROC
    
    
ENDMODULE