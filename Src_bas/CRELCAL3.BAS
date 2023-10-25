DECLARE SUB Editor (Text$, Row%, LeftCol%, RightCol%, Ky$, ValidInput$, Colr%)
DECLARE SUB GetCountRegime (Spread#, Counts%, StartInterval$)
DECLARE SUB GetDates (StartYear%, StartMonth%, StartDay%, StopYear%, StopMonth%, StopDay%)
DECLARE SUB GetStartPoints (StartPointTotal%)
DECLARE SUB GetStrata (WEperStrata%, WDperStrata%)
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
'
'   Module CRELCAL3 - Part of CREELCAL program
'
'   Written by:  Dirk D. Miller
'                Wyoming Game and Fish Department
'                528 S. Adams Street
'                Laramie, WY  82070
'                (307) 745-4046
'
'
'   This program is designed to set up creel surveys based on a stratified 2
'   stage sample design with systematic sampling at the second stage.  The
'   primary sampling unit is the fishing day which is stratified into weekdays,
'   weekend days and holidays.  The second stage is the angler counts within a
'   day, these are made at equally spaced intervals with a randomly selected
'   start time for the first count.
'
'   A non-uniform sampling probability component has been added.
'
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'$DYNAMIC

'$INCLUDE: 'creelcal.bi'

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
' This routine was borrowed from the files sent with the book
' "BASIC Techniques and Utilities" by Ethan Winer.  I modified it to fit my
' needs.
'
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
SUB Editor (Text$, Row%, LeftCol%, RightCol%, Ky$, ValidInput$, Colr%)

   '----- Set the cursor.
   IF InsStatus% THEN
      LOCATE Row%, , , Cnf.CurSize \ 2, Cnf.CurSize
   ELSE
      LOCATE Row%, , , Cnf.CurSize - 1, Cnf.CurSize
   END IF

   '----- Work with a temporary copy.
   Edit$ = SPACE$(RightCol% - LeftCol% + 1)
   LSET Edit$ = Text$

   '----- See where to begin editing and print the string.
   'see if this helps editing the numbers entered
   '  This is the code originally here
   '   TxtPos% = POS(0) - LeftCol% + 1
   '   IF TxtPos% < 1 THEN TxtPos% = 1
   '   IF TxtPos% > LEN(Edit$) THEN TxtPos% = LEN(Edit$)
   'I added this block of code
   IF LEN(RTRIM$(Edit$)) = 0 THEN
      TxtPos% = 1
   ELSEIF LEN(Edit$) AND LEFT$(Edit$, 1) = " " THEN
      TxtPos% = 2
   ELSE
      TxtPos% = 1
   END IF
  
   LOCATE , LeftCol%
   CALL MQPrint(Edit$, Colr%)

   '----- This is the main loop for handling key presses.
   DO
      LOCATE , LeftCol% + TxtPos% - 1, 1

      IF Cnf.Mouse THEN 'MOUSE IS PRESENT
         'GET A KEYBOARD ENTRY BUT CHECK FOR MOUSE ACTIVITY
         'AND EXIT IF IT HAS BEEN PRESSED
         DO
            CALL DisplayTime
            Ky$ = INKEY$
            GetCursor MCol%, MRow%, Button%
         LOOP UNTIL LEN(Ky$) OR (Button% AND 1)       'wait for a keypress
         IF (Button% AND 1) THEN EXIT DO
      ELSE
         DO
            CALL DisplayTime
            Ky$ = INKEY$
         LOOP UNTIL LEN(Ky$)        'wait for a keypress
      END IF
 
      KeyCode% = KeyPressed%(Ky$)

      '----- Branch according to the key pressed.
      SELECT CASE KeyCode%

         '----- Backspace: decrement the pointer and the
         '      cursor, and ignore if in the first column.
         CASE BKSP
            TxtPos% = TxtPos% - 1
            LOCATE , LeftCol% + TxtPos% - 1, 0
            IF TxtPos% > 0 THEN
               IF InsStatus% THEN
                  MID$(Edit$, TxtPos%) = MID$(Edit$, TxtPos% + 1) + " "
               ELSE
                  MID$(Edit$, TxtPos%) = " "
               END IF
'               PRINT MID$(Edit$, TxtPos%);
               CALL MQPrint(MID$(Edit$, TxtPos%), Colr%)
            END IF

         '----- Enter or Escape: this block is optional in
         '      case you want to handle these separately.
         'CASE Enter, Escape
         '  EXIT DO                'exit the subprogram

         '----- Letter keys: turn off the cursor to hide
         '      the printing, handle Insert mode as needed.
         CASE 32 TO 125
            IF INSTR(ValidInput$, Ky$) THEN
               'this allows flexibility in the type of input which is accepted
               'without having to have separate routines for integers, text, etc.
               LOCATE , , 0
               IF InsStatus% THEN      'expand the string
                  MID$(Edit$, TxtPos%) = Ky$ + MID$(Edit$, TxtPos%)
                  CALL MQPrint(MID$(Edit$, TxtPos%), Colr%)
'                  PRINT MID$(Edit$, TxtPos%);
               ELSE                   'else insert character
                  MID$(Edit$, TxtPos%) = Ky$
                  CALL MQPrint(Ky$, Colr%)
'                  PRINT Ky$;
               END IF
               TxtPos% = TxtPos% + 1    'update position counter
            ELSE
               EXIT DO
            END IF

         '----- Left arrow: decrement the position counter.
         CASE LeftArrow
            TxtPos% = TxtPos% - 1

         '----- Right arrow: increment position counter.
         CASE RightArrow
            TxtPos% = TxtPos% + 1

         '----- Home: jump to the first character position.
         CASE Home
            TxtPos% = 1

         '----- End: search for the last non-blank, and
         '      make that the current editing position.
         CASE Ennd
            FOR N% = LEN(Edit$) TO 1 STEP -1
               IF MID$(Edit$, N%, 1) <> " " THEN EXIT FOR
            NEXT
            TxtPos% = N% + 1
            IF TxtPos% > LEN(Edit$) THEN TxtPos% = LEN(Edit$)

         '----- Insert key: toggle the Insert state and
         '      adjust the cursor size.
         CASE Insirt
            InsStatus% = NOT InsStatus%
            IF InsStatus% THEN
               LOCATE , , , Cnf.CurSize \ 2, Cnf.CurSize
            ELSE
               LOCATE , , , Cnf.CurSize - 1, Cnf.CurSize
            END IF

         '----- Delete: delete the current character and
         '      reprint what remains in the string.
         CASE Deleat
            MID$(Edit$, TxtPos%) = MID$(Edit$, TxtPos% + 1) + " "
            LOCATE , , 0
'            PRINT MID$(Edit$, TxtPos%);
            CALL MQPrint(MID$(Edit$, TxtPos%), Colr%)

         '---- All other keys: exit the subprogram
         CASE ELSE
            EXIT DO
      END SELECT

   '----- Loop until the cursor moves out of the field.
   LOOP UNTIL TxtPos% < 1 OR TxtPos% > LEN(Edit$)

   Text$ = RTRIM$(Edit$)         'trim the text
   LOCATE , , 0                  'turn the cursor off

END SUB

SUB GetCountRegime (Spread#, Counts%, StartInterval$)
   Row%(1) = 11     ' Counts
   Row%(2) = 13     ' Spread
   Pointer% = 1
   LastPointer% = 1
   RePrint% = True
   RePrintIntrvl% = True
   CountChanged% = False
   HideCursor
   CALL WindowMgr(0, 1, 1, 1, 25, 80, Cnf.Normal)     'open next available window
   ShowCursor
   StartDate$ = RTRIM$(MonthData(StartMonth%).Mname) + STR$(StartDay%) + "," + STR$(StartYear%)
   StopDate$ = RTRIM$(MonthData(StopMonth%).Mname) + STR$(StopDay%) + "," + STR$(StopYear%)

   CALL QPrintRC("ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป", 1, 1, Cnf.Normal)
   CALL QPrintRC("บ                                                                              บ", 2, 1, Cnf.Normal)
   CALL QPrintRC("บ                                                                              บ", 3, 1, Cnf.Normal)
   CALL QPrintRC("บ  You need to set the count regime up according to the desired survey design. บ", 4, 1, Cnf.Normal)
   CALL QPrintRC("บ  Maximum and minimum values are given.  Be sure that the values you select   บ", 5, 1, Cnf.Normal)
   CALL QPrintRC("บ  make sense for the survey design you have selected.                         บ", 6, 1, Cnf.Normal)
   CALL QPrintRC("ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน", 7, 1, Cnf.Normal)
   CALL QPrintRC("บ   Start Date:                              End Date:                         บ", 8, 1, Cnf.Normal)
   CALL QPrintRC(StartDate$, 8, 17, Cnf.Normal)
   CALL QPrintRC(StopDate$, 8, 56, Cnf.Normal)
   CALL QPrintRC("ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน", 9, 1, Cnf.Normal)
   CALL QPrintRC("บ                                                                              บ", 10, 1, Cnf.Normal)
   IF Counts% = 0 THEN
      CALL QPrintRC("บ      Fishermen counts per day (This just selects count days, no start times) บ", 11, 1, Cnf.Normal)
   ELSE
      CALL QPrintRC("บ      Fishermen counts per day (must be between 0 and " + FUsing$(STR$(MaxCounts), "##") + ", 0 selects days only)บ", 11, 1, Cnf.Normal)
   END IF
   CALL QPrintRC("บ                                                                              บ", 12, 1, Cnf.Normal)
   CALL QPrintRC("บ      Hours.Minutes between counts  (default " + FUsing$(STR$(DefaultSpread#), "##.##") + ", must be between 0 and 12) บ", 13, 1, Cnf.Normal)
   CALL QPrintRC("บ       1.30 is 1 hour 30 minutes, 0.55 is 55 minutes, 0 program sets interval บ", 14, 1, Cnf.Normal)
   CALL QPrintRC("ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน", 15, 1, Cnf.Normal)
   CALL QPrintRC("บ       INTERVAL SELECTION - Highlight the desired choice and press F4         บ", 16, 1, Cnf.Normal)
   CALL QPrintRC("บ               5 minutes is the recommended interval to use                   บ", 17, 1, Cnf.Normal)
   CALL QPrintRC("ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน", 22, 1, Cnf.Normal)
   CALL QPrintRC("บ                    F1 for Help, F4 selects an interval,                      บ", 23, 1, Cnf.Normal)
   CALL QPrintRC("บ                   Arrow keys move cursor, F10 to proceed.                    บ", 24, 1, Cnf.Normal)
   CALL QPrintRC("ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ", 25, 1, Cnf.Normal)
                 '         1         2         3         4         5         6         7         8
                 '12345678901234567890123456789012345678901234567890123456789012345678901234567890
   CALL QPrintRC(Buffer$ + MainTitle$, 2, 2, Cnf.HiLite)
   CALL QPrintRC("F1", 23, 22, Cnf.HiLite)
   CALL QPrintRC("F4", 23, 35, Cnf.HiLite)
   CALL QPrintRC("F10", 24, 45, Cnf.HiLite)
   CALL QPrintRC(" " + FUsing$(STR$(Counts%), "##") + " ", 11, 2, Cnf.MenBox)
   CALL QPrintRC(FUsing$(STR$(Spread#), "##.##"), 13, 2, Cnf.MenBox)
   CALL QPrintRC(" 5 minute ", 18, 7, Cnf.MenBox)
   CALL QPrintRC(" 15 minute ", 19, 7, Cnf.MenBox)
   CALL QPrintRC(" 30 minute ", 20, 7, Cnf.MenBox)
   CALL QPrintRC(" 1 hour ", 21, 7, Cnf.MenBox)
   DO
      IF Counts% > 0 THEN
         DefaultSpread# = FIX(12 / Counts%)
      ELSEIF DefaultSpread# = 0 THEN
         DefaultSpread# = 2
      END IF
      IF Spread# < 0 OR Spread# > 12 THEN Spread# = DefaultSpread#
      IF RePrintIntrvl% THEN
         CALL QPrintRC("บ      5 minute intervals between start times is not selected                  บ", 18, 1, Cnf.Normal)
         CALL QPrintRC("บ      15 minute intervals between start times is not selected                 บ", 19, 1, Cnf.Normal)
         CALL QPrintRC("บ      30 minute intervals between start times is not selected                 บ", 20, 1, Cnf.Normal)
         CALL QPrintRC("บ      1 hour intervals between start times is not selected                    บ", 21, 1, Cnf.Normal)
         IF StartInterval$ = "5 minutes" THEN
            CALL QPrintRC("บ      5 minute intervals between start times is currently selected            บ", 18, 1, Cnf.HiLite)
         ELSEIF StartInterval$ = "15 minutes" THEN
            CALL QPrintRC("บ      15 minute intervals between start times is currently selected           บ", 19, 1, Cnf.HiLite)
         ELSEIF StartInterval$ = "30 minutes" THEN
            CALL QPrintRC("บ      30 minute intervals between start times is currently selected           บ", 20, 1, Cnf.HiLite)
         ELSEIF StartInterval$ = "hour" THEN
            CALL QPrintRC("บ      1 hour intervals between start times is currently selected              บ", 21, 1, Cnf.HiLite)
         END IF
         CALL QPrintRC(" 5 minute ", 18, 7, Cnf.MenBox)
         CALL QPrintRC(" 15 minute ", 19, 7, Cnf.MenBox)
         CALL QPrintRC(" 30 minute ", 20, 7, Cnf.MenBox)
         CALL QPrintRC(" 1 hour ", 21, 7, Cnf.MenBox)
         RePrintIntrvl% = False
      END IF
      'RESTORE LAST HILIGHTED SELECTION TO NORMAL DISPLAY
      SELECT CASE LastPointer%
         CASE 1
            CALL QPrintRC(" " + FUsing$(STR$(Counts%), "##") + " ", 11, 2, Cnf.MenBox)
         CASE 2
            CALL QPrintRC(FUsing$(STR$(Spread#), "##.##"), 13, 2, Cnf.MenBox)
         CASE 3
            CALL QPrintRC(" 5 minute ", 18, 7, Cnf.MenBox)
         CASE 4
            CALL QPrintRC(" 15 minute ", 19, 7, Cnf.MenBox)
         CASE 5
            CALL QPrintRC(" 30 minute ", 20, 7, Cnf.MenBox)
         CASE 6
            CALL QPrintRC(" 1 hour ", 21, 7, Cnf.MenBox)
      END SELECT
      'HILIGHT THE CURRENT CURSOR LOCATION
      SELECT CASE Pointer%
         CASE 1
            CALL QPrintRC(" " + FUsing$(STR$(Counts%), " ## ") + " ", 11, 2, Cnf.Revers)
         CASE 2
            CALL QPrintRC(FUsing$(STR$(Spread#), "##.##"), 13, 2, Cnf.Revers)
         CASE 3
            CALL QPrintRC(" 5 minute ", 18, 7, Cnf.Revers)
         CASE 4
            CALL QPrintRC(" 15 minute ", 19, 7, Cnf.Revers)
         CASE 5
            CALL QPrintRC(" 30 minute ", 20, 7, Cnf.Revers)
         CASE 6
            CALL QPrintRC(" 1 hour ", 21, 7, Cnf.Revers)
      END SELECT
      IF Pointer% < 3 THEN
         IF Pointer% = 1 THEN
            EnteredString$ = LTRIM$(STR$(Counts%))
         ELSEIF Pointer% = 2 THEN
            EnteredString$ = LTRIM$(STR$(Spread#))
         END IF
         LeftCol% = 2        'set the left column
         RightCol% = 6       'and the right column
         ValidInput$ = Numbers$
         CALL Editor(EnteredString$, Row%(Pointer%), LeftCol%, RightCol%, Ch$, ValidInput$, Cnf.Revers)
         SELECT CASE Pointer%
            CASE 1  'COUNTS PER DAY
               NewNum% = VAL(EnteredString$)
               IF NewNum% < 0 OR NewNum% > MaxCounts THEN
                  CALL OOPS
               ELSE
                  Counts% = NewNum%
                  'RePrint% = True
               END IF
            CASE 2  'HOURS BETWEEN COUNTS
               NewNum# = VAL(EnteredString$)
               IF NewNum# < 0 OR NewNum# > 12 THEN
                  CALL OOPS
               ELSEIF (NewNum# - FIX(NewNum#)) * 100 > 59 THEN
                  CALL OOPS
               ELSE
                  Spread# = NewNum#
               END IF
         END SELECT
      ELSE
         'WAIT FOR INPUT FROM KEYBOARD
         Ch$ = GetAKey$
      END IF
      KeyCode% = KeyPressed%(Ch$)

      'MMMMMMMMMMMMMMMM Start of Mouse Handling Code MMMMMMMMMMMMMMMMMMM
      IF Cnf.Mouse THEN
         CALL ButtonPress(1, Down%, Presses%, MCol%, MRow%)
         Button1% = False
         IF Presses% THEN Button1% = True

         CALL GetCursor(MCol%, MRow%, Button%)
         MRow% = (MRow% \ Cnf.NPixLines) + 1
         MCol% = (MCol% \ 8) + 1

         IF MRow% <> LMRow% OR MCol% <> LMCol% OR Presses% THEN
            'CURSOR WAS PRESSED OR MOUSE WAS MOVED SINCE LAST CHECK
            'DETERMINE IF CURSOR IS WITHIN ACCEPTABLE DATA ENTRY BOX
            'OR IF ONE OF THE SPECIAL AREAS IS HIGHLIGHTED
            IF MRow% = 23 AND Button1% THEN
               'MOUSE SELECTED FUNCTION KEY ACTION
               SELECT CASE MCol%
                  CASE 22, 23
                     KeyCode% = F1
                  CASE 35, 36
                     KeyCode% = F4
                  CASE ELSE
                     CALL OOPS
               END SELECT
            ELSEIF MRow% = 24 AND Button1% THEN
               'MOUSE SELECTED FUNCTION KEY ACTION
               SELECT CASE MCol%
                  CASE 45, 46, 47
                     KeyCode% = F10
                  CASE ELSE
                     CALL OOPS
               END SELECT
            ELSEIF MRow% = 11 AND Button1% AND MCol% >= 2 AND MCol% <= 6 THEN
               LastPointer% = Pointer%
               Pointer% = 1
               KeyCode% = 0
            ELSEIF MRow% = 13 AND Button1% AND MCol% >= 2 AND MCol% <= 6 THEN
               LastPointer% = Pointer%
               Pointer% = 2
               KeyCode% = 0
            ELSEIF MRow% = 18 AND Button1% AND MCol% >= 7 AND MCol% <= 16 THEN
               LastPointer% = Pointer%
               Pointer% = 3
               KeyCode% = 0
            ELSEIF MRow% = 19 AND Button1% AND MCol% >= 7 AND MCol% <= 17 THEN
               LastPointer% = Pointer%
               Pointer% = 4
               KeyCode% = 0
            ELSEIF MRow% = 20 AND Button1% AND MCol% >= 7 AND MCol% <= 17 THEN
               LastPointer% = Pointer%
               Pointer% = 5
               KeyCode% = 0
            ELSEIF MRow% = 21 AND Button1% AND MCol% >= 7 AND MCol% <= 14 THEN
               LastPointer% = Pointer%
               Pointer% = 6
               KeyCode% = 0
            END IF
         ELSEIF Button2% THEN        'USER PRESSED ESCAPE BUTTON ??
            Ky$ = CHR$(27)
            KeyCode% = Escape
            DO
               CALL GetCursor(MCol%, MRow%, Button%)
            LOOP WHILE Button% AND 2
         END IF
    
         LMRow% = MRow%
         LMCol% = MCol%
      END IF  'end of if a mouse is present
      'MMMMMMMMMMMMMMMMMMM End of mouse handling code MMMMMMMMMMMMMMMMMMMMMM

      IF KeyCode% < 0 THEN
         'IF KEYBOARD INPUT IS FUNCTION OR NUMERIC/ARROW KEY
         SELECT CASE KeyCode%
            CASE UpArrow, LeftArrow
               LastPointer% = Pointer%
               IF Pointer% = 1 THEN
                  Pointer% = 6
               ELSE
                  Pointer% = Pointer% - 1
               END IF
            CASE RightArrow, DownArrow
               LastPointer% = Pointer%
               IF Pointer% = 6 THEN
                  Pointer% = 1
               ELSE
                  Pointer% = Pointer% + 1
               END IF
            CASE F4        'F4   SELECT A STRATA
               SELECT CASE Pointer%
                  CASE 3
                     StartInterval$ = "5 minutes"
                  CASE 4
                     StartInterval$ = "15 minutes"
                  CASE 5
                     StartInterval$ = "30 minutes"
                  CASE 6
                     StartInterval$ = "hour"
                  CASE ELSE
                     CALL OOPS
               END SELECT
               RePrintIntrvl% = True
               CountChanged% = True
            CASE F1        'HELP
               CALL HelpMe(0, "COUNTS", 2, 2, 24, 79)
            CASE F10        'F10  quit
               CALL ClearBuf
               EXIT DO
         END SELECT
      END IF
   LOOP

   HideCursor
   CALL WindowMgr(0, 0, 0, 0, 0, 0, 0)
   ShowCursor

END SUB 'GetCountRegime

SUB GetDates (StartYear%, StartMonth%, StartDay%, StopYear%, StopMonth%, StopDay%)
   Col%(1) = 42: Row%(1) = 13     ' Start Month
   Col%(2) = 42: Row%(2) = 14     ' End Month
   Col%(3) = 49: Row%(3) = 13     ' Start Day
   Col%(4) = 49: Row%(4) = 14     ' End Day
   Col%(5) = 56: Row%(5) = 13     ' Start Year
   Col%(6) = 56: Row%(6) = 14     ' Stop Year
   Pointer% = 1
   LastPointer% = 1
   DateChanged% = False
   HideCursor
   CALL WindowMgr(0, 1, 1, 1, 25, 80, Cnf.Normal)     'open next available window
   ShowCursor
   CALL QPrintRC("ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป", 1, 1, Cnf.Normal)
   CALL QPrintRC("บ                                                                              บ", 2, 1, Cnf.Normal)
   CALL QPrintRC("บ                                                                              บ", 3, 1, Cnf.Normal)
   CALL QPrintRC("บ   Set the starting and ending dates for the creel survey.  Move the          บ", 4, 1, Cnf.Normal)
   CALL QPrintRC("บ   highlighted bar with Tab or the arrow keys and change the number for       บ", 5, 1, Cnf.Normal)
   CALL QPrintRC("บ   the year, month or date (It works best to set the year first).             บ", 6, 1, Cnf.Normal)
   CALL QPrintRC("บ       F1 for Help, F10 to proceed with starting and ending dates shown.      บ", 7, 1, Cnf.Normal)
   CALL QPrintRC("F1", 7, 9, Cnf.HiLite)
   CALL QPrintRC("F10", 7, 22, Cnf.HiLite)
   CALL QPrintRC("ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน", 8, 1, Cnf.Normal)
   CALL QPrintRC("บ                                                                              บ", 9, 1, Cnf.Normal)
   CALL QPrintRC("วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤยฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ", 10, 1, Cnf.Normal)
                 '         1         2         3         4         5         6         7         8
                 '12345678901234567890123456789012345678901234567890123456789012345678901234567890
   CALL QPrintRC("บ                      ณ         Month         ณ Date ณ Year ณ                 บ", 11, 1, Cnf.Normal)
   CALL QPrintRC("บ                      รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤลฤฤฤฤฤฤลฤฤฤฤฤฤด                 บ", 12, 1, Cnf.Normal)
   CALL QPrintRC("บ                Start ณ                ณ      ณ      ณ      ณ                 บ", 13, 1, Cnf.Normal)
   CALL QPrintRC("บ                End   ณ                ณ      ณ      ณ      ณ                 บ", 14, 1, Cnf.Normal)
   CALL QPrintRC("บ                      ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤมฤฤฤฤฤฤมฤฤฤฤฤฤู                 บ", 15, 1, Cnf.Normal)
   CALL QPrintRC("บ                                                                              บ", 16, 1, Cnf.Normal)
   CALL QPrintRC("บ                                                                              บ", 17, 1, Cnf.Normal)
   CALL QPrintRC("บ                                                                              บ", 18, 1, Cnf.Normal)
   CALL QPrintRC("บ                                                                              บ", 19, 1, Cnf.Normal)
   CALL QPrintRC("บ                                                                              บ", 20, 1, Cnf.Normal)
   CALL QPrintRC("บ                                                                              บ", 21, 1, Cnf.Normal)
   CALL QPrintRC("บ                                                                              บ", 22, 1, Cnf.Normal)
   CALL QPrintRC("บ                                                                              บ", 23, 1, Cnf.Normal)
   CALL QPrintRC("บ                                                                              บ", 24, 1, Cnf.Normal)
   CALL QPrintRC("ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ", 25, 1, Cnf.Normal)
   CALL QPrintRC(Buffer$ + MainTitle$, 2, 2, Cnf.HiLite)
   CALL PrintCalendar(StartYear%, StartMonth%, 16, 3)
   CALL PrintCalendar(StopYear%, StopMonth%, 16, 51)

   CALL QPrintRC("  " + FUsing$(STR$(StartMonth%), "##") + "  ", Row%(1), Col%(1), Cnf.MenBox)
   CALL QPrintRC("  " + FUsing$(STR$(StopMonth%), "##") + "  ", Row%(2), Col%(2), Cnf.MenBox)
   CALL QPrintRC("  " + FUsing$(STR$(StartDay%), "##") + "  ", Row%(3), Col%(3), Cnf.MenBox)
   CALL QPrintRC("  " + FUsing$(STR$(StopDay%), "##") + "  ", Row%(4), Col%(4), Cnf.MenBox)
   CALL QPrintRC(" " + FUsing$(STR$(StartYear%), "####") + " ", Row%(5), Col%(5), Cnf.MenBox)
   CALL QPrintRC(" " + FUsing$(STR$(StopYear%), "####") + " ", Row%(6), Col%(6), Cnf.MenBox)
   CALL QPrintRC(MonthData(StartMonth%).Mname, Row%(1), 30, Cnf.Normal)
   CALL QPrintRC(MonthData(StopMonth%).Mname, Row%(2), 30, Cnf.Normal)

   StartDateNum% = CrescentDate%(StartDay%, StartMonth%, StartYear%)
   StopDateNum% = CrescentDate%(StopDay%, StopMonth%, StopYear%)
   DO
      SELECT CASE LastPointer%
         CASE 1
            CALL QPrintRC("  " + FUsing$(STR$(StartMonth%), "##") + "  ", Row%(1), Col%(1), Cnf.MenBox)
         CASE 2
            CALL QPrintRC("  " + FUsing$(STR$(StopMonth%), "##") + "  ", Row%(2), Col%(2), Cnf.MenBox)
         CASE 3
            CALL QPrintRC("  " + FUsing$(STR$(StartDay%), "##") + "  ", Row%(3), Col%(3), Cnf.MenBox)
         CASE 4
            CALL QPrintRC("  " + FUsing$(STR$(StopDay%), "##") + "  ", Row%(4), Col%(4), Cnf.MenBox)
         CASE 5
            CALL QPrintRC(" " + FUsing$(STR$(StartYear%), "####") + " ", Row%(5), Col%(5), Cnf.MenBox)
         CASE 6
            CALL QPrintRC(" " + FUsing$(STR$(StopYear%), "####") + " ", Row%(6), Col%(6), Cnf.MenBox)
      END SELECT
      SELECT CASE Pointer%
         CASE 1
            CALL QPrintRC("  " + FUsing$(STR$(StartMonth%), "##") + "  ", Row%(1), Col%(1), Cnf.Revers)
            EnteredString$ = LTRIM$(STR$(StartMonth%))
         CASE 2
            CALL QPrintRC("  " + FUsing$(STR$(StopMonth%), "##") + "  ", Row%(2), Col%(2), Cnf.Revers)
            EnteredString$ = LTRIM$(STR$(StopMonth%))
         CASE 3
            CALL QPrintRC("  " + FUsing$(STR$(StartDay%), "##") + "  ", Row%(3), Col%(3), Cnf.Revers)
            EnteredString$ = LTRIM$(STR$(StartDay%))
         CASE 4
            CALL QPrintRC("  " + FUsing$(STR$(StopDay%), "##") + "  ", Row%(4), Col%(4), Cnf.Revers)
            EnteredString$ = LTRIM$(STR$(StopDay%))
         CASE 5
            CALL QPrintRC(" " + FUsing$(STR$(StartYear%), "####") + " ", Row%(5), Col%(5), Cnf.Revers)
            EnteredString$ = LTRIM$(STR$(StartYear%))
         CASE 6
            CALL QPrintRC(" " + FUsing$(STR$(StopYear%), "####") + " ", Row%(6), Col%(6), Cnf.Revers)
            EnteredString$ = LTRIM$(STR$(StopYear%))
      END SELECT
      LeftCol% = Col%(Pointer%)        'set the left column
      RightCol% = Col%(Pointer%) + 5   'and the right column
      ValidInput$ = Numbers$
      CALL Editor(EnteredString$, Row%(Pointer%), LeftCol%, RightCol%, Ch$, ValidInput$, Cnf.Revers)
      NewNum% = VAL(EnteredString$)
      KeyCode% = KeyPressed%(Ch$)
''??
'locate 1,1:print using "Start dy=## mn=## yr=#### Cres=######  Stop dy=## mn=## yr=#### Cres=######"; StartDay%; StartMonth%; StartYear%; StartDateNum%; StopDay%; StopMonth%; StopYear%; StopDateNum%;
      SELECT CASE Pointer%
         CASE 1  'STARTING MONTH
            IF NewNum% < 1 OR NewNum% > 12 OR (NewNum% > StopMonth% AND StartYear% = StopYear%) THEN
               CALL OOPS
            ELSEIF StopDateNum% <= CrescentDate%(StartDay%, NewNum%, StartYear%) THEN
               CALL OOPS
            ELSE
               IF NewNum% <> StartMonth% THEN
                  StartMonth% = NewNum%
                  IF StartDay% > StopDay% AND StopMonth% = StartMonth% THEN
                     StartDay% = StopDay%
                     CALL OOPS
                  END IF
                  CALL PrintCalendar(StartYear%, StartMonth%, 16, 3)
                  CALL QPrintRC(MonthData(StartMonth%).Mname, Row%(1), 30, Cnf.Normal)
                  StartDateNum% = CrescentDate%(StartDay%, StartMonth%, StartYear%)
                  DateChanged% = True
               END IF
            END IF
         CASE 2 'ENDING MONTH
            IF NewNum% < 1 OR NewNum% > 12 OR (NewNum% < StartMonth% AND StartYear% = StopYear%) THEN
               CALL OOPS
            ELSEIF StartDateNum% >= CrescentDate%(StopDay%, NewNum%, StopYear%) THEN
               CALL OOPS
            ELSEIF NewNum% <> StopMonth% THEN
               StopMonth% = NewNum%
               IF StartDay% > StopDay% AND StopMonth% = StopMonth% THEN
                  StopDay% = StartDay%
                  CALL OOPS
               END IF
               CALL PrintCalendar(StopYear%, StopMonth%, 16, 51)
               CALL QPrintRC(MonthData(StopMonth%).Mname, Row%(2), 30, Cnf.Normal)
               StopDateNum% = CrescentDate%(StopDay%, StopMonth%, StopYear%)
               DateChanged% = True
            END IF
         CASE 3 'STARTING DAY
            IF NewNum% < 1 OR NewNum% > DaysPerMonth%(StartMonth%, StartYear%) THEN
               CALL OOPS
            ELSEIF StopDateNum% <= CrescentDate%(NewNum%, StartMonth%, StartYear%) THEN
'            ELSEIF StopMonth% = StartMonth% AND NewNum% > StopDay% THEN
               CALL OOPS
            ELSE
               StartDay% = NewNum%
               StartDateNum% = CrescentDate%(StartDay%, StartMonth%, StartYear%)
               DateChanged% = True
            END IF
         CASE 4 'ENDING DAY
            IF NewNum% < 1 OR NewNum% > DaysPerMonth%(StopMonth%, StopYear%) THEN
               CALL OOPS
            ELSEIF StartDateNum% >= CrescentDate%(NewNum%, StopMonth%, StopYear%) THEN
'            ELSEIF StopMonth% = StartMonth% AND NewNum% < StartDay% THEN
               CALL OOPS
            ELSE
               StopDay% = NewNum%
               StopDateNum% = CrescentDate%(StopDay%, StopMonth%, StopYear%)
               DateChanged% = True
            END IF
         CASE 5 'START YEAR
            IF NewNum% < 1988 OR NewNum% > 2099 OR StopYear% - NewNum% > 1 THEN
               CALL OOPS
            ELSEIF StopDateNum% <= CrescentDate%(StartDay%, StartMonth%, NewNum%) THEN
               CALL OOPS
            ELSE
               IF NewNum% <> StartYear% THEN
                  StartYear% = NewNum%
                  StartDateNum% = CrescentDate%(StartDay%, StartMonth%, StartYear%)
                  DateChanged% = True
                  CALL PrintCalendar(StartYear%, StartMonth%, 16, 3)
               END IF
            END IF
         CASE 6 'END YEAR
            IF NewNum% < 1988 OR NewNum% > 2099 OR NewNum% - StartYear% > 1 THEN
               CALL OOPS
            ELSEIF StartDateNum% >= CrescentDate%(StopDay%, StopMonth%, NewNum%) THEN
               CALL OOPS
            ELSE
               IF NewNum% <> StopYear% THEN
                  StopYear% = NewNum%
                  StopDateNum% = CrescentDate%(StopDay%, StopMonth%, StopYear%)
                  DateChanged% = True
                  CALL PrintCalendar(StopYear%, StopMonth%, 16, 51)
               END IF
            END IF
      END SELECT
''??
'locate 2,1:print using "Start dy=## mn=## yr=#### Cres=######  Stop dy=## mn=## yr=#### Cres=######"; StartDay%; StartMonth%; StartYear%; StartDateNum%; StopDay%; StopMonth%; StopYear%; StopDateNum%;
      'MMMMMMMMMMMMMMMM Start of Mouse Handling Code MMMMMMMMMMMMMMMMMMM

      IF Cnf.Mouse THEN
         CALL ButtonPress(1, Down%, Presses%, MCol%, MRow%)
         Button1% = False
         IF Presses% THEN Button1% = True

         CALL GetCursor(MCol%, MRow%, Button%)
         MRow% = (MRow% \ Cnf.NPixLines) + 1
         MCol% = (MCol% \ 8) + 1

         IF MRow% <> LMRow% OR MCol% <> LMCol% OR Presses% THEN
            'CURSOR WAS PRESSED OR MOUSE WAS MOVED SINCE LAST CHECK
            'DETERMINE IF CURSOR IS WITHIN ACCEPTABLE DATA ENTRY BOX
            'OR IF ONE OF THE SPECIAL AREAS IS HIGHLIGHTED
            IF MRow% = 7 AND Button1% THEN
               'MOUSE SELECTED FUNCTION KEY ACTION
               SELECT CASE MCol%
                  CASE 9, 10
                     KeyCode% = F1
                  CASE 22, 23, 24
                     KeyCode% = F10
                  CASE ELSE
                     CALL OOPS
               END SELECT
            ELSEIF MRow% = 13 AND Button1% AND MCol% >= 42 AND MCol% <= 61 THEN
               KeyCode% = 0
               LastPointer% = Pointer%
               IF MCol% >= 42 AND MCol% <= 47 THEN
                  Pointer% = 1
               ELSEIF MCol% >= 49 AND MCol% <= 54 THEN
                  Pointer% = 3
               ELSEIF MCol% >= 56 AND MCol% <= 61 THEN
                  Pointer% = 5
               END IF
            ELSEIF MRow% = 14 AND Button1% AND MCol% >= 42 AND MCol% <= 61 THEN
               KeyCode% = 0
               LastPointer% = Pointer%
               IF MCol% >= 42 AND MCol% <= 47 THEN
                  Pointer% = 2
               ELSEIF MCol% >= 49 AND MCol% <= 54 THEN
                  Pointer% = 4
               ELSEIF MCol% >= 56 AND MCol% <= 61 THEN
                  Pointer% = 6
               END IF
            END IF
         ELSEIF Button2% THEN        'USER PRESSED ESCAPE BUTTON ??
            Ky$ = CHR$(27)
            KeyCode% = Escape
            DO
               CALL GetCursor(MCol%, MRow%, Button%)
            LOOP WHILE Button% AND 2
         END IF
    
         LMRow% = MRow%
         LMCol% = MCol%
      END IF  'end of if a mouse is present
      'MMMMMMMMMMMMMMMMMMM End of mouse handling code MMMMMMMMMMMMMMMMMMMMMM

811      SELECT CASE KeyCode%
         CASE Tabb
            LastPointer% = Pointer%
            SELECT CASE LastPointer%
               CASE 1
                  Pointer% = 3
               CASE 2
                  Pointer% = 4
               CASE 3
                  Pointer% = 5
               CASE 4
                  Pointer% = 6
               CASE 5
                  Pointer% = 2
               CASE 6
                  Pointer% = 1
            END SELECT
         CASE ShftTabb
            LastPointer% = Pointer%
            SELECT CASE LastPointer%
               CASE 1
                  Pointer% = 6
               CASE 2
                  Pointer% = 5
               CASE 3
                  Pointer% = 1
               CASE 4
                  Pointer% = 2
               CASE 5
                  Pointer% = 3
               CASE 6
                  Pointer% = 4
            END SELECT
         CASE UpArrow, DownArrow
            LastPointer% = Pointer%
            SELECT CASE LastPointer%
               CASE 1
                  Pointer% = 2
               CASE 2
                  Pointer% = 1
               CASE 3
                  Pointer% = 4
               CASE 4
                  Pointer% = 3
               CASE 5
                  Pointer% = 6
               CASE 6
                  Pointer% = 5
            END SELECT
         CASE RightArrow
            LastPointer% = Pointer%
            SELECT CASE LastPointer%
               CASE 1
                  Pointer% = 3
               CASE 2
                  Pointer% = 4
               CASE 3
                  Pointer% = 5
               CASE 4
                  Pointer% = 6
               CASE 5
                  Pointer% = 1
               CASE 6
                  Pointer% = 2
            END SELECT
         CASE LeftArrow
            LastPointer% = Pointer%
            SELECT CASE LastPointer%
               CASE 1
                  Pointer% = 5
               CASE 2
                  Pointer% = 6
               CASE 3
                  Pointer% = 1
               CASE 4
                  Pointer% = 2
               CASE 5
                  Pointer% = 3
               CASE 6
                  Pointer% = 4
            END SELECT
         CASE F1        'HELP
            CALL HelpMe(0, "DATES", 1, 2, 25, 79)
         CASE F10
            EXIT DO
813      END SELECT
814   LOOP

   HideCursor
   CALL WindowMgr(0, 0, 0, 0, 0, 0, 0)
   ShowCursor

   IF DateChanged% THEN
      CALL SetYear
      CALL SetStrata
      CALL SetHolidays
   END IF

END SUB 'GetDates

SUB GetStartPoints (StartPointTotal%)

   Col%(1) = 8
   Col%(2) = 25
   Col%(3) = 58
   Row%(1) = 14
   Row%(2) = 15
   Row%(3) = 16
   Row%(4) = 17
   Row%(5) = 18
   Row%(6) = 19
   Row%(7) = 20
   Row%(8) = 21
   ThisCol% = 1
   ThisRow% = 1
   LastRow% = 1
   ThisRowPtr% = 1
   LastRowPtr% = 1
   LastCol% = 1
   RePrint% = True
   FirstStartOnScreen% = 1

   Ymin% = 6
   YMax% = Ymin% + Rows% - 1
   NewLine% = Ymin%
   NewCol% = 1
   OldLine% = NewLine%
   OldCol% = NewCol%
'                     1         2         3         4         5         6         7         8
'            12345678901234567890123456789012345678901234567890123456789012345678901234567890

   HideCursor
   CALL WindowMgr(0, 1, 1, 1, 25, 80, Cnf.Normal)     'open next available window
   DO
      ShowCursor
      IF RePrint% THEN
         HideCursor
         StartDate$ = RTRIM$(MonthData(StartMonth%).Mname) + STR$(StartDay%) + "," + STR$(StartYear%)
         StopDate$ = RTRIM$(MonthData(StopMonth%).Mname) + STR$(StopDay%) + "," + STR$(StopYear%)
         CALL QPrintRC("", 1, 1, Cnf.Normal)
         CALL QPrintRC("ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป", 1, 1, Cnf.Normal)
         CALL QPrintRC("บ                                                                              บ", 2, 1, Cnf.Normal)
         CALL QPrintRC("บ                                                                              บ", 3, 1, Cnf.Normal)
         CALL QPrintRC("บ  You can define the names of up to " + FUsing$(STR$(MaxStartPoints), "##") + " separate starting points or water      บ", 4, 1, Cnf.Normal)
         CALL QPrintRC("บ  names for the setup.  This will allow you to schedule multiple waters on    บ", 5, 1, Cnf.Normal)
         CALL QPrintRC("บ  one calendar or randomly select starting points.  F1 for more details.      บ", 6, 1, Cnf.Normal)
'                                1         2         3         4         5         6         7         8
'                       12345678901234567890123456789012345678901234567890123456789012345678901234567890
         CALL QPrintRC("ฬอออออออออออออออออออออออัออออออออออออออออหอออออออออออออออัอออออออออออออออออออออน", 7, 1, Cnf.Normal)
         CALL QPrintRC("บ Count Strata          ณ                บ  Start Date   ณ                     บ", 8, 1, Cnf.Normal)
         CALL QPrintRC(Strata$, 8, 27, Cnf.Normal)
         CALL QPrintRC(StartDate$, 8, 60, Cnf.Normal)
         CALL QPrintRC("บ Weekdays / Strata     ณ       " + FUsing$(STR$(WDperStrata%), "##") + "       บ  End Date     ณ                     บ", 9, 1, Cnf.Normal)
         CALL QPrintRC(StopDate$, 9, 60, Cnf.Normal)
         CALL QPrintRC("บ Weekend Days / Strata ณ       " + FUsing$(STR$(WEperStrata%), "##") + "       บ  Counts / day ณ         " + FUsing$(STR$(Counts%), "##") + "          บ", 10, 1, Cnf.Normal)
         CALL QPrintRC("ฬอออออออออออออออออออัอออฯออออออออออออออออสออออออออออัออออฯอออออออออออออออออออออน", 11, 1, Cnf.Normal)
         CALL QPrintRC("บ Name (8 Chr limit)ณ     Description               ณ   Proportion of Sample   บ", 12, 1, Cnf.Normal)
         CALL QPrintRC("วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ", 13, 1, Cnf.Normal)
         CALL QPrintRC("บ                   ณ                               ณ            ฺฤฤฤฤฤฤฤฤฤฤฤฤฤถ", 14, 1, Cnf.Normal)
         CALL QPrintRC("บ                   ณ                               ณ            ณ Cumulative  บ", 15, 1, Cnf.Normal)
         CALL QPrintRC("บ                   ณ                               ณ            ณ   Total     บ", 16, 1, Cnf.Normal)
         CALL QPrintRC("บ                   ณ                               ณ            ณ(must=1 or 0)บ", 17, 1, Cnf.Normal)
         CALL QPrintRC("บ                   ณ                               ณ            ณ             บ", 18, 1, Cnf.Normal)
         CALL QPrintRC("บ                   ณ                               ณ            ภฤฤฤฤฤฤฤฤฤฤฤฤฤถ", 19, 1, Cnf.Normal)
         CALL QPrintRC("บ                   ณ                               ณ                          บ", 20, 1, Cnf.Normal)
         CALL QPrintRC("บ                   ณ                               ณ                          บ", 21, 1, Cnf.Normal)
         CALL QPrintRC("ฬอออออออออออออออออออฯอออออออออออออออออออออออออออออออฯออออออออออออออออออออออออออน", 22, 1, Cnf.Normal)
         CALL QPrintRC("บ      F1=Help, F6=uniform proportions, F7=distribute remainder, F10=quit,     บ", 23, 1, Cnf.Normal)
         CALL QPrintRC("บ      Arrrow/Tab Keys moves between entries, Page Up/Page Down to scroll.     บ", 24, 1, Cnf.Normal)
         CALL QPrintRC("ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ", 25, 1, Cnf.Normal)
         CALL QPrintRC("F1", 23, 8, Cnf.HiLite)
         CALL QPrintRC("F6", 23, 17, Cnf.HiLite)
         CALL QPrintRC("F7", 23, 41, Cnf.HiLite)
         CALL QPrintRC("F10", 23, 66, Cnf.HiLite)
         CALL QPrintRC(Buffer$ + MainTitle$, 2, 2, Cnf.HiLite)
         RePrint% = False
         ReprintNames% = True
         ShowCursor
      END IF
      IF ReprintNames% THEN
         HideCursor
         FOR j% = 0 TO 7
            CALL QPrintRC(FUsing$(STR$(FirstStartOnScreen% + j%), "##"), j% + 14, 3, Cnf.Normal)
            CALL QPrintRC(StartPoint(FirstStartOnScreen% + j%).Abrev, j% + 14, 8, Cnf.Normal)
            CALL QPrintRC(StartPoint(FirstStartOnScreen% + j%).Descript, j% + 14, 25, Cnf.Normal)
            CALL QPrintRC(FUsing$(STR$(StartPoint(FirstStartOnScreen% + j%).AsignPortion), "#.###"), j% + 14, 58, Cnf.Normal)
         NEXT j%
         ReprintNames% = False
         ShowCursor
      END IF

      LastRowPtr% = LastRow% + FirstStartOnScreen% - 1
      ThisRowPtr% = ThisRow% + FirstStartOnScreen% - 1

      IF FirstStartOnScreen% < MaxStartPoints - 7 THEN
         CALL QPrintRC("", 22, 2, Cnf.HiLite)
         CALL QPrintRC("", 22, 21, Cnf.HiLite)
         CALL QPrintRC("", 22, 53, Cnf.HiLite)
         CALL QPrintRC("", 22, 79, Cnf.HiLite)
      ELSE
         CALL QPrintRC("อ", 22, 2, Cnf.Normal)
         CALL QPrintRC("ฯ", 22, 21, Cnf.Normal)
         CALL QPrintRC("ฯ", 22, 53, Cnf.Normal)
         CALL QPrintRC("อ", 22, 79, Cnf.Normal)
      END IF

      IF FirstStartOnScreen% > 1 THEN
         CALL QPrintRC("", 13, 2, Cnf.HiLite)
         CALL QPrintRC("", 13, 21, Cnf.HiLite)
         CALL QPrintRC("", 13, 53, Cnf.HiLite)
         CALL QPrintRC("", 13, 79, Cnf.HiLite)
      ELSE
         CALL QPrintRC("ฤ", 13, 2, Cnf.Normal)
         CALL QPrintRC("ล", 13, 21, Cnf.Normal)
         CALL QPrintRC("ล", 13, 53, Cnf.Normal)
         CALL QPrintRC("ฤ", 13, 79, Cnf.Normal)
      END IF

      IF LastCol% = 1 THEN
         CALL QPrintRC(StartPoint(LastRowPtr%).Abrev, Row%(LastRow%), Col%(LastCol%), Cnf.Normal)
      ELSEIF LastCol% = 2 THEN
         CALL QPrintRC(StartPoint(LastRowPtr%).Descript, Row%(LastRow%), Col%(LastCol%), Cnf.Normal)
      ELSEIF LastCol% = 3 THEN
         CALL QPrintRC(FUsing$(STR$(StartPoint(LastRowPtr%).AsignPortion), "#.###"), Row%(LastRow%), Col%(LastCol%), Cnf.Normal)
      END IF
      IF ThisCol% = 1 THEN
         CALL QPrintRC(StartPoint(ThisRowPtr%).Abrev, Row%(ThisRow%), Col%(ThisCol%), Cnf.Normal)
      ELSEIF ThisCol% = 2 THEN
         CALL QPrintRC(StartPoint(ThisRowPtr%).Descript, Row%(ThisRow%), Col%(ThisCol%), Cnf.Normal)
      ELSEIF ThisCol% = 3 THEN
         CALL QPrintRC(FUsing$(STR$(StartPoint(ThisRowPtr%).AsignPortion), "#.###"), Row%(ThisRow%), Col%(ThisCol%), Cnf.Normal)
      END IF

      StartPointTotal% = 0
      FOR j% = 1 TO MaxStartPoints
         IF NOT IsBlank%(StartPoint(j%).Abrev) OR StartPoint(j%).AsignPortion > 0 THEN
            StartPointTotal% = StartPointTotal% + 1
         END IF
      NEXT j%

      Sum! = 0
      FOR j% = 1 TO StartPointTotal%
         Sum! = Sum! + StartPoint(j%).AsignPortion
      NEXT j%
   
      CALL QPrintRC("           ", 18, 68, Cnf.Normal)
      CALL QPrintRC(FUsing$(STR$(Sum!), "#.###"), 18, 70, Cnf.Revers)

      ValidInput$ = Alphabet$ + Punctuation$ + Numbers$
      SELECT CASE ThisCol%
         CASE 1
            LeftCol% = 8                   'set the left column
            RightCol% = 15                 'and the right column
'            LOCATE Row%(ThisRow%)
            EnteredString$ = RTRIM$(StartPoint(ThisRowPtr%).Abrev)
            CALL Editor(EnteredString$, Row%(ThisRow%), LeftCol%, RightCol%, Ch$, ValidInput$, Cnf.Revers)
            StartPoint(ThisRowPtr%).Abrev = EnteredString$
         CASE 2
            LeftCol% = 25                   'set the left column
            RightCol% = 49                  'and the right column
'            LOCATE Row%(ThisRow%)
            EnteredString$ = RTRIM$(StartPoint(ThisRowPtr%).Descript)
            CALL Editor(EnteredString$, Row%(ThisRow%), LeftCol%, RightCol%, Ch$, ValidInput$, Cnf.Revers)
            StartPoint(ThisRowPtr%).Descript = EnteredString$
         CASE 3
            LeftCol% = Col%(3)                   'set the left column
            RightCol% = Col%(3) + 4              'and the right column
'            LOCATE Row%(ThisRow%)
            EnteredString$ = LTRIM$(STR$(StartPoint(ThisRowPtr%).AsignPortion))
            IF StartPoint(ThisRowPtr%).AsignPortion < 1 AND StartPoint(ThisRowPtr%).AsignPortion > 0 THEN
               EnteredString$ = "0" + EnteredString$
            END IF
            CALL Editor(EnteredString$, Row%(ThisRow%), LeftCol%, RightCol%, Ch$, ValidInput$, Cnf.Revers)
            StartPoint(ThisRowPtr%).AsignPortion = VAL(EnteredString$)
      END SELECT
'      COLOR NormalFG%, NormalBG%
      KeyCode% = KeyPressed%(Ch$)

      'MMMMMMMMMMMMMMMM Start of Mouse Handling Code MMMMMMMMMMMMMMMMMMM

      IF Cnf.Mouse THEN
         CALL ButtonPress(1, Down%, Presses%, MCol%, MRow%)
         Button1% = False
         IF Presses% THEN Button1% = True

         CALL GetCursor(MCol%, MRow%, Button%)
         MRow% = (MRow% \ Cnf.NPixLines) + 1
         MCol% = (MCol% \ 8) + 1

         IF MRow% <> LMRow% OR MCol% <> LMCol% OR Presses% THEN
            'CURSOR WAS PRESSED OR MOUSE WAS MOVED SINCE LAST CHECK
            'DETERMINE IF CURSOR IS WITHIN ACCEPTABLE DATA ENTRY BOX
            'OR IF ONE OF THE SPECIAL AREAS IS HIGHLIGHTED
            IF MRow% = 23 AND Button1% THEN
               'MOUSE SELECTED FUNCTION KEY ACTION
               SELECT CASE MCol%
                  CASE 8, 9
                     KeyCode% = F1
                  CASE 17, 18
                     KeyCode% = F6
                  CASE 41, 42
                     KeyCode% = F7
                  CASE 66, 67, 68
                     KeyCode% = F10
                  CASE ELSE
                     CALL OOPS
               END SELECT
            ELSEIF MRow% = 13 AND Button1% THEN
               'POSSIBLY A SCROLL DOWN SELECTION
               IF MCol% = 2 OR MCol% = 21 OR MCol% = 53 OR MCol% = 79 THEN
                  KeyCode% = PageUp
               END IF
            ELSEIF MRow% = 22 AND Button1% THEN
               'POSSIBLY A SCROLL UP SELECTION
               IF MCol% = 2 OR MCol% = 21 OR MCol% = 53 OR MCol% = 79 THEN
                  KeyCode% = PageDown
               END IF
            ELSEIF MRow% >= 14 AND MRow% <= 21 AND Button1% AND MCol% >= 2 AND MCol% <= 65 THEN
               KeyCode% = 0
               LastCol% = ThisCol%
               LastRow% = ThisRow%
               ThisRow% = MRow% - 13
               IF MCol% >= 2 AND MCol% <= 20 THEN
                  ThisCol% = 1
               ELSEIF MCol% >= 22 AND MCol% <= 52 THEN
                  ThisCol% = 2
               ELSEIF MCol% >= 54 AND MCol% <= 65 THEN
                  ThisCol% = 3
               END IF
            END IF
         ELSEIF Button2% THEN        'USER PRESSED ESCAPE BUTTON ??
            Ky$ = CHR$(27)
            KeyCode% = Escape
            DO
               CALL GetCursor(MCol%, MRow%, Button%)
            LOOP WHILE Button% AND 2
         END IF
    
         LMRow% = MRow%
         LMCol% = MCol%
      END IF  'end of if a mouse is present
      'MMMMMMMMMMMMMMMMMMM End of mouse handling code MMMMMMMMMMMMMMMMMMMMMM

      IF KeyCode% = Tabb THEN
         LastCol% = ThisCol%
         LastRow% = ThisRow%
         SELECT CASE LastCol%
            CASE 1, 2
               ThisCol% = ThisCol% + 1
            CASE 3
               ThisCol% = 1
               SELECT CASE LastRow%
                  CASE 8
                     ThisRow% = 1
                  CASE ELSE
                     ThisRow% = ThisRow% + 1
               END SELECT
         END SELECT
      ELSEIF KeyCode% = ShftTabb THEN
         KeyCode% = LeftArrow%
         LastCol% = ThisCol%
         LastRow% = ThisRow%
         SELECT CASE LastCol%
            CASE 2, 3
               ThisCol% = ThisCol% - 1
            CASE 1
               ThisCol% = 3
               SELECT CASE LastRow%
                  CASE 1
                     ThisRow% = 8
                  CASE ELSE
                     ThisRow% = ThisRow% - 1
               END SELECT
         END SELECT
      ELSEIF KeyCode% < 0 THEN
         SELECT CASE KeyCode%
            CASE RightArrow
               LastCol% = ThisCol%
               LastRow% = ThisRow%
               SELECT CASE LastCol%
                  CASE 1, 2
                     ThisCol% = ThisCol% + 1
                  CASE 3
                     ThisCol% = 1
               END SELECT
            CASE LeftArrow
               LastCol% = ThisCol%
               LastRow% = ThisRow%
               SELECT CASE LastCol%
                  CASE 2, 3
                     ThisCol% = ThisCol% - 1
                  CASE 1
                     ThisCol% = 3
               END SELECT
            CASE UpArrow
               LastCol% = ThisCol%
               LastRow% = ThisRow%
               SELECT CASE LastRow%
                  CASE 1
                     ThisRow% = 8
                  CASE ELSE
                     ThisRow% = ThisRow% - 1
               END SELECT
            CASE DownArrow
               LastCol% = ThisCol%
               LastRow% = ThisRow%
               SELECT CASE LastRow%
                  CASE 8
                     ThisRow% = 1
                  CASE ELSE
                     ThisRow% = ThisRow% + 1
               END SELECT
            CASE PageDown
               LastCol% = ThisCol%
               LastRow% = ThisRow%
               FirstStartOnScreen% = FirstStartOnScreen% + 1
               IF FirstStartOnScreen% + 7 > MaxStartPoints THEN
                  FirstStartOnScreen% = MaxStartPoints - 7
               ELSE
                  HideCursor
                  CALL ScrollU(14, 2, 21, 65, 1, -1)
                  CALL QPrintRC("บ                   ณ                               ณ           ", 21, 1, Cnf.Normal)
                  CALL QPrintRC(FUsing$(STR$(FirstStartOnScreen% + 7), "##"), 21, 3, Cnf.Normal)
                  CALL QPrintRC(StartPoint(FirstStartOnScreen% + 7).Abrev, 21, 8, Cnf.Normal)
                  CALL QPrintRC(StartPoint(FirstStartOnScreen% + 7).Descript, 21, 25, Cnf.Normal)
                  CALL QPrintRC(FUsing$(STR$(StartPoint(FirstStartOnScreen% + 7).AsignPortion), "#.###"), 21, 58, Cnf.Normal)
                  ShowCursor
               END IF
               SELECT CASE LastRow%
                  CASE 1
                     ThisRow% = 1
                  CASE ELSE
                     ThisRow% = ThisRow% - 1
               END SELECT
            CASE PageUp
               LastCol% = ThisCol%
               LastRow% = ThisRow%
               FirstStartOnScreen% = FirstStartOnScreen% - 1
               IF FirstStartOnScreen% = 0 THEN
                  FirstStartOnScreen% = 1
               ELSE
                  HideCursor
                  CALL ScrollD(14, 2, 21, 65, 1, -1)
                  CALL QPrintRC("บ                   ณ                               ณ           ", 14, 1, Cnf.Normal)
                  CALL QPrintRC(FUsing$(STR$(FirstStartOnScreen%), "##"), 14, 3, Cnf.Normal)
                  CALL QPrintRC(StartPoint(FirstStartOnScreen%).Abrev, 14, 8, Cnf.Normal)
                  CALL QPrintRC(StartPoint(FirstStartOnScreen%).Descript, 14, 25, Cnf.Normal)
                  CALL QPrintRC(FUsing$(STR$(StartPoint(FirstStartOnScreen%).AsignPortion), "#.###"), 14, 58, Cnf.Normal)
                  ShowCursor
               END IF
               SELECT CASE LastRow%
                  CASE 8
                     ThisRow% = 8
                  CASE ELSE
                     ThisRow% = ThisRow% + 1
               END SELECT
            CASE F6        'assign uniform probabilities
               FOR j% = 1 TO StartPointTotal%
                  StartPoint(j%).AsignPortion = 1 / StartPointTotal%
               NEXT j%
               ReprintNames% = True
            CASE F7        'distribute remainder
               Iterations% = 0
               AdjustPoint% = 0
               DO
                  Iterations% = Iterations% + 1
                  Sum! = 0
                  FOR j% = 1 TO StartPointTotal%
                     StartPoint(j%).AsignPortion = VAL(QPROUND$(CDBL(StartPoint(j%).AsignPortion), 3))
                     Sum! = Sum! + StartPoint(j%).AsignPortion
                  NEXT j%
                  IF ABS(1 - Sum!) < .0001 THEN EXIT DO
                  FOR j% = 1 TO StartPointTotal%
                     StartPoint(j%).AsignPortion = StartPoint(j%).AsignPortion + ((1 - Sum!) / StartPointTotal%)
                  NEXT j%
                  IF Iterations% > 20 THEN
                     'add a small amount to one to get it off dead center
                     AdjustPoint% = AdjustPoint% + 1
                     IF AdjustPoint% > StartPointTotal% THEN AdjustPoint% = 1
                     Iterations% = 0
                     StartPoint(AdjustPoint%).AsignPortion = StartPoint(AdjustPoint%).AsignPortion + .001
                  END IF
               LOOP
               ReprintNames% = True
            CASE F1        'HELP
               CALL HelpMe(0, "STARTPT", 1, 2, 25, 79)
            CASE F10        'F10  quit
               Sum! = 0!
               FOR j% = 1 TO StartPointTotal%
                  Sum! = Sum! + StartPoint(j%).AsignPortion
               NEXT j%
               IF ABS(1 - Sum!) < .0001 OR Sum! = 0! THEN
                  EXIT DO
               ELSE
                  CALL OOPS
               END IF
         END SELECT
      END IF
   LOOP

   StartPointTotal% = 0
   FOR j% = 1 TO MaxStartPoints
      IF NOT IsBlank%(StartPoint(j%).Abrev) THEN
         StartPointTotal% = StartPointTotal% + 1
      END IF
   NEXT j%

   HideCursor
   CALL WindowMgr(0, 0, 0, 0, 0, 0, 0)
   ShowCursor

END SUB 'GetStartPoints

SUB GetStrata (WEperStrata%, WDperStrata%)
   Pointer% = 1
   LastPointer% = 1
   StrataChanged% = False
   Row%(1) = 11     ' WE per Strata
   Row%(2) = 13     ' WD per Strata

      'DEFINE MAXIMUM NUMBER OF WEEKDAYS AND WEEKEND DAYS PER STRATA
      SELECT CASE Strata$
         CASE "month"
            MaxWD% = 23
            MaxWE% = 10
         CASE "2 weeks"
            MaxWD% = 10
            MaxWE% = 4
         CASE "week"
            MaxWD% = 5
            MaxWE% = 2
      END SELECT

   HideCursor
   CALL WindowMgr(0, 1, 1, 1, 25, 80, Cnf.Normal)     'open next available window
   ShowCursor

   StartDate$ = RTRIM$(MonthData(StartMonth%).Mname) + STR$(StartDay%) + "," + STR$(StartYear%)
   StopDate$ = RTRIM$(MonthData(StopMonth%).Mname) + STR$(StopDay%) + "," + STR$(StopYear%)
   CALL QPrintRC("ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป", 1, 1, Cnf.Normal)
   CALL QPrintRC("บ                                                                              บ", 2, 1, Cnf.Normal)
   CALL QPrintRC("บ                                                                              บ", 3, 1, Cnf.Normal)
   CALL QPrintRC("บ  Select a sampling regime which meets your survey requirements.  Maximum and บ", 4, 1, Cnf.Normal)
   CALL QPrintRC("บ  minimum values are given which are dependent upon the strata selected.      บ", 5, 1, Cnf.Normal)
   CALL QPrintRC("บ                                                                              บ", 6, 1, Cnf.Normal)
   CALL QPrintRC("ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน", 7, 1, Cnf.Normal)
   CALL QPrintRC("บ   Start Date:                              End Date:                         บ", 8, 1, Cnf.Normal)
   CALL QPrintRC("ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน", 9, 1, Cnf.Normal)
   CALL QPrintRC("บ                                                                              บ", 10, 1, Cnf.Normal)
   CALL QPrintRC("บ      Weekend days every             (must be between 1 and " + FUsing$(STR$(MaxWE%), "##") + ")               บ", 11, 1, Cnf.Normal)
   CALL QPrintRC("บ                                                                              บ", 12, 1, Cnf.Normal)
   CALL QPrintRC("บ      Week days every             (must be between 1 and " + FUsing$(STR$(MaxWD%), "##") + ")                  บ", 13, 1, Cnf.Normal)
   CALL QPrintRC("บ                                                                              บ", 14, 1, Cnf.Normal)
   CALL QPrintRC("ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน", 15, 1, Cnf.Normal)
   CALL QPrintRC("บ         STRATA SELECTION - Highlight the desired choice and hit F4           บ", 16, 1, Cnf.Normal)
   CALL QPrintRC("บ                                                                              บ", 17, 1, Cnf.Normal)
   CALL QPrintRC("บ                                                                              บ", 21, 1, Cnf.Normal)
   CALL QPrintRC("ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน", 22, 1, Cnf.Normal)
   CALL QPrintRC("บ                      F1 for Help, F4 selects a strata,                       บ", 23, 1, Cnf.Normal)
   CALL QPrintRC("บ                   Arrow keys move cursor, F10 to proceed.                    บ", 24, 1, Cnf.Normal)
   CALL QPrintRC("ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ", 25, 1, Cnf.Normal)
                 '         1         2         3         4         5         6         7         8
                 '12345678901234567890123456789012345678901234567890123456789012345678901234567890
   CALL QPrintRC(Buffer$ + MainTitle$, 2, 2, Cnf.HiLite)
   CALL QPrintRC(StartDate$, 8, 17, Cnf.Normal)
   CALL QPrintRC(StopDate$, 8, 56, Cnf.Normal)
   CALL QPrintRC(Strata$, 11, 27, Cnf.Normal)
   CALL QPrintRC(Strata$, 13, 24, Cnf.Normal)

   CALL QPrintRC("F1", 23, 24, Cnf.HiLite)
   CALL QPrintRC("F4", 23, 37, Cnf.HiLite)
   CALL QPrintRC("F10", 24, 45, Cnf.HiLite)

   CALL QPrintRC(" " + FUsing$(STR$(WEperStrata%), "##") + " ", 11, 4, Cnf.MenBox)
   CALL QPrintRC(" " + FUsing$(STR$(WDperStrata%), "##") + " ", 13, 4, Cnf.MenBox)
   RePrint% = True

   DO
      'DEFINE MAXIMUM NUMBER OF WEEKDAYS AND WEEKEND DAYS PER STRATA
      SELECT CASE Strata$
         CASE "month"
            MaxWD% = 23
            MaxWE% = 10
         CASE "2 weeks"
            MaxWD% = 10
            MaxWE% = 4
         CASE "week"
            MaxWD% = 5
            MaxWE% = 2
      END SELECT

      IF RePrint% THEN
         CALL QPrintRC("บ      month is not selected                                                   บ", 18, 1, Cnf.Normal)
         CALL QPrintRC("บ      2 weeks is not selected                                                 บ", 19, 1, Cnf.Normal)
         CALL QPrintRC("บ      week is not selected                                                    บ", 20, 1, Cnf.Normal)
         IF Strata$ = "month" THEN
            CALL QPrintRC("บ      month is currently selected                                             บ", 18, 1, Cnf.HiLite)
         ELSEIF Strata$ = "2 weeks" THEN
            CALL QPrintRC("บ      2 weeks is currently selected                                           บ", 19, 1, Cnf.HiLite)
         ELSEIF Strata$ = "week" THEN
            CALL QPrintRC("บ      week is currently selected                                              บ", 20, 1, Cnf.HiLite)
         END IF
         CALL QPrintRC(" month ", 18, 7, Cnf.MenBox)
         CALL QPrintRC(" 2 weeks ", 19, 7, Cnf.MenBox)
         CALL QPrintRC(" week ", 20, 7, Cnf.MenBox)
         CALL QPrintRC("บ      Weekend days every             (must be between 1 and " + FUsing$(STR$(MaxWE%), "##") + ")               บ", 11, 1, Cnf.Normal)
         CALL QPrintRC("บ      Week days every             (must be between 1 and " + FUsing$(STR$(MaxWD%), "##") + ")                  บ", 13, 1, Cnf.Normal)
         CALL QPrintRC(" " + FUsing$(STR$(WEperStrata%), "##") + " ", 11, 4, Cnf.MenBox)
         CALL QPrintRC(" " + FUsing$(STR$(WDperStrata%), "##") + " ", 13, 4, Cnf.MenBox)
         CALL QPrintRC(Strata$, 11, 27, Cnf.Normal)
         CALL QPrintRC(Strata$, 13, 24, Cnf.Normal)
         RePrint% = False
      END IF

      'RESTORE LAST HILIGHTED SELECTION TO NORMAL DISPLAY
      SELECT CASE LastPointer%
         CASE 1
            CALL QPrintRC(" " + FUsing$(STR$(WEperStrata%), "##") + " ", 11, 4, Cnf.MenBox)
         CASE 2
            CALL QPrintRC(" " + FUsing$(STR$(WDperStrata%), "##") + " ", 13, 4, Cnf.MenBox)
         CASE 3
            CALL QPrintRC(" month ", 18, 7, Cnf.MenBox)
         CASE 4
            CALL QPrintRC(" 2 weeks ", 19, 7, Cnf.MenBox)
         CASE 5
            CALL QPrintRC(" week ", 20, 7, Cnf.MenBox)
      END SELECT
      'HILIGHT THE CURRENT CURSOR LOCATION
      SELECT CASE Pointer%
         CASE 1
            CALL QPrintRC(" " + FUsing$(STR$(WEperStrata%), "##") + " ", 11, 4, Cnf.Revers)
         CASE 2
            CALL QPrintRC(" " + FUsing$(STR$(WDperStrata%), "##") + " ", 13, 4, Cnf.Revers)
         CASE 3
            CALL QPrintRC(" month ", 18, 7, Cnf.Revers)
         CASE 4
            CALL QPrintRC(" 2 weeks ", 19, 7, Cnf.Revers)
         CASE 5
            CALL QPrintRC(" week ", 20, 7, Cnf.Revers)
      END SELECT
      'CHECK THAT THE COUNTS PER STRATA ARE REASONABLE
      IF WEperStrata% > MaxWE% THEN
         ErrorExists% = True
         CALL OOPS
         CALL QPrintRC(" " + FUsing$(STR$(WEperStrata%), "##") + " ", 11, 4, Cnf.Revers + 128)
      END IF
      IF WDperStrata% > MaxWD% THEN
         ErrorExists% = True
         CALL OOPS
         CALL QPrintRC(" " + FUsing$(STR$(WDperStrata%), "##") + " ", 13, 4, Cnf.Revers + 128)
      END IF
'      COLOR NormalFG%, NormalBG%
      IF Pointer% < 3 THEN
         IF Pointer% = 1 THEN
            EnteredString$ = LTRIM$(STR$(WEperStrata%))
         ELSEIF Pointer% = 2 THEN
            EnteredString$ = LTRIM$(STR$(WDperStrata%))
         END IF
         LeftCol% = 4        'set the left column
         RightCol% = 7   'and the right column
         ValidInput$ = Numbers$
         CALL Editor(EnteredString$, Row%(Pointer%), LeftCol%, RightCol%, Ch$, ValidInput$, Cnf.Revers)
         NewNum% = VAL(EnteredString$)
         SELECT CASE Pointer%
            CASE 1 'WEEKEND DAYS PER STRATA
               IF NewNum% < 1 OR NewNum% > MaxWE% THEN
                  CALL OOPS
               ELSE
                  WEperStrata% = NewNum%
               END IF
            CASE 2 'WEEK DAYS PER STRATA
               IF NewNum% < 1 OR NewNum% > MaxWD% THEN
                  CALL OOPS
               ELSE
                  WDperStrata% = NewNum%
               END IF
         END SELECT
      ELSE
         'WAIT FOR INPUT FROM KEYBOARD
         Ch$ = GetAKey$
      END IF
      KeyCode% = KeyPressed%(Ch$)

      'MMMMMMMMMMMMMMMM Start of Mouse Handling Code MMMMMMMMMMMMMMMMMMM

      IF Cnf.Mouse THEN
         CALL ButtonPress(1, Down%, Presses%, MCol%, MRow%)
         Button1% = False
         IF Presses% THEN Button1% = True

         CALL GetCursor(MCol%, MRow%, Button%)
         MRow% = (MRow% \ Cnf.NPixLines) + 1
         MCol% = (MCol% \ 8) + 1

         IF MRow% <> LMRow% OR MCol% <> LMCol% OR Presses% THEN
            'CURSOR WAS PRESSED OR MOUSE WAS MOVED SINCE LAST CHECK
            'DETERMINE IF CURSOR IS WITHIN ACCEPTABLE DATA ENTRY BOX
            'OR IF ONE OF THE SPECIAL AREAS IS HIGHLIGHTED
            IF MRow% = 23 AND Button1% THEN
               'MOUSE SELECTED FUNCTION KEY ACTION
               SELECT CASE MCol%
                  CASE 24, 25
                     KeyCode% = F1
                  CASE 37, 38
                     KeyCode% = F4
                  CASE ELSE
                     CALL OOPS
               END SELECT
            ELSEIF MRow% = 24 AND Button1% THEN
               'MOUSE SELECTED FUNCTION KEY ACTION
               SELECT CASE MCol%
                  CASE 45, 46, 47
                     KeyCode% = F10
                  CASE ELSE
                     CALL OOPS
               END SELECT
            ELSEIF MRow% = 11 AND Button1% AND MCol% >= 4 AND MCol% <= 7 THEN
               LastPointer% = Pointer%
               Pointer% = 1
            ELSEIF MRow% = 13 AND Button1% AND MCol% >= 4 AND MCol% <= 7 THEN
               LastPointer% = Pointer%
               Pointer% = 2
            ELSEIF MRow% = 18 AND Button1% AND MCol% >= 8 AND MCol% <= 12 THEN
               LastPointer% = Pointer%
               Pointer% = 3
            ELSEIF MRow% = 19 AND Button1% AND MCol% >= 8 AND MCol% <= 14 THEN
               LastPointer% = Pointer%
               Pointer% = 4
            ELSEIF MRow% = 20 AND Button1% AND MCol% >= 8 AND MCol% <= 11 THEN
               LastPointer% = Pointer%
               Pointer% = 5
            END IF
         ELSEIF Button2% THEN        'USER PRESSED ESCAPE BUTTON ??
            Ky$ = CHR$(27)
            KeyCode% = Escape
            DO
               CALL GetCursor(MCol%, MRow%, Button%)
            LOOP WHILE Button% AND 2
         END IF
    
         LMRow% = MRow%
         LMCol% = MCol%
      END IF  'end of if a mouse is present
      'MMMMMMMMMMMMMMMMMMM End of mouse handling code MMMMMMMMMMMMMMMMMMMMMM

      IF KeyCode% < 0 THEN
         'IF KEYBOARD INPUT IS FUNCTION OR NUMERIC/ARROW KEY
         SELECT CASE KeyCode%
            CASE LeftArrow, UpArrow
               LastPointer% = Pointer%
               IF Pointer% = 1 THEN
                  Pointer% = 5
               ELSE
                  Pointer% = Pointer% - 1
               END IF
            CASE RightArrow, DownArrow
               LastPointer% = Pointer%
               IF Pointer% = 5 THEN
                  Pointer% = 1
               ELSE
                  Pointer% = Pointer% + 1
               END IF
            CASE F4           'F4   SELECT A STRATA
               RePrint% = True
               SELECT CASE Pointer%
                  CASE 3
                     Strata$ = "month"
                  CASE 4
                     Strata$ = "2 weeks"
                  CASE 5
                     Strata$ = "week"
                  CASE ELSE
                     CALL OOPS
                     RePrint% = False
               END SELECT
               StrataChanged% = True
            CASE F1        'HELP
               CALL HelpMe(0, "STRATA", 1, 2, 25, 79)
            CASE F10        'F10  quit
               EXIT DO
         END SELECT
      END IF
   LOOP

   IF StrataChanged% THEN
      CALL SetYear
      CALL SetStrata
   END IF

   HideCursor
   CALL WindowMgr(0, 0, 0, 0, 0, 0, 0)
   ShowCursor

END SUB 'GetStrata

