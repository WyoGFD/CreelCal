DECLARE SUB EditHoliday (Pointer%, EditLine%)
DECLARE FUNCTION GetAKey$ ()
DECLARE SUB GetHelpText (LookUp$, HelpLines%, Widest%)
DECLARE SUB GetHolidays ()
DECLARE SUB HelpMe (Action%, LookUp$, ULRow%, ULCol%, LRRow%, LRCol%)
DECLARE SUB HelpOnHelp ()
DECLARE SUB HelpSetup (FileName$)
'DECLARE FUNCTION IsLeapYear% (N%)
DECLARE SUB OOPS ()
DECLARE SUB ReadWrite (WhyCalled%, RW%, WDperStrata%, WEperStrata%, Strata$, TotalStrata%, DataFileName$)
DECLARE SUB SetHolidays ()
DECLARE SUB ViewMonths (StartYear%, StopYear%)

'$DYNAMIC

'$INCLUDE: 'creelcal.bi'

FUNCTION Date2Day% (Dat$) STATIC
   '******** Date2Day.Bas - finds the day of the week number from a Date String

   'Copyright (c) 1987 Donald R. Malin
   'Copyright (c) 1987 Crescent Software

   'Caller passes a Date String in the form MMDDYY or MMDDYYYY, and this
   'subprogram returns a day of the week number (1-7), where 1 = Sunday.
    Date2Day% = Num2Day%(Date2Num%(Dat$))
END FUNCTION

FUNCTION DaysPerMonth% (Month%, Year%)
   DaysPerMonth% = MonthData(Month%).Number
'   locate 24,1
'   print using " Days=##  Month=##  Year=####  "; MonthData(Month%).Number, Month%, Year%;

   ' Compensate if requested year is a leap year:
   ' and if February, add one to the month's days:
   IF IsLeapYear(Year%) AND Month% = 2 THEN
      DaysPerMonth% = 29
'      print using " Days=##  Month=##  Year=####  "; 29, Month%, Year%;
   ELSEIF Month% = 2 THEN
      DaysPerMonth% = 28
'      print using " Days=##  Month=##  Year=####  "; 28, Month%, Year%;
   END IF

END FUNCTION

SUB EditHoliday (Pointer%, EditLine%)

   Spot% = 1
   LastSpot% = 1
   Form1$ = " \  \ "
   Form2$ = "  ##  "
   Form3$ = " #### "
   Col%(1) = 3
   Col%(2) = 13
   Col%(3) = 22
   Col%(4) = 31
   Col%(5) = 42

   HideCursor
   CALL WindowMgr(0, 1, 13, 1, 23, 80, CNF.Normal)     'open next available window
   ShowCursor
   CALL QPrintRC(" Tab key to edit Field, PgUp/PgDn to Change Option, ", 17, 16, CNF.Normal)
   CALL QPrintRC(" F1 for Help, F5 to view calendars for the year, F10 to quit.", 18, 10, CNF.Normal)
        '         1         2         3         4         5         6         7         8
        '12345678901234567890123456789012345678901234567890123456789012345678901234567890
   CALL QPrintRC("F1", 18, 11, CNF.HiLite)
   CALL QPrintRC("F5", 18, 24, CNF.HiLite)
   CALL QPrintRC("F10", 18, 59, CNF.HiLite)
   CALL QPrintRC("Option 0 = Ignore entry; Option 1 = Treat as weekend and include in random", 20, 4, CNF.Normal)
   CALL QPrintRC("selection of weekends; Option 2 = Mandatory inclusion in sampling schedule;", 21, 4, CNF.Normal)
   CALL QPrintRC("Option 3 = Mandatory exclusion from sampling schedule", 22, 4, CNF.Normal)

   DO
      'PRINT STARTING VALUES
      CALL PUsing(STR$(Holiday(Pointer%).Selected), Form2$, CNF.MenBox, CNF.MenBox, -1, EditLine%, Col%(1))
      CALL PUsing(STR$(Holiday(Pointer%).Month), Form2$, CNF.MenBox, CNF.MenBox, -1, EditLine%, Col%(2))
      CALL PUsing(STR$(Holiday(Pointer%).Day), Form2$, CNF.MenBox, CNF.MenBox, -1, EditLine%, Col%(3))
      CALL PUsing(STR$(Holiday(Pointer%).Year), Form3$, CNF.MenBox, CNF.MenBox, -1, EditLine%, Col%(4))
      CALL QPrintRC(Holiday(Pointer%).HName, EditLine%, Col%(5), CNF.MenBox)
      'PRINT VALUE OF LAST CURSOR LOCATION IN NORMAL MENU COLOR
      SELECT CASE LastSpot%
         CASE 1
             CALL PUsing(STR$(Holiday(Pointer%).Selected), Form2$, CNF.MenBox, CNF.MenBox, -1, EditLine%, Col%(LastSpot%))
         CASE 2
            CALL PUsing(STR$(Holiday(Pointer%).Month), Form2$, CNF.MenBox, CNF.MenBox, -1, EditLine%, Col%(LastSpot%))
         CASE 3
            CALL PUsing(STR$(Holiday(Pointer%).Day), Form2$, CNF.MenBox, CNF.MenBox, -1, EditLine%, Col%(LastSpot%))
         CASE 4
            CALL PUsing(STR$(Holiday(Pointer%).Year), Form3$, CNF.MenBox, CNF.MenBox, -1, EditLine%, Col%(LastSpot%))
         CASE 5
            CALL QPrintRC(Holiday(Pointer%).HName, EditLine%, Col%(LastSpot%), CNF.MenBox)
      END SELECT
      'PRINT VALUE OF CURRENT CURSOR LOCATION IN REVERSE VIDEO
      SELECT CASE Spot%
         CASE 1
             CALL PUsing(STR$(Holiday(Pointer%).Selected), Form2$, CNF.Revers, CNF.Revers, -1, EditLine%, Col%(Spot%))
         CASE 2
            CALL PUsing(STR$(Holiday(Pointer%).Month), Form2$, CNF.Revers, CNF.Revers, -1, EditLine%, Col%(Spot%))
         CASE 3
            CALL PUsing(STR$(Holiday(Pointer%).Day), Form2$, CNF.Revers, CNF.Revers, -1, EditLine%, Col%(Spot%))
         CASE 4
            CALL PUsing(STR$(Holiday(Pointer%).Year), Form3$, CNF.Revers, CNF.Revers, -1, EditLine%, Col%(Spot%))
         CASE 5
            CALL QPrintRC(Holiday(Pointer%).HName, EditLine%, Col%(Spot%), CNF.Revers)
      END SELECT
      IF Spot% = 1 THEN 'get a keystroke
         Ch$ = GetAKey$
      ELSEIF Spot% = 5 THEN 'Holiday Name
         LeftCol% = Col%(Spot%)        'set the left column
         RightCol% = Col%(Spot%) + 29  'and the right column
         ValidInput$ = Numbers$ + Alphabet$ + Punctuation$
         EnteredString$ = RTRIM$(Holiday(Pointer%).HName)
         CALL Editor(EnteredString$, EditLine%, LeftCol%, RightCol%, Ch$, ValidInput$, CNF.Revers)
         Holiday(Pointer%).HName = EnteredString$
      ELSE
         LeftCol% = Col%(Spot%)        'set the left column
         RightCol% = Col%(Spot%) + 5   'and the right column
         ValidInput$ = Numbers$
         IF Spot% = 2 THEN
            EnteredString$ = LTRIM$(STR$(Holiday(Pointer%).Month))
         ELSEIF Spot% = 3 THEN
            EnteredString$ = LTRIM$(STR$(Holiday(Pointer%).Day))
         ELSEIF Spot% = 4 THEN
            EnteredString$ = LTRIM$(STR$(Holiday(Pointer%).Year))
         END IF
         CALL Editor(EnteredString$, EditLine%, LeftCol%, RightCol%, Ch$, ValidInput$, CNF.Revers)
         NewNum% = VAL(EnteredString$)
         SELECT CASE Spot%
            CASE 2  ' NEW MONTH
               IF NewNum% < 1 OR NewNum% > 12 THEN
                  CALL OOPS
               ELSE
                  Holiday(Pointer%).Month = NewNum%
               END IF
            CASE 3  ' NEW DAY
               IF NewNum% < 1 OR NewNum% > DaysPerMonth%(Holiday(Pointer%).Month, Holiday(Pointer%).Year) THEN
                  CALL OOPS
               ELSE
                  Holiday(Pointer%).Day = NewNum%
               END IF
            CASE 4  ' NEW YEAR
               IF NewNum% < 1975 OR NewNum% > 2100 THEN
                  CALL OOPS
               ELSE
                  Holiday(Pointer%).Year = NewNum%
               END IF
         END SELECT
      END IF
      KeyCode% = KeyPressed%(Ch$)
      SELECT CASE KeyCode%
         CASE Tabb
            LastSpot% = Spot%
            SELECT CASE LastSpot%
               CASE 1, 2, 3, 4
                  Spot% = Spot% + 1
               CASE 5
                  Spot% = 1
            END SELECT
         CASE LeftArrow, UpArrow
            LastSpot% = Spot%
            SELECT CASE LastSpot%
               CASE 2, 3, 4, 5
                  Spot% = Spot% - 1
               CASE 1
                  Spot% = 5
            END SELECT
         CASE DownArrow, RightArrow
            LastSpot% = Spot%
            SELECT CASE LastSpot%
               CASE 1, 2, 3, 4
                  Spot% = Spot% + 1
               CASE 5
                  Spot% = 1
            END SELECT
         CASE PageUp, PageDown
            LastSpot% = Spot%
            Spot% = 1
            SELECT CASE Holiday(Pointer%).Selected
               CASE 0 TO 2
                  Holiday(Pointer%).Selected = Holiday(Pointer%).Selected + 1
               CASE ELSE
                  Holiday(Pointer%).Selected = 0
            END SELECT
         CASE F5        'F5   VIEW CALENDAR FOR YEAR
            CALL ViewMonths(StartYear%, StopYear%)
         CASE F1        'HELP
            CALL HelpMe(0, "HOLIDAY", 1, 2, 25, 79)
         CASE F10        'F10  quit
            EXIT DO
      END SELECT
   LOOP
   'BE SURE THAT A HOLIDAY IS NOT SELECTED WHICH HAS A BAD DATE
'   FOR j% = 1 TO MaxHolidays
'     IF ((Holiday(j%).Day) <= 0) OR ((Holiday(j%).Month) <= 0) THEN
'        Holiday(j%).Selected = False
'        Holiday(j%).Month = 0
'        Holiday(j%).Day = 0
'        Holiday(j%).Year = 0
'        Holiday(j%).HName = ""
'     END IF
'   NEXT j%
   HolidayCount% = 0
   FOR j% = 1 TO MaxHolidays
     IF Holiday(j%).Selected <> 0 THEN
        HolidayCount% = HolidayCount% + 1
     END IF
   NEXT j%
   'RESET THE DATES FOR ALL HOLIDAYS
   FOR j% = 1 TO MaxHolidays
      Holiday(j%).CrescDate = CrescentDate%(Holiday(j%).Day, Holiday(j%).Month, Holiday(j%).Year)
   NEXT j%
   'SORT THE HOLIDAYS BY DATE
   DO
     sorted% = True
     FOR jj% = 1 TO MaxHolidays - 1
        IF Holiday(jj%).CrescDate > Holiday(jj% + 1).CrescDate AND Holiday(jj% + 1).CrescDate > 0 THEN  'dates should be switched
           IF Holiday(jj%).CrescDate > 0 AND Holiday(jj% + 1).CrescDate > 0 THEN
              'both dates are valid dates, this keeps invalid dates at bottom of list
              sorted% = False
           ELSEIF Holiday(jj%).CrescDate <0 AND Holiday(jj% + 1).CrescDate > 0 THEN
              'first date invalid, this keeps invalid dates at bottom of list
              sorted% = False
           END IF
           IF sorted% = False THEN
              SWAP Holiday(jj%).Selected, Holiday(jj% + 1).Selected
              SWAP Holiday(jj%).Month, Holiday(jj% + 1).Month
              SWAP Holiday(jj%).Day, Holiday(jj% + 1).Day
              SWAP Holiday(jj%).Year, Holiday(jj% + 1).Year
              SWAP Holiday(jj%).HName, Holiday(jj% + 1).HName
              SWAP Holiday(jj%).CrescDate, Holiday(jj% + 1).CrescDate
           END IF
        END IF
     NEXT jj%
   LOOP UNTIL sorted% = True
   HideCursor
   CALL WindowMgr(0, 0, 0, 0, 0, 0, 0)
   ShowCursor
END SUB 'EditHoliday

FUNCTION GetAKey$
   Dummy$ = ""
   Button% = False
   IF CNF.Mouse THEN 'MOUSE IS PRESENT
      'GET A KEYBOARD ENTRY BUT CHECK FOR MOUSE ACTIVITY
      'AND EXIT IF IT HAS BEEN PRESSED
      DO
         CALL DisplayTime
         Dummy$ = INKEY$
         GetCursor MCol%, MRow%, Button%
      LOOP UNTIL LEN(Dummy$) OR (Button% AND 1)       'wait for a keypress
   ELSE
      DO
         CALL DisplayTime
         Dummy$ = INKEY$
      LOOP UNTIL LEN(Dummy$)        'wait for a keypress
   END IF
   GetAKey$ = Dummy$
END FUNCTION

SUB GetHelpText (LookUp$, HelpLines%, Widest%)

'???
   HelpLines% = 0
'   REDIM HelpText$(MaxHelpLines)
   FOR i% = 1 TO MaxHelpLines
      HelpText$(i%) = ""
   NEXT i%

   FOR i% = 1 TO MaxHelpCalls
      IF UCASE$(RTRIM$(HelpSpot(i%).LookUp)) = UCASE$(RTRIM$(LookUp$)) THEN
         HelpLines% = HelpSpot(i%).lines
         IF HelpLines% > MaxHelpLines THEN HelpLines% = MaxHelpLines
         HelpText$(0) = RTRIM$(HelpSpot(i%).Label)
         SEEK #HelpFileNum%, HelpSpot(i%).Byte
         FOR K% = 1 TO HelpLines%
            LINE INPUT #HelpFileNum%, HelpText$(K%)
            HelpText$(K%) = QPTRIM$(HelpText$(K%))
            'MAY WANT TO DO WORD WRAPPING HERE BASED ON USER SELECTED
            'WINDOW WIDTH RATHER THAT TRUNCATING AS IS CURRENTLY DONE
            IF LEN(HelpText$(K%)) > Widest% THEN
               IF LEN(HelpText$(K%)) > MaxHelpWidth THEN
                  HelpText$(K%) = LEFT$(HelpText$(K%), MaxHelpWidth)
               END IF
               Widest% = LEN(HelpText$(K%))
            END IF
         NEXT K%
         EXIT FOR
      END IF
   NEXT i%

END SUB 'GetHelpText

SUB GetHolidays

   Row%(1) = 14
   Row%(2) = 15
   Row%(3) = 16
   Row%(4) = 17
   Row%(5) = 18
   Row%(6) = 19
   Row%(7) = 20
   Pointer% = 1
   LastPointer% = 1
   RePrint% = True
   RePrintHolidays% = True
   'CALL SetHolidays

   HolidayCount% = 0
   FOR j% = 1 TO MaxHolidays
      IF Holiday(j%).Selected <> False THEN
         HolidayCount% = HolidayCount% + 1
      END IF
   NEXT j%

   HideCursor
   CALL WindowMgr(0, 1, 1, 1, 25, 80, CNF.Normal)     'open next available window
   ShowCursor

   DO
      IF RePrint% THEN
         StartDate$ = RTRIM$(MonthData(StartMonth%).Mname) + STR$(StartDay%) + "," + STR$(StartYear%)
         StopDate$ = RTRIM$(MonthData(StopMonth%).Mname) + STR$(StopDay%) + "," + STR$(StopYear%)

         CALL QPrintRC("ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป", 1, 1, CNF.Normal)
         CALL QPrintRC("บ                                                                              บ", 2, 1, CNF.Normal)
         CALL QPrintRC("บ                                                                              บ", 3, 1, CNF.Normal)
         CALL QPrintRC("บ  You need to select the Holidays or special days (fishing derbies, etc.)     บ", 4, 1, CNF.Normal)
         CALL QPrintRC("บ  you want to include in the creel survey.  These days will be included       บ", 5, 1, CNF.Normal)
         CALL QPrintRC("บ  in the sampling schedule depending on the option selected for each holiday. บ", 6, 1, CNF.Normal)
         CALL QPrintRC("ฬอออออออออออออออออออออออัออออออออออออออออหอออออออออออออออัอออออออออออออออออออออน", 7, 1, CNF.Normal)
         CALL QPrintRC("บ Count Strata          ณ                บ  Start Date   ณ                     บ", 8, 1, CNF.Normal)
         CALL QPrintRC(Strata$, 8, 27, CNF.Normal)
         CALL QPrintRC(StartDate$, 8, 60, CNF.Normal)
         CALL QPrintRC("บ Weekdays / Strata     ณ      " + FUsing(STR$(WDperStrata%), "##") + "        บ  End Date     ณ                     บ", 9, 1, CNF.Normal)
         CALL QPrintRC(StopDate$, 9, 60, CNF.Normal)
         CALL QPrintRC("บ Weekend Days / Strata ณ      " + FUsing(STR$(WEperStrata%), "##") + "        บ  Counts / day ณ         " + FUsing(STR$(Counts%), "##") + "          บ", 10, 1, CNF.Normal)
         CALL QPrintRC("ฬออออออออออัออออออออัอออฯออออัออออออออัออสอออออออออออออออฯอออออออออออออออออออออน", 11, 1, CNF.Normal)
         CALL QPrintRC("บ  Option  ณ Month  ณ  Day   ณ  Year  ณ              Holiday                   บ", 12, 1, CNF.Normal)
         CALL QPrintRC("วฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ", 13, 1, CNF.Normal)
           '                     1         2         3         4         5         6         7         8
           '            12345678901234567890123456789012345678901234567890123456789012345678901234567890
'?? need to add holiday year to this subroutine
         CALL QPrintRC("บ          ณ        ณ        ณ        ณ                                        บ", 14, 1, CNF.Normal)
         CALL QPrintRC("บ          ณ        ณ        ณ        ณ                                        บ", 15, 1, CNF.Normal)
         CALL QPrintRC("บ          ณ        ณ        ณ        ณ                                        บ", 16, 1, CNF.Normal)
         CALL QPrintRC("บ          ณ        ณ        ณ        ณ                                        บ", 17, 1, CNF.Normal)
         CALL QPrintRC("บ          ณ        ณ        ณ        ณ                                        บ", 18, 1, CNF.Normal)
         CALL QPrintRC("บ          ณ        ณ        ณ        ณ                                        บ", 19, 1, CNF.Normal)
         CALL QPrintRC("บ          ณ        ณ        ณ        ณ                                        บ", 20, 1, CNF.Normal)
         CALL QPrintRC("ฬออออออออออฯออออออออฯออออออออฯออออออออฯออออออออออออออออออออออออออออออออออออออออน", 21, 1, CNF.Normal)
'                       12345678901234567890123456789012345678901234567890123456789012345678901234567890
'                                1         2         3         4         5         6         7         8
         CALL QPrintRC("บ   Use the Arrrow Keys to move between entries, F4 to Edit or Add a holiday,  บ", 22, 1, CNF.Normal)
         CALL QPrintRC("บ         F1 for Help, F5 to view calendars for the year, F10 to quit.         บ", 23, 1, CNF.Normal)
         CALL QPrintRC("บ             You should select a blank line to add a new holiday.             บ", 24, 1, CNF.Normal)
         CALL QPrintRC("ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ", 25, 1, CNF.Normal)
         CALL QPrintRC("F1", 23, 11, CNF.HiLite)
         CALL QPrintRC("F4", 22, 50, CNF.HiLite)
         CALL QPrintRC("F5", 23, 24, CNF.HiLite)
         CALL QPrintRC("F10", 23, 59, CNF.HiLite)
         CALL QPrintRC(Buffer$ + MainTitle$, 2, 2, CNF.HiLite)
         RePrint% = False
         RePrintHolidays% = True
      END IF

      IF TopHoliday% > 1 THEN
         CALL QPrintRC("", 13, 12, CNF.HiLite)
         CALL QPrintRC("", 13, 21, CNF.HiLite)
         CALL QPrintRC("", 13, 30, CNF.HiLite)
         CALL QPrintRC("", 13, 39, CNF.HiLite)
      ELSE
         CALL QPrintRC("ล", 13, 12, CNF.Normal)
         CALL QPrintRC("ล", 13, 21, CNF.Normal)
         CALL QPrintRC("ล", 13, 30, CNF.Normal)
         CALL QPrintRC("ล", 13, 39, CNF.Normal)
      END IF
      IF TopHoliday% + 6 < HolidayCount% THEN
         CALL QPrintRC("", 21, 12, CNF.HiLite)
         CALL QPrintRC("", 21, 21, CNF.HiLite)
         CALL QPrintRC("", 21, 30, CNF.HiLite)
         CALL QPrintRC("", 21, 39, CNF.HiLite)
      ELSE
         CALL QPrintRC("ฯ", 21, 12, CNF.Normal)
         CALL QPrintRC("ฯ", 21, 21, CNF.Normal)
         CALL QPrintRC("ฯ", 21, 30, CNF.Normal)
         CALL QPrintRC("ฯ", 21, 39, CNF.Normal)
      END IF

'need to figure this out
      IF RePrintHolidays% THEN
         FOR j% = 1 TO HolidayCount%
            IF Holiday(j%).Selected OR Holiday(j%).Month OR Holiday(j%).Day THEN
               CALL QPrintRC("บ    " + FUsing(STR$(Holiday(j%).Selected), "##") + "    ณ   " + FUsing(STR$(Holiday(j%).Month), "##") + "   ณ   " + FUsing(STR$(Holiday(j%).Day), "##") + "   ณ  " + FUsing(STR$(Holiday(j%).Year), "####") +  _
"  ณ                                        บ", j% + 13, 1, CNF.Normal)
               CALL QPrintRC(Holiday(j%).HName, j% + 13, 41, CNF.Normal)
            ELSE
               CALL QPrintRC("บ    " + FUsing(STR$(Holiday(j%).Selected), "##") + "    ณ        ณ        ณ        ณ                                        บ", j% + 13, 1, CNF.Normal)
            END IF
         NEXT j%
         RePrintHolidays% = False
      END IF
      IF Holiday(LastPointer%).Selected OR Holiday(LastPointer%).Month * Holiday(LastPointer%).Day > 0 THEN
         CALL QPrintRC("บ    " + FUsing(STR$(Holiday(LastPointer%).Selected), "##") + "    ณ   " + FUsing(STR$(Holiday(LastPointer%).Month), "##") + "   ณ   " + FUsing(STR$(Holiday(LastPointer%).Day), "##") + "   ณ  " + FUsing(STR$(Holiday( _
LastPointer%).Year), "####") + "  ณ                                        บ", Row%(LastPointer%), 1, CNF.Normal)
         CALL QPrintRC(Holiday(LastPointer%).HName, Row%(LastPointer%), 41, CNF.Normal)
      ELSE
         CALL QPrintRC("บ    " + FUsing(STR$(Holiday(LastPointer%).Selected), "##") + "    ณ        ณ        ณ        ณ                                        บ", Row%(LastPointer%), 1, CNF.Normal)
      END IF
      IF Holiday(Pointer%).Selected OR Holiday(Pointer%).Month * Holiday(Pointer%).Day > 0 THEN
         CALL QPrintRC("บ    " + FUsing(STR$(Holiday(Pointer%).Selected), "##") + "    ณ   " + FUsing(STR$(Holiday(Pointer%).Month), "##") + "   ณ   " + FUsing(STR$(Holiday(Pointer%).Day), "##") + "   ณ  " + FUsing(STR$(Holiday(Pointer%).Year),  _
"####") + "  ณ                                        บ", Row%(Pointer%), 1, CNF.Revers)
         CALL QPrintRC(Holiday(Pointer%).HName, Row%(Pointer%), 41, CNF.Revers)
      ELSE
             CALL QPrintRC("บ    " + FUsing(STR$(Holiday(Pointer%).Selected), "##") + "    ณ        ณ        ณ        ณ                                        บ", Row%(Pointer%), 1, CNF.Revers)
      END IF
      Ch$ = GetAKey$
      KeyCode% = KeyPressed%(Ch$)

      'MMMMMMMMMMMMMMMM Start of Mouse Handling Code MMMMMMMMMMMMMMMMMMM
      IF CNF.Mouse THEN
         CALL ButtonPress(1, Down%, Presses%, MCol%, MRow%)
         Button1% = False
         IF Presses% THEN Button1% = True

         CALL GetCursor(MCol%, MRow%, Button%)
         MRow% = (MRow% \ CNF.NPixLines) + 1
         MCol% = (MCol% \ 8) + 1

         IF MRow% <> LMRow% OR MCol% <> LMCol% OR Presses% THEN
            'CURSOR WAS PRESSED OR MOUSE WAS MOVED SINCE LAST CHECK
            'DETERMINE IF CURSOR IS WITHIN ACCEPTABLE DATA ENTRY BOX
            'OR IF ONE OF THE SPECIAL AREAS IS HIGHLIGHTED
            IF MRow% = 22 AND Button1% AND (MCol% = 50 OR MCol% = 51) THEN
                  KeyCode% = F1
            ELSEIF MRow% = 23 AND Button1% THEN
               'MOUSE SELECTED FUNCTION KEY ACTION
               SELECT CASE MCol%
                  CASE 11, 12
                     KeyCode% = F1
                  CASE 24, 25
                     KeyCode% = F5
                  CASE 59, 60, 61
                     KeyCode% = F10
                  CASE ELSE
                     CALL OOPS
               END SELECT
            ELSEIF MRow% >= 14 AND MRow% <= 20 AND Button1% THEN
'?? this may be wrong
               KeyCode% = 0
               LastPointer% = Pointer%
               Pointer% = MRow% - 13
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
         SELECT CASE KeyCode%
            CASE LeftArrow, UpArrow
               LastPointer% = Pointer%
               SELECT CASE LastPointer%
                  CASE 1
                     Pointer% = 7
                  CASE ELSE
                     Pointer% = Pointer% - 1
               END SELECT
            CASE RightArrow, DownArrow
               LastPointer% = Pointer%
               SELECT CASE LastPointer%
                  CASE 7
                     Pointer% = 1
                  CASE ELSE
                     Pointer% = Pointer% + 1
               END SELECT
'?
         CASE PageDown
            TopHoliday% = TopHoliday% + 1
            IF TopHoliday% + 7 > MaxHolidays THEN
               TopHoliday% = MaxHolidays - 7
               IF TopHoliday% < 1 THEN TopHoliday% = 1
            ELSE
               HideCursor
               CALL ScrollU(14, 2, 20, 79, 1, -1)
               Row% = 20
            '?   GOSUB PrintHolRow
               ShowCursor
               SELECT CASE LastRow%
                  CASE 11
                     RowPoint% = 11
                  CASE ELSE
                     RowPoint% = RowPoint% - 1
               END SELECT
            END IF
         CASE PageUp
            TopHoliday% = TopHoliday% - 1
            IF TopHoliday% < 1 THEN
               TopHoliday% = 1
            ELSE
               HideCursor
               CALL ScrollD(14, 2, 20, 79, 1, -1)
               Row% = 11
            '?   GOSUB PrintHolRow
               ShowCursor
               SELECT CASE LastRow%
                  CASE 20
                     RowPoint% = 20
                  CASE ELSE
                     RowPoint% = RowPoint% + 1
               END SELECT
            END IF
         CASE CtrlHome
            TopHoliday% = 1
            IF TotalStrata% - TopHoliday% < 8 THEN
               BottomRow% = TotalStrata% - TopHoliday% + 11
            END IF
            NewStrata% = True
         CASE CtrlEnnd
            TopHoliday% = TotalStrata% - 9
            IF TopHoliday% < 1 THEN TopHoliday% = 1
            BottomRow% = 20
            NewStrata% = True

            CASE F1        'HELP
               CALL HelpMe(0, "HOLIDAY", 1, 2, 25, 79)
            CASE F4        'F4   Edit a line
               CALL EditHoliday(Pointer%, 15)
               RePrintHolidays% = True
               HolidayCount% = 0
               FOR j% = 1 TO MaxHolidays
                  IF Holiday(j%).Selected <> False THEN
                     HolidayCount% = HolidayCount% + 1
                  END IF
               NEXT j%
             CASE F5        'F5   view calendar for any month
               CALL ViewMonths(StartYear%, StopYear%)
            CASE F10        'F10  quit
               EXIT DO
         END SELECT
      END IF
   LOOP

   HideCursor
   CALL WindowMgr(0, 0, 0, 0, 0, 0, 0)
   ShowCursor
   
END SUB 'GetHolidays

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
'  Routine to display help text.
'
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
SUB HelpMe (Action%, LookUp$, ULRow%, ULCol%, LRRow%, LRCol%) STATIC

   IF Action% = 0 THEN
      FULCol% = ULCol%
      FULRow% = ULRow%
      FLRCol% = LRCol%
      FLRRow% = LRRow%
      FirstLookUp$ = LookUp$
   END IF

   CALL GetHelpText(LookUp$, HelpLines%, Widest%)

   HideCursor
   CALL WindowMgr(0, 1, ULRow%, ULCol%, LRRow%, LRCol%, CNF.Revers)

   CALL QPrintRC("ศ", LRRow% - 1, ULCol%, CNF.Revers)
   CALL QPrintRC(STRING$(LRCol% - ULCol% - 1, "อ"), LRRow% - 1, ULCol% + 1, CNF.Revers)
   CALL QPrintRC("ผ", LRRow% - 1, LRCol%, CNF.Revers)

   IF Widest% > 76 OR Widest% < 1 THEN
      LeftMargin% = 3
   ELSE
      LeftMargin% = INT(40 - (Widest% / 2)) + 1
   END IF

   HelpDone% = False

   TopPointer% = 1
   ScreenFull% = LRRow% - ULRow% - 2 'TOP & BOTTOM FRAME + INSTRUCTIONS
   IF HelpLines% > ScreenFull% THEN
      BottomPointer% = ScreenFull%
   ELSE
      BottomPointer% = HelpLines%
   END IF

  IF HelpLines% = 0 THEN
     CALL QPrintRC("No Help is Available For HELP LookUp key = " + LookUp$, ULRow% + 2, 14, CNF.Revers)
     FOR i% = 1 TO HelpNum%
        CALL QPrintRC("----------------------------------------------------------------------", ULRow% + 3 + i%, 5, CNF.Revers)
        CALL QPrintRC(HelpSpot(i%).LookUp, ULRow% + 3 + i%, 7, CNF.Revers)
        CALL QPrintRC(HelpSpot(i%).Label, ULRow% + 3 + i%, 20, CNF.Revers)
'        LOCATE , 5: PRINT "----------------------------------------------------------------------";
'        LOCATE , 7: PRINT HelpSpot(I%).LookUp;
'        LOCATE , 20: PRINT HelpSpot(I%).Label
     NEXT i%
     CALL QPrintRC("Press any key to continue", LRRow% - 2, 28, CNF.Revers)
'     LOCATE LRRow% - 2, 28: PRINT "Press any key to continue"
     c$ = GetAKey$
     HideCursor
     CALL WindowMgr(0, 0, 0, 0, 0, 0, 0)
     ShowCursor
     'window close
     EXIT SUB
  END IF
 
  'define the bounds for the scrolling area - just inside frame defined above
  LCol% = 3                 'left column
  TRow% = ULRow% + 1       'top row
  RCol% = 78                'right column
  BRow% = LRRow% - 2    'bottom row without moving instructions
                             
  RePrint% = True
  LOCATE , , 0

  CALL QPrintRC(HelpText$(0), ULRow%, 10, CNF.Revers)
  IF HelpLines% > ScreenFull% THEN
     CALL QPrintRC("Use  or PgUp/PgDn to Scroll; Home for beginning; End to end. Escape to Quit ", LRRow%, 2, CNF.NonMen)
  ELSE
     CALL QPrintRC("                             Press Escape to Quit                             ", LRRow%, 2, CNF.NonMen)
  END IF

   InitMouse Temp%                         'Define the Mouse cursor
   TextCursor -2, -2                       'set mouse colors to inverse
   ShowCursor                              'Turn it on
  
  DO
      IF HelpLines% > ScreenFull% THEN
         IF TopPointer% > 1 THEN
            CALL QPrintRC(" More: UpArrow, PgUp ", ULRow%, 57, CNF.Normal)
            CALL QPrintRC("", ULRow%, ULCol%, CNF.HiLite)
            CALL QPrintRC("", ULRow%, LRCol%, CNF.HiLite)
         ELSE
            CALL QPrintRC("   Top of Help Text  ", ULRow%, 57, CNF.Normal)
            CALL QPrintRC("ษ", ULRow%, ULCol%, CNF.Revers)
            CALL QPrintRC("ป", ULRow%, LRCol%, CNF.Revers)
         END IF
         IF BottomPointer% < HelpLines% THEN
            CALL QPrintRC(" More: DnArrow, PgDn ", LRRow% - 1, 57, CNF.Normal)
            CALL QPrintRC("", LRRow% - 1, ULCol%, CNF.HiLite)
            CALL QPrintRC("", LRRow% - 1, LRCol%, CNF.HiLite)
         ELSE
            CALL QPrintRC(" Bottom of Help Text ", LRRow% - 1, 57, CNF.Normal)
            CALL QPrintRC("ศ", LRRow% - 1, ULCol%, CNF.Revers)
            CALL QPrintRC("ผ", LRRow% - 1, LRCol%, CNF.Revers)
         END IF
      END IF

     IF RePrint% THEN
        HideCursor
        IF HelpLines% > ScreenFull% THEN
           Row% = ULRow% + 1
        ELSE
           Row% = ULRow% + INT((ScreenFull% - HelpLines%) / 2)
        END IF
        FOR i% = TopPointer% TO BottomPointer%
           CALL QPrintRC(HelpText$(i%), Row%, LeftMargin%, CNF.Revers)
'           CALL QPrintRC("บ", Row%, 78, CNF.Revers)
           Row% = Row% + 1
        NEXT i%
        ShowCursor
        RePrint% = False
     END IF

     Ch$ = GetAKey$ 'FUNCTION USED TO WAIT FOR KEYBOARD INPUT OF LENGTH=1 or 2
     KeyCode% = KeyPressed%(Ch$)

      'MMMMMMMMMMMMMMMM Start of Mouse Handling Code MMMMMMMMMMMMMMMMMMM
      IF CNF.Mouse THEN
         CALL ButtonPress(1, Down%, Presses%, MCol%, MRow%)
         Button1% = False
         IF Presses% THEN Button1% = True

       '  CALL GetCursor(MCol%, MRow%, Button%)
         MRow% = (MRow% \ CNF.NPixLines) + 1
         MCol% = (MCol% \ 8) + 1

'locate 1,1:print using "MRow=## MCol=## ULRow=##  LRRow=##";MRow%; MCol%;ULRow%; LRRow%

         IF MRow% <> LMRow% OR MCol% <> LMCol% OR Presses% THEN
            'CURSOR WAS PRESSED OR MOUSE WAS MOVED SINCE LAST CHECK
            'DETERMINE IF CURSOR IS WITHIN ACCEPTABLE DATA ENTRY BOX
            'OR IF ONE OF THE SPECIAL AREAS IS HIGHLIGHTED
            IF MRow% = ULRow% AND Button1% THEN
               'MOUSE SELECTED FUNCTION KEY ACTION
               SELECT CASE MCol%
                  CASE ULCol%, LRCol%  '2, 79
                     KeyCode% = UpArrow
                  CASE ELSE
                     CALL OOPS
               END SELECT
            ELSEIF MRow% = LRRow% - 1 AND Button1% THEN
               'MOUSE SELECTED FUNCTION KEY ACTION
               SELECT CASE MCol%
                  CASE ULCol%, LRCol%  '2, 79
                     KeyCode% = DownArrow
                  CASE ELSE
                     CALL OOPS
               END SELECT
            ELSEIF MRow% = LRRow% AND Button1% THEN
               'MOUSE SELECTED FUNCTION KEY ACTION
               SELECT CASE MCol%
                  CASE 6
                     KeyCode% = UpArrow
                  CASE 7
                     KeyCode% = DownArrow
                  CASE 12, 13, 14, 15
                     KeyCode% = PageUp
                  CASE 17, 18, 19, 20
                     KeyCode% = PageDown
                  CASE 33, 34, 35, 36
                     KeyCode% = Home
                  CASE 53, 54, 55
                     KeyCode% = Ennd
                  CASE 65, 66, 67, 68, 69, 70
                     KeyCode% = Escape
                  CASE ELSE
                     CALL OOPS
               END SELECT
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

      IF KeyCode% < 0 OR KeyCode% = Escape THEN
         SELECT CASE KeyCode%
            CASE F1
               IF Action% <> 1 THEN
                  HideCursor
                  CALL HelpOnHelp
                  ShowCursor
                  'Recall initial help text
                  CALL GetHelpText(FirstLookUp$, HelpLines%, Widest%)
                  'Recall initial box dimensions
                  ULCol% = FULCol%
                  ULRow% = FULRow%
                  LRCol% = FLRCol%
                  LRRow% = FLRRow%
                  'define the bounds for the scrolling area - just inside frame defined above
                  LCol% = 3                 'left column
                  TRow% = ULRow% + 1        'top row
                  RCol% = 78                'right column
                  BRow% = LRRow% - 2        'bottom row without moving instructions
                  HelpDone% = False
                  LOCATE , , 0
               END IF
            CASE Escape, F10  'RETURN TO MAIN PROGRAM
               HelpDone% = True
            CASE DownArrow, RightArrow
               IF BottomPointer% < HelpLines% THEN
                  HideCursor
                  CALL ScrollU(TRow%, LCol%, BRow%, RCol% - 1, 1, -1)
                  BottomPointer% = BottomPointer% + 1
                  TopPointer% = TopPointer% + 1
                  CALL QPrintRC(HelpText$(BottomPointer%), LRRow% - 2, LeftMargin%, CNF.Revers)
    '              CALL QPrintRC("บ", LRRow% - 2, 78, CNF.Revers)
                  ShowCursor
               ELSE
                  CALL OOPS
               END IF
            CASE UpArrow, LeftArrow
               IF TopPointer% > 1 THEN
                  HideCursor
                  CALL ScrollD(TRow%, LCol%, BRow%, RCol% - 1, 1, -1)
                  TopPointer% = TopPointer% - 1
                  BottomPointer% = BottomPointer% - 1
                  CALL QPrintRC(HelpText$(TopPointer%), ULRow% + 1, LeftMargin%, CNF.Revers)
    '              CALL QPrintRC("บ", ULRow% + 1, 78, CNF.Revers)
                  ShowCursor
               ELSE
                  CALL OOPS
               END IF
            CASE PageDown
               IF BottomPointer% < HelpLines% THEN
                  RePrint% = True
                  HideCursor
                  CALL ClearScr(TRow%, LCol%, BRow%, RCol% - 1, CNF.Revers, -1)
                  ShowCursor
                  TopPointer% = TopPointer% + ScreenFull%
                  BottomPointer% = BottomPointer% + ScreenFull%
                  IF BottomPointer% > HelpLines% THEN
                     BottomPointer% = HelpLines%
                     TopPointer% = BottomPointer% - ScreenFull% + 1
                  END IF
               ELSE
                  CALL OOPS
               END IF
            CASE PageUp
               IF TopPointer% > 1 THEN
                  RePrint% = True
                  HideCursor
                  CALL ClearScr(TRow%, LCol%, BRow%, RCol% - 1, CNF.Revers, -1)
                  ShowCursor
                  TopPointer% = TopPointer% - ScreenFull%
                  BottomPointer% = BottomPointer% - ScreenFull%
                  IF TopPointer% < 1 THEN
                     TopPointer% = 1
                     BottomPointer% = ScreenFull%
                  END IF
               ELSE
                  CALL OOPS
               END IF
            CASE Home
               RePrint% = True
               HideCursor
               CALL ClearScr(TRow%, LCol%, BRow%, RCol% - 1, CNF.Revers, -1)
               ShowCursor
               TopPointer% = 1
               BottomPointer% = ScreenFull%
               IF BottomPointer% > HelpLines% THEN BottomPointer% = HelpLines%
            CASE Ennd
               RePrint% = True
               HideCursor
               CALL ClearScr(TRow%, LCol%, BRow%, RCol% - 1, CNF.Revers, -1)
               ShowCursor
               BottomPointer% = HelpLines%
               TopPointer% = BottomPointer% - ScreenFull% + 1
               IF TopPointer% < 1 THEN TopPointer% = 1
            CASE ELSE 'some other inappropriate extended character key
               CALL OOPS
         END SELECT
      END IF
   LOOP UNTIL HelpDone%

   HideCursor
   CALL WindowMgr(0, 0, 0, 0, 0, 0, 0)
   InitMouse Temp%                         'Define the Mouse cursor
   TextCursor -2, -2                       'set mouse colors to inverse
   ShowCursor                              'Turn it on

END SUB

SUB HelpOnHelp

   REDIM Items$(HelpNum%)                    'Dim the "Items$" array
   FOR i% = 1 TO HelpNum%
      Items$(i%) = HelpSpot(i%).Label
   NEXT i%

   '----- Print instructions
   HideCursor
   CALL WindowMgr(0, 1, 1, 15, 25, 65, CNF.PulBar)
   LOCATE , , 0                            'Turn the cursor off
   CALL QPrintRC("HELP INDEX", 3, 35, CNF.PulBar)
   CALL QPrintRC("Enter to view highlighted Help Text", 5, 22, CNF.HiLite)
   CALL QPrintRC("[ESC] to Quit", 6, 34, CNF.HiLite)

   MaxLen = LEN(HelpSpot(i%).Label)      'this is the menu width
   BoxBot = 23             'limit the box length to go no lower than line 20
   Action = 0              '0 means stay in the menu until they select something

HelpAgain:

   LOCATE 8, 18, 0          'set upper left corner of menu, turn off the cursor
   ShowCursor
   CALL VertMenu(Items$(), Choice, MaxLen, BoxBot, Ky$, Action)

   KeyCode% = KeyPressed%(Ky$)
   '----- Print selected help choice
   IF Choice AND KeyCode% <> Escape THEN
      CALL HelpMe(1, HelpSpot(Choice).LookUp, 2, 2, 25, 79)
      GOTO HelpAgain
   END IF

   HideCursor
   CALL WindowMgr(0, 0, 0, 0, 0, 0, 0)
   LOCATE , , 0                            'Turn the cursor off
   ShowCursor

END SUB

'
' This routine is called once to read in the help file and establish
' the lookup keys and file offsets for subsequent help calls
'
SUB HelpSetup (FileName$)

   IF NOT Exist%(FileName$) THEN EXIT SUB
   HelpFileNum% = FREEFILE
   OPEN FileName$ FOR INPUT AS #HelpFileNum%
   NumberOfLines% = 0
   HelpNum% = 0
   DO
      LINE INPUT #HelpFileNum%, NextLine$
      IF LEFT$(NextLine$, 3) = "@#@" THEN
         HelpSpot(HelpNum%).lines = NumberOfLines%
         NumberOfLines% = 0
         HelpNum% = HelpNum% + 1
         HelpSpot(HelpNum%).Byte = SEEK(HelpFileNum%)
         HelpSpot(HelpNum%).LookUp = MID$(NextLine$, 4, 8)
         HelpSpot(HelpNum%).Label = RIGHT$(NextLine$, LEN(NextLine$) - 12)
      ELSE
         NumberOfLines% = NumberOfLines% + 1
      END IF
   LOOP UNTIL EOF(HelpFileNum%)
   HelpSpot(HelpNum%).lines = NumberOfLines%
END SUB

FUNCTION IsBlank% (TestStr$)
   AllBlanks% = True
   FOR j% = 1 TO LEN(TestStr$)
      IF MID$(TestStr$, j%, 1) <> " " AND ASC(MID$(TestStr$, j%, 1)) > 0 THEN
         AllBlanks% = False
         EXIT FOR
      END IF
   NEXT j%
   IsBlank% = AllBlanks%

'PRINT USING "IsBlank =###"; AllBlanks%
'PRINT "press a key"
'c$ = INPUT$(1)

END FUNCTION

'
' ====================== ISLEAPYEAR ==========================
'         Determines if a year is a leap year or not.
' ============================================================
'
FUNCTION IsLeapYear (N%) STATIC

   ' If the year is evenly divisible by 4 and not divisible
   ' by 100, or if the year is evenly divisible by 400, then
   ' it's a leap year:
   IsLeapYear = (N% MOD 4 = 0 AND N% MOD 100 <> 0) OR (N% MOD 400 = 0)
END FUNCTION

FUNCTION KeyPressed% (Ch$)
   IF LEN(Ch$) = 1 THEN         'create a key code
      KeyPressed% = ASC(Ch$)       'regular character key
   ELSEIF LEN(Ch$) = 2 THEN     'extended key
      KeyPressed% = -ASC(RIGHT$(Ch$, 1))
   ELSE
      KeyPressed% = 0
   END IF
END FUNCTION

SUB OOPS
   'easier sounding
'   Call Chime(2)
'   CALL Chime(7)
   CALL Chime(8)
'   CALL Chime(9)
   'harsher sounding
'   Call Chime(4)
'   Call Chime(10)
END SUB

'********** QPROUND
'
'This function accepts a double precision value and a number of decimal
'places to round it to, and returns the answer in a string.  The number
'must be processed as a string because many double precision values can
'not be represented accurately any other way.  To prove this, enter the
'following line into the QuickBASIC 4 editor and watch what happens:
'
'    X# = .0691#
'
'
FUNCTION QPROUND$ (Amount#, Places%) STATIC

'External routines required:  ASCII%
'                             FUsing

    Amount$ = STR$(Amount#)                     'first convert into a string
   
    '10-15-91: The four lines below that are now comments have been replaced
    'with the three lines that follow.  This makes QPROUND$ nearly 7 times
    'faster.  Thanks to Peter Alachi for this improvement!

    'Lead% = 1                          'allow one leading digit
    'DO WHILE ABS(Amount#) + .000001# >= 10# ^ Lead%
    '   Lead% = Lead% + 1               'the .000001# avoids a QB bug w/80x87
    'LOOP

    Lead = INSTR(LTRIM$(Amount$), ".")  'get the number of leading digits
    IF Lead > 1 THEN Lead = Lead - 1
    IF Lead = 0 THEN Lead = LEN(LTRIM$(Amount$))

    Dec$ = MID$(".", ABS(Places% = 0) + 1)      'make a "." if needed
    Plus$ = MID$("+", SGN(Amount#) + 2)         'make a "+" if needed
    Mask$ = Plus$ + STRING$(Lead% + 1, "#") + Dec$ + STRING$(Places%, "#") '+1
    Mask$ = FUsing$(Amount$, Mask$)             'let Chris do the dirty work

    IF ASCII%(Mask$) = 48 THEN                  'if there's a leading zero
       Mask$ = MID$(Mask$, 2)                   'strip it
    END IF

    QPROUND$ = LTRIM$(Mask$)                    'assign the function

END FUNCTION

SUB ReadWrite (WhyCalled%, RW%, WDperStrata%, WEperStrata%, Strata$, TotalStrata%, DataFileName$)

   HideCursor
   CALL WindowMgr(0, 1, 1, 1, 25, 80, CNF.Normal)     'open next available window
   ShowCursor
RestartFileName:
   FileNameFinished% = False
   IF DataFileName$ = "" THEN DataFileName$ = "CREELCAL.DAT"
   IF WhyCalled% = 1 THEN
      IF LEFT$(DataFileName$, 1) = "{" THEN DataFileName$ = "CREELCAL.DAT"
      DO
         IF RW% THEN
            CALL QPrintRC("Enter the name of the file to read the data from", 10, 10, CNF.Normal)
         ELSE
            CALL QPrintRC("Enter the name of the file to write the data to", 10, 10, CNF.Normal)
         END IF
         CALL QPrintRC("Filename =", 12, 15, CNF.Normal)
'                             1         2         3         4         5         6         7
'                    1234567890123456789012345678901234567890123456789012345678901234567890
         CALL QPrintRC("F4 to pick from a list of the files in the default directory, F1 for Help,", 20, 4, CNF.Normal)
         CALL QPrintRC("F10 to proceed with the filename shown, ESC to return to menu.", 21, 9, CNF.Normal)
         CALL QPrintRC("F4", 20, 4, CNF.HiLite)
         CALL QPrintRC("F1", 20, 66, CNF.HiLite)
         CALL QPrintRC("F10", 21, 9, CNF.HiLite)
         CALL QPrintRC("ESC", 21, 49, CNF.HiLite)
'         LOCATE 12, 27: PRINT USING " \          \"; DataFileName$
         CALL QPrintRC(" " + DataFileName$, 12, 27, CNF.Revers)
         LeftCol% = 27                     'set the left column
         RightCol% = 39                   'and the right column
         ValidInput$ = OKforFileName$
         CALL Editor(DataFileName$, 12, LeftCol%, RightCol%, Ch$, ValidInput$, CNF.Revers)
         KeyCode% = KeyPressed%(Ch$)
         IF KeyCode% = Escape THEN GOTO GetOut
         IF KeyCode% < 0 THEN
            SELECT CASE KeyCode%
               CASE F4   'VIEW LIST OF FILES ON CURRENT DIRECTORY
                  CALL PickFileList(DummyFileName$)
                  IF DummyFileName$ <> "" THEN DataFileName$ = DummyFileName$
               CASE F1        'HELP
                  CALL HelpMe(0, "FILES", 1, 2, 25, 79)
               CASE F10  'quit
                  FileNameFinished% = True
               CASE ELSE
                  CALL OOPS
            END SELECT
         END IF
         '
         'CHECK THAT THE FILE NAME IS ACCEPTABLE
         '
         IF LEN(DataFileName$) > 12 OR INSTR(DataFileName$, ".") > 9 OR INSTR(DataFileName$, ".") = 1 OR (LEN(DataFileName$) > 8 AND INSTR(DataFileName$, ".") = 0) THEN
            HideCursor
            CALL WindowMgr(0, 1, 13, 4, 21, 76, CNF.Revers)     'open next available window
            ShowCursor
            CALL QPrintRC("WARNING!", 14, 36, CNF.Revers)
            CALL QPrintRC(DataFileName$ + " is not an acceptable file name!", 16, 18, CNF.Revers)
            CALL QPrintRC("Press any key to enter another file name.", 19, 20, CNF.Revers)
            CALL OOPS
            c$ = GetAKey$
            DataFileName$ = LEFT$(DataFileName$, 12)
            FileNameFinished% = False
            HideCursor
            CALL WindowMgr(0, 0, 0, 0, 0, 0, 0)
            ShowCursor
         ELSEIF RW% AND NOT Exist%(DataFileName$) THEN
            HideCursor
            CALL WindowMgr(0, 1, 13, 4, 21, 76, CNF.Revers)     'open next available window
            ShowCursor
            CALL QPrintRC("WARNING!", 14, 36, CNF.Revers)
            CALL QPrintRC("A file named " + DataFileName$ + " was not found in the current directory.", 16, 7, CNF.Revers)
            CALL QPrintRC("Press any key to enter another file name.", 19, 20, CNF.Revers)
            CALL OOPS
            c$ = GetAKey$
            FileNameFinished% = False
            HideCursor
            CALL WindowMgr(0, 0, 0, 0, 0, 0, 0)
            ShowCursor
         ELSEIF NOT RW% AND Exist%(DataFileName$) THEN
            'CHECK FOR FILES PRESENCE AND PROMPT FOR OVERWRITE
            HideCursor
            CALL WindowMgr(0, 1, 13, 4, 21, 76, CNF.Revers)     'open next available window
            ShowCursor
            CALL QPrintRC("WARNING!", 14, 36, CNF.Revers)
            CALL QPrintRC("A file named " + DataFileName$ + " was found in the current directory.", 16, 9, CNF.Revers)
            CALL QPrintRC("Press 'O' to overwrite, any other key to enter another file name.", 19, 7, CNF.Revers)
            CALL OOPS
            c$ = UCASE$(GetAKey$)
            IF c$ = "O" THEN
               FileNameFinished% = True
            ELSE
               FileNameFinished% = False
            END IF
            HideCursor
            CALL WindowMgr(0, 0, 0, 0, 0, 0, 0)
            ShowCursor
         END IF
      LOOP UNTIL FileNameFinished%
   END IF
   IF RW% THEN  'READ FROM A DATA FILE
      FileNum% = FREEFILE
      OPEN DataFileName$ FOR INPUT AS #FileNum%
      IF WhyCalled% = 1 THEN
         TitleEntered% = False
         DatesEntered% = False
         StrataEntered% = False
         CountEntered% = False
         FishingDayEntered% = False
         HolidayEntered% = False
      END IF
      INPUT #FileNum%, MainTitle$
'?? fix year stuff
      INPUT #FileNum%, StartMonth%, StartDay%, StartYear%, StopMonth%, StopDay%, StopYear%
      INPUT #FileNum%, Counts%, Spread#
      INPUT #FileNum%, WDperStrata%, WEperStrata%, Strata$, StartInterval$
      INPUT #FileNum%, HolidayCount%
      FOR j% = 1 TO HolidayCount%
         INPUT #FileNum%, Holiday(j%).HName, Holiday(j%).Selected, Holiday(j%).Month, Holiday(j%).Day, Holiday(j%).Year
      NEXT j%
      INPUT #FileNum%, TotalStrata%
      FOR j% = 1 TO TotalStrata%
         INPUT #FileNum%, FishingDay(j%).StartTime, FishingDay(j%).EndTime, FishingDay(j%).FirstCount, FishingDay(j%).LastCount, StrataDate(j%).FirstMn, StrataDate(j%).FirstDy, StrataDate(j%).LastMn, StrataDate(j%).LastDy
      NEXT j%
      INPUT #FileNum%, StartPointTotal%
      FOR j% = 1 TO StartPointTotal%
         INPUT #FileNum%, StartPoint(j%).Abrev, StartPoint(j%).Descript, StartPoint(j%).AsignPortion
      NEXT j%
      IF MainTitle$ <> "" THEN TitleEntered% = True
'?? fix year stuff
      IF SelectedYear% > 0 AND CLNG(StartMonth% * StartDay% * StopMonth% * StopDay%) > 0 THEN
         DatesEntered% = True
      END IF
      IF Strata$ <> "" AND TotalStrata% > 0 THEN
         StrataEntered% = True
      END IF
      IF Counts% + Spread# > 0 AND StartInterval$ <> "" THEN
         CountEntered% = True
      END IF
      IF FishingDay(1).StartTime + FishingDay(1).EndTime > 0 THEN
         FishingDayEntered% = True
      END IF
      IF HolidayCount% > 0 AND Holiday(1).HName <> "" THEN
         HolidayEntered% = True
         FOR j% = 1 TO MaxHolidays
            Holiday(j%).CrescDate = CrescentDate%(Holiday(j%).Day, Holiday(j%).Month, Holiday(j%).Year)
         NEXT j%
      ELSE
         FOR j% = 1 TO MaxHolidays
            Holiday(j%).Selected = False
         NEXT j%
      END IF
   ELSE 'WRITE TO A FILE
      FileNum% = FREEFILE
      OPEN DataFileName$ FOR OUTPUT AS #FileNum%
      WRITE #FileNum%, MainTitle$
'?? fix year stuff
      WRITE #FileNum%, StartMonth%, StartDay%, StartYear%, StopMonth%, StopDay%, StopYear%
      WRITE #FileNum%, Counts%, Spread#
      WRITE #FileNum%, WDperStrata%, WEperStrata%, Strata$, StartInterval$
      HolidayCount% = 0
      FOR j% = 1 TO MaxHolidays
         IF Holiday(j%).Selected THEN
            HolidayCount% = HolidayCount% + 1
         END IF
      NEXT j%
      WRITE #FileNum%, HolidayCount%
      FOR j% = 1 TO MaxHolidays
         IF Holiday(j%).Selected THEN
            WRITE #FileNum%, Holiday(j%).HName, Holiday(j%).Selected, Holiday(j%).Month, Holiday(j%).Day, Holiday(j%).Year
         END IF
      NEXT j%
      WRITE #FileNum%, TotalStrata%
      FOR j% = 1 TO TotalStrata%
         WRITE #FileNum%, FishingDay(j%).StartTime, FishingDay(j%).EndTime, FishingDay(j%).FirstCount, FishingDay(j%).LastCount, StrataDate(j%).FirstMn, StrataDate(j%).FirstDy, StrataDate(j%).LastMn, StrataDate(j%).LastDy
      NEXT j%
      StartPointTotal% = 0
      FOR j% = 1 TO 7
          IF NOT IsBlank%(StartPoint(j%).Abrev) THEN
             StartPointTotal% = StartPointTotal% + 1
          END IF
      NEXT j%
      WRITE #FileNum%, StartPointTotal%
      FOR j% = 1 TO 7
          IF NOT IsBlank%(StartPoint(j%).Abrev) THEN
             WRITE #FileNum%, StartPoint(j%).Abrev, StartPoint(j%).Descript, StartPoint(j%).AsignPortion
          END IF
      NEXT j%
   END IF
   CLOSE #FileNum%
   'ON ERROR GOTO 0
   'IF ERR = 53 THEN
   IF NOT Exist%(DataFileName$) THEN
      HideCursor
      CALL WindowMgr(0, 1, 13, 4, 21, 76, CNF.Revers)     'open next available window
      ShowCursor
      CALL QPrintRC("WARNING!", 14, 36, CNF.Revers)
      CALL QPrintRC("A file named " + DataFileName$ + " does not exist in the current directory.", 16, 10, CNF.Revers)
      CALL QPrintRC("Press any key to continue", 19, 28, CNF.Revers)
      CALL OOPS
      c$ = GetAKey$
      HideCursor
      CALL WindowMgr(0, 0, 0, 0, 0, 0, 0)
      ShowCursor
      GOTO RestartFileName
   END IF
GetOut:
   HideCursor
   CALL WindowMgr(0, 0, 0, 0, 0, 0, 0)
   ShowCursor
END SUB 'ReadWrite

SUB SetHolidays

   'RESET THE DATES FOR SELECTED HOLIDAYS
   FOR j% = 1 TO MaxHolidays
      Holiday(j%).CrescDate = CrescentDate%(Holiday(j%).Day, Holiday(j%).Month, Holiday(j%).Year)
   NEXT j%
   'clear any records if not selected or outside current limits
   FOR j% = 1 TO MaxHolidays
      IF Holiday(j%).CrescDate < StrataDate(1).FirstCres OR Holiday(j%).CrescDate > StrataDate(TotalStrata%).LastCres OR NOT Holiday(j%).Selected THEN
         Holiday(j%).HName = ""
         Holiday(j%).Year = 0
         Holiday(j%).Month = 0
         Holiday(j%).Day = 0
         Holiday(j%).Selected = False
         Holiday(j%).CrescDate = -32678
      END IF
   NEXT j%
   'SORT THE HOLIDAYS BY DATE
   DO
     sorted% = True
     FOR j% = 1 TO MaxHolidays - 1
        IF Holiday(j%).CrescDate > Holiday(j% + 1).CrescDate THEN  'dates should be switched
           IF Holiday(j%).CrescDate > -32678 AND Holiday(j% + 1).CrescDate > -32678 THEN
              'both dates are valid dates, this keeps invalid dates at bottom of list
              sorted% = False
           ELSEIF Holiday(j%).CrescDate = -32678 AND Holiday(j% + 1).CrescDate > -32678 THEN
              'first date invalid, this keeps invalid dates at bottom of list
              sorted% = False
           END IF
        END IF
        IF NOT sorted% THEN
           SWAP Holiday(j%).Selected, Holiday(j% + 1).Selected
           SWAP Holiday(j%).Month, Holiday(j% + 1).Month
           SWAP Holiday(j%).Day, Holiday(j% + 1).Day
           SWAP Holiday(j%).Year, Holiday(j% + 1).Year
           SWAP Holiday(j%).HName, Holiday(j% + 1).HName
           SWAP Holiday(j%).Selected, Holiday(j% + 1).Selected
           SWAP Holiday(j%).CrescDate, Holiday(j% + 1).CrescDate
        END IF
     NEXT j%
   LOOP UNTIL sorted%

   FOR YR% = StrataDate(1).FirstYr TO StrataDate(TotalStrata%).LastYr
      '*******  New Years
      CheckDate% = CrescentDate%(1, 1, YR%)
      IF CheckDate% >= StrataDate(1).FirstCres AND CheckDate% <= StrataDate(TotalStrata%).LastCres THEN
         HDay% = 1
         HMon% = 1
         HYear% = YR%
         HName$ = "New Years Day"
         CheckDay% = DayNumber%(1, 1, YR%)
         IF CheckDay% = 7 THEN 'Saturday
            HDay% = 31
            HMon% = 12
            HYear% = YR% - 1
         ELSEIF CheckDay% = 1 THEN 'Sunday
            HDay% = 2
         END IF
         GOSUB AddAHoliday
      END IF
      '*******  Martin Luther King Day
      SearchDay% = 2  'search for mondays
      CALL FindWeekDay(YR%, 1, SearchDay%, FirstMonday%, LastMonday%)
      CheckDate% = CrescentDate%(FirstMonday% + 14, 1, YR%)
      IF CheckDate% >= StrataDate(1).FirstCres AND CheckDate% <= StrataDate(TotalStrata%).LastCres THEN
         HDay% = FirstMonday% + 14
         HMon% = 1
         HYear% = YR%
         HName$ = "Martin Luther King Day"
         GOSUB AddAHoliday
      END IF
      '*******  Presidents Day
      SearchDay% = 2  'search for mondays
      CALL FindWeekDay(YR%, 2, SearchDay%, FirstMonday%, LastMonday%)
      CheckDate% = CrescentDate%(FirstMonday% + 14, 2, YR%)
      IF CheckDate% >= StrataDate(1).FirstCres AND CheckDate% <= StrataDate(TotalStrata%).LastCres THEN
         HDay% = FirstMonday% + 14
         HMon% = 2
         HYear% = YR%
         HName$ = "Presidents Day"
         GOSUB AddAHoliday
      END IF
      '*******  Memorial Day
      SearchDay% = 2  'search for mondays
      CALL FindWeekDay(YR%, 5, SearchDay%, FirstMonday%, LastMonday%)
      CheckDate% = CrescentDate%(LastMonday%, 5, YR%)
      IF CheckDate% >= StrataDate(1).FirstCres AND CheckDate% <= StrataDate(TotalStrata%).LastCres THEN
         HDay% = LastMonday%
         HMon% = 5
         HYear% = YR%
         HName$ = "Memorial Day"
         GOSUB AddAHoliday
      END IF
      '*******  July 4th
      CheckDate% = CrescentDate%(4, 7, YR%)
      IF CheckDate% >= StrataDate(1).FirstCres AND CheckDate% <= StrataDate(TotalStrata%).LastCres THEN
         HDay% = 4
         HMon% = 7
         HYear% = YR%
         HName$ = "Fourth of July"
         CheckDay% = DayNumber%(4, 7, YR%)
         IF CheckDay% = 7 THEN 'Saturday
            HDay% = 3
         ELSEIF CheckDay% = 1 THEN 'Sunday
            HDay% = 5
         END IF
         GOSUB AddAHoliday
      END IF
      '*******  Labor Day
      SearchDay% = 2  'search for mondays
      CALL FindWeekDay(YR%, 9, SearchDay%, FirstMonday%, LastMonday%)
      CheckDate% = CrescentDate%(FirstMonday%, 9, YR%)
      IF CheckDate% >= StrataDate(1).FirstCres AND CheckDate% <= StrataDate(TotalStrata%).LastCres THEN
         HDay% = FirstMonday%
         HMon% = 9
         HYear% = YR%
         HName$ = "Labor Day"
         GOSUB AddAHoliday
      END IF
      '*******  Veterans Day
      CheckDate% = CrescentDate%(11, 11, YR%)
      IF CheckDate% >= StrataDate(1).FirstCres AND CheckDate% <= StrataDate(TotalStrata%).LastCres THEN
         HDay% = 11
         HMon% = 11
         HYear% = YR%
         HName$ = "Veterans Day"
         CheckDay% = DayNumber%(11, 11, YR%)
         IF CheckDay% = 7 THEN 'Saturday
            HMon% = 10
         ELSEIF CheckDay% = 1 THEN 'Sunday
            HDay% = 12
         END IF
         GOSUB AddAHoliday
      END IF
      '*******  ThanksGiving
      SearchDay% = 5  'search for thursdays
      CALL FindWeekDay(YR%, 11, SearchDay%, FirstThursday%, LastThursday%)
      CheckDate% = CrescentDate%(FirstThursday% + 21, 11, YR%)
      IF CheckDate% >= StrataDate(1).FirstCres AND CheckDate% <= StrataDate(TotalStrata%).LastCres THEN
         HDay% = FirstThursday% + 21
         HMon% = 11
         HYear% = YR%
         HName$ = "ThanksGiving Day"
         GOSUB AddAHoliday
      END IF
      '*******  Christmas
      CheckDate% = CrescentDate%(25, 12, YR%)
      IF CheckDate% >= StrataDate(1).FirstCres AND CheckDate% <= StrataDate(TotalStrata%).LastCres THEN
         HDay% = 25
         HMon% = 12
         HYear% = YR%
         HName$ = "Christmas Day"
         CheckDay% = DayNumber%(25, 12, YR%)
         IF CheckDay% = 7 THEN 'Saturday
            HDay% = 24
         ELSEIF CheckDay% = 1 THEN 'Sunday
            HDay% = 26
         END IF
         GOSUB AddAHoliday
      END IF
   NEXT YR%

   HolidayCount% = 0
   FOR j% = 1 TO MaxHolidays
     IF Holiday(j%).Selected <> 0 THEN
        HolidayCount% = HolidayCount% + 1
     END IF
   NEXT j%
   'RESET THE DATES FOR SELECTED HOLIDAYS
   FOR j% = 1 TO MaxHolidays
      Holiday(j%).CrescDate = CrescentDate%(Holiday(j%).Day, Holiday(j%).Month, Holiday(j%).Year)
   NEXT j%
   'SORT THE HOLIDAYS BY DATE
   DO
     sorted% = True
     FOR j% = 1 TO MaxHolidays - 1
        IF Holiday(j%).CrescDate > Holiday(j% + 1).CrescDate THEN  'dates should be switched
           IF Holiday(j%).CrescDate > -32678 AND Holiday(j% + 1).CrescDate > -32678 THEN
              'both dates are valid dates, this keeps invalid dates at bottom of list
              sorted% = False
           ELSEIF Holiday(j%).CrescDate = -32678 AND Holiday(j% + 1).CrescDate > -32678 THEN
              'first date invalid, this keeps invalid dates at bottom of list
              sorted% = False
           END IF
        END IF
        IF NOT sorted% THEN
           SWAP Holiday(j%).Selected, Holiday(j% + 1).Selected
           SWAP Holiday(j%).Month, Holiday(j% + 1).Month
           SWAP Holiday(j%).Day, Holiday(j% + 1).Day
           SWAP Holiday(j%).Year, Holiday(j% + 1).Year
           SWAP Holiday(j%).HName, Holiday(j% + 1).HName
           SWAP Holiday(j%).Selected, Holiday(j% + 1).Selected
           SWAP Holiday(j%).CrescDate, Holiday(j% + 1).CrescDate
        END IF
     NEXT j%
   LOOP UNTIL sorted%

   EXIT SUB

AddAHoliday:
   'LOOP TO ADD HOLIDAY TO LIST
   DateFound% = False
   'SEE IF DATE IS ALREADY ON LIST
   FOR j% = 1 TO MaxHolidays
      IF Holiday(j%).CrescDate = CheckDate% THEN
         Holiday(j%).HName = HName$
         Holiday(j%).Year = HYear%
         Holiday(j%).Month = HMon%
         Holiday(j%).Day = HDay%
         Holiday(j%).Selected = 3  'Mandatory exclusion
         'Holiday(j%).CrescDate = CheckDate%
         DateFound% = True
         EXIT FOR
      END IF
   NEXT j%
   'SEE IF NAME IS ALREADY ON LIST
   IF NOT DateFound% THEN
      FOR j% = 1 TO MaxHolidays
         IF LTRIM$(UCASE$(Holiday(j%).HName)) = LTRIM$(UCASE$(HName$)) AND Holiday(j%).Year = HYear% THEN
            'Holiday(j%).HName = HName$
            Holiday(j%).Year = HYear%
            Holiday(j%).Month = HMon%
            Holiday(j%).Day = HDay%
            Holiday(j%).Selected = 3  'default mandatory exclusion
            Holiday(j%).CrescDate = CheckDate%
            DateFound% = True
            EXIT FOR
         END IF
      NEXT j%
   END IF
   'ADD IT TO LIST IF NOT ALREADY FOUND
   IF NOT DateFound% THEN
      FOR j% = 1 TO MaxHolidays
         IF IsBlank%(Holiday(j%).HName) THEN
            Holiday(j%).HName = HName$
            Holiday(j%).Year = HYear%
            Holiday(j%).Month = HMon%
            Holiday(j%).Day = HDay%
            Holiday(j%).Selected = 3  'default mandatory exclusion
            Holiday(j%).CrescDate = CheckDate%
            DateFound% = True
            EXIT FOR
         END IF
      NEXT j%
   END IF
RETURN

END SUB

SUB ViewMonths (StartYear%, StopYear%)

   InKy% = 1
   HideCursor
   CALL WindowMgr(0, 1, 1, 2, 13, 79, CNF.Normal)     'open next available window
   ShowCursor
   CALL QPrintRC("Arrow Keys to go see calendars for other months, F1 for Help", 2, 10, CNF.HiLite)
   CALL QPrintRC("Any other key to return to Holiday Selection Screen", 3, 14, CNF.HiLite)
   Year% = StartYear%
   DO
      SELECT CASE InKy%
          CASE 1
            CALL PrintCalendar(Year%, 1, 4, 7)
            CALL PrintCalendar(Year%, 2, 4, 47)
         CASE 2
            CALL PrintCalendar(Year%, 3, 4, 7)
            CALL PrintCalendar(Year%, 4, 4, 47)
         CASE 3
            CALL PrintCalendar(Year%, 5, 4, 7)
            CALL PrintCalendar(Year%, 6, 4, 47)
         CASE 4
            CALL PrintCalendar(Year%, 7, 4, 7)
            CALL PrintCalendar(Year%, 8, 4, 47)
         CASE 5
            CALL PrintCalendar(Year%, 9, 4, 7)
            CALL PrintCalendar(Year%, 10, 4, 47)
         CASE 6
            CALL PrintCalendar(Year%, 11, 4, 7)
            CALL PrintCalendar(Year%, 12, 4, 47)
      END SELECT
      Ch$ = GetAKey$
      KeyCode% = KeyPressed%(Ch$)
      IF KeyCode% < 0 THEN
         SELECT CASE KeyCode%
            CASE LeftArrow, UpArrow
               InKy% = InKy% - 1
               IF InKy% = 0 THEN
                  InKy% = 6
                  IF StartYear% <> StopYear% AND Year% > StartYear% THEN
                     Year% = Year% - 1
                  END IF
               END IF
            CASE RightArrow, DownArrow
               InKy% = InKy% + 1
               IF InKy% = 7 THEN
                  InKy% = 1
                  IF StartYear% <> StopYear% AND Year% < StopYear% THEN
                     Year% = Year% + 1
                  END IF
               END IF
            CASE Home
               InKy% = 1
            CASE Ennd
               InKy% = 6
            CASE F1        'HELP
               CALL HelpMe(0, "CALENDAR", 1, 2, 25, 79)
            CASE ELSE
               CALL OOPS
         END SELECT
      ELSE
         EXIT DO
      END IF
   LOOP
   HideCursor
   CALL WindowMgr(0, 0, 0, 0, 0, 0, 0)
   ShowCursor
END SUB

