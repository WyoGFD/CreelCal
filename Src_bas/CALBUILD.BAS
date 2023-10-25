DECLARE SUB AssignDays (DaysAvailable%(), NumberOfDays%, DaysPossible$, DaysLeft$, CountsPerStrata%, StartsAssigned%(), StartsAvailable%())
DECLARE SUB AssignRandom (DaysAvailable%(), NumberOfDays%, CountsPerStrata%, StartsAssigned%(), StartsAvailable%())
DECLARE SUB BuildCal (CalendarPrinted%, TotalStrata%)
DECLARE SUB ComputeMonth (Year%, Month%, DayOfWeek%, DaysInMonth%, CumulativeDays%)
DECLARE FUNCTION DaysPerMonth% (Month%, Year%)
DECLARE SUB FindWeekDay (Year%, Month%, SearchDay%, First%, Last%)
DECLARE SUB PrintCalendar (Year%, Month%, Row%, Col%)
DECLARE SUB SetStrata ()
DECLARE SUB SetYear ()
DECLARE SUB TimeLag (EarlyTime%, LateTime%, Hours%, Minutes%, HrsMin%)
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
'
'   Module CALBUILD - Part of CREELCAL program
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
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'$DYNAMIC

'$INCLUDE: 'creelcal.bi'

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
'  This routine assigns sample days on a random basis but spreads the sample
'  days among the weekdays available.  This means that once a Monday is
'  sampled another monday will not be sampled until all the other days
'  available (DaysPossible$) are sampled.
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
SUB AssignDays (DaysAvailable%(), NumberOfDays%, DaysPossible$, DaysLeft$, CountsPerStrata%, StartsAssigned%(), StartsAvailable%()) STATIC

   RANDOMIZE TIMER
   CountsToMake% = CountsPerStrata%
   IF CountsToMake% <= 0 OR NumberOfDays% <= 0 THEN EXIT SUB
   Tries% = 0

'FOR zz% = 1 TO 5
'   LOCATE zz%, 1: PRINT "                                        "
'NEXT zz%
'LOCATE 1, 1:       PRINT "Inside AssignDays"
'LOCATE 2, 1: PRINT USING "   NumberOfDays=### "; NumberOfDays%
'LOCATE 3, 1:       PRINT "   DaysPossible="; DaysPossible$
'LOCATE 4, 1:       PRINT "       DaysLeft="; DaysLeft$
'LOCATE 5, 1: PRINT USING "CountsPerStrata=##"; CountsPerStrata%
'c$ = INPUT$(1)

   DO
      Tries% = Tries% + 1
      IF LEN(DaysLeft$) = 0 OR Tries% > 50 THEN
         DaysLeft$ = DaysPossible$
         Tries% = 0
      END IF

      'CHECK TO BE SURE THE SELECTED COUNT IS AVAILABLE -  THIS TRIES TO SPREAD
      'THE COUNT DAYS AROUND BY MAKING SURE THAT EACH DAY IS ASSIGNED ONCE
      'BEFORE ONE IS DUPLICATED.  THIS MEANS THAT IF A MONDAY IS PICKED ANOTHER
      'MONDAY CANNOT BE ASSIGNED UNTIL ALL THE OTHER WEEK DAYS HAVE BEEN
      'ASSIGNED AND WEEKEND DAYS ALTERNATE.
      '
      'SELECT A DAY FROM THE LIST OF THOSE POSSIBLE
      '
      InKy% = VAL(QPRound$(Rand#(1#, CDBL(NumberOfDays%)), 0))
'      InKy% = INT(RND * NumberOfDays%) + 1
      Selection$ = LTRIM$(STR$(DaysAvailable%(0, InKy%)))  'Day of Week
      Spot% = INSTR(DaysLeft$, Selection$)
      IF Spot% > 0 THEN
         Year(DaysAvailable%(1, InKy%)).Sampled = True
'LOCATE 6, 1: PRINT "                                        "
'LOCATE 7, 1: PRINT "                                        "
'LOCATE 8, 1: PRINT "                                        "
'LOCATE 9, 1: PRINT "                                        "
'LOCATE 10, 1: PRINT "                                        "
'LOCATE 6, 1: PRINT "DaysLeft = "; DaysLeft$
'LOCATE 7, 1: PRINT "Selection= "; Selection$
'LOCATE 8, 1: PRINT using "     Spot=## InKy=### "; Spot%; InKy%
         DaysLeft$ = LEFT$(DaysLeft$, Spot% - 1) + RIGHT$(DaysLeft$, LEN(DaysLeft$) - Spot%)
         NumberOfDays% = NumberOfDays% - 1
         CountsToMake% = CountsToMake% - 1
'LOCATE 9, 1: PRINT "DaysLeft = "; DaysLeft$
'LOCATE 10, 1: PRINT USING "   NumberOfDays=### "; NumberOfDays%
'c$ = INPUT$(1)
         IF StartPointTotal% > 0 THEN
            '
            ' INSERT A START AREA IF THEY ARE USED
            '
            DO
               IF RandomStartPoints% THEN
                  NextStart% = VAL(QPRound$(Rand#(1#, CDBL(StartPointTotal%)), 0))
                  Year(DaysAvailable%(1, InKy%)).Area = StartPoint(NextStart%).Abrev
                  EXIT DO
               ELSE
                  Spot% = VAL(QPRound$(Rand#(1#, 1000#), 0))
                  Year(DaysAvailable%(1, InKy%)).Area = StartPoint(StartPointProb(Spot%).Location).Abrev
                  EXIT DO
               END IF
            LOOP
         ELSEIF Counts% = 0 AND StartPointTotal% = 0 THEN
            '
            'JUST ASSIGN A DAY WITH NO COUNT TIMES
            '
            Year(DaysAvailable%(1, InKy%)).Area = "Survey"
         END IF
         FOR j% = InKy% TO NumberOfDays%
            DaysAvailable%(0, j%) = DaysAvailable%(0, j% + 1)
            DaysAvailable%(1, j%) = DaysAvailable%(1, j% + 1)
         NEXT j%
      END IF
   LOOP WHILE CountsToMake% > 0
'PRINT "press a key to continue"
'c$ = INPUT$(1)

END SUB 'AssignDays

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
'  This routine assigns sample days on a totally random basis.  It makes no
'  effort to spread the sample around among the days of the week.
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
SUB AssignRandom (DaysAvailable%(), NumberOfDays%, CountsPerStrata%, StartsAssigned%(), StartsAvailable%()) STATIC

   RANDOMIZE TIMER
   CountsToMake% = CountsPerStrata%
   IF CountsToMake% <= 0 OR NumberOfDays% <= 0 THEN EXIT SUB
   DO
      '
      'SELECT A DAY FROM THE LIST OF THOSE POSSIBLE
      '
      InKy% = VAL(QPRound$(Rand#(1#, CDBL(NumberOfDays%)), 0))
'      InKy% = INT(RND * NumberOfDays%) + 1
      Selection$ = LTRIM$(STR$(DaysAvailable%(0, InKy%)))
      Spot% = INSTR(DaysLeft$, Selection$)
      Year(DaysAvailable%(1, InKy%)).Sampled = True
      NumberOfDays% = NumberOfDays% - 1
      CountsToMake% = CountsToMake% - 1
      IF StartPointTotal% > 0 THEN
         '
         ' INSERT A START AREA IF THEY ARE USED
         '
         DO
            NextStart% = VAL(QPRound$(Rand#(1#, CDBL(StartPointTotal%)), 0))
'            NextStart% = INT(RND * StartPointTotal%) + 1
            IF RandomStartPoints% THEN
               Year(DaysAvailable%(1, InKy%)).Area = StartPoint(NextStart%).Abrev
               EXIT DO
            ELSE
               IF StartsAssigned%(NextStart%) < StartsAvailable%(NextStart%) THEN
                  Year(DaysAvailable%(1, InKy%)).Area = StartPoint(NextStart%).Abrev
                  StartsAssigned%(NextStart%) = StartsAssigned%(NextStart%) + 1
                  EXIT DO
               END IF
            END IF
         LOOP
      ELSEIF Counts% = 0 AND StartPointTotal% = 0 THEN
         '
         'JUST ASSIGN A DAY WITH NO COUNT TIMES
         '
         Year(DaysAvailable%(1, InKy%)).Area = "Survey"
      END IF
      FOR j% = InKy% TO NumberOfDays%
         DaysAvailable%(0, j%) = DaysAvailable%(0, j% + 1)
         DaysAvailable%(1, j%) = DaysAvailable%(1, j% + 1)
      NEXT j%
   LOOP WHILE CountsToMake% > 0
END SUB 'AssignRandom

SUB BuildCal (CalendarPrinted%, TotalStrata%)

   REDIM WDAvailable%(1, 31)    'The number of week days available as count days
   REDIM WEAvailable%(1, 31)    'The number of weekend day available to survey
   REDIM Week$(7, MaxCounts)
   REDIM Dte$(1 TO 7)
   REDIM BBox$(12)
   REDIM WDStartsAssigned%(MaxStartPoints)
   REDIM WEStartsAssigned%(MaxStartPoints)
   REDIM WDStartsAvailable%(MaxStartPoints)
   REDIM WEStartsAvailable%(MaxStartPoints)

    BBox$(1) = "ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
   BBox$(10) = "บ                                                                             บ"
    BBox$(2) = "บ\                                                                           \บ"
    BBox$(3) = "บ                                \            \                               บ"
    BBox$(4) = "วฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤถ"
    BBox$(5) = "บ Sunday   ณ  Monday  ณ Tuesday  ณ Wednesday ณ Thursday ณ  Friday  ณ Saturday บ"
    BBox$(6) = "วฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤถ"
    BBox$(7) = "บ\ \       ณ\ \       ณ\ \       ณ\ \        ณ\ \       ณ\ \       ณ\ \       บ"
    BBox$(8) = "บ  \    \  ณ  \    \  ณ  \    \  ณ  \    \   ณ  \    \  ณ  \    \  ณ  \    \  บ"
   BBox$(12) = "บ \      \ ณ \      \ ณ \      \ ณ \      \  ณ \      \ ณ \      \ ณ \      \ บ"
   BBox$(11) = "บ          ณ          ณ          ณ           ณ          ณ          ณ          บ"
    BBox$(9) = "ศออออออออออฯออออออออออฯออออออออออฯอออออออออออฯออออออออออฯออออออออออฯออออออออออผ"

   RANDOMIZE TIMER
   '
   'PRINT THE MESSAGE BOX
   '
   Text$ = "|  Your creel survey calendar is being produced."
   Text$ = Text$ + "| | | | | | | | | | | |"

   LOCATE 5, 1            ' position the starting line for box
   Wdth = 55              ' set text width
   CSMsgBox Text$, Wdth, CNF.Revers
   'COLOR ReverseFG%, ReverseBG%

   CALL SetYear
   CALL SetStrata

   SaveWD% = WDperStrata% 'INPUT OF DAYS TO SAMPLE
   SaveWE% = WEperStrata%
   IntervalWarning% = False

   Sum! = 0!

   '
   ' DETERMINE HOW MANY SAMPLE DAYS SHOULD BE ASSIGNED TO EACH START AREA
   '
   FOR j% = 1 TO StartPointTotal%
      Sum! = Sum! + StartPoint(j%).AsignPortion
   NEXT j%

   IF Sum! = 0! THEN
      'start point assignment is completely random
      RandomStartPoints% = True
   ELSE
      RandomStartPoints% = False
      '
      ' ASSIGN PROBABILITIES TO EACH START POINT
      ' StartPointProb is an array with 1000 elements. Each is assigned the label
      ' for the starting points based on the assigned probability.  Points are
      ' then randomly selected from the list.
      '
      REDIM BreakPoint%(0 TO StartPointTotal%)
      FOR j% = 1 TO StartPointTotal%
         BreakPoint%(j%) = BreakPoint%(j% - 1) + (1000 * VAL(QPRound$(CDBL(StartPoint(j%).AsignPortion), 3)))
      NEXT j%
      FOR j% = 1 TO StartPointTotal%
         FOR k% = BreakPoint%(j% - 1) + 1 TO BreakPoint%(j%)
            StartPointProb(k%).Location = j%
         NEXT k%
      NEXT j%
   END IF

   '
   ' DayOYear% AND DayOfWeek% ARE ASSIGNED HERE AND INCREMENTED WITHIN
   ' THE FOR Day% LOOP
   '
   CALL ComputeMonth(StrataDate(1).FirstYr, StrataDate(1).FirstMn, DayOfWeek%, DaysInMonth%, CumulativeDays%)
   FOR Day% = 1 TO StrataDate(1).FirstDy
      DayOfWeek% = DayOfWeek% + 1
      IF DayOfWeek% = 8 THEN DayOfWeek% = 1
   NEXT Day%

   DayOYear% = CumulativeDays% + StrataDate(1).FirstDy

   FOR CurrentStrata% = 1 TO TotalStrata%

      CALL QPrintRC("Assigning days and count times for strata " + FUsing$(STR$(CurrentStrata%), "##"), 10, 18, CNF.Normal)

      IF StrataDate(1).FirstYr <> StrataDate(CurrentStrata%).FirstYr THEN
         'YEAR HAS CHANGED
         CALL ComputeMonth(StrataDate(CurrentStrata%).FirstYr, StrataDate(CurrentStrata%).FirstMn, DayOfWeek%, DaysInMonth%, CumulativeDays%)
         FOR Day% = 1 TO StrataDate(CurrentStrata%).FirstDy
            DayOfWeek% = DayOfWeek% + 1
            IF DayOfWeek% = 8 THEN DayOfWeek% = 1
         NEXT Day%
         DayOYear% = CumulativeDays% + StrataDate(CurrentStrata%).FirstDy + 366
      END IF

      '
      'RESTORE THESE TO ORIGINAL VALUES FOR EACH STRATA
      '
      WDperStrata% = SaveWD%
      WEperStrata% = SaveWE%

      '
      'DEFINE NUMBER OF DAYS IN STRATA AND CHECK TO BE SURE THAT
      'EACH STRATA INCLUDES THE FULL NUMBER OF DAYS
      '
      IF StrataDate(CurrentStrata%).LastMn <> StrataDate(CurrentStrata%).FirstMn THEN
         'IF STRATA STRETCHES OVER 2 MONTHS
         TotalDays% = DaysPerMonth%(StrataDate(CurrentStrata%).FirstMn, StrataDate(CurrentStrata%).FirstYr) - (StrataDate(CurrentStrata%).FirstDy) + StrataDate(CurrentStrata%).LastDy + 1
      ELSE
         TotalDays% = StrataDate(CurrentStrata%).LastDy - StrataDate(CurrentStrata%).FirstDy + 1
      END IF

      SELECT CASE Strata$
         CASE "month"
            DaysInStrata% = DaysPerMonth%(StrataDate(CurrentStrata%).LastMn, StrataDate(CurrentStrata%).LastYr)
         CASE "2 weeks"
            DaysInStrata% = 14
         CASE "week"
            DaysInStrata% = 7
      END SELECT
      '
      'ADJUST SAMPLE DAYS DOWNWARDS WHEN THERE ARE LESS DAYS TO SAMPLE
      '
      IF TotalDays% < DaysInStrata% THEN
         WDperStrata% = VAL(QPRound$((WDperStrata% * (TotalDays% / DaysInStrata%)), 0))
         WEperStrata% = VAL(QPRound$((WEperStrata% * (TotalDays% / DaysInStrata%)), 0))
         DaysInStrata% = TotalDays%
      END IF

      MandatoryIncludeWD% = 0
      MandatoryIncludeWE% = 0
      ExcludeCntWD% = 0
      ExcludeCntWE% = 0
      NumberOfWD% = 0
      NumberOfWE% = 0
      HolidayAvailWD$ = ""
      HolidayAvailWE$ = ""
      '
      'LOOP TO ASSIGN WEEKDAY AND WEEKEND DAYS AVAILABLE FOR SAMPLING
      '
      FOR Day% = 1 TO DaysInStrata%
         '
         'CHECK TO SEE IF THE DAY IS A HOLIDAY
         '
         HolidayToday% = False
         FOR q% = 1 TO MaxHolidays
            IF Holiday(q%).Selected <> 0 AND Holiday(q%).Month = Year(DayOYear%).Month THEN
               '
               'IF A HOLIDAY WAS SELECTED AND THEY ARE IN THE SAME MONTH
               'THEN SEE IF IT WAS THIS DAY OF THE STRATA
               '
               CALL ComputeMonth(Holiday(q%).Year, Holiday(q%).Month, HDayOfWeek%, DaysInMonth%, CumulativeDays%)
               HolidayDay% = CumulativeDays% + Holiday(q%).Day
               IF StrataDate(1).FirstYr <> Holiday(q%).Year THEN
                  HolidayDay% = HolidayDay% + 366
               END IF
               IF HolidayDay% = DayOYear% THEN
'LOCATE 1, 1: PRINT "Found a holiday           ";
'LOCATE 2, 1: PRINT USING "Year=#### Mon=## Day=## \                         \"; Holiday(q%).Year; Holiday(q%).Month; Holiday(q%).Day; Holiday(q%).HName
'c$ = INPUT$(1)
'LOCATE 1, 1: PRINT "                                                     "
'LOCATE 2, 1: PRINT "                                                     "

                  HolidayToday% = True
                  IF Holiday(q%).Selected = 1 THEN 'Random Holiday
                     IF HDayOfWeek% = 1 OR HDayOfWeek% = 7 THEN
                        'DON'T BOTHER WITH HOLIDAYS THAT ARE WEEKENDS
                        'THEY WILL BE AUTOMATICALLY INCLUDED AS WEEKENDS ANYWAY
                        HolidayToday% = False
                        HolidayAvailWE$ = HolidayAvailWE$ + LTRIM$(STR$(HDayOfWeek%))
                        EXIT FOR
                     END IF
                     'Year(HolidayDay%).Sampled = 1
                     'OTHERWISE INCREMENT WE COUNTERS FOR WEEK DAYS THAT WILL BE
                     'INCLUDED IN RANDOM WEEKEND DAY SELECTION
                     NumberOfWE% = NumberOfWE% + 1
                     WEAvailable%(0, NumberOfWE%) = HDayOfWeek%
                     WEAvailable%(1, NumberOfWE%) = HolidayDay%
                     HolidayAvailWE$ = HolidayAvailWE$ + LTRIM$(STR$(HDayOfWeek%))
                  ELSEIF Holiday(q%).Selected = 2 THEN  'Mandatory include
                     Year(HolidayDay%).Sampled = 2
                     IF HDayOfWeek% = 1 OR HDayOfWeek% = 7 THEN
                        MandatoryIncludeWE% = MandatoryIncludeWE% + 1
                     ELSE
                        MandatoryIncludeWD% = MandatoryIncludeWD% + 1
                     END IF
                     'SELECT A STARTING POINT FOR THE SAMPLE
                     NextStart% = VAL(QPRound$(Rand#(1#, CDBL(StartPointTotal%)), 0))
                     Year(HolidayDay%).Area = StartPoint(NextStart%).Abrev
                  ELSEIF Holiday(q%).Selected = 3 THEN  'Mandatory Exclude
                     Year(HolidayDay%).Sampled = 3
                     IF HDayOfWeek% = 1 OR HDayOfWeek% = 7 THEN
                        ExcludeCntWE% = ExcludeCntWE% + 1
                     ELSE
                        ExcludeCntWD% = ExcludeCntWD% + 1
                     END IF
                  END IF
               END IF
            END IF
            IF HolidayToday% THEN EXIT FOR 'QUIT IF A HOLIDAY IS FOUND
         NEXT q%
         '
         ' ADD NON HOLIDAY DAYS TO DAYS AVAILABLE FOR RANDOM SELECTION
         '
         IF NOT HolidayToday% THEN
            SELECT CASE DayOfWeek%
               CASE 1, 7  'WEEKEND DAYS
                  NumberOfWE% = NumberOfWE% + 1
                  WEAvailable%(0, NumberOfWE%) = DayOfWeek%
                  WEAvailable%(1, NumberOfWE%) = DayOYear%
               CASE ELSE  'WEEK DAYS
                  NumberOfWD% = NumberOfWD% + 1
                  WDAvailable%(0, NumberOfWD%) = DayOfWeek%
                  WDAvailable%(1, NumberOfWD%) = DayOYear%
            END SELECT
         END IF
         DayOfWeek% = DayOfWeek% + 1
         DayOYear% = DayOYear% + 1
         IF DayOfWeek% = 8 THEN DayOfWeek% = 1
      NEXT Day%

'?
'      StrataDate(CurrentStrata%).WEavail = NumberOfWE% + MandatoryIncludeWE% - ExcludeCntWE%
'      StrataDate(CurrentStrata%).WDavail = NumberOfWD% + MandatoryIncludeWD% - ExcludeCntWD%
      StrataDate(CurrentStrata%).WEavail = NumberOfWE% + MandatoryIncludeWD%
      StrataDate(CurrentStrata%).WDavail = NumberOfWD%

LOCATE 1, 1: PRINT USING "Strata=## Start ##-##-####  End ##-##-####"; CurrentStrata%; StrataDate(CurrentStrata%).FirstMn; StrataDate(CurrentStrata%).FirstDy; StrataDate(CurrentStrata%).FirstYr; StrataDate(CurrentStrata%).LastMn; StrataDate( _
CurrentStrata%).LastDy; StrataDate(CurrentStrata%).LastYr
LOCATE 2, 1: PRINT USING "StrataDate().WEavail=## NumberOfWE=## MandInclWE=## ExclCntWE=## "; StrataDate(CurrentStrata%).WEavail; NumberOfWE%; MandatoryIncludeWE%; ExcludeCntWE%
LOCATE 3, 1: PRINT USING "StrataDate().WDavail=## NumberOfWD=## MandInclWD=## ExclCntWD=## "; StrataDate(CurrentStrata%).WDavail; NumberOfWD%; MandatoryIncludeWD%; ExcludeCntWD%
FOR q% = 1 TO MaxHolidays
   LOCATE q% + 3, 1: PRINT USING " ## ##-##-#### \                         \ Selected=### "; q%; Holiday(q%).Month; Holiday(q%).Day; Holiday(q%).Year; Holiday(q%).HName; Holiday(q%).Selected
NEXT q%
c$ = INPUT$(1)
FOR q% = 1 TO MaxHolidays + 3
   LOCATE q%, 1: PRINT "                                                                           "
NEXT q%
      '
      'ASSIGN THE WEEKEND DAYS
      '
      'NumberOfWE% IS THE NUMBER OF WEEKEND DAYS AVAILABLE TO SAMPLE FROM
      'CountsPerStrata IS THE NUMBER OF COUNTS TO MAKE IN THE STRATA
      '
      IF WEperStrata% > StrataDate(CurrentStrata%).WEavail THEN
         'DESIRED SAMPLE DAYS > DAYS AVAILABLE
         CountsPerStrata% = StrataDate(CurrentStrata%).WEavail
      ELSE
         CountsPerStrata% = WEperStrata%
      END IF
'      StrataDate(CurrentStrata%).WEsampled = CountsPerStrata% + MandatoryInclude%
      StrataDate(CurrentStrata%).WEsampled = CountsPerStrata%
      IF CountsPerStrata% > 0 AND NumberOfWE% > 0 THEN
         DaysPossible$ = "17" + HolidayAvail$
         IF LEN(WEDaysLeft$) = 0 OR LEN(HolidayAvailWE$) > 0 THEN WEDaysLeft$ = DaysPossible$

         CALL AssignDays(WEAvailable%(), NumberOfWE%, DaysPossible$, WEDaysLeft$, CountsPerStrata%, WEStartsAssigned%(), WEStartsAvailable%())
         'REMOVE HOLIDAY DAYS FROM DaysLeft$
         FOR q% = 1 TO LEN(HolidayAvailWE$)
            Spot% = INSTR(WEDaysLeft$, MID$(HolidayAvailWE$, q%, 1))
            IF Spot% > 0 THEN
               WEDaysLeft$ = LEFT$(WEDaysLeft$, Spot% - 1) + RIGHT$(WEDaysLeft$, LEN(WEDaysLeft$) - Spot%)
            END IF
         NEXT q%
      END IF
      '
      'ASSIGN THE WEEK DAYS
      '
      'NumberOfWD% IS THE NUMBER OF WEEK DAYS AVAILABLE TO SAMPLE FROM
      'CountsPerStrata IS THE NUMBER OF DAYS TO SAMPLE IN THE STRATA
      IF WDperStrata% > StrataDate(CurrentStrata%).WDavail THEN
         CountsPerStrata% = StrataDate(CurrentStrata%).WDavail
      ELSE
         CountsPerStrata% = WDperStrata%
      END IF
      StrataDate(CurrentStrata%).WDsampled = CountsPerStrata%
      DaysPossible$ = "23456"
      IF CountsPerStrata% > 0 AND NumberOfWD% > 0 THEN
         IF LEN(WDDaysLeft$) = 0 THEN WDDaysLeft$ = DaysPossible$
         CALL AssignDays(WDAvailable%(), NumberOfWD%, DaysPossible$, WDDaysLeft$, CountsPerStrata%, WDStartsAssigned%(), WDStartsAvailable%())
         'REMOVE HOLIDAY DAYS FROM DaysLeft$
         FOR q% = 1 TO LEN(HolidayAvailWD$)
            Spot% = INSTR(WDDaysLeft$, MID$(HolidayAvailWD$, q%, 1))
            IF Spot% > 0 THEN
               WEDaysLeft$ = LEFT$(WDDaysLeft$, Spot% - 1) + RIGHT$(WDDaysLeft$, LEN(WEDaysLeft$) - Spot%)
            END IF
         NEXT q%
      END IF
      '
      ' IF COUNTS = ZERO THEN SKIP TIME CALCULATIONS
      '
      IF Counts% = 0 THEN GOTO SkipTimeStuff
      '
      'DETERMINE AVAILABLE START TIMES FOR EACH STRATA
      '
      CALL TimeLag(FishingDay(CurrentStrata%).FirstCount, FishingDay(CurrentStrata%).LastCount, Hours%, Minutes%, HrsMin%)
      HoursInDay% = HrsMin% 'CALCULATED IN LAST CALL TO TIMELAG
      '
      'OptIntervals IS THE NUMBER OF SAMPLING INTERVALS AVAILABLE IN THE
      '  SAMPLING DAY BASED ON A UNIFORM SAMPLING DISTRIBUTION ACROSS THE DAY
      '
      SELECT CASE StartInterval$
         CASE "5 minutes"
            Timing(CurrentStrata%).OptIntervals = Hours% * 12 + INT(Minutes% / 5) + 1
         CASE "15 minutes"
            Timing(CurrentStrata%).OptIntervals = Hours% * 4 + INT(Minutes% / 15) + 1
         CASE "30 minutes"
            Timing(CurrentStrata%).OptIntervals = Hours% * 2 + INT(Minutes% / 30) + 1
         CASE "hour"
            Timing(CurrentStrata%).OptIntervals = Hours% + 1
      END SELECT
      '
      '  CountStart().Available IS THE NUMBER OF STARTING TIMES AVAILABLE
      '  DURING THE SAMPLE DAY.
      '
      IF Spread# = 0 THEN
         IntervalsInDay% = Timing(CurrentStrata%).OptIntervals
         CountStart(CurrentStrata%).Available = INT(IntervalsInDay% / Counts%)
      ELSE
         'CALCULATE HOURS & MINUTES BETWEEN COUNTS
         Hours% = FIX(Spread#)
         Temp# = Spread# * 100
         Minutes% = INT(Temp# - INT(100 * Hours%))
         'CALCULATE THE TIME FROM FIRST COUNT TO LAST COUNT
         SampleDay% = ((Hours% + (Minutes% \ 60)) * 100 * (Counts% - 1)) + (Minutes% MOD 60)
         'HOW MUCH TIME IS AVAILABLE FOR START TIMES
         CALL TimeLag(SampleDay%, HoursInDay%, Hours%, Minutes%, HrsMin%)
         HoursAvailable% = HrsMin%
         '
         'HOW MANY STARTS ARE AVAILABLE
         '
         SELECT CASE StartInterval$
            CASE "5 minutes"
               CountStart(CurrentStrata%).Available = (Hours% * 12 + Minutes% / 5) + 1
            CASE "15 minutes"
               CountStart(CurrentStrata%).Available = (Hours% * 4 + Minutes% / 15) + 1
            CASE "30 minutes"
               CountStart(CurrentStrata%).Available = (Hours% * 2 + Minutes% / 30) + 1
            CASE "hour"
               CountStart(CurrentStrata%).Available = Hours% + 1
         END SELECT
      END IF
      '
      'SET MINUTES BETWEEN COUNTS
      '
      IF Spread# = 0 THEN
         SELECT CASE StartInterval$
            CASE "5 minutes"
               Timing(CurrentStrata%).IntervalMn = CountStart(CurrentStrata%).Available * 5
            CASE "15 minutes"
               Timing(CurrentStrata%).IntervalMn = CountStart(CurrentStrata%).Available * 15
            CASE "30 minutes"
               Timing(CurrentStrata%).IntervalMn = CountStart(CurrentStrata%).Available * 30
            CASE "hour"
               Timing(CurrentStrata%).IntervalMn = CountStart(CurrentStrata%).Available * 60
         END SELECT
      ELSE
         Hours% = FIX(Spread#)
         Temp# = Spread# * 100
         Minutes% = INT(Temp# - INT(100 * Hours%))
         Timing(CurrentStrata%).IntervalMn = (Hours% * 60) + Minutes%
      END IF
      '
      'CALCULATE THE TIME FROM FIRST COUNT TO LAST COUNT
      ' = THE MINIMUM WORK DAY
      '
      Minutes% = Timing(CurrentStrata%).IntervalMn * (Counts% - 1)
      FishingDay(CurrentStrata%).WorkDay = ((Minutes% \ 60) * 100) + (Minutes% MOD 60)
      Warning(CurrentStrata%).IntervalsLost = Timing(CurrentStrata%).OptIntervals - (CountStart(CurrentStrata%).Available * Counts%)
      IF Warning(CurrentStrata%).IntervalsLost <> 0 THEN
         IntervalWarning% = True
         SELECT CASE StartInterval$
            CASE "5 minutes"
               Warning(CurrentStrata%).TimeLost = Warning(CurrentStrata%).IntervalsLost * 5
            CASE "15 minutes"
               Warning(CurrentStrata%).TimeLost = Warning(CurrentStrata%).IntervalsLost * 15
            CASE "30 minutes"
               Warning(CurrentStrata%).TimeLost = Warning(CurrentStrata%).IntervalsLost * 30
            CASE "hour"
               Warning(CurrentStrata%).TimeLost = Warning(CurrentStrata%).IntervalsLost * 60
         END SELECT
      END IF
SkipTimeStuff:

   NEXT CurrentStrata%

   'SET STARTS USED = FALSE
   FOR j% = 0 TO MaxStarts
      CountStart(j%).Used = False
   NEXT j%

   CALL QPrintRC("Printing results to CREELCAL.OUT", 12, 24, CNF.Normal)
   'OPEN OUTPUT FILE AND PRINT PRELIMINARY INFORMATION
   OutFile% = FREEFILE
   OPEN "CREELCAL.OUT" FOR OUTPUT AS OutFile%
   OutFileTab% = FREEFILE
   OPEN "Creelcal.txt" FOR OUTPUT AS OutFileTab%

   PRINT #OutFile%,
   PRINT #OutFile%,
   PRINT #OutFile%, "                Output from Program CREELCAL " + Version$ + "."
   PRINT #OutFile%, USING "                      This calendar was created &."; DATE$
   PRINT #OutFile%,
   PRINT #OutFile%, "     The creel survey sampling schedule which follows is based on a"
   PRINT #OutFile%, "     stratified 2 stage sampling regime with systematic sampling at the"
   PRINT #OutFile%, "     second stage.  The fishing day is the primary sampling unit and is"
   PRINT #OutFile%, "     stratified into week days, weekend days and holidays.  Sample days"
   PRINT #OutFile%, "     within each strata are randomly selected.  The second stage of"
   PRINT #OutFile%, "     sampling is the fishermen counts which are made at equally spaced"
   PRINT #OutFile%, "     intervals during the sample day.  The time of the first count is"
   PRINT #OutFile%, "     randomly selected.  Below is a listing of the options on which this"
   PRINT #OutFile%, "     sampling plan is based."
   PRINT #OutFile%, : PRINT #OutFile%, USING "&"; Buffer$ + MainTitle$
   PRINT #OutFile%,
   PRINT #OutFile%, USING "          ## fishermen counts per sample day"; Counts%
   PRINT #OutFile%, USING "          ## week days sampled every &"; SaveWD%; Strata$
   PRINT #OutFile%, USING "          ## weekend days sampled every &"; SaveWE%; Strata$
   PRINT #OutFile%, USING "          Start of survey ##_/##_/####"; StartMonth%; StartDay%; StartYear%
   PRINT #OutFile%, USING "          End of survey   ##_/##_/####"; StopMonth%; StopDay%; StopYear%
   PRINT #OutFile%,
   PRINT #OutFile%,
   HolidayCount% = 0

   PRINT #OutFileTab%, "Output from Program CREELCAL " + Version$ + "."
   PRINT #OutFileTab%, "This calendar was created " + DATE$
   PRINT #OutFileTab%,
   PRINT #OutFileTab%, "The creel survey sampling schedule which follows is based on a"
   PRINT #OutFileTab%, "stratified 2 stage sampling regime with systematic sampling at the"
   PRINT #OutFileTab%, "second stage.  The fishing day is the primary sampling unit and is"
   PRINT #OutFileTab%, "stratified into week days, weekend days and holidays.  Sample days"
   PRINT #OutFileTab%, "within each strata are randomly selected.  The second stage of"
   PRINT #OutFileTab%, "sampling is the fishermen counts which are made at equally spaced"
   PRINT #OutFileTab%, "intervals during the sample day.  The time of the first count is"
   PRINT #OutFileTab%, "randomly selected.  Below is a listing of the options on which this"
   PRINT #OutFileTab%, "sampling plan is based."
   PRINT #OutFileTab%,
   PRINT #OutFileTab%, MainTitle$
   PRINT #OutFileTab%,
   PRINT #OutFileTab%, USING "## fishermen counts per sample day"; Counts%
   PRINT #OutFileTab%, USING "## week days sampled every &"; SaveWD%; Strata$
   PRINT #OutFileTab%, USING "## weekend days sampled every &"; SaveWE%; Strata$
   PRINT #OutFileTab%, USING "Start of survey ##_/##_/####"; StartMonth%; StartDay%; StartYear%
   PRINT #OutFileTab%, USING "End of survey   ##_/##_/####"; StopMonth%; StopDay%; StopYear%

'Outfile2% = FREEFILE
'OPEN "strdate.dat" FOR OUTPUT AS Outfile2%
'         PRINT #Outfile2%, "|    FIRST          |    LAST           |     WE      |     WD      |"
'         PRINT #Outfile2%, "|  dd mm year cresc |  dd mm year cresc | Avail Sampl | Avail Sampl |"
'FOR x% = 1 TO MaxStrata
'  PRINT #Outfile2%, USING "|  ## ## #### ##### |  ## ## #### ##### |  ###   ###  |  ###   ###  |"; StrataDate(x%).FirstDy; StrataDate(x%).FirstMn; StrataDate(x%).FirstYr; StrataDate(x%).FirstCres; StrataDate(x%).LastDy; StrataDate(x%).LastMn;  _
'StrataDate(x%).LastYr; StrataDate(x%).LastCres; StrataDate(x%).WEavail; StrataDate(x%).WEsampled; StrataDate(x%).WDavail; StrataDate(x%).WDsampled
'NEXT x%
'         PRINT #Outfile2%,
'         PRINT #Outfile2%,
'         PRINT #Outfile2%, "|  dd mm year cresc   Selected   Name"
'   FOR j% = 1 TO MaxHolidays
'      PRINT #Outfile2%, USING "|  ## ## #### #####   ##      &"; Holiday(j%).Day; Holiday(j%).Month; Holiday(j%).Year; Holiday(j%).CrescDate; Holiday(j%).Selected; Holiday(j%).HName
'   NEXT j%
'CLOSE #Outfile2%

   FOR j% = 1 TO MaxHolidays
      IF Holiday(j%).Selected <> 0 AND Holiday(j%).CrescDate >= StrataDate(1).FirstCres AND Holiday(j%).CrescDate <= StrataDate(TotalStrata%).LastCres THEN
         HolidayCount% = HolidayCount% + 1
      END IF
   NEXT j%
   IF HolidayCount% >= 1 THEN
      PRINT #OutFile%, "   The following 'Special Days' were included in the sampling set-up, all"
      PRINT #OutFile%, "   these days may not have been sampled depending on their status."
      PRINT #OutFile%, "   Note that all the days listed are shown as weekend days in the listing"
      PRINT #OutFile%, "   of the days available to be sampled."
      PRINT #OutFile%,
      PRINT #OutFile%, "   The status codes have the following meanings:"
      PRINT #OutFile%, "      Status 1 means the day was included with the weekends in the"
      PRINT #OutFile%, "          appropriate strata for possible random selection;"
      PRINT #OutFile%, "      Status 2 means that the day was intentionally included in the sample;"
      PRINT #OutFile%, "      Status 3 means that the day was intentionally excluded from the sample."
      PRINT #OutFile%,
      PRINT #OutFile%, "      Status      Date        Description"
      PRINT #OutFile%, "      ========================================================"

      PRINT #OutFileTab%,
      PRINT #OutFileTab%,
      PRINT #OutFileTab%, "The following 'Special Days' were included in the sampling set-up, all"
      PRINT #OutFileTab%, "these days may not have been sampled depending on their status."
      PRINT #OutFileTab%, "Note that all the days listed are shown as weekend days in the listing"
      PRINT #OutFileTab%, "of the days available to be sampled."
      PRINT #OutFileTab%,
      PRINT #OutFileTab%, "The status codes have the following meanings:"
      PRINT #OutFileTab%, "Status 1 means the day was included with the weekends in the"
      PRINT #OutFileTab%, "appropriate strata for possible random selection;"
      PRINT #OutFileTab%, "Status 2 means that the day was intentionally included in the sample;"
      PRINT #OutFileTab%, "Status 3 means that the day was intentionally excluded from the sample."
      PRINT #OutFileTab%,
      PRINT #OutFileTab%, "Status      Date        Description"
      PRINT #OutFileTab%, "========================================================"

      FOR j% = 1 TO MaxHolidays
         IF Holiday(j%).Selected <> 0 AND Holiday(j%).CrescDate >= StrataDate(1).FirstCres AND Holiday(j%).CrescDate <= StrataDate(TotalStrata%).LastCres THEN
            Dt$ = RIGHT$("00" + LTRIM$(STR$(Holiday(j%).Month)), 2) + "/" + RIGHT$("00" + LTRIM$(STR$(Holiday(j%).Day)), 2) + "/" + LTRIM$(STR$(Holiday(j%).Year))
            PRINT #OutFile%, USING "         #      &    & "; Holiday(j%).Selected; Dt$; Holiday(j%).HName
            PRINT #OutFileTab%, USING "   #      &    & "; Holiday(j%).Selected; Dt$; Holiday(j%).HName
         END IF
      NEXT j%
   END IF
   IF StartPointTotal% > 0 THEN
      PRINT #OutFile%,
      PRINT #OutFile%,
      PRINT #OutFile%, "   The following Starting points or water names were used"
      PRINT #OutFile%,
      PRINT #OutFile%, "           Code     Probability    Description"
      PRINT #OutFile%, "          ========================================================"

      PRINT #OutFileTab%,
      PRINT #OutFileTab%,
      PRINT #OutFileTab%, "The following Starting points or water names were used"
      PRINT #OutFileTab%,
      PRINT #OutFileTab%, " Code     Probability    Description"
      PRINT #OutFileTab%, "========================================================"

      FOR j% = 1 TO StartPointTotal%
         PRINT #OutFile%, USING "         \       \    #.###         & "; StartPoint(j%).Abrev; StartPoint(j%).AsignPortion; StartPoint(j%).Descript
         PRINT #OutFileTab%, USING "\       \    #.###         & "; StartPoint(j%).Abrev; StartPoint(j%).AsignPortion; StartPoint(j%).Descript
      NEXT j%
   END IF

   PRINT #OutFile%, CHR$(12)'FORM FEED
   PRINT #OutFileTab%, CHR$(12)'FORM FEED

   PRINT #OutFile%, "     The following is a list of the scheduling intervals used for"
   PRINT #OutFile%, "     the survey, and the number of week days and weekend days"
   PRINT #OutFile%, "     available to be sampled and the actual number sampled in each"
   PRINT #OutFile%, "     interval.  These intervals may not be related to the strata used"
   PRINT #OutFile%, "     when calculating the creel estimates."
   PRINT #OutFile%, : PRINT #OutFile%, USING "&"; Buffer$ + MainTitle$
   PRINT #OutFile%,
   PRINT #OutFile%, "             ษอออออออออออออออออออออออออออหอออออออออออัอออออออออออป"
   PRINT #OutFile%, "             บ                           บ   Days    ณ   Days    บ"
   PRINT #OutFile%, "             บ   Scheduling intervals    บ Available ณ  Sampled  บ"
   PRINT #OutFile%, "             วฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤืฤฤฤฤฤยฤฤฤฤฤลฤฤฤฤฤยฤฤฤฤฤถ"
   PRINT #OutFile%, "             บ No. ณFirst Day ณ Last Day บ WE  ณ WD  ณ WE  ณ WD  บ"
   PRINT #OutFile%, "             ฬอออออุออออออออออุออออออออออฮอออออุอออออุอออออุอออออน"

   PRINT #OutFileTab%, "The following is a list of the scheduling intervals used for the survey, and "
   PRINT #OutFileTab%, "the number of week days and weekend days available to be sampled and the actual"
   PRINT #OutFileTab%, "sampled in each interval.  These intervals may not be related to the strata "
   PRINT #OutFileTab%, "used when calculating the creel estimates."
   PRINT #OutFileTab%,
   PRINT #OutFileTab%, MainTitle$
   PRINT #OutFileTab%,
   PRINT #OutFileTab%, CHR$(9) + CHR$(9) + CHR$(9) + "Days" + CHR$(9) + CHR$(9) + "Days"
   PRINT #OutFileTab%, CHR$(9) + "Scheduling intervals" + CHR$(9) + CHR$(9) + "Available" + CHR$(9) + CHR$(9) + "Sampled"
   PRINT #OutFileTab%, "No." + CHR$(9) + "First Day" + CHR$(9) + "Last Day" + CHR$(9) + "WE" + CHR$(9) + "WD" + CHR$(9) + "WE" + CHR$(9) + "WD"

  FOR StrCnt% = 1 TO TotalStrata%
      CALL TimeLag(FishingDay(StrCnt%).StartTime, FishingDay(StrCnt%).EndTime, Hours%, Minutes%, HrsMin%)
      DayLength! = Hours% + (Minutes% / 60)
      IF DayLength! < 0 THEN DayLength! = 0
         Fday$ = FUsing$(STR$(StrataDate(StrCnt%).FirstMn), "##") + "/" + RIGHT$("00" + QPtrim$(STR$(StrataDate(StrCnt%).FirstDy)), 2) + "/" + RIGHT$(STR$(StrataDate(StrCnt%).FirstYr), 2)
         Lday$ = FUsing$(STR$(StrataDate(StrCnt%).LastMn), "##") + "/" + RIGHT$("00" + QPtrim$(STR$(StrataDate(StrCnt%).LastDy)), 2) + "/" + RIGHT$(STR$(StrataDate(StrCnt%).LastYr), 2)
      PRINT #OutFile%, USING "             บ ##  ณ \      \ ณ \      \ บ ### ณ ### ณ ### ณ ### บ"; StrCnt%; Fday$; Lday$; StrataDate(StrCnt%).WEavail; StrataDate(StrCnt%).WDavail; StrataDate(StrCnt%).WEsampled; StrataDate(StrCnt%).WDsampled
      PRINT #OutFileTab%, StrCnt%; CHR$(9); Fday$; CHR$(9); Lday$; CHR$(9); StrataDate(StrCnt%).WEavail; CHR$(9); StrataDate(StrCnt%).WDavail; CHR$(9); StrataDate(StrCnt%).WEsampled; CHR$(9); StrataDate(StrCnt%).WDsampled
   NEXT StrCnt%
   PRINT #OutFile%, "             ศอออออฯออออออออออฯออออออออออสอออออฯอออออฯอออออฯอออออผ"
   PRINT #OutFile%, "            Note that holidays are included in count of Weekend Days"
   PRINT #OutFile%, CHR$(12)'FORM FEED
   PRINT #OutFileTab%, "Note that holidays are included in count of Weekend Days"
   PRINT #OutFileTab%, CHR$(12)'FORM FEED
   '
   'DEFINE MONTHS AND YEARS FOR ENTIRE SURVEY
   '
   MonthCount% = 0
   LastMonth% = 0
   LastYear% = 0

   FOR StrCnt% = 1 TO TotalStrata%
      IF LastMonth% <> StrataDate(StrCnt%).FirstMn OR LastYear% <> StrataDate(StrCnt%).FirstYr THEN
         MonthCount% = MonthCount% + 1
         'SurveyBound(MonthCount%).Month = StrataDate(StrCnt%).LastMn
         'SurveyBound(MonthCount%).Year = StrataDate(StrCnt%).LastYr
         'LastMonth% = StrataDate(StrCnt%).LastMn
         'LastYear% = StrataDate(StrCnt%).LastYr
         SurveyBound(MonthCount%).Month = StrataDate(StrCnt%).FirstMn
         SurveyBound(MonthCount%).Year = StrataDate(StrCnt%).FirstYr
         LastMonth% = StrataDate(StrCnt%).FirstMn
         LastYear% = StrataDate(StrCnt%).FirstYr
      ELSEIF LastMonth% <> StrataDate(StrCnt%).LastMn OR LastYear% <> StrataDate(StrCnt%).LastYr THEN
         MonthCount% = MonthCount% + 1
         SurveyBound(MonthCount%).Month = StrataDate(StrCnt%).LastMn
         SurveyBound(MonthCount%).Year = StrataDate(StrCnt%).LastYr
         LastMonth% = StrataDate(StrCnt%).LastMn
         LastYear% = StrataDate(StrCnt%).LastYr
      END IF
   NEXT StrCnt%
   '
   '
   'LOOP TO PRINT THE CALENDAR FOR EACH MONTH.  THE START TIME FOR EACH
   '  COUNT DAY IS SELECTED WHENEVER A COUNT DAY IS ENCOUNTERED.  OTHERWISE
   '  THE DATE WITH NO TIMES IS PRINTED.
   '
   '
   FOR x% = 1 TO MonthCount%
      Month% = SurveyBound(x%).Month
      Year% = SurveyBound(x%).Year
       
      'PRINT PRELIMINARY INFORMATION FOR MONTH TO FILE
      PRINT #OutFile%, BBox$(1)
      PRINT #OutFile%, BBox$(10)
      PRINT #OutFile%, USING BBox$(2); Buffer$ + MainTitle$
      PRINT #OutFile%, BBox$(10)
      PRINT #OutFile%, USING BBox$(3); RTRIM$(MonthData(Month%).Mname) + STR$(Year%)
      PRINT #OutFile%, BBox$(10)
      PRINT #OutFile%, BBox$(4)
      PRINT #OutFile%, BBox$(5)

      PRINT #OutFileTab%, MainTitle$
      PRINT #OutFileTab%, RTRIM$(MonthData(Month%).Mname) + " " + STR$(Year%)
      PRINT #OutFileTab%, "Sunday" + CHR$(9) + "Monday" + CHR$(9) + "Tuesday" + CHR$(9) + "Wednesday" + CHR$(9) + "Thursday" + CHR$(9) + "Friday" + CHR$(9) + "Saturday"


      CALL ComputeMonth(Year%, Month%, DayOfWeek%, DaysInMonth%, CumulativeDays%)
      FirstDay% = DayOfWeek% + 1
      FirstWeek% = True
      DayOfMonth% = 1
      IF Year% = SurveyBound(1).Year THEN
         DayOYear% = CumulativeDays% + DayOfMonth%
      ELSE
         DayOYear% = CumulativeDays% + DayOfMonth% + 366
      END IF

      DO
         FOR j% = 1 TO 7
            IF (FirstWeek% AND j% < FirstDay%) OR DayOfMonth% > DaysPerMonth%(Month%, Year%) THEN
               'A WEEK DAY AT FIRST OR LAST OF MONTH WHICH IS NOT "IN MONTH"
               Dte$(j%) = ""
               FOR k% = 0 TO MaxCounts
                  Week$(j%, k%) = ""
               NEXT k%
            ELSE
               Dte$(j%) = STR$(DayOfMonth%)
               'IF Year(DayOYear%).Sampled <> 0 AND Year(DayOYear%).Sampled <> 3 THEN
               IF Year(DayOYear%).Sampled <> False AND Year(DayOYear%).Sampled <> 3 THEN
                  'IF A SELECTED COUNT DAY AND NOT AN EXCLUDED HOLIDAY
                  CurrentStrata% = Year(DayOYear%).Strata

                  Week$(j%, 0) = Year(DayOYear%).Area

                  IF Counts% > 0 THEN
                     'RESET COUNTS AVAILABLE IF IT CHANGES BETWEEN STRATA
                     IF CountStart(0).Used >= CountStart(CurrentStrata%).Available THEN
                        FOR k% = 0 TO MaxStarts
                           CountStart(k%).Used = False
                        NEXT k%
                     END IF
                     '
                     'SELECT A START TIME
                     '
                     DO
                        'RANDOMLY SELECT THE NEXT START TIME FROM AVAILABLE
                        NextStart% = VAL(QPRound$(Rand#(1#, CDBL(CountStart(CurrentStrata%).Available)), 0))
                        IF CountStart(NextStart%).Used = False THEN
                           CountStart(NextStart%).Used = True
                           CountStart(0).Used = CountStart(0).Used + 1
                           IF CountStart(0).Used >= CountStart(CurrentStrata%).Available THEN
                              FOR k% = 0 TO MaxStarts
                                 CountStart(k%).Used = False
                              NEXT k%
                           END IF
                           EXIT DO
                        END IF
                     LOOP
                     '
                     'SET DIFFERENTIAL BETWEEN FIRST POSSIBLE START AND
                     'THE TIME SELECTED
                     '
                     SELECT CASE StartInterval$
                        CASE "5 minutes"
                           OffSetTime% = (NextStart% - 1) * 5
                        CASE "15 minutes"
                           OffSetTime% = (NextStart% - 1) * 15
                        CASE "30 minutes"
                           OffSetTime% = (NextStart% - 1) * 30
                        CASE "hour"
                           OffSetTime% = (NextStart% - 1) * 60
                     END SELECT
                     '
                     ' ADJUST OffsetTime% FOR MINUTES INCLUDED IN START
                     ' OF THE SAMPLE DAY
                     '
                     OffSetTime% = OffSetTime% + FishingDay(CurrentStrata%).FirstCount MOD 100
                     '
                     'CALCULATE THE ACTUAL START TIME
                     '
                     StartTime% = (((FishingDay(CurrentStrata%).FirstCount \ 100) + (OffSetTime% \ 60)) * 100) + (OffSetTime% MOD 60)
                     '
                     'ASSIGN THE ACTUAL COUNT TIMES
                     '
                     FOR k% = 0 TO Counts% - 1
                        Hours% = ((StartTime% \ 100) + ((Timing(CurrentStrata%).IntervalMn * k%) \ 60)) * 100
                        Minutes% = (StartTime% MOD 100) + ((Timing(CurrentStrata%).IntervalMn * k%) MOD 60)
                        IF Minutes% > 59 THEN
                           Hours% = Hours% + (Minutes% \ 60) * 100
                           Minutes% = Minutes% MOD 60
                        END IF
                        Year(DayOYear%).Times(k% + 1) = Hours% + Minutes%
                        Week$(j%, k% + 1) = STR$(Hours% + Minutes%)
                        IF k% = 0 THEN
                           FirstHr% = Hours%
                           FirstMn% = Minutes%
                        END IF
                     NEXT k%
                  END IF ' IF COUNTS > 0
               ELSE  'NOT A COUNT DAY
                  FOR k% = 0 TO MaxCounts
                     Week$(j%, k%) = ""
                  NEXT k%
               END IF
               DayOfMonth% = DayOfMonth% + 1
               DayOYear% = DayOYear% + 1
            END IF
         NEXT j% 'FOR EACH DAY OF WEEK
         FirstWeek% = False
         '
         'PRINT THE RESULTS TO THE OUTPUT FILE
         '
         PRINT #OutFile%, BBox$(6)
         'PRINT THE DATES
         PRINT #OutFile%, USING BBox$(7); Dte$(1); Dte$(2); Dte$(3); Dte$(4); Dte$(5); Dte$(6); Dte$(7)
         PRINT #OutFileTab%, Dte$(1) + CHR$(9) + Dte$(2) + CHR$(9) + Dte$(3) + CHR$(9) + Dte$(4) + CHR$(9) + Dte$(5) + CHR$(9) + Dte$(6) + CHR$(9) + Dte$(7)
         'PRINT THE COUNT TIMES
         IF StartPointTotal% > 0 THEN
             PRINT #OutFile%, USING BBox$(12); Week$(1, 0); Week$(2, 0); Week$(3, 0); Week$(4, 0); Week$(5, 0); Week$(6, 0); Week$(7, 0)
             PRINT #OutFileTab%, Week$(1, 0) + CHR$(9) + Week$(2, 0) + CHR$(9) + Week$(3, 0) + CHR$(9) + Week$(4, 0) + CHR$(9) + Week$(5, 0) + CHR$(9) + Week$(6, 0) + CHR$(9) + Week$(7, 0)
             LowerBound% = 1
         ELSE
             PRINT #OutFile%, USING BBox$(8); Week$(1, 1); Week$(2, 1); Week$(3, 1); Week$(4, 1); Week$(5, 1); Week$(6, 1); Week$(7, 1)
             PRINT #OutFileTab%, Week$(1, 1) + CHR$(9) + Week$(2, 1) + CHR$(9) + Week$(3, 1) + CHR$(9) + Week$(4, 1) + CHR$(9) + Week$(5, 1) + CHR$(9) + Week$(6, 1) + CHR$(9) + Week$(7, 1)
             LowerBound% = 2
         END IF
         FOR k% = LowerBound% TO Counts%
            PRINT #OutFile%, USING BBox$(8); Week$(1, k%); Week$(2, k%); Week$(3, k%); Week$(4, k%); Week$(5, k%); Week$(6, k%); Week$(7, k%)
            PRINT #OutFileTab%, Week$(1, k%) + CHR$(9) + Week$(2, k%) + CHR$(9) + Week$(3, k%) + CHR$(9) + Week$(4, k%) + CHR$(9) + Week$(5, k%) + CHR$(9) + Week$(6, k%) + CHR$(9) + Week$(7, k%)
         NEXT k%
         IF DayOfMonth% > DaysPerMonth%(Month%, Year%) THEN
            PRINT #OutFile%, BBox$(9)
            PRINT #OutFile%, CHR$(12) 'FORM FEED
            PRINT #OutFileTab%, CHR$(12) 'FORM FEED
            EXIT DO
         END IF
      LOOP
   NEXT x%

   WDperStrata% = SaveWD%
   WEperStrata% = SaveWE%

   PRINT #OutFile%, "     The following is a list of the scheduling intervals used for the survey,"
   PRINT #OutFile%, "     the start, end and average length of the fishing day for each interval,"
   PRINT #OutFile%, "     and the start and end of the sample day as well as the time between the"
   PRINT #OutFile%, "     first and last counts on each sample day."
   PRINT #OutFile%, : PRINT #OutFile%, USING "&"; Buffer$ + MainTitle$
   PRINT #OutFile%,
   PRINT #OutFile%, " ษอออออออออออออออออออออหออออออออออออออออออออออหออออออออออออออออออออออหออออออออป"
   PRINT #OutFile%, " บ                     บ                      บ                      บ Length บ"
   PRINT #OutFile%, " บ Scheduling interval บ     Fishing Day      บ      Sample Day      บ   of   บ"
   PRINT #OutFile%, " วฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤืฤฤฤฤฤฤยฤฤฤฤฤฤยฤฤฤฤฤฤฤฤืฤฤฤฤฤฤยฤฤฤฤฤฤยฤฤฤฤฤฤฤฤถ Count  บ"
   PRINT #OutFile%, " บ First Dayณ Last Day บ Startณ End  ณ Length บ Startณ End  ณ Length บ  Day   บ"
   PRINT #OutFile%, " ฬออออออออออุออออออออออฮออออออุออออออุออออออออฮออออออุออออออุออออออออฮออออออออน"

   PRINT #OutFileTab%, "The following is a list of the scheduling intervals used for the survey,"
   PRINT #OutFileTab%, "the start, end and average length of the fishing day for each interval,"
   PRINT #OutFileTab%, "and the start and end of the sample day as well as the time between the"
   PRINT #OutFileTab%, "first and last counts on each sample day."
   PRINT #OutFileTab%,
   PRINT #OutFileTab%, MainTitle$
   PRINT #OutFileTab%,
   PRINT #OutFileTab%, "Scheduling interval" + CHR$(9) + CHR$(9) + "Fishing Day" + CHR$(9) + CHR$(9) + CHR$(9) + "Sample Day" + CHR$(9) + CHR$(9) + CHR$(9)
   PRINT #OutFileTab%, "First Day" + CHR$(9) + "Last Day" + CHR$(9) + "Start" + CHR$(9) + "End" + CHR$(9) + "Length" + CHR$(9) + "Start" + CHR$(9) + "End" + CHR$(9) + "Length" + CHR$(9) + "Length of Count Day"

   FOR StrCnt% = 1 TO TotalStrata%
      CALL TimeLag(FishingDay(StrCnt%).StartTime, FishingDay(StrCnt%).EndTime, Hours%, Minutes%, HrsMin%)
      CALL TimeLag(FishingDay(StrCnt%).FirstCount, FishingDay(StrCnt%).LastCount, SampHrs%, SampMnt%, HrsMin%)
      Flen$ = FUsing$(STR$(Hours%), "##") + ":" + RIGHT$("00" + QPtrim$(STR$(Minutes%)), 2)
      Slen$ = FUsing$(STR$(SampHrs%), "##") + ":" + RIGHT$("00" + QPtrim$(STR$(SampMnt%)), 2)
      Clen$ = FUsing$(STR$(FishingDay(StrCnt%).WorkDay \ 100), "##") + ":" + RIGHT$("00" + QPtrim$(STR$(FishingDay(StrCnt%).WorkDay MOD 100)), 2)
      Fday$ = FUsing$(STR$(StrataDate(StrCnt%).FirstMn), "##") + "/" + RIGHT$("00" + QPtrim$(STR$(StrataDate(StrCnt%).FirstDy)), 2) + "/" + RIGHT$(STR$(StrataDate(StrCnt%).FirstYr), 2)
      Lday$ = FUsing$(STR$(StrataDate(StrCnt%).FirstMn), "##") + "/" + RIGHT$("00" + QPtrim$(STR$(StrataDate(StrCnt%).LastDy)), 2) + "/" + RIGHT$(STR$(StrataDate(StrCnt%).LastYr), 2)
      PRINT #OutFile%, USING " บ \      \ ณ \      \ บ #### ณ #### ณ  \   \ บ #### ณ #### ณ  \   \ บ  \   \ บ"; Fday$; Lday$; FishingDay(StrCnt%).StartTime; FishingDay(StrCnt%).EndTime; Flen$; FishingDay(StrCnt%).FirstCount; FishingDay(StrCnt%). _
LastCount; Slen$; Clen$
      PRINT #OutFileTab%, Fday$; CHR$(9); Lday$; CHR$(9); FishingDay(StrCnt%).StartTime; CHR$(9); FishingDay(StrCnt%).EndTime; CHR$(9); Flen$; CHR$(9); FishingDay(StrCnt%).FirstCount; CHR$(9); FishingDay(StrCnt%).LastCount; CHR$(9); Slen$; CHR$(9);  _
Clen$
   NEXT StrCnt%
   PRINT #OutFile%, " ศออออออออออฯออออออออออสออออออฯออออออฯออออออออสออออออฯออออออฯออออออออสออออออออผ"
   PRINT #OutFile%, "              Note that day lengths are given as hours and minutes"
   PRINT #OutFile%, "              i.e., 8:30 is 8 hours and 30 minutes"
   PRINT #OutFile%, "   The count day is the time from the first count to the last count, as such"
   PRINT #OutFile%, "   it is a minimum estimate of the working day involved with making counts."

   PRINT #OutFileTab%, "Note that day lengths are given as hours and minutes"
   PRINT #OutFileTab%, "i.e., 8:30 is 8 hours and 30 minutes"
   PRINT #OutFileTab%, "The count day is the time from the first count to the last count, as such"
   PRINT #OutFileTab%, "it is a minimum estimate of the working day involved with making counts."

   IF IntervalWarning% THEN
      PRINT #OutFile%, CHR$(12) 'FORM FEED
      PRINT #OutFile%, "   This is a warning concerning the assumption of uniform sampling during"
      PRINT #OutFile%, "   the sample day.  It is difficult to explain here but suffice to say that"
      PRINT #OutFile%, "   if this warning occurs on your printout you may need to adjust the length"
      PRINT #OutFile%, "   of the sampling day.  Entries in the undersampling column indicate that"
      PRINT #OutFile%, "   a period at the end of the day is not being sampled.  The best remedy is "
      PRINT #OutFile%, "   to either shorten the sample day by that much or lengthen it some "
      PRINT #OutFile%, "   (see the documentation or call me for a better explanation).  The "
      PRINT #OutFile%, "   oversampling column indicates the number of sample intervals which"
      PRINT #OutFile%, "   have the chance of being sampled more than once (again, a thorough "
      PRINT #OutFile%, "   explanation of this problem is available from me but I don't have the"
      PRINT #OutFile%, "   room here)."
      PRINT #OutFile%,
      PRINT #OutFile%, USING "&"; Buffer$ + MainTitle$
      PRINT #OutFile%,
      PRINT #OutFile%, "   Assumptions used in the set-up of this schedule:"
      PRINT #OutFile%, USING "     & between possible start times (Intervals)."; StartInterval$
      PRINT #OutFile%, USING "     ## fishermen counts per sample day."; Counts%
      PRINT #OutFile%, USING "     ##.## Hours between each count. "; Spread#
      PRINT #OutFile%, "          (note that 0 means the program sets the optimum interval)"
      PRINT #OutFile%,
      PRINT #OutFile%, " ษอออออออออออออออออออออหอออออออออออออออหออออออออออออออัอออออออออออออป"
      PRINT #OutFile%, " บ    Sample Strata    บ  Sample Day   บ     Time     ณ             บ"
      PRINT #OutFile%, " วฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤืฤฤฤฤฤฤฤยฤฤฤฤฤฤฤถ UnderSampled ณ  Intervals  บ"
      PRINT #OutFile%, " บ First Dayณ Last Day บ Start ณ  End  บ  (minutes)   ณ OverSampled บ"
      PRINT #OutFile%, " ฬออออออออออุออออออออออฮอออออออุอออออออฮออออออออออออออุอออออออออออออน"

      PRINT #OutFileTab%, CHR$(12) 'FORM FEED
      PRINT #OutFileTab%, "This is a warning concerning the assumption of uniform sampling during"
      PRINT #OutFileTab%, "the sample day.  It is difficult to explain here but suffice to say that"
      PRINT #OutFileTab%, "if this warning occurs on your printout you may need to adjust the length"
      PRINT #OutFileTab%, "of the sampling day.  Entries in the undersampling column indicate that"
      PRINT #OutFileTab%, "a period at the end of the day is not being sampled.  The best remedy is "
      PRINT #OutFileTab%, "to either shorten the sample day by that much or lengthen it some "
      PRINT #OutFileTab%, "(see the documentation or call me for a better explanation).  The "
      PRINT #OutFileTab%, "oversampling column indicates the number of sample intervals which"
      PRINT #OutFileTab%, "have the chance of being sampled more than once (again, a thorough "
      PRINT #OutFileTab%, "explanation of this problem is available from me but I don't have the"
      PRINT #OutFileTab%, "room here)."
      PRINT #OutFileTab%,
      PRINT #OutFileTab%, MainTitle$
      PRINT #OutFileTab%,
      PRINT #OutFileTab%, "Assumptions used in the set-up of this schedule:"
      PRINT #OutFileTab%, USING "& between possible start times (Intervals)."; StartInterval$
      PRINT #OutFileTab%, USING "## fishermen counts per sample day."; Counts%
      PRINT #OutFileTab%, USING "##.## Hours between each count. "; Spread#
      PRINT #OutFileTab%, "(note that 0 means the program sets the optimum interval)"
      PRINT #OutFileTab%,
      PRINT #OutFileTab%, "Scheduling Interval" + CHR$(9) + CHR$(9) + "Sample Day" + CHR$(9) + CHR$(9)
      PRINT #OutFileTab%, "First Day" + CHR$(9) + "Last Day" + CHR$(9) + "Start" + CHR$(9) + "End" + CHR$(9) + "Time Undersampled (minutes)" + CHR$(9) + "Intervals OverSampled"

      FOR StrCnt% = 1 TO TotalStrata%
         CALL TimeLag(FishingDay(StrCnt%).FirstCount, FishingDay(StrCnt%).LastCount, SampHrs%, SampMnt%, HrsMin%)
         Fday$ = FUsing$(STR$(StrataDate(StrCnt%).FirstMn), "##") + "/" + RIGHT$("00" + QPtrim$(STR$(StrataDate(StrCnt%).FirstDy)), 2) + "/" + RIGHT$(STR$(StrataDate(StrCnt%).FirstYr), 2)
         Lday$ = FUsing$(STR$(StrataDate(StrCnt%).FirstMn), "##") + "/" + RIGHT$("00" + QPtrim$(STR$(StrataDate(StrCnt%).LastDy)), 2) + "/" + RIGHT$(STR$(StrataDate(StrCnt%).LastYr), 2)
         IF Warning(StrCnt%).IntervalsLost > 0 THEN
            PRINT #OutFile%, USING " บ \      \ ณ \      \ บ  #### ณ  #### บ     ####     ณ             บ"; Fday$; Lday$; FishingDay(StrCnt%).FirstCount; FishingDay(StrCnt%).LastCount; Warning(StrCnt%).TimeLost
            PRINT #OutFileTab%, Fday$; CHR$(9); Lday$; CHR$(9); FishingDay(StrCnt%).FirstCount; CHR$(9); FishingDay(StrCnt%).LastCount; CHR$(9); Warning(StrCnt%).TimeLost
         ELSEIF Warning(StrCnt%).IntervalsLost < 0 THEN
            PRINT #OutFile%, USING " บ \      \ ณ \      \ บ  #### ณ  #### บ              ณ     ###     บ"; Fday$; Lday$; FishingDay(StrCnt%).FirstCount; FishingDay(StrCnt%).LastCount; Warning(StrCnt%).IntervalsLost
            PRINT #OutFileTab%, Fday$; CHR$(9); Lday$; CHR$(9); FishingDay(StrCnt%).FirstCount; CHR$(9); FishingDay(StrCnt%).LastCount; CHR$(9); Warning(StrCnt%).IntervalsLost
         ELSE
            PRINT #OutFile%, USING " บ \      \ ณ \      \ บ  #### ณ  #### บ              ณ             บ"; Fday$; Lday$; FishingDay(StrCnt%).FirstCount; FishingDay(StrCnt%).LastCount
            PRINT #OutFileTab%, Fday$; CHR$(9); Lday$; CHR$(9); FishingDay(StrCnt%).FirstCount; CHR$(9); FishingDay(StrCnt%).LastCount
         END IF
      NEXT StrCnt%
      PRINT #OutFile%, " ศออออออออออฯออออออออออสอออออออฯอออออออสออออออออออออออฯอออออออออออออผ"
   END IF
   CLOSE #OutFile%
   CLOSE #OutFileTab%

'OutFile% = FREEFILE
'OPEN "yearchek.dat" FOR OUTPUT AS OutFile%
'FOR x% = 1 TO UBOUND(Year)
'  PRINT #OutFile%, USING "x=### day=## mon=## Cresc=##### strata=## Sampled=## WeWd=## Area=\      \"; x%; Year(x%).Day; Year(x%).Month; Year(x%).CrescDate; Year(x%).Strata; Year(x%).Sampled; Year(x%).WeWd; Year(x%).Area
'NEXT x%
'CLOSE #OutFile%

   CalenderPrinted% = True
   CALL QPrintRC("Calendar Creation Complete", 14, 27, CNF.Normal)
   CALL QPrintRC("Press any Key to Return to Menu", 16, 25, CNF.Revers + 128)
   c$ = GetAKey$
   CSMsgBox "", 1, CNF.Revers

END SUB

'
' ====================== COMPUTEMONTH ========================
'     Computes the first day and the total days in a month.
'     DayOfWeek% returns the last day of the previous month
'     allowing you to determine which day of the week is the
'     first day of the month.  DaysInMonth% returns the actual
'     number of days in a month (checking for leap years).
'     CumulativeDays% returns the number of days in the year up to
'     the end of the previous month.
' ============================================================
'
SUB ComputeMonth (Year%, Month%, DayOfWeek%, DaysInMonth%, CumulativeDays%) STATIC
   ''shared MonthData() as MonthType

   CONST LEAP = 366 MOD 7
   CONST Normal = 365 MOD 7

   ' Calculate total number of days (NumDays) since 1/1/1899.

   ' Start with whole years:
   NumDays% = 0
   FOR i% = 1899 TO Year% - 1
      IF IsLeapYear(i%) THEN          ' If year is leap, add
         NumDays% = NumDays% + LEAP   ' 366 MOD 7.
      ELSE                            ' If normal year, add
         NumDays% = NumDays% + Normal ' 365 MOD 7.
      END IF
   NEXT

   ' Compensate if requested year is a leap year:
   IF IsLeapYear(Year%) THEN          ' If year is leap, add
      MonthData(2).Number = 29
   ELSE                               ' If normal year, add
      MonthData(2).Number = 28
   END IF

   CumulativeDays% = 0
   ' Next, add in days from whole months:
   FOR i% = 1 TO Month% - 1
      NumDays% = NumDays% + MonthData(i%).Number
      CumulativeDays% = CumulativeDays% + MonthData(i%).Number
   NEXT

   ' Set the number of days in the requested month:
   DaysInMonth% = MonthData(Month%).Number

   ' 1/1/1899 was a Sunday, so calculating "NumDays MOD 7"
   ' gives the day of week (Sunday = 0, Monday = 1, Tuesday = 2,
   ' and so on) for the first day of the input month:
   DayOfWeek% = NumDays% MOD 7
END SUB

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
' Subroutine to find the dates of the first and last occurence of any day of
' the week.  Sunday=1, Monday=2, ..., Saturday=7.  This is useful for finding
' holidays or changes in daylight savings time.  For example the first Sunday
' in April is when daylight savings time starts and the first Monday in
' September is Labor Day.
'
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
SUB FindWeekDay (Year%, Month%, SearchDay%, First%, Last%)
   First% = 0
   Last% = 0
   done% = False
   CALL ComputeMonth(Year%, Month%, DayOfWeek%, DaysInMonth%, CumulativeDays%)
   DayOfWeek% = DayOfWeek% + 1 'ComputeMonth RETURNS LAST DAY OF PREVIOUS MONTH
   IF DayOfWeek% = 8 THEN DayOfWeek% = 1
   DayOfMonth% = 1
   DO
      IF DayOfWeek% = SearchDay% THEN
         IF First% = 0 THEN
            First% = DayOfMonth%
         END IF
         IF DayOfMonth% + 7 > DaysInMonth% THEN
            Last% = DayOfMonth%
            done% = True
         END IF
      END IF
      DayOfMonth% = DayOfMonth% + 1
      DayOfWeek% = DayOfWeek% + 1
      IF DayOfWeek% = 8 THEN DayOfWeek% = 1
   LOOP UNTIL done%
END SUB 'FindWeekDay

'
' ===================== PRINTCALENDAR ========================
'     Prints a formatted calendar given the year and month.
' ============================================================
'
SUB PrintCalendar (Year%, Month%, Row%, Col%) STATIC

   ' Compute starting day (Su M Tu ...) and total days
   ' for the month:
   CALL ComputeMonth(Year%, Month%, FirstDay%, DaysInMonth%, CumulativeDays%)

   'blank space on screen for new calendar
   FOR NextRow% = Row% TO Row% + 8
      CALL QPrintRC(STRING$(28, " "), NextRow%, Col%, CNF.Normal)
   NEXT NextRow%

   ' Calculates location for centering month and year:
   Header$ = RTRIM$(MonthData(Month%).Mname) + "," + STR$(Year%)
   LeftMargin% = (28 - LEN(Header$)) \ 2

   ' Print header:
   CALL QPrintRC(Header$, Row%, Col% + LeftMargin%, CNF.Normal)
   CALL QPrintRC(" Su   M  Tu   W  Th   F  Sa", Row% + 2, Col%, CNF.Normal)

   ' Recalculate and print tab to the first day
   ' of the month (Su M Tu ...):
   Edit$ = SPACE$(4 * FirstDay%)
   NextRow% = Row% + 2

   ' Print out the days of the month:
   FOR i% = 1 TO DaysInMonth%
      Edit$ = Edit$ + " " + FUsing$(STR$(i%), "##") + " "

      IF QPlen%(Edit$) > 24 OR i% = DaysInMonth% THEN
         NextRow% = NextRow% + 1
         CALL QPrintRC(Edit$, NextRow%, Col%, CNF.Normal)
         Edit$ = ""
      END IF

   NEXT i%

END SUB  'PrintCalendar

FUNCTION Rand# (Lo#, Hi#) STATIC

   RANDOMIZE TIMER
   Rand# = RND(1) * (Hi# - Lo#) + Lo#

END FUNCTION

SUB SetStrata

   StrataDate(1).FirstDy = StartDay%
   StrataDate(1).FirstMn = StartMonth%
   StrataDate(1).FirstYr = StartYear%
   TotalStrata% = 1
   SELECT CASE Strata$
      CASE "month"
         IF StartMonth% = StopMonth% AND StartYear% = StopYear% AND StopDay% < DaysPerMonth%(StopMonth%, StopYear%) THEN
            StrataDate(1).LastDy = StopDay%
            StrataDate(1).LastMn = StopMonth%
            StrataDate(1).LastYr = StopYear%
            StrataDone% = True
         ELSE
            StrataDate(1).LastDy = DaysPerMonth%(StartMonth%, StartYear%)
            StrataDate(1).LastMn = StartMonth%
            StrataDate(1).LastYr = StartYear%
         END IF
      CASE "2 weeks"
         IF (StartDay% + 13) > DaysPerMonth%(StartMonth%, StartYear%) THEN
            StrataDate(1).LastDy = 13 - DaysPerMonth%(StartMonth%, StartYear%) + StartDay%
            StrataDate(1).LastMn = StartMonth% + 1
            'Check for next year
            IF StrataDate(1).LastMn = 13 THEN
               StrataDate(1).LastMn = 1
               StrataDate(1).LastYr = StartYear% + 1
            ELSE
               StrataDate(1).LastYr = StartYear%
            END IF
         ELSE
            StrataDate(1).LastDy = StartDay% + 13
            StrataDate(1).LastMn = StartMonth%
            StrataDate(1).LastYr = StartYear%
         END IF
      CASE "week"
         IF (StartDay% + 6) > DaysPerMonth%(StartMonth%, StartYear%) THEN
            StrataDate(1).LastDy = 6 - DaysPerMonth%(StartMonth%, StartYear%) + StartDay%
            StrataDate(1).LastMn = StartMonth% + 1
            'Check for next year
            IF StrataDate(1).LastMn = 13 THEN
               StrataDate(1).LastMn = 1
               StrataDate(1).LastYr = StartYear% + 1
            ELSE
               StrataDate(1).LastYr = StartYear%
            END IF
         ELSE
            StrataDate(1).LastDy = StartDay% + 6
            StrataDate(1).LastMn = StartMonth%
            StrataDate(1).LastYr = StartYear%
         END IF
   END SELECT
   IF StrataDate(1).LastMn = StopMonth% AND StrataDate(1).LastDy >= StopDay% AND StrataDate(1).LastYr = StopYear% THEN
      StrataDate(1).LastDy = StopDay%
      StrataDone% = True
   ELSE
      StrataDone% = False
   END IF

   IF StrataDone% THEN GOTO SkipLoop

   DO
      TotalStrata% = TotalStrata% + 1

      'SET FIRST DATE FOR STRATA = END OF PREVIOUS STRATA PLUS 1 DAY
      ThisYear% = (StrataDate(TotalStrata% - 1).LastYr)
      ThisMonth% = (StrataDate(TotalStrata% - 1).LastMn)
      ThisDay% = (StrataDate(TotalStrata% - 1).LastDy)
      CALL ComputeMonth(ThisYear%, ThisMonth%, FirstDay%, DaysInMonth%, CumulativeDays%)
      DaysThisMonth% = DaysInMonth%
      IF (ThisDay% + 1) <= DaysThisMonth% THEN
         StrataDate(TotalStrata%).FirstDy = ThisDay% + 1
         StrataDate(TotalStrata%).FirstMn = ThisMonth%
            StrataDate(TotalStrata%).FirstYr = ThisYear%
      ELSE
         'FIRST DAY OF NEXT MONTH
         StrataDate(TotalStrata%).FirstDy = 1
         StrataDate(TotalStrata%).FirstMn = ThisMonth% + 1
         IF StrataDate(TotalStrata%).FirstMn = 13 THEN
            StrataDate(TotalStrata%).FirstYr = ThisYear% + 1
            StrataDate(TotalStrata%).FirstMn = 1
         ELSE
            StrataDate(TotalStrata%).FirstYr = ThisYear%
         END IF
      END IF

      'SET LAST DAY OF STRATA
      ThisYear% = (StrataDate(TotalStrata%).FirstYr)
      ThisMonth% = (StrataDate(TotalStrata%).FirstMn)
      ThisDay% = (StrataDate(TotalStrata%).FirstDy)
      CALL ComputeMonth(ThisYear%, ThisMonth%, FirstDay%, DaysInMonth%, CumulativeDays%)
      DaysThisMonth% = DaysInMonth%
      SELECT CASE Strata$
         CASE "month"
             NewDay% = DaysThisMonth%
             NewMonth% = ThisMonth%
             NewYear% = ThisYear%
             DaysInStrata% = DaysInMonth%
         CASE "2 weeks"
            IF (ThisDay% + 13) > DaysThisMonth% THEN
               NewDay% = 13 - DaysThisMonth% + ThisDay%
               NewMonth% = ThisMonth% + 1
               IF NewMonth% = 13 THEN
                  NewMonth% = 1
                  NewYear% = ThisYear% + 1
               ELSE
                  NewYear% = ThisYear%
               END IF
            ELSE
               NewDay% = ThisDay% + 13
               NewMonth% = ThisMonth%
               NewYear% = ThisYear%
            END IF
            DaysInStrata% = 14
         CASE "week"
            IF (ThisDay% + 6) > DaysThisMonth% THEN
               NewDay% = 6 - DaysThisMonth% + ThisDay%
               NewMonth% = ThisMonth% + 1
               IF NewMonth% = 13 THEN
                  NewMonth% = 1
                  NewYear% = ThisYear% + 1
               ELSE
                  NewYear% = ThisYear%
               END IF
            ELSE
               NewDay% = ThisDay% + 6
               NewMonth% = ThisMonth%
               NewYear% = ThisYear%
            END IF
            DaysInStrata% = 7
      END SELECT
      IF (NewMonth% > StopMonth% OR (NewMonth% >= StopMonth% AND NewDay% >= StopDay%)) AND NewYear% = StopYear% THEN
         StrataDate(TotalStrata%).LastYr = StopYear%
         StrataDate(TotalStrata%).LastMn = StopMonth%
         StrataDate(TotalStrata%).LastDy = StopDay%
         StrataDone% = True
         IF NewDay% <> StopDay% THEN
            DaysInStrata% = StrataDate(TotalStrata%).LastDy - StrataDate(TotalStrata%).FirstDy + 1
         END IF
      ELSE
         StrataDate(TotalStrata%).LastYr = NewYear%
         StrataDate(TotalStrata%).LastMn = NewMonth%
         StrataDate(TotalStrata%).LastDy = NewDay%
      END IF
 
   LOOP WHILE TotalStrata% < MaxStrata AND NOT StrataDone%

SkipLoop:


   'SET THE STRATA VARIABLE IN THE YEAR ARRAY
   DayOfYear% = CumulativeDays% + StrataDate(TotalStrata%).FirstDy
   FOR Str% = 1 TO TotalStrata%
      CALL ComputeMonth(StrataDate(Str%).FirstYr, StrataDate(Str%).FirstMn, FirstDay%, DaysInMonth%, CumulativeDays%)
      IF StrataDate(Str%).LastMn = StrataDate(Str%).FirstMn THEN
         DaysInStrata% = StrataDate(Str%).LastDy - StrataDate(Str%).FirstDy + 1
      ELSE
         DaysInStrata% = DaysPerMonth%(StrataDate(Str%).FirstMn, StrataDate(Str%).FirstYr) - StrataDate(Str%).FirstDy + 1 + StrataDate(Str%).LastDy
      END IF
      WhichYear% = StrataDate(Str%).FirstYr - StartYear%
      DayOfYear% = (WhichYear% * 366) + CumulativeDays% + StrataDate(Str%).FirstDy
      FOR j% = DayOfYear% TO (DayOfYear% + DaysInStrata% - 1)
         Year(j%).Strata = Str%
      NEXT j%
      StrataDate(Str%).FirstCres = CrescentDate%(StrataDate(Str%).FirstDy, StrataDate(Str%).FirstMn, StrataDate(Str%).FirstYr)
      StrataDate(Str%).LastCres = CrescentDate%(StrataDate(Str%).LastDy, StrataDate(Str%).LastMn, StrataDate(Str%).LastYr)
   NEXT Str%

END SUB 'SetStrata

SUB SetYear
   'LOCATE 5, 1: PRINT "starting set year  "

   FOR YR% = StartYear% TO StopYear%
      DayOfYear% = 1
      WhichYear% = YR% - StartYear%
' LOCATE 10, 1: PRINT USING "StartYear=####  StopYear=####  WhichYear=####  Yr=####"; StartYear%; StopYear%; WhichYear%; YR%
      FOR Mnth% = 1 TO 12
' LOCATE 8, 1: PRINT "before calling computemonth"
         CALL ComputeMonth(YR%, Mnth%, DayOfWeek%, DaysInMonth%, CumulativeDays%)
' LOCATE 8, 1: PRINT "after calling computemonth "
         FOR DY% = 1 TO DaysInMonth%
            j% = DayOfYear% + (WhichYear% * 366)
            Year(j%).Month = Mnth%
            Year(j%).Day = DY%
            Year(j%).Strata = 0
            Year(j%).Sampled = 0
            Year(j%).CrescDate = CrescentDate%(DY%, Mnth%, YR%)
            IF DayOfWeek% = 0 OR DayOfWeek% = 6 THEN
               Year(j%).WeWd = 1
            ELSE
               Year(j%).WeWd = 0
            END IF
            DayOfYear% = DayOfYear% + 1
            DayOfWeek% = DayOfWeek% + 1
            IF DayOfWeek% = 7 THEN DayOfWeek% = 0
         NEXT DY%
      NEXT Mnth%
   NEXT YR%

' LOCATE 6, 1: PRINT "exiting set year"

END SUB

SUB TimeLag (EarlyTime%, LateTime%, Hours%, Minutes%, HrsMin%)
   Hours% = (LateTime% \ 100) - (EarlyTime% \ 100)
   IF (LateTime% MOD 100) < (EarlyTime% MOD 100) THEN
      Hours% = Hours% - 1
      Minutes% = (LateTime% MOD 100) + (60 - (EarlyTime% MOD 100))
   ELSE
      Minutes% = (LateTime% MOD 100) - (EarlyTime% MOD 100)
   END IF
   HrsMin% = Hours% * 100 + Minutes%
END SUB

