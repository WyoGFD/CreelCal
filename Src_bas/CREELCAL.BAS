DECLARE SUB GetHours ()
' ==============================================================================
' ==============================================================================
' ==============================================================================
'
'   PROGRAM CREELCAL
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
'   Non-uniform probability sampling components have been added to the program.
'
'   UPDATES:
'       Ver 1.1  1-26-89, added ability to have start times every 15 minutes,
'                   30 minutes, or every hour.
'       Ver 1.2  4/23/90, changed video files, added start up screen,
'                  added video configuration change/check
'       Ver 1.3  12/10/90, fixed bug in "weekly" setup, added ability to
'                  generate calendars with no sample times, added ability
'                  to set first and last count times in relation to fishing
'                  day, also added ability to copy times to all cells below it
'       Ver 1.4  03/27/91, added ability to have count time every five minutes,
'                  also added automatic interval between counts to avoid
'                  oversampling certain times of the day (times where counts
'                  could overlap)
'
'       Ver 1.5  03/09/92 - Translate to QuickBasic 4.5
'                Problems to fix:  undersampling if automatic interval doesn't
'                fit into sample day, add possibility to add start areas or
'                multiple waters
'
'       Ver 2.0  05/26/92 - Translate to PDS 7.1
'
'       Ver 2.1  08/25/92 - Added help facility, a few other minor
'                modifications
'       Ver 2.11 04/06/93 - Added probability to starting points
'       Ver 2.12 05/05/93 - updated starting point probability to allow
'                completely random selection
'       Ver 3.00 05/18/93 -  added a new interface using Crescent Software's
'                menu and other utilities from them.
'                Also added an additional starting point and holiday
'       Ver 3.01 04/22/94 -  changed the way it handles starting points and
'                add more starting points.  Add mouse support.
'       Ver 3.02 12/15/94 -  tinkered with dates to get math working across
'                years, used Crescent's Date2Num instead of the julian date
'                math I had used.
'       Ver 3.02 1/3/95 -  worked to get holidays set correctly
'       Ver 4.00 5/26/04 -  continued working on holidays.  Fixed several other
'                bugs that Dave had found - week and 2 week strata, partial
'                strata, named starting points, and others.
'
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'  SUBROUTINE descriptions
'
'  In CRELCAL2.BAS
'    PrintCalendar - print a monthly calendar to the screen
'    ComputeMonth
'    EditHoliday - select holidays to include in the creel survey
'    FindWeekDay
'    GetAKey$ - waits for one keystroke
'    GetHelpText
'    GetHolidays
'    HelpMe
'    HelpOnHelp
'    HelpSetup
'    IsLeapYear%
'    OOPS
'    QPROUND$
'    ReadWrite - Reads or Writes data to ASCII file
'    SetHolidays
'    SetStrata - defines the starting and ending dates for each strata
'    SetYear
'    ViewMonths
'  In CRELCAL3.BAS
'    Editor
'    GetStartPoints
'    GetCountRegime
'    GetDates
'    GetHours
'    GetStrata
'  In CALBUILD.BAS
'    AssignDays - randomly assigns the counts days to each strata
'    AssignRandom
'    BuildCal
'    TimeLag
'  In PROMENU.BAS
'    BarPrint
'    CheckMark
'    CSMsgBox
'    DisplayTime
'    PickFileList
'    PullDnMs
'    PullMenKey%
'    SetColor
'    SetStatus
'    VertMenu
'    WindowMgr
'
'*****************************************************************************
' The calendar portion of this program was taken from the MicroSoft
' QuickBasic 4.5 Manual Page 122.  They called the program CAL.BAS,
' I borrowed a couple subroutines from them.
'*****************************************************************************
'*****************************************************************************
'
' Many subroutines (most in PROMENU.BAS file) were taken from
' Crescent Software's QuickPak Professional Library.  Some modifications were
' made to those.
'
'*****************************************************************************
'*************************  Start of Main Program Code  **********************
'*****************************************************************************

'$DYNAMIC

'$INCLUDE: 'creelcal.bi'

Version$ = "QBX-4.00  06/01/04"

'Use Testing Phase to skip menu and go directly to making calendar with defaults
'TestingPhase% = True
TestingPhase% = False


' ---------------------------------------------------------------------------
' --------------------------  START OF SETCNF.BI  ---------------------------
' ---------------------------------------------------------------------------
CALL InitMouse(Cnf.Mouse)
IF Cnf.Mouse THEN
   GetVMode 0, 0, 0, Temp%, 0     'Setup the mouse
   MouseTrap 1, 1, Temp%, 80
   Cnf.NPixLines = 8
   IF Temp% > 43 AND DOSVer% >= 1000 THEN Cnf.NPixLines = 7
ELSE
   Cnf.NPixLines = 8
END IF

Cnf.MonTyp = Monitor

ClrDisp% = False
CGAwMono% = False
MonoDisp% = True

ClrDisp% = Cnf.MonTyp = 3 OR (Cnf.MonTyp >= 5 AND Cnf.MonTyp <= 9) OR Cnf.MonTyp = 11
IF INSTR(COMMAND$, "/B") THEN ClrDisp% = 0

IF Cnf.MonTyp = 1 OR Cnf.MonTyp = 2 THEN
   MonoDisp% = True
   ClrDisp% = False
   CGAwMono% = False
ELSEIF Cnf.MonTyp = 3 AND NOT INSTR(COMMAND$, "/B") THEN
   ClrDisp% = False
   CGAwMono% = False
   MonoDisp% = True
   CALL SetColor(ClrDisp%, CGAwMono%, MonoDisp%)
   LOCATE 1
   MQPrint STRING$(25 * 80, "²"), 112    'Make a background for the intro
   PAGE% = -1 'current page
   ULRow% = 8
   LRRow% = 20
   ULCol% = 15
   LRCol% = 66

   Text$ = "A hardware check has determined that you have a "
   Text$ = Text$ + "CGA card (Color Graphics Adapter) installed on this "
   Text$ = Text$ + "computer.  It is not possible to detect whether a "
   Text$ = Text$ + "monochrome or color adapter is attached to it.  "
   Text$ = Text$ + "Press 'C' if you have a color monitor or "
   Text$ = Text$ + "'M' if you have a monochrome monitor.| |"
   Text$ = Text$ + "                 Press 'M' or 'C'."
   LOCATE ULRow%, 1                         'position the starting line for box
   Wdth = LRCol% - ULCol% + 1               'set text width
   CSMsgBox Text$, Wdth, Cnf.MenBox

   DO
      c$ = UCASE$(INPUT$(1))
   LOOP UNTIL INSTR("MC", c$)
   MonoDisp% = False
   IF c$ = "M" THEN
      CGAwMono% = True
      ClrDisp% = False
   ELSE
      ClrDisp% = True
      CGAwMono% = False
   END IF

   CSMsgBox "", 1, Cnf.MenBox

END IF

CALL SetColor(ClrDisp%, CGAwMono%, MonoDisp%)

IF Cnf.MonTyp >= 3 AND Cnf.MonTyp <= 5 THEN
   Cnf.CurSize = 7
ELSE
   Cnf.CurSize = 12
END IF
' ----------------------------------------------------------------------------
' ----------------------------  END OF SETCNF.BI  ----------------------------
' ----------------------------------------------------------------------------

' ==============================================================================
' ==============================================================================
' ------------------------------ PRELIMINARY DECLARATIONS ----------------------
' ==============================================================================
' ==============================================================================
Punctuation$ = "/.,?><;:\][}{|=+_'"
Alphabet$ = "abcdefghijklmnopqrstuvwxyz"
Alphabet$ = Alphabet$ + UCASE$(Alphabet$) + " "
Numbers$ = "1234567890.- "
'' can replace with Valid%(DataFileName$) from QP
OKforFileName$ = LEFT$(Alphabet$, 52) + Numbers$ + "\:_!@#$%&(){}"

' Data for the months of the year:
MonthData(0).Mname = "None entered": MonthData(0).Number = 0
MonthData(1).Mname = "January": MonthData(1).Number = 31
MonthData(2).Mname = "February": MonthData(2).Number = 28
MonthData(3).Mname = "March": MonthData(3).Number = 31
MonthData(4).Mname = "April": MonthData(4).Number = 30
MonthData(5).Mname = "May": MonthData(5).Number = 31
MonthData(6).Mname = "June": MonthData(6).Number = 30
MonthData(7).Mname = "July": MonthData(7).Number = 31
MonthData(8).Mname = "August": MonthData(8).Number = 31
MonthData(9).Mname = "September": MonthData(9).Number = 30
MonthData(10).Mname = "October": MonthData(10).Number = 31
MonthData(11).Mname = "November": MonthData(11).Number = 30
MonthData(12).Mname = "December": MonthData(12).Number = 31
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'READ IN HELP TEXT BEFORE STARTING MENU
CALL HelpSetup("CREELCAL.HLP")

' ==============================================================================
' ------------------------------ START OF CODE ---------------------------------
' ==============================================================================

REDIM Banner$(10)
 Banner$(1) = "ÛÛÛÛÛÛÛÛ                                      ÛÛÛÛÛÛÛÛ                  "
 Banner$(2) = "ÛÛßßßßßß ÛÛÛÛÛÛÛÛÛ ÛÛÛÛÛÛÛÛ ÛÛÛÛÛÛÛÛ ÛÛ       ÛÛßßßßßß ÛÛÛÛÛÛÛÛ ÛÛ      "
 Banner$(3) = "ÛÛ       ÛÛßßßßßÛÛ ÛÛßßßßßß ÛÛßßßßßß ÛÛ       ÛÛ       ÛÛßßßßÛÛ ÛÛ      "
 Banner$(4) = "ÛÛ       ÛÛ     ÛÛ ÛÛ       ÛÛ       ÛÛ       ÛÛ       ÛÛ    ÛÛ ÛÛ      "
 Banner$(5) = "ÛÛ       ÛÛ     ÛÛ ÛÛÜÜÜÜÜÜ ÛÛÜÜÜÜÜÜ ÛÛ       ÛÛ       ÛÛÜÜÜÜÛÛ ÛÛ      "
 Banner$(6) = "ÛÛ       ÛÛÛÛÛÛÛÛÛ ÛÛÛÛÛÛÛÛ ÛÛÛÛÛÛÛÛ ÛÛ       ÛÛ       ÛÛÛÛÛÛÛÛ ÛÛ      "
 Banner$(7) = "ÛÛ       ÛÛ   ÞÛÝ  ÛÛ       ÛÛ       ÛÛ       ÛÛ       ÛÛ    ÛÛ ÛÛ      "
 Banner$(8) = "ÛÛ       ÛÛ    ÛÛ  ÛÛ       ÛÛ       ÛÛ       ÛÛ       ÛÛ    ÛÛ ÛÛ      "
 Banner$(9) = "ÛÛÜÜÜÜÜÜ ÛÛ    ÛÛÝ ÛÛÜÜÜÜÜÜ ÛÛÜÜÜÜÜÜ ÛÛÜÜÜÜÜÜ ÛÛÜÜÜÜÜÜ ÛÛ    ÛÛ ÛÛÜÜÜÜÜÜ"
Banner$(10) = "ÛÛÛÛÛÛÛÛ ÛÛ   ÞÛÛÛ ÛÛÛÛÛÛÛÛ ÛÛÛÛÛÛÛÛ ÛÛÛÛÛÛÛÛ ÛÛÛÛÛÛÛÛ ÛÛ    ÛÛ ÛÛÛÛÛÛÛÛ"


CLS
LOCATE 1
MQPrint STRING$(25 * 80, "²"), Cnf.Normal    'Make a background for the intro

Lines% = 1
PAGE% = -1 'current page
ULRow% = 3
ULCol% = 3
LRRow% = 14
LRCol% = 78
BannerColor% = 116 '20
'OPEN NEXT AVAILABLE WINDOW
HideCursor
CALL WindowMgr(0, 1, ULRow% - 1, ULCol% - 1, LRRow% + 1, LRCol% + 1, BannerColor%)
ShowCursor

'SCROLL Banner$ ONTO PAGE FROM top to bottom
FOR j% = 10 TO 1 STEP -1
   CALL ScrollD(ULRow%, ULCol%, LRRow%, LRCol%, Lines%, PAGE%)
   CALL QPrintRC(Banner$(j%), ULRow% + 1, 5, BannerColor%)
   CALL Pause(1)
NEXT j%

CALL Pause(5)

'Define and Display Credits
 Banner$(1) = "               Version " + Version$
 Banner$(2) = ""
 Banner$(3) = "       Program Developed by:"
 Banner$(4) = "               Dirk D. Miller"
 Banner$(5) = "               Wyoming Game and Fish Department"
 Banner$(6) = "               528 S. Adams Street                    "
 Banner$(7) = "               Laramie, WY  82070                     "
 Banner$(8) = "               (307) 745-4046 (Work) 745-8270 (FAX)   "
 Banner$(9) = "                   @Copyright 1994-Dirk Miller        "
 Banner$(10) = "               <PRESS A KEY TO CONTINUE>             "
HideCursor
CALL WindowMgr(0, 1, 14, 11, 25, 70, Cnf.HiLite)     'open next available window
ShowCursor
FOR K% = 1 TO 10
   ScrollU 15, 12, 24, 69, Lines%, PAGE%
   CALL QPrintRC(Banner$(K%), 24, 13, Cnf.HiLite)
   CALL Pause(1)
NEXT K%

Ky$ = ""
DO
   IF Cnf.Mouse THEN CALL ButtonPress(1, Down%, Presses%, MCol%, MRow%)
   Ky$ = INKEY$
LOOP UNTIL LEN(Ky$) OR Presses%

'Scroll next set of comments onto screen
 Banner$(1) = "              General Operation tips:                   "
 Banner$(2) = "                                                        "
 Banner$(3) = "   Press ALT to activate the menu, Highlight the        "
 Banner$(4) = "   desired option and press ENTER or press the          "
 Banner$(5) = "   Highlighted Letter of the desired option.            "
 Banner$(6) = "   The menu is also navigable using a mouse.            "
 Banner$(7) = "   Help is available throughout the program by pressing "
 Banner$(8) = "   F1, return to the program from Help by pressing ESC. "
 Banner$(9) = "                                                        "
 Banner$(10) = "               <PRESS A KEY TO CONTINUE>               "

FOR K% = 0 TO 9
   ScrollD 15, 12, 24, 69, Lines%, PAGE%
   CALL QPrintRC(Banner$(10 - K%), 15, 13, Cnf.HiLite)
   CALL Pause(1)
NEXT K%

Ky$ = ""
DO
   IF Cnf.Mouse THEN CALL ButtonPress(1, Down%, Presses%, MCol%, MRow%)
   Ky$ = INKEY$
LOOP UNTIL LEN(Ky$) OR Presses%

'Clear windows and start main loop
HideCursor
WindowMgr 0, 0, 0, 0, 0, 0, 0               'close previous window
WindowMgr 0, 0, 0, 0, 0, 0, 0               'close previous window
ShowCursor

' =============================================================================
' -------------------------------- END IF INTRO CODE --------------------------
' =============================================================================
CLS

LOCATE 2
MQPrint STRING$(23 * 80, "²"), Cnf.Normal    'Make a background for the menu
LOCATE 24, 1                                 'Print instructions
MQPrint " Press [Alt] key to Activate MENU.  Items marked ® must be completed to proceed.", Cnf.InActHiLt
LOCATE 25, 1
MQPrint "                       CreelCal Version " + Version$ + ".                      ", Cnf.InActHiLt
LOCATE 25, 2
MQPrint DATE$, Cnf.InActHiLt
LOCATE 13, 41, 1

' =============================================================================
'INITITALIZE THE VARIABLES WHICH DEFINE WHETHER THE SET UP IS COMPLETE
TitleEntered% = False
DatesEntered% = False
StrataEntered% = False
CountEntered% = False
FishingDayEntered% = False
HolidayEntered% = False
SetUpReady% = False
CalendarPrinted% = False

DataFileName$ = "CREELCAL.DAT"

'Define default data for starting program
MainTitle$ = "Main Title"
Buffer$ = SPACE$(38 - INT(LEN(MainTitle$) / 2))

'DEFINE DEFAULT PARAMETER VALUES

StartDay% = 1     'June 1
StartMonth% = 6   'June
StartYear% = VAL(RIGHT$(DATE$, 4))
StopDay% = 31     'October 31
StopMonth% = 10   'October
StopYear% = StartYear%
Strata$ = "month"
WDperStrata% = 3
WEperStrata% = 2
Counts% = 3
Spread# = 0#
StartInterval$ = "5 minutes"

TotalStrata% = 9

CALL SetYear
CALL SetStrata

FOR j% = 1 TO MaxStrata
  FishingDay(j%).StartTime = 800
  FishingDay(j%).EndTime = 2000
NEXT j%

CALL SetHolidays

HolidayCount% = 0

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
'  Menu Definition for QuickPak PullDnMs Menu System
'
'To set the "Hot Key" character for a menu Choice, set the High byte of the
'corresponding element of the "Stat()" array to the offset into the string
'of the letter.
'
'Example: Make the "A" hot in "Save As...".
'
'                  0123456789
'   Menu$(5, 0) = "Save As..."
'                ÚÄÄÄÄÄÄÙ
'   Stat(5, 0) = 5 * 256 + 1
'                          ^ Optional to make the Choice inactive.
'
'If you later want to reset the status (active/inactive) without affecting
'the hot key position in the high byte, use the "SetStatus" sub program as
'follows:
'
' Status = LEN(FileName$) = 0          'Can't save a file until one is loaded
' CALL SetStatus(Stat(5, 0), Status)   'If there was no file name, Choice 5
'                                      '  of menu 0 will be made inactive.
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'----- Define the Menu Choices here
REDIM Menu$(9, 3)                      'Allocate space for the Menu Choices
REDIM Stat(9, 3)                       'Define the Choice status array

'----- Define the menus
Menu$(0, 0) = "Calendar Options"
Stat(0, 0) = 0
Menu$(1, 0) = "Title                                             "
Menu$(2, 0) = "Year and Months to Survey                         "
Menu$(3, 0) = "Define Sample Strata                              "
Menu$(4, 0) = "Define Count Regime                               "
Menu$(5, 0) = "Define Fishing Day/Sample Day                     "
Menu$(6, 0) = "Define Holidays and Special Days                  "
Menu$(7, 0) = "Starting Points/Water Names                       "
Menu$(8, 0) = "-"
Menu$(9, 0) = "Set up and Print Calendar based on Current Options"
 Stat(1, 0) = 0
 Stat(2, 0) = 0
 Stat(3, 0) = 7 * 256
 Stat(4, 0) = 7 * 256
 Stat(5, 0) = 7 * 256
 Stat(6, 0) = 7 * 256
 Stat(7, 0) = 9 * 256
 Stat(9, 0) = 1 * 256
CALL CheckMark(Menu$(), 0, 1, False)
CALL CheckMark(Menu$(), 0, 2, False)
CALL CheckMark(Menu$(), 0, 3, False)
CALL CheckMark(Menu$(), 0, 4, False)
CALL CheckMark(Menu$(), 0, 5, False)
CALL CheckMark(Menu$(), 0, 6, False)
                          
Menu$(0, 1) = "Other Options"
Stat(0, 1) = 0
Menu$(1, 1) = "Read Calendar Options From A Data File "
Menu$(2, 1) = "Write Current Options To A Data File   "
Menu$(3, 1) = "Display Current Program Limits         "
 Stat(3, 1) = 24 * 256
Menu$(4, 1) = "Display Video Options                  "
 Stat(4, 1) = 8 * 256
    
Menu$(0, 2) = "QUIT"
Stat(0, 2) = 0
Menu$(1, 2) = "Quit Program and return to DOS"

Menu$(0, 3) = "Help"
Stat(0, 3) = 12
Menu$(1, 3) = "General Program HELP"
Menu$(2, 3) = "HELP Index"

' =============================================================================
' --------------------------- END OF MENU DEFINITIONS -------------------------
' =============================================================================

CALL SplitColor(Cnf.Normal, NormalFG%, NormalBG%)
CALL SplitColor(Cnf.Revers, ReverseFG%, ReverseBG%)
CALL SplitColor(Cnf.ActivCh, MenuFG%, MenuBG%)
CALL SplitColor(Cnf.HiLite, HiLightFG%, HilightBG%)
CALL SplitColor(Cnf.Blink, BlinkFG%, BlinkBG%)
CALL SplitColor(Cnf.ReverseBlink, ReverseBlinkFG%, ReverseBlinkBG%)

' =======================================================================
' Main Loop : Stay in loop until loop is exited
' =======================================================================

' PULLDNMS SETUP BEFORE STARTING MAIN LOOP
   InitMouse Temp%                         'Define the Mouse cursor
   TextCursor -2, -2                       'set mouse colors to inverse
   SetCursor 1, 1                          'Locate it at upper left of screen
   ShowCursor                              'Turn it on

   BarPrint Menu$(), Stat()                'Display the menu bar and build a
                                           '  table of Hot Keys for menu bar.
   Action% = 0                             'Action 0 (no polling) is the easiest
                                           '  way to use PullDown.
   LOCATE 13, 41, 1
   'THIS ALLOWS THE DISPLAY OF THE MENU TO START WITH THE FIRST SET PULLED DOWN
   Menu% = 0
   Choice% = 1
   'REDISPLAYS THE MOST CURRENT MENU ON THE FIRST PASS THROUGH THE INKEY LOOP
   FirstPass% = True

   Tim! = TIMER                            'init the time

' ----------------------------------------------------------------------------
' ----------------------------------------------------------------------------
' ------------------------- START OF MAIN LOOP -------------------------------
' ----------------------------------------------------------------------------
' ----------------------------------------------------------------------------


DO

   CALL DisplayTime

   Ky$ = INKEY$                         'Get a key press from the buffer
                                        'See if it's a hot-key or mouse
                                        '  pressed on menu bar.
   IF NOT FirstPass% THEN
      Menu% = PullMenKey(Ky$, Menu$(), Stat())
      KeyCode% = KeyPressed%(Ky$)
   ELSE
      KeyCode% = Tabb
      FirstPass% = False
   END IF

   IF Menu% > -1 AND KeyCode% <> Escape THEN                    'Yes
                                        
      'CHECK TO SEE IF EVERYTHING IS SET UP SO THAT THE CALENDAR CAN BE CREATED
      IF TitleEntered% AND DatesEntered% AND FishingDayEntered% AND StrataEntered% AND CountEntered% AND HolidayEntered% THEN
         SetUpReady% = True
      END IF

      CALL CheckMark(Menu$(), 0, 1, TitleEntered%)
      CALL CheckMark(Menu$(), 0, 2, DatesEntered%)
      CALL CheckMark(Menu$(), 0, 3, StrataEntered%)
      CALL CheckMark(Menu$(), 0, 4, CountEntered%)
      CALL CheckMark(Menu$(), 0, 5, FishingDayEntered%)
      CALL CheckMark(Menu$(), 0, 6, HolidayEntered%)

      PullDnMs Menu$(), Stat(), Menu%, Choice%, Ky$, Action%

   IF TestingPhase% THEN
      CALL BuildCal(CalendarPrinted%, TotalStrata%)
      CalendarPrinted% = True
      Menu% = 2
   END IF

      KeyCode% = KeyPressed%(Ky$)

      IF KeyCode% = Escape THEN GOTO SkipAction

      SELECT CASE Menu%                  'Branch according to "Menu"
         CASE 0        'Main Options
            'LOCATE 10, 10
            SELECT CASE Choice%          'Branch according to "Choice"
               CASE 1  'Title

                  HideCursor
                  CALL WindowMgr(0, 1, 5, 1, 20, 80, Cnf.Normal)     'open next available window
                  ShowCursor
                  CALL QPrintRC("You should enter a title that will appear on all output for", 7, 10, Cnf.Normal)
                  CALL QPrintRC("this creel calendar.  The title should contain the name of", 8, 10, Cnf.Normal)
                  CALL QPrintRC("the body of water being surveyed and should fit inside the box.", 9, 10, Cnf.Normal)
                  CALL QPrintRC("F1 for Help, F10 to proceed.", 17, 26, Cnf.Normal)
       '         1         2         3         4         5         6         7         8
       '12345678901234567890123456789012345678901234567890123456789012345678901234567890
                  CALL QPrintRC("F1", 17, 26, Cnf.HiLite)
                  CALL QPrintRC("F10", 17, 39, Cnf.HiLite)
                  DO
                     CALL QPrintRC("                                                                            ", 14, 3, Cnf.Revers)
                     CALL QPrintRC(MainTitle$, 14, 3, Cnf.Revers)
                     LeftCol% = 3                     'set the left column
                     RightCol% = 77                   'and the right column
                     ValidInput$ = Alphabet$ + Punctuation$ + Numbers$
                     CALL Editor(MainTitle$, 14, LeftCol%, RightCol%, Ch$, ValidInput$, Cnf.Revers)
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
                           IF MRow% = 17 AND Button1% THEN
                              'MOUSE SELECTED FUNCTION KEY ACTION
                              SELECT CASE MCol%
                                 CASE 26, 27
                                    KeyCode% = F1
                                 CASE 39, 40, 41
                                    KeyCode% = F10
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

                     IF KeyCode% < 0 THEN
                        SELECT CASE KeyCode%
                           CASE F1        'HELP
                              CALL HelpMe(0, "TITLE", 2, 2, 24, 79)
                           CASE F10        'QUIT
                              EXIT DO
                           CASE ELSE
                              CALL OOPS
                        END SELECT '1
                     END IF
                  LOOP
                  TitleEntered% = True
                  Buffer$ = SPACE$(38 - INT(LEN(MainTitle$) / 2))
                  CALL CheckMark(Menu$(), 0, 1, TitleEntered%)
                  HideCursor
                  CALL WindowMgr(0, 0, 0, 0, 0, 0, 0)
                  ShowCursor
               CASE 2  'Year and Month

                  CALL GetDates(StartYear%, StartMonth%, StartDay%, StopYear%, StopMonth%, StopDay%)
                  DatesEntered% = True
                  CALL CheckMark(Menu$(), 0, 2, DatesEntered%)

               CASE 3  'Sample Strata

                  CALL GetStrata(WEperStrata%, WDperStrata%)
                  StrataEntered% = True
                  CALL CheckMark(Menu$(), 0, 3, StrataEntered%)

               CASE 4  'Count Regime

                  CALL GetCountRegime(Spread#, Counts%, StartInterval$)
                  CountEntered% = True
                  CALL CheckMark(Menu$(), 0, 4, CountEntered%)

               CASE 5  'Fishing/Sample Day

                  CALL GetHours
                  FishingDayEntered% = True
                  CALL CheckMark(Menu$(), 0, 5, FishingDayEntered%)

               CASE 6  'Holidays and Special days

                  CALL GetHolidays
                  HolidayEntered% = True
                  CALL CheckMark(Menu$(), 0, 6, HolidayEntered%)

               CASE 7  'Starting Points/Water Names

                  CALL GetStartPoints(StartPointTotal%)
                  StartPointsEntered% = True
                  'CALL CheckMark(Menu$(), 0, 7, StartPointsEntered%)

               CASE 9    'Create Calendar
                  IF SetUpReady% THEN
                     CALL BuildCal(CalendarPrinted%, TotalStrata%)
                     CalendarPrinted% = True
                     Menu% = 2
                  ELSE
                     CALL OOPS
                     Text$ = "You can not set up the calendar until the first 6 menu items have been "
                     Text$ = Text$ + "completed.  These menu items are necessary since they answer all the "
                     Text$ = Text$ + "preliminary questions about the creel survey.  A double arrow (®) after "
                     Text$ = Text$ + "a menu item indicates that the item has not been completed.| |"
                     Text$ = Text$ + "        Press any key to return to the main menu"
                     LOCATE 5, 1            ' position the starting line for box
                     Wdth = 60              ' set text width
                     CSMsgBox Text$, Wdth, Cnf.MenBox
                     c$ = GetAKey$
                     CalendarPrinted% = False
                     CSMsgBox "", 1, Cnf.MenBox
                  END IF
            END SELECT '2
         CASE 1
            SELECT CASE Choice%
               CASE 1  'Read From Data File
                  RW% = True
                  CALL ReadWrite(1, RW%, WDperStrata%, WEperStrata%, Strata$, TotalStrata%, DataFileName$)
                  CALL SetYear
                  CALL SetStrata
                  Buffer$ = SPACE$(38 - INT(LEN(MainTitle$) / 2))
               CASE 2  'Write to data file
                  RW% = False
                  CALL ReadWrite(1, RW%, WDperStrata%, WEperStrata%, Strata$, TotalStrata%, DataFileName$)
               CASE 3  'Display program limits
                  Text$ = "             Current Program Limits"
                  Text$ = Text$ + "| | | | | | | | |      Press any key to return to the main menu"
                  LOCATE 8, 1            ' position the starting line for box
                  Wdth = 50              ' set text width
                  CSMsgBox Text$, Wdth, Cnf.MenBox
                  CALL QPrintRC(" Maximum number of counts per day  =" + STR$(MaxCounts), 11, 20, Cnf.MenBox)
                  CALL QPrintRC(" Maximum number of sample strata   =" + STR$(MaxStrata), 12, 20, Cnf.MenBox)
                  CALL QPrintRC(" Maximum number of start times     =" + STR$(MaxStarts), 13, 20, Cnf.MenBox)
                  CALL QPrintRC(" Maximum number of holidays        =" + STR$(MaxHolidays), 14, 20, Cnf.MenBox)
                  CALL QPrintRC(" Maximum number of starting points =" + STR$(MaxStartPoints), 15, 20, Cnf.MenBox)
                  CALL QPrintRC(" Maximum lines of help text/ entry =" + STR$(MaxHelpLines), 16, 20, Cnf.MenBox)
                  c$ = GetAKey$
                  CSMsgBox "", 1, Cnf.MenBox
               CASE 4  'Display video options
                  ULRow% = 3
                  ULCol% = 3
                  LRRow% = 23
                  LRCol% = 78
                  'OPEN NEXT AVAILABLE WINDOW
                  HideCursor
                  CALL WindowMgr(0, 1, ULRow - 1, ULCol - 1, LRRow + 1, LRCol + 1, Cnf.HiLite)
                  ShowCursor
                  CALL QPrintRC("MenuClr      CNF.MenBox = " + STR$(Cnf.MenBox), 4, 48, Cnf.MenBox)
                  CALL QPrintRC("HotLtrClr   CNF.ActivCh = " + STR$(Cnf.ActivCh), 5, 48, Cnf.ActivCh)
                  CALL QPrintRC("HiliteClr    CNF.HiLite = " + STR$(Cnf.HiLite), 6, 48, Cnf.HiLite)
                  CALL QPrintRC("HiliteHotClr CNF.PulBar = " + STR$(Cnf.PulBar), 7, 48, Cnf.PulBar)
                  CALL QPrintRC("InActClr    CNF.InActCh = " + STR$(Cnf.InActCh), 8, 48, Cnf.InActCh)
                  CALL QPrintRC("          CNF.InActHiLt = " + STR$(Cnf.InActHiLt), 9, 48, Cnf.InActHiLt)
                  CALL QPrintRC("             CNF.NonMen = " + STR$(Cnf.NonMen), 10, 48, Cnf.NonMen)
                  CALL QPrintRC("             CNF.Revers = " + STR$(Cnf.Revers), 11, 48, Cnf.Revers)
                  CALL QPrintRC("             CNF.Normal = " + STR$(Cnf.Normal), 12, 48, Cnf.Normal)

                  LOCATE 4, 5
                  CALL MColorPick(Colr%, Cnf.HiLite, ExitCode%)
                  CALL QPrintRC("Color picked was " + STR$(Colr%), 22, 27, Colr%)
                  CALL QPrintRC("Press Any Key To Continue", 23, 25, Cnf.HiLite)
                  c$ = GetAKey$
                  HideCursor
                  WindowMgr 0, 0, 0, 0, 0, 0, 0       'close previous window
                  ShowCursor
            END SELECT '1
         CASE 2          'QUIT
            SELECT CASE Choice%
               CASE 1
                  EXIT DO
            END SELECT '4
         CASE 3          'HELP
            'LOCATE 10, 10
            SELECT CASE Choice%
               CASE 1
                  CALL HelpMe(0, "MENU", 2, 2, 24, 79)
               CASE 2
                  CALL HelpOnHelp
            END SELECT '5
      END SELECT 'Menu%

SkipAction:

   IF KeyCode% <> Escape THEN FirstPass% = True
   END IF

LOOP

' =======================================================================
' End Program
' =======================================================================

HideCursor                              'Turn the mouse cursor off

LOCATE 1, 1
MQPrint STRING$(25 * 80, " "), Cnf.Normal
CALL QPrintRC("Execution terminated", 3, 30, Cnf.Normal)
IF CalendarPrinted% THEN
   CALL QPrintRC("Creel Survey Sampling Schedule created and stored in CREELCAL.OUT", 5, 7, Cnf.Normal)
ELSE
   CALL QPrintRC("There was no output file created", 5, 24, Cnf.Normal)
END IF
LOCATE 7

END

' ****************************************************************************
' ****************************************************************************
' ------------------------- END OF MAIN PROGRAM ------------------------------
' ****************************************************************************
' ****************************************************************************

FUNCTION CrescentDate% (Day%, Month%, Year%)
   IF Month% < 10 THEN
      Dat$ = "0" + FUsing$(STR$(Month%), "#") + "/"
   ELSE
      Dat$ = FUsing$(STR$(Month%), "##") + "/"
   END IF
   IF Day% < 10 THEN
      Dat$ = Dat$ + "0" + FUsing$(STR$(Day%), "#") + "/" + FUsing$(STR$(Year%), "####")
   ELSE
      Dat$ = Dat$ + FUsing$(STR$(Day%), "##") + "/" + FUsing$(STR$(Year%), "####")
   END IF
   CrescentDate% = Date2Num%(Dat$)
'LOCATE 22, 1: PRINT USING "Day=## month=## year=####  Date=\              \   Number=########"; Day%; Month%; Year%; Dat$; Date2Num%(Dat$)
END FUNCTION

FUNCTION DayNumber% (Day%, Month%, Year%)
   IF Month% < 10 THEN
      Dat$ = "0" + FUsing$(STR$(Month%), "#") + "/"
   ELSE
      Dat$ = FUsing$(STR$(Month%), "##") + "/"
   END IF
   IF Day% < 10 THEN
      Dat$ = Dat$ + "0" + FUsing$(STR$(Day%), "#") + "/" + FUsing$(STR$(Year%), "####")
   ELSE
      Dat$ = Dat$ + FUsing$(STR$(Day%), "##") + "/" + FUsing$(STR$(Year%), "####")
   END IF
   DayNumber% = Date2Day%(Dat$)
'LOCATE 22, 1: PRINT USING "Day=## month=## year=####  Date=\              \   DayOfWeek=##"; Day%; Month%; Year%; Dat$; Date2Day%(Dat$)
END FUNCTION

SUB GetHours
   Col%(1) = 44   'START OF FISHING DAY
   Col%(2) = 53   'END OF FISHING DAY
   Col%(3) = 62   'EARLIEST FIRST COUNT
   Col%(4) = 71   'LAST LATE COUNT
   RowPoint% = 11
   ColPoint% = 1
   LastRow% = RowPoint%
   LastCol% = ColPoint%
   TopStrata% = 1
   IF TotalStrata% - TopStrata% < 9 THEN
      BottomRow% = TotalStrata% - TopStrata% + 11
   ELSE
      BottomRow% = 20
   END IF
   'SET FIRST AND LAST COUNTS ACCORDING TO THE FISHING DAY AND TIME
   'BETWEEN COUNTS
   FOR j% = 1 TO TotalStrata% 'SET FIRST AND LAST COUNTS TO SUNRISE AND
                              'SUNSET IF THEY HAVE NOT BEEN DEFINED
      IF FishingDay(j%).FirstCount = 0 THEN FishingDay(j%).FirstCount = FishingDay(j%).StartTime
      IF FishingDay(j%).LastCount = 0 THEN FishingDay(j%).LastCount = FishingDay(j%).EndTime
   NEXT j%
   HideCursor
   CALL WindowMgr(0, 1, 1, 1, 25, 80, Cnf.Normal)     'open next available window
   ShowCursor
   CALL QPrintRC("ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»", 1, 1, Cnf.Normal)
   CALL QPrintRC("º                                                                              º", 2, 1, Cnf.Normal)
   CALL QPrintRC("º  You should set the times for all strata so they make sense for the fishery  º", 3, 1, Cnf.Normal)
   CALL QPrintRC("º  you are dealing with.  All times are military (i.e. 0800 = 8 AM, 1200 =     º", 4, 1, Cnf.Normal)
   CALL QPrintRC("º  noon, and 2230 = 10:30 PM).                                                 º", 5, 1, Cnf.Normal)
   CALL QPrintRC("ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÍÍ¹", 6, 1, Cnf.Normal)
   CALL QPrintRC("º        Sampling Strata         º       Fishing Day        º        º         º", 7, 1, Cnf.Normal)
   CALL QPrintRC("ÇÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ×ÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄ¶ First  º Last    º", 8, 1, Cnf.Normal)
                 '         1         2         3         4         5         6         7         8
                 '12345678901234567890123456789012345678901234567890123456789012345678901234567890
   CALL QPrintRC("º  Number  ³First Day ³ Last Day º  Hours ³  Start ³  Stop  º Count  º Count   º", 9, 1, Cnf.Normal)
   CALL QPrintRC("ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍÍÍ¹", 10, 1, Cnf.Normal)
   CALL QPrintRC("º          ³          ³          º        ³        ³        º        º         º", 11, 1, Cnf.Normal)
   CALL QPrintRC("º          ³          ³          º        ³        ³        º        º         º", 12, 1, Cnf.Normal)
   CALL QPrintRC("º          ³          ³          º        ³        ³        º        º         º", 13, 1, Cnf.Normal)
   CALL QPrintRC("º          ³          ³          º        ³        ³        º        º         º", 14, 1, Cnf.Normal)
   CALL QPrintRC("º          ³          ³          º        ³        ³        º        º         º", 15, 1, Cnf.Normal)
   CALL QPrintRC("º          ³          ³          º        ³        ³        º        º         º", 16, 1, Cnf.Normal)
   CALL QPrintRC("º          ³          ³          º        ³        ³        º        º         º", 17, 1, Cnf.Normal)
   CALL QPrintRC("º          ³          ³          º        ³        ³        º        º         º", 18, 1, Cnf.Normal)
   CALL QPrintRC("º          ³          ³          º        ³        ³        º        º         º", 19, 1, Cnf.Normal)
   CALL QPrintRC("º          ³          ³          º        ³        ³        º        º         º", 20, 1, Cnf.Normal)
   CALL QPrintRC("ÌÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÍÍ¹", 21, 1, Cnf.Normal)
   CALL QPrintRC("º PgUp, PgDn, Ctrl/Home and Ctrl/End to view other strata.  Tab or Arrow keys  º", 22, 1, Cnf.Normal)
   CALL QPrintRC("º to move cursor.  F1 for Help, F6 to copy time down, F9 set count in relation º", 23, 1, Cnf.Normal)
   CALL QPrintRC("º to fishing day (i.e., first count 1 hr after start), F10 when finished.      º", 24, 1, Cnf.Normal)
   CALL QPrintRC("ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼", 25, 1, Cnf.Normal)
   CALL QPrintRC("F1", 23, 20, Cnf.HiLite)
   CALL QPrintRC("F6", 23, 33, Cnf.HiLite)
   CALL QPrintRC("F9", 23, 55, Cnf.HiLite)
   CALL QPrintRC("F10", 24, 56, Cnf.HiLite)
   CALL QPrintRC(Buffer$ + MainTitle$, 2, 2, Cnf.HiLite)
   NewStrata% = True
   DO
      IF TopStrata% > 1 THEN
         CALL QPrintRC("", 10, 43, Cnf.HiLite)
         CALL QPrintRC("", 10, 52, Cnf.HiLite)
         CALL QPrintRC("", 10, 61, Cnf.HiLite)
         CALL QPrintRC("", 10, 70, Cnf.HiLite)
      ELSE
         CALL QPrintRC("Ø", 10, 43, Cnf.Normal)
         CALL QPrintRC("Ø", 10, 52, Cnf.Normal)
         CALL QPrintRC("Î", 10, 61, Cnf.Normal)
         CALL QPrintRC("Î", 10, 70, Cnf.Normal)
      END IF
      IF TopStrata% + 9 < TotalStrata% THEN
         CALL QPrintRC("", 21, 43, Cnf.HiLite)
         CALL QPrintRC("", 21, 52, Cnf.HiLite)
         CALL QPrintRC("", 21, 61, Cnf.HiLite)
         CALL QPrintRC("", 21, 70, Cnf.HiLite)
      ELSE
         CALL QPrintRC("Ï", 21, 43, Cnf.Normal)
         CALL QPrintRC("Ï", 21, 52, Cnf.Normal)
         CALL QPrintRC("Ê", 21, 61, Cnf.Normal)
         CALL QPrintRC("Ê", 21, 70, Cnf.Normal)
      END IF

      IF NewStrata% THEN
         FOR Row% = 11 TO 20
            GOSUB PrintRow
         NEXT Row%
         NewStrata% = False
      END IF
      'RESTORE LAST HILIGHT TO MENU COLOR
      StrCnt% = TopStrata% + LastRow% - 11
      SELECT CASE LastCol%
         CASE 1
            CALL QPrintRC("  " + FUsing$(STR$(FishingDay(StrCnt%).StartTime), "####") + "  ", LastRow%, Col%(LastCol%), Cnf.MenBox)
         CASE 2
            CALL QPrintRC("  " + FUsing$(STR$(FishingDay(StrCnt%).EndTime), "####") + "  ", LastRow%, Col%(LastCol%), Cnf.MenBox)
         CASE 3
            CALL QPrintRC("  " + FUsing$(STR$(FishingDay(StrCnt%).FirstCount), "####") + "  ", LastRow%, Col%(LastCol%), Cnf.MenBox)
         CASE 4
            CALL QPrintRC("  " + FUsing$(STR$(FishingDay(StrCnt%).LastCount), "####") + "  ", LastRow%, Col%(LastCol%), Cnf.MenBox)
      END SELECT
      'HILIGHT CURRENT CURSOR LOCATION
      StrCnt% = TopStrata% + RowPoint% - 11
      SELECT CASE ColPoint%
         CASE 1
            CALL QPrintRC("  " + FUsing$(STR$(FishingDay(StrCnt%).StartTime), "####") + "  ", RowPoint%, Col%(ColPoint%), Cnf.Revers)
            EnteredString$ = LTRIM$(STR$(FishingDay(StrCnt%).StartTime))
         CASE 2
            CALL QPrintRC("  " + FUsing$(STR$(FishingDay(StrCnt%).EndTime), "####") + "  ", RowPoint%, Col%(ColPoint%), Cnf.Revers)
            EnteredString$ = LTRIM$(STR$(FishingDay(StrCnt%).EndTime))
         CASE 3
            CALL QPrintRC("  " + FUsing$(STR$(FishingDay(StrCnt%).FirstCount), "####") + "  ", RowPoint%, Col%(ColPoint%), Cnf.Revers)
            EnteredString$ = LTRIM$(STR$(FishingDay(StrCnt%).FirstCount))
         CASE 4
            CALL QPrintRC("  " + FUsing$(STR$(FishingDay(StrCnt%).LastCount), "####") + "  ", RowPoint%, Col%(ColPoint%), Cnf.Revers)
            EnteredString$ = LTRIM$(STR$(FishingDay(StrCnt%).LastCount))
      END SELECT
      'INPUT A TIME
'      LOCATE RowPoint%, Col%(ColPoint%)
      LeftCol% = Col%(ColPoint%)        'set the left column
      RightCol% = Col%(ColPoint%) + 7   'and the right column
      ValidInput$ = "1234567890 "
      CALL Editor(EnteredString$, RowPoint%, LeftCol%, RightCol%, Ch$, ValidInput$, Cnf.Revers)
      NewNum% = VAL(EnteredString$)
      KeyCode% = KeyPressed%(Ch$)
      StrCnt% = TopStrata% + RowPoint% - 11
      SELECT CASE ColPoint%
         CASE 1 'START OF FISHING DAY
            IF NewNum% < 0 OR NewNum% > 2400 OR (NewNum% MOD 100) > 59 THEN
               CALL OOPS
            ELSE
               FishingDay(StrCnt%).StartTime = NewNum%
            END IF
         CASE 2 'END OF FISHING DAY
            IF NewNum% < 0 OR NewNum% > 2400 OR (NewNum% MOD 100) > 59 THEN
               CALL OOPS
            ELSE
               FishingDay(StrCnt%).EndTime = NewNum%
            END IF
         CASE 3 'TIME FOR FIRST COUNT
            IF NewNum% < FishingDay(StrCnt%).StartTime - 300 OR (NewNum% MOD 100) > 59 OR NewNum% > FishingDay(StrCnt%).EndTime THEN
               CALL OOPS
            ELSE
               FishingDay(StrCnt%).FirstCount = NewNum%
            END IF
         CASE 4 'TIME FOR LAST COUNT
            IF NewNum% < FishingDay(StrCnt%).FirstCount OR (NewNum% MOD 100) > 59 OR NewNum% > FishingDay(StrCnt%).EndTime + 300 THEN
               CALL OOPS
            ELSE
               FishingDay(StrCnt%).LastCount = NewNum%
            END IF
      END SELECT
      'CALCULATE HOURS AND MINUTES IN FISHING DAY
      IF ColPoint% = 1 OR ColPoint% = 2 THEN
         Hours% = (FishingDay(StrCnt%).EndTime \ 100) - (FishingDay(StrCnt%).StartTime \ 100)
         IF (FishingDay(StrCnt%).EndTime MOD 100) < (FishingDay(StrCnt%).StartTime MOD 100) THEN
            Hours% = Hours% - 1
            Minutes% = (FishingDay(StrCnt%).EndTime MOD 100) + (60 - (FishingDay(StrCnt%).StartTime MOD 100))
         ELSE
            Minutes% = (FishingDay(StrCnt%).EndTime MOD 100) - (FishingDay(StrCnt%).StartTime MOD 100)
         END IF
         CALL QPrintRC("  " + FUsing$(STR$(Hours% + (Minutes% / 60)), "##.##") + " ", RowPoint%, 35, Cnf.Normal)
      END IF
      LastRow% = RowPoint%
      LastCol% = ColPoint%

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
            IF MRow% = 24 AND Button1% AND (MCol% = 56 OR MCol% = 57 OR MCol% = 58) THEN
               KeyCode% = F10
            ELSEIF MRow% = 23 AND Button1% THEN
               'MOUSE SELECTED FUNCTION KEY ACTION
               SELECT CASE MCol%
                  CASE 20, 21
                     KeyCode% = F1
                  CASE 33, 34
                     KeyCode% = F6
                  CASE 55, 56
                     KeyCode% = F9
                  CASE ELSE
                     CALL OOPS
               END SELECT
            ELSEIF MRow% >= 11 AND MRow% <= BottomRow% AND Button1% THEN
               IF MCol% >= 35 AND MCol% <= 79 THEN
                  KeyCode% = 0
                  RowPoint% = MRow%
                  IF MCol% >= 44 AND MCol% <= 51 THEN
                     ColPoint% = 1
                  ELSEIF MCol% >= 53 AND MCol% <= 60 THEN
                     ColPoint% = 2
                  ELSEIF MCol% >= 62 AND MCol% <= 69 THEN
                     ColPoint% = 3
                  ELSEIF MCol% >= 71 AND MCol% <= 79 THEN
                     ColPoint% = 4
                  END IF
               END IF
            ELSEIF MRow% = 10 AND Button1% THEN
               'POSSIBLY A SCROLL DOWN SELECTION
               IF MCol% = 43 OR MCol% = 52 OR MCol% = 61 OR MCol% = 70 THEN
                  KeyCode% = PageUp
               END IF
            ELSEIF MRow% = 21 AND Button1% THEN
               'POSSIBLY A SCROLL UP SELECTION
               IF MCol% = 43 OR MCol% = 52 OR MCol% = 61 OR MCol% = 70 THEN
                  KeyCode% = PageDown
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

      SELECT CASE KeyCode%
         CASE Tabb
            SELECT CASE LastCol%
               CASE 4
                  ColPoint% = 1
                  SELECT CASE LastRow%
                     CASE BottomRow%
                        RowPoint% = 11
                     CASE ELSE
                        RowPoint% = RowPoint% + 1
                  END SELECT
               CASE ELSE
                  ColPoint% = ColPoint% + 1
            END SELECT
         CASE ShftTabb
            SELECT CASE LastCol%
               CASE 1
                  ColPoint% = 4
                  SELECT CASE LastRow%
                     CASE 11
                        RowPoint% = BottomRow%
                     CASE ELSE
                        RowPoint% = RowPoint% - 1
                  END SELECT
               CASE ELSE
                  ColPoint% = ColPoint% - 1
            END SELECT
         CASE UpArrow
            SELECT CASE LastRow%
               CASE 11
                  RowPoint% = BottomRow%
               CASE ELSE
                  RowPoint% = RowPoint% - 1
            END SELECT
         CASE DownArrow
            SELECT CASE LastRow%
               CASE BottomRow%
                  RowPoint% = 11
               CASE ELSE
                  RowPoint% = RowPoint% + 1
            END SELECT
         CASE RightArrow
            SELECT CASE LastCol%
               CASE 4
                  ColPoint% = 1
               CASE ELSE
                  ColPoint% = ColPoint% + 1
            END SELECT
         CASE LeftArrow
            SELECT CASE LastCol%
               CASE 1
                  ColPoint% = 4
               CASE ELSE
                  ColPoint% = ColPoint% - 1
            END SELECT
         CASE PageDown
            TopStrata% = TopStrata% + 1
            IF TopStrata% + 9 > TotalStrata% THEN
               TopStrata% = TotalStrata% - 9
               IF TopStrata% < 1 THEN TopStrata% = 1
            ELSE
               HideCursor
               CALL ScrollU(11, 2, 20, 79, 1, -1)
               Row% = 20
               GOSUB PrintRow
               ShowCursor
               SELECT CASE LastRow%
                  CASE 11
                     RowPoint% = 11
                  CASE ELSE
                     RowPoint% = RowPoint% - 1
               END SELECT
            END IF
         CASE PageUp
            TopStrata% = TopStrata% - 1
            IF TopStrata% < 1 THEN
               TopStrata% = 1
            ELSE
               HideCursor
               CALL ScrollD(11, 2, 20, 79, 1, -1)
               Row% = 11
               GOSUB PrintRow
               ShowCursor
               SELECT CASE LastRow%
                  CASE 20
                     RowPoint% = 20
                  CASE ELSE
                     RowPoint% = RowPoint% + 1
               END SELECT
            END IF
         CASE CtrlHome
            TopStrata% = 1
            IF TotalStrata% - TopStrata% < 8 THEN
               BottomRow% = TotalStrata% - TopStrata% + 11
            END IF
            NewStrata% = True
         CASE CtrlEnnd
            TopStrata% = TotalStrata% - 9
            IF TopStrata% < 1 THEN TopStrata% = 1
            BottomRow% = 20
            NewStrata% = True
         CASE F1        'HELP
            CALL HelpMe(0, "TIME", 1, 2, 25, 79)
         CASE F6        'F6  copy this time to the other strata
            StrCnt% = TopStrata% + RowPoint% - 11
            SELECT CASE ColPoint%
               CASE 1
                  FOR j% = StrCnt% + 1 TO TotalStrata%
                     FishingDay(j%).StartTime = FishingDay(StrCnt%).StartTime
                  NEXT j%
               CASE 2
                  FOR j% = StrCnt% + 1 TO TotalStrata%
                     FishingDay(j%).EndTime = FishingDay(StrCnt%).EndTime
                  NEXT j%
               CASE 3
                  FOR j% = StrCnt% + 1 TO TotalStrata%
                     FishingDay(j%).FirstCount = FishingDay(StrCnt%).FirstCount
                  NEXT j%
               CASE 4
                  FOR j% = StrCnt% + 1 TO TotalStrata%
                     FishingDay(j%).LastCount = FishingDay(StrCnt%).LastCount
                  NEXT j%
            END SELECT
            NewStrata% = True
         CASE F9        'F9  adjust count times according to fishing day
            IF ColPoint% <> 3 AND ColPoint% <> 4 THEN
               CALL OOPS
            ELSEIF ColPoint% = 3 THEN
               HideCursor
               CALL WindowMgr(0, 1, 5, 1, 18, 80, Cnf.Revers)     'open next available window
               ShowCursor
               CALL QPrintRC("This option will adjust the First count time in relation to the start of", 8, 5, Cnf.Revers)
               CALL QPrintRC("the fishing day.  If you enter 0 the earliest count time will be set to", 9, 5, Cnf.Revers)
               CALL QPrintRC("the start of the fishing day, entering 1 will set the first count as ", 10, 5, Cnf.Revers)
               CALL QPrintRC("hour after the start of the fishing day, etc.,.", 11, 5, Cnf.Revers)
               CALL QPrintRC("Enter the Hours difference   ", 14, 12, Cnf.Revers)
               CALL QPrintRC("Enter the minutes difference ", 15, 12, Cnf.Revers)
               LeftCol% = 42        'set the left column
               RightCol% = 43       'and the right column
               ValidInput$ = Numbers$
               AddHrs% = 0
               DO
                  EnteredString$ = QPTrim$(STR$(AddHrs%))
                  CALL Editor(EnteredString$, 14, LeftCol%, RightCol%, Ch$, ValidInput$, Cnf.Revers)
                  NewNum% = VAL(EnteredString$)
                  IF NewNum% < 0 OR NewNum% > 24 THEN
                     CALL OOPS
                     EnteredString$ = ""
                  ELSE
                     AddHrs% = NewNum%
                  END IF
                  IF KeyPressed%(Ch$) = Enter THEN EXIT DO
               LOOP
               AddMin% = 0
               DO
                  EnteredString$ = QPTrim$(STR$(AddMin%))
                  'EnteredString$ = ""
                  CALL Editor(EnteredString$, 15, LeftCol%, RightCol%, Ch$, ValidInput$, Cnf.Revers)
                  NewNum% = VAL(EnteredString$)
                  IF NewNum% < 0 OR NewNum% > 59 THEN
                     CALL OOPS
                     EnteredString$ = ""
                  ELSE
                     AddMin% = NewNum%
                  END IF
                  IF KeyPressed%(Ch$) = Enter THEN EXIT DO
               LOOP

               FOR StrCnt% = 1 TO TotalStrata%
                  Hours% = (FishingDay(StrCnt%).StartTime \ 100) + AddHrs%
                  Minutes% = (FishingDay(StrCnt%).StartTime MOD 100) + AddMin%
                  IF Minutes% > 59 THEN
                     Hours% = Hours% + (Minutes% \ 60)
                     Minutes% = Minutes% - (60 * (Minutes% \ 60))
                  END IF
                  FishingDay(StrCnt%).FirstCount = Hours% * 100 + Minutes%
               NEXT StrCnt%
               HideCursor
               CALL WindowMgr(0, 0, 0, 0, 0, 0, 0)
               ShowCursor
               NewStrata% = True
            ELSEIF ColPoint% = 4 THEN
               HideCursor
               CALL WindowMgr(0, 1, 5, 1, 18, 80, Cnf.Revers)     'open next available window
               ShowCursor
               CALL QPrintRC("This option will adjust the Last count time in relation to the end of", 8, 5, Cnf.Revers)
               CALL QPrintRC("the fishing day.  If you enter 0 the earliest count time will be set to", 9, 5, Cnf.Revers)
               CALL QPrintRC("the end of the fishing day, entering 1 will set the first count as ", 10, 5, Cnf.Revers)
               CALL QPrintRC("hour before the end of the fishing day, etc.,.", 11, 5, Cnf.Revers)
               CALL QPrintRC("Enter the Hours difference   ", 14, 12, Cnf.Revers)
               CALL QPrintRC("Enter the minutes difference ", 15, 12, Cnf.Revers)

               LeftCol% = 42        'set the left column
               RightCol% = 43       'and the right column
               ValidInput$ = Numbers$
               AddHrs% = 0
               DO
                  EnteredString$ = QPTrim$(STR$(AddHrs%))
                  CALL Editor(EnteredString$, 14, LeftCol%, RightCol%, Ch$, ValidInput$, Cnf.Revers)
                  NewNum% = VAL(EnteredString$)
                  IF NewNum% < 0 OR NewNum% > 24 THEN
                     CALL OOPS
                     EnteredString$ = ""
                  ELSE
                     AddHrs% = NewNum%
                  END IF
                  IF KeyPressed%(Ch$) = Enter THEN EXIT DO
               LOOP
               AddMin% = 0
               DO
                  EnteredString$ = QPTrim$(STR$(AddMin%))
                  'EnteredString$ = ""
                  CALL Editor(EnteredString$, 15, LeftCol%, RightCol%, Ch$, ValidInput$, Cnf.Revers)
                  NewNum% = VAL(EnteredString$)
                  IF NewNum% < 0 OR NewNum% > 59 THEN
                     CALL OOPS
                     EnteredString$ = ""
                  ELSE
                     AddMin% = NewNum%
                  END IF
                  IF KeyPressed%(Ch$) = Enter THEN EXIT DO
               LOOP

               FOR StrCnt% = 1 TO TotalStrata%
                  Hours% = (FishingDay(StrCnt%).EndTime \ 100) - AddHrs%
                  Minutes% = (FishingDay(StrCnt%).EndTime MOD 100) - AddMin%
                  IF Minutes% < 0 THEN
                     Hours% = Hours% - 1
                     Minutes% = 60 + Minutes%
                  END IF
                  FishingDay(StrCnt%).LastCount = Hours% * 100 + Minutes%
               NEXT StrCnt%
               NewStrata% = True
               HideCursor
               CALL WindowMgr(0, 0, 0, 0, 0, 0, 0)
               ShowCursor
            END IF
         CASE F10        'F10  quit
            EXIT DO
      END SELECT
   LOOP
   HideCursor
   CALL WindowMgr(0, 0, 0, 0, 0, 0, 0)
   ShowCursor
EXIT SUB

PrintRow:
   StrCnt% = TopStrata% + Row% - 11
   CALL QPrintRC("º          ³          ³          º        ³        ³        º        º         º", Row%, 1, Cnf.Normal)
   IF StrCnt% <= TotalStrata% THEN
      CALL QPrintRC(FUsing$(STR$(StrCnt%), "##") + " of", Row%, 3, Cnf.Normal)
      CALL QPrintRC(FUsing$(STR$(TotalStrata%), "##"), Row%, 9, Cnf.Normal)
      CALL QPrintRC(FUsing$(STR$(StrataDate(StrCnt%).FirstMn), "##") + "/", Row%, 14, Cnf.Normal)
      CALL QPrintRC(FUsing$(STR$(StrataDate(StrCnt%).FirstDy), "##") + "/", Row%, 17, Cnf.Normal)
      CALL QPrintRC(RIGHT$(STR$(StrataDate(StrCnt%).FirstYr), 2), Row%, 20, Cnf.Normal)
      CALL QPrintRC(FUsing$(STR$(StrataDate(StrCnt%).LastMn), "##") + "/", Row%, 25, Cnf.Normal)
      CALL QPrintRC(FUsing$(STR$(StrataDate(StrCnt%).LastDy), "##") + "/", Row%, 28, Cnf.Normal)
      CALL QPrintRC(RIGHT$(STR$(StrataDate(StrCnt%).LastYr), 2), Row%, 31, Cnf.Normal)

      Hours% = (FishingDay(StrCnt%).EndTime \ 100) - (FishingDay(StrCnt%).StartTime \ 100)
      IF (FishingDay(StrCnt%).EndTime MOD 100) < (FishingDay(StrCnt%).StartTime MOD 100) THEN
         Hours% = Hours% - 1
         Minutes% = (FishingDay(StrCnt%).EndTime MOD 100) + (60 - (FishingDay(StrCnt%).StartTime MOD 100))
      ELSE
         Minutes% = (FishingDay(StrCnt%).EndTime MOD 100) - (FishingDay(StrCnt%).StartTime MOD 100)
      END IF
      CALL QPrintRC("  " + FUsing$(STR$(Hours% + (Minutes% / 60)), "##.##") + " ", Row%, 35, Cnf.Normal)
      'HILIGHT ALL THE ITEMS WHICH CAN BE CHANGED BY THE USER
      CALL QPrintRC("  " + FUsing$(STR$(FishingDay(StrCnt%).StartTime), "####") + "  ", Row%, Col%(1), Cnf.MenBox)
      CALL QPrintRC("  " + FUsing$(STR$(FishingDay(StrCnt%).EndTime), "####") + "  ", Row%, Col%(2), Cnf.MenBox)
      CALL QPrintRC("  " + FUsing$(STR$(FishingDay(StrCnt%).FirstCount), "####") + "  ", Row%, Col%(3), Cnf.MenBox)
      CALL QPrintRC("  " + FUsing$(STR$(FishingDay(StrCnt%).LastCount), "####") + "  ", Row%, Col%(4), Cnf.MenBox)
   END IF

RETURN

END SUB 'GetHours

