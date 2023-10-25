'
' Include file for CREELCAL.BAS
'
DEFINT A-Z
'
'''''''''''''''''''''''''' TYPE Declarations ''''''''''''''''''''''''
'
' Define the type needed for INTERRUPT
'
TYPE RegType
     AX    AS INTEGER
     bx    AS INTEGER
     CX    AS INTEGER
     dx    AS INTEGER
     bp    AS INTEGER
     si    AS INTEGER
     di    AS INTEGER
     flags AS INTEGER
END TYPE

TYPE HelpSpotType
   Byte AS LONG
   LookUp AS STRING * 8
   Label AS STRING * 40
   Lines AS INTEGER
END TYPE

TYPE Config
    MonTyp AS INTEGER           'Monitor Type
    PulBar AS INTEGER           'Horizontal Bar Color
    MenBox AS INTEGER           'Pulled down Box Color
    ActivCh AS INTEGER          'Active menu choice color
    InActCh AS INTEGER          'Inactive (Non-selectable) Choice Color
    HiLite AS INTEGER           'Menu Bar Color for active choices
    InActHiLt AS INTEGER        'Dito for inactive choices
    NonMen AS INTEGER           'Normal screen color
    Revers AS INTEGER           'Reverse of Normal screen color
    ReverseBlink AS INTEGER     'Blinking Reverse of Normal screen color
    Blink AS INTEGER            'Blinking Normal screen color
    Normal AS INTEGER           'Reverse of Normal screen color
    CurSize AS INTEGER          'Cursor Scan Lines
    Mouse AS INTEGER            'Mouse in residence Flag
    NPixLines AS INTEGER        'Number of Pixel Lines for Mouse use
END TYPE

TYPE MonthType
   Number AS INTEGER     ' Number of days in the month
   Mname AS STRING * 9   ' Name of the month
END TYPE

TYPE HolidayType
   Selected AS INTEGER    'boolean indicating holiday selection
   Year AS INTEGER        'Date for holidays
   Month AS INTEGER       'Date for holidays
   Day AS INTEGER         'Date for holidays
   HName AS STRING * 30   'Name of Holiday
   CrescDate AS INTEGER   'date value from Crescent's Date2Num routine
END TYPE

TYPE DateType
   FirstDy AS INTEGER   'Day of month of start of strata
   LastDy AS INTEGER    'Day of month of end of strata
   FirstMn AS INTEGER   'Month of start of strata
   LastMn AS INTEGER    'Month of end of strata
   FirstYr AS INTEGER   'Year of start of strata
   LastYr AS INTEGER    'Year of end of strata
   FirstCres as INTEGER 'date value from Crescent's Date2Num routine
   LastCres as INTEGER  'date value from Crescent's Date2Num routine
   WEavail AS INTEGER   'Weekend Days in strata
   WEsampled AS INTEGER 'Weekend Days sampled in strata
   WDavail AS INTEGER   'Week Days in strata
   WDsampled AS INTEGER 'Week Days sampled in strata
END TYPE

TYPE DayType
   StartTime AS INTEGER    'start of fishing day (military time)
   EndTime AS INTEGER      'end of fishing day (military time)
   FirstCount AS INTEGER   'Time for first Count (military time)
   LastCount AS INTEGER    'Time for Last Count (military time)
   WorkDay AS INTEGER      'time between first and last counts
END TYPE

TYPE StartType
   Available AS INTEGER
   Used AS INTEGER
END TYPE

TYPE DayType2
   Month AS INTEGER           'month
   Day AS INTEGER             'day of month
   Strata AS INTEGER          'strata that the date is in
   Sampled AS INTEGER         'was the day included in the sample
   WeWd AS INTEGER            '0=WD, 1=WE, and -1=Holiday
' set the dimension for Times to equal MaxCounts
   Times(6) AS INTEGER        'Count Times
   Area AS STRING * 8         'Start area name
   CrescDate AS INTEGER       'date value from Crescent's Date2Num routine
END TYPE

TYPE SurveyBoundType
   Month AS INTEGER
   Year AS INTEGER
END TYPE

TYPE StartPointType
  Abrev AS STRING * 8
  Descript AS STRING * 25
  AsignPortion AS SINGLE
END TYPE

TYPE StartPointProbType
  Location AS INTEGER
END TYPE

TYPE TimingType
   OptIntervals AS INTEGER
   IntervalMn AS INTEGER
END TYPE

TYPE WarningType
   TimeLost AS INTEGER
   IntervalsLost AS INTEGER
END TYPE

' ===========================================================================
'
'      DECLARATIONS for Menu Subroutines and Functions from Crescent Library
'
' ===========================================================================
DECLARE SUB APrint (BYVAL Address, NumEls, MidFirst, NumChars, Colr, Page)
DECLARE SUB APrint0 (BYVAL Address%, NumEls%, StartCol%, NumChars%, Colr%)
DECLARE SUB Box (ULRow%, ULCol%, LRRow%, LRCol%, BoxStyle%, Colr%, Page%)
DECLARE SUB BarPrint (Choice$(), Stat%())
DECLARE SUB Box0 (ULRow%, ULCol%, BrRow%, BrCol%, BoxType%, Clr%)
DECLARE SUB ButtonPress (Button%, Status%, Count%, X%, Y%)
DECLARE SUB ClearBuf ()
DECLARE SUB ClearScr (ULRow%, ULCol%, LRRow%, LRCol%, Colr%, Page%)
DECLARE SUB CSMsgBox (Message$, Wdth, Colr%)
DECLARE SUB DeleteT (SEG Element, ElSize, NumEls)
'DECLARE SUB Editor (Text$, Row%, LeftCol%, RightCol%, Ky$, ValidInput$, Colr%)
DECLARE SUB GetCursor (X%, Y%, Button%)
DECLARE SUB GetVMode (Mode%, Page%, PageSize%, Rows%, Columns%)
DECLARE SUB HideCursor ()
DECLARE SUB InitMouse (MouseThere%)
DECLARE SUB MColorPick (Colr%, BoxColor%, ExitCode%)
DECLARE SUB MGetState (MouseStor$)
DECLARE SUB MouseTrap (ULRow%, ULCol%, LRRow%, LRCol%)
DECLARE SUB MPaintBox (ULRow%, ULCol%, LRRow%, LRCol%, Colr%)
DECLARE SUB MQPrint (Work$, Colr%)
DECLARE SUB MScrnSave (ULRow%, ULCol%, LRRow%, LRCol%, SEG Element%)
DECLARE SUB MScrnRest (ULRow%, ULCol%, LRRow%, LRCol%, SEG Elememt%)
DECLARE SUB MSetState (MouseStor$)
DECLARE SUB Pause (Ticks%)
DECLARE SUB PickList (Items$(), Picked%(), NPicked%)
DECLARE SUB PullDnMs (Menu$(), Stat%(), Menu%, Choice%, Ky$, Action%)
DECLARE SUB QPrintRC (Work$, Row%, Col%, Clr%)
DECLARE SUB ReadFile (BYVAL Address)
DECLARE SUB ScrollU (ULRow%, ULCol%, LRRow%, LRCol%, Lines%, Page%)
DECLARE SUB ScrollD (ULRow%, ULCol%, LRRow%, LRCol%, Lines%, Page%)
DECLARE SUB ScrollL (ULRow%, ULCol%, LRRow%, LRCol%, Lines%, Page%)
DECLARE SUB ScrollR (ULRow%, ULCol%, LRRow%, LRCol%, Lines%, Page%)
DECLARE SUB SetCursor (X%, Y%)
DECLARE SUB ShowCursor ()
DECLARE SUB TextCursor (FG%, BG%)
DECLARE SUB VertMenu (Item$(), Choice%, MaxLen%, BoxBot%, Ky$, Action%)
DECLARE SUB WindowMgr (WindowNumber%, Action%, ULRow%, ULCol%, LRRow%, LRCol%, Colr%)

DECLARE FUNCTION AltKey% ()
DECLARE FUNCTION ArraySize% (ULRow%, ULCol%, LRRow%, LRCol%)
DECLARE FUNCTION ASCII% (Text$)
DECLARE FUNCTION Date2Num% (Dat$)
DECLARE FUNCTION Date2Day% (Dat$)
DECLARE FUNCTION DOSVer% ()
DECLARE FUNCTION Exist% (FileName$)
DECLARE FUNCTION FCount% (FileSpec$)
DECLARE FUNCTION FindLast% (BYVAL Address, Size)
DECLARE FUNCTION FUsing$ (Number$, Mask$)       'asm - used by ROUND#
DECLARE FUNCTION InStat% ()
DECLARE FUNCTION MBuffSize% ()
DECLARE FUNCTION MinInt% (Var1%, Var2%)
DECLARE FUNCTION Monitor% ()
DECLARE FUNCTION Num2Day% (Days%)
DECLARE FUNCTION PDQTimer& ()
DECLARE FUNCTION PullMenKey% (Ky$, Menu$(), Stat%())
DECLARE FUNCTION QPLen% (Work$)
DECLARE FUNCTION QPLTrim$ (Work$)
DECLARE FUNCTION QPMid$ (Work$, StartChar%, NumChars%)
DECLARE FUNCTION QPROUND$ (Number#, Places%)
DECLARE FUNCTION QPTrim$ (Work$)
DECLARE FUNCTION Rand# (Lo#, Hi#)
DECLARE FUNCTION Valid% (FileName$)

'DECLARE SUB ComputeMonth (Year%, Month%, DayOfWeek%, DaysInMonth%, CumulativeDays%)
DECLARE FUNCTION KeyPressed% (Ch$)   'My Function in CRELCAL3.BAS
DECLARE FUNCTION IsBlank% (TestStr$) 'My Function in CRELCAL2.BAS
DECLARE FUNCTION GetAKey$ ()
DECLARE FUNCTION DaysPerMonth% (Month%, Year%)
DECLARE FUNCTION CrescentDate% (Day%, Month%, Year%)
DECLARE FUNCTION DayNumber% (Day%, Month%, Year%)
DECLARE FUNCTION IsLeapYear% (N%)

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''''''''''''''''''''''' COMMON SHARED BLOCK ''''''''''''''''''''''''''''''''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
COMMON SHARED MonthData() AS MonthType, Holiday() AS HolidayType
COMMON SHARED StrataDate() AS DateType, FishingDay() AS DayType
COMMON SHARED CountStart() AS StartType, Year() AS DayType2
COMMON SHARED StartPoint() AS StartPointType, StartPointProb() AS StartPointProbType
COMMON SHARED Timing() AS TimingType, SurveyBound() AS SurveyBoundType
COMMON SHARED Warning() AS WarningType, Row%(), Col%(), RandomStartPoints%
COMMON SHARED HolidayEntered%, TitleEntered%, StartPointsEntered%
COMMON SHARED DatesEntered%, StrataEntered%, CountEntered%, FishingDayEntered%
COMMON SHARED Alphabet$, Punctuation$, Numbers$, BS$, OKforFileName$
COMMON SHARED CsrSize%, InsStatus%, TotalStrata%, Strata$, Buffer$, MainTitle$
COMMON SHARED StartMonth%, StartDay%, StopMonth%, StopDay%, MaxWE%, MaxWD%
COMMON SHARED StartYear%, StopYear%, HolidayCount%, StartInterval$, Counts%
COMMON SHARED StartPointTotal%, WEperStrata%, WDperStrata%, Spread#
COMMON SHARED HelpSpot() AS HelpSpotType, HelpText$(), HelpNum%, Version$
COMMON SHARED PrintOnOff%, PrintFormat%, HelpFileNum%
COMMON SHARED NormalFG%, NormalBG%, MenuFG%, MenuBG%, HiLightFG%, HiLightBG%
COMMON SHARED ReverseFG%, ReverseBG%, BlinkFG%, BlinkBG%, ReverseBlinkFG%, ReverseBlinkBG%
COMMON SHARED Cnf AS Config, Tim!
'COMMON SHARED DataBox() AS DataBoxType


'******************************************************************************
'
' QB.BI - Assembly Support Include File
'
'       Copyright <C> 1987 Microsoft Corporation
'
' Purpose:
'      This include file defines the types and gives the DECLARE
'       statements for the assembly language routine INTERRUPT
'
' Generate a software interrupt, loading all but the segment registers
'
DECLARE SUB Interrupt (intnum AS INTEGER, inreg AS RegType, outreg AS RegType)
'*************************** END OF QB.BI *************************************

'
'CONSTANT DECLARATIONS
'
CONST False = 0, True = NOT False

CONST MaxStrata = 52      'maximum number of strata
CONST MaxHolidays = 10     'maximum number of holidays
CONST MaxCounts = 6       'maximum number of counts per day
     'if this is changed be sure to change DayType2.Time(MaxCounts)
CONST MaxStarts = 144     'maximum number of start times
CONST MaxStartPoints = 20 'maximum number of starting points
CONST MaxYears = 2        'maximum number of years

CONST MaxHelpCalls = 10
CONST MaxHelpLines = 100
CONST MaxHelpWidth = 76

CONST BKSP = 8
CONST Tabb = 9
CONST ShftTabb = -15
CONST Enter = 13
CONST Escape = 27
CONST Space = 32
'extended character codes
CONST UpArrow = -72
CONST DownArrow = -80
CONST RightArrow = -77
CONST LeftArrow = -75
CONST PageUp = -73
CONST PageDown = -81
CONST Deleat = -83
CONST Insirt = -82
CONST Home = -71
CONST Ennd = -79
CONST CtrlUpArrow = -141
CONST CtrlDownArrow = -145
CONST CtrlRightArrow = -116
CONST CtrlLeftArrow = -115
CONST CtrlDeleat = -147
CONST CtrlInsirt = -146
CONST CtrlPageUp = -132
CONST CtrlPageDown = -118
CONST CtrlHome = -119
CONST CtrlEnnd = -117
'CONST ShftUpArrow = 56
'CONST ShftDownArrow = 50
'CONST ShftRightArrow = 54
'CONST ShftLeftArrow = 52
'CONST ShftDeleat = 46
'CONST ShftInsirt = 48
'CONST ShftPageUp = 57
'CONST ShftPageDown = 51
'CONST ShftHome = 55
'CONST ShftEnnd = 49
'CONST AltUpArrow = 8
'CONST AltDownArrow = 2
CONST AltRightArrow = 6
CONST AltLeftArrow = 4
'CONST AltDeleat = 0
'CONST AltInsirt = 0
'CONST AltPageUp = 9
'CONST AltPageDown = 3
'CONST AltHome = 7
'CONST AltEnnd = 1
CONST F1 = -59
CONST F2 = -60
CONST F3 = -61
CONST F4 = -62
CONST F5 = -63
CONST F6 = -64
CONST F7 = -65
CONST F8 = -66
CONST F9 = -67
CONST F10 = -68
CONST F11 = -133
CONST F12 = -134
'CONST CtrlF1 = -94
'CONST CtrlF2 = -95
'CONST CtrlF3 = -96
'CONST CtrlF4 = -97
'CONST CtrlF5 = -98
'CONST CtrlF6 = -99
'CONST CtrlF7 = -100
'CONST CtrlF8 = -101
'CONST CtrlF9 = -102
'CONST CtrlF10 = -103
'CONST CtrlF11 = -137
'CONST CtrlF12 = -138
'CONST AltF1 = -104
'CONST AltF2 = -105
'CONST AltF3 = -106
'CONST AltF4 = -107
'CONST AltF5 = -108
'CONST AltF6 = -109
'CONST AltF7 = -110
'CONST AltF8 = -111
'CONST AltF9 = -112
'CONST AltF10 = -113
'CONST AltF11 = -139
'CONST AltF12 = -140
'CONST ShftF1 = -84
'CONST ShftF2 = -85
'CONST ShftF3 = -86
'CONST ShftF4 = -87
'CONST ShftF5 = -88
'CONST ShftF6 = -89
'CONST ShftF7 = -90
'CONST ShftF8 = -91
'CONST ShftF9 = -92
'CONST ShftF10 = -93
'CONST ShftF11 = -135
'CONST ShftF12 = -136

CONST BarLine = 1               'Line number for the menu bar
CONST BarLCol = 1               'Left column where the bar starts
CONST BarRCol = 80              'Right column where the bar ends

CONST AltChars$ = "1234567890     QWERTYUIOP    ASDFGHJKL     ZXCVBNM"

''''''''''''''''''' ARRAY DECLARATIONS ''''''''''''''''''''''''''''''''
DIM MonthData(0 TO 12) AS MonthType
DIM Holiday(MaxHolidays) AS HolidayType
DIM StrataDate(MaxStrata) AS DateType
DIM FishingDay(MaxStrata) AS DayType
DIM CountStart(MaxStarts) AS StartType
'DIM Year(366) AS DayType2
DIM Year(MaxYears * 366) AS DayType2
DIM SurveyBound(MaxYears * 12) AS SurveyBoundType
DIM StartPoint(MaxStartPoints) AS StartPointType
DIM StartPointProb(1000) AS StartPointProbType
DIM Timing(MaxStrata) AS TimingType
DIM Warning(MaxStrata) AS WarningType
DIM HelpSpot(0 TO MaxHelpCalls) AS HelpSpotType
DIM HelpText$(0 TO MaxHelpLines)

DIM Col%(10)
DIM Row%(10)

'print "stack =";STACK
'print "press a key"
'c$=input$(1)

'STACK 4096
'STACK 4608
STACK 8192

