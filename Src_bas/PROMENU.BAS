DECLARE SUB BarPrint (Choice$(), Stat%())
DECLARE SUB CheckMark (Menu$(), Menu%, Choice%, Status%)
DECLARE SUB CSMsgBox (Message$, Wdth%, Colr%)
DECLARE SUB DisplayTime ()
DECLARE SUB PickFileList (FileName$)
DECLARE SUB PullDnMs (Choice$(), Stat%(), Menu%, Choice%, Ky$, Action%)
DECLARE FUNCTION PullMenKey% (Ky$, Choice$(), Stat%())
DECLARE SUB SetColor (ClrDisp%, CGAwMono%, MonoDisp%)
DECLARE SUB SetStatus (Status%, Truth%)
DECLARE SUB VertMenu (Item$(), Choice%, MaxLen%, BoxBot%, Ky$, Action%)
DECLARE SUB WindowMgr (WindowNumber%, Action%, ULRow%, ULCol%, LRRow%, LRCol%, Colr%)

'$DYNAMIC

'$INCLUDE: 'creelcal.bi'

'****************************************************************************
'Displays the menu bar on the line specified by the constant "BarLine".
'Also initializes a "Hot Key" table for use with "PullMenKey"
'
'   Choice$() -
'     The "Choice$" Array holds the Menu Choices.
'     Choice$(0, Menus) hold the Bar Titles.
'
'   Stat() -
'     This is an integer array parallel to the Choice$ array.
'     A value in the low byte of elements (M, 0) indicates the offset + 2
'     from the left menu title to display.  The high byte of the element
'     indicates the offset of the hot key letter.
'
'Note: This is separate so your Program can display the Menu Bar before it
'      makes any calls to the PullMenKey or PullDnMS Sub Programs.
'****************************************************************************
'
SUB BarPrint (Choice$(), Stat()) STATIC
    SHARED HotKeys$                             'Share "Menu" Hot key table

    IF NOT BeenHere THEN
       Cmd$ = UCASE$(COMMAND$)
       BeenHere = -1
    END IF

    LastMenu = UBOUND(Choice$, 2)               'Get the last menu number
    HotKeys$ = STRING$(LastMenu + 1, 0)         'Make string for Hot key table
    Bar$ = SPACE$(BarRCol - BarLCol + 1)        'Make a string for Menu Bar
    Temp = 4                                    'Put first Menu title @ pos. 4

    FOR N = 0 TO LastMenu                       'Examine each Menu title
                                                'Get the Hot key character
                                                '  from the given offset
        HotChr$ = UCASE$(MID$(Choice$(0, N), Stat(0, N) \ 256 + 1, 1))
        HotKey = INSTR(AltChars$, HotChr$)      'Get it's position in table
        IF HotKey <= 15 THEN                    'Is it a number?
           HotKey = 119 + HotKey                'Add 119
           IF HotKey = 119 THEN HotKey = 0      'Special case for "M"
        END IF
        MID$(HotKeys$, N + 1) = CHR$(HotKey)    'Put the character in table
        MID$(Bar$, Temp) = Choice$(0, N)        'Put Menu title in Bar string
        IF N < LastMenu THEN                    'Get position for next title
           Temp = Temp + Stat(0, N + 1) MOD 256 + LEN(Choice$(0, N)) + 2
        END IF
    NEXT

    HideCursor
    QPrintRC Bar$, BarLine, BarLCol, Cnf.MenBox    'Print the Menu title line
    ShowCursor

END SUB

SUB CheckMark (Menu$(), Menu%, Choice%, Status%)
   IF Status% THEN
      MID$(Menu$(Choice%, Menu%), LEN(Menu$(Choice%, Menu%)), 1) = " "
   ELSE
      MID$(Menu$(Choice%, Menu%), LEN(Menu$(Choice%, Menu%)), 1) = "®"
   END IF
END SUB

SUB CSMsgBox (Message$, Wdth, Colr%) STATIC

    IF LEN(Message$) THEN
       Row = CSRLIN                             'Get the Row Position

       REDIM Temp$(23)                          'Make a temporary array to
                                                '  hold the parsed up lines
       MesLen = LEN(Message$)                   'Get the total length of the
                                                '  help message

       LineNum% = 0                             'Line number
       Strt = 1                                 'Starting parse position

       '*** Parse the message into lines
       DO
          ' WHILE MID$(Message$, Strt, 1) = " "  'Skip over blanks
          '    Strt = Strt + 1
          ' WEND

           ND = Strt + Wdth                     'Guess at the end of line
                                                'Loop backwards to find
           WHILE MID$(Message$, ND, 1) <> " " AND ND <= MesLen AND ND > Strt
               ND = ND - 1                      '  blank space
           WEND

           'check for new line marker in text
           NewLine% = False
           FOR j% = Strt TO ND
              IF MID$(Message$, j%, 1) = "|" THEN
                 MID$(Message$, j%, 1) = " "
                 NewLine% = True
                 EXIT FOR
              END IF
           NEXT j%
           IF NewLine% THEN ND = j%

           LineNum% = LineNum% + 1              'Increment the Line number
                                                'Pull out the new line
           Temp$(LineNum%) = MID$(Message$, Strt, ND - Strt)
           Strt = ND + 1                        'Reset starting posit. for
                                                '  next line
       LOOP UNTIL ND >= MesLen                  'Get more unless @ end of mes

       BotRow = Row + LineNum% + 1              'Find the bottom row #
       IF BotRow > 24 THEN BotRow = 24          'No farther than line 24

       AraySize = 80 * (BotRow - Row + 2)       'Find the array size to use
       REDIM ScrBuf(AraySize)                   '  for the background screen
       MScrnSave Row, 1, BotRow + 1, 80, ScrBuf(0)

       BoxWdth = Wdth + 4                       'Find the outer box width
       ColPos = 40 - (BoxWdth \ 2)              'Starting posit for centered
                                                '  box
       LOCATE Row, ColPos                       'Print the top of box
       MQPrint "É" + STRING$(Wdth + 2, "Í") + "»", Colr%
       Boxx$ = "º" + SPACE$(Wdth + 2) + "º"      'Make the box sides

       FOR N = 1 TO LineNum%                     'Print message lines
           LOCATE N + Row, ColPos
           MQPrint Boxx$, Colr%
           LOCATE , ColPos + 2
           MQPrint Temp$(N), Colr%
       NEXT

       LOCATE N + Row, ColPos                   'Print the bottom of box
       MQPrint "È" + STRING$(Wdth + 2, "Í") + "¼", Colr%

       MPaintBox Row + 1, ColPos + BoxWdth, N + Row, ColPos + BoxWdth + 1, 8
       MPaintBox N + Row + 1, ColPos + 2, N + Row + 1, ColPos + BoxWdth + 1, 8

       Pass = 1                                 'Set flag say we've been here
       ERASE Temp$                              'Don't need this anymore

    ELSEIF Pass = 1 THEN                        'If we've been here before,
                                                '  restore the screen
       MScrnRest Row, 1, BotRow + 1, 80, SEG ScrBuf(0)
       ERASE ScrBuf                             'Clean up memory
       Pass = 0                                 'Reset flag for next call
    END IF

END SUB

SUB DisplayTime
   '----- Update the time every second
   IF TIMER > Tim! + 1 THEN
      Tim! = TIMER
      Row% = CSRLIN
      Col% = POS(0)
      LOCATE 25, 72
      MQPrint TIME$, Cnf.InActHiLt
      LOCATE Row%, Col%
   END IF
END SUB

SUB PickFileList (FileName$)

   '----- Read a directory of the disk
   NFiles = FCount%("*.*")                 'Find the number of files
   REDIM Items$(NFiles)                    'Dim the "Items$" array
   Items$(0) = "*.*"                       'Put file spec. in element 0
   FOR N = 1 TO NFiles                     'Pad elements to 12 for names
       Items$(N) = SPACE$(12)
   NEXT
   ReadFile VARPTR(Items$(0))              'Get the file names

   '----- Print instructions
   CALL WindowMgr(0, 1, 1, 20, 25, 61, Cnf.PulBar)
   LOCATE , , 0                            'Turn the cursor off
   CALL QPrintRC("File Selection Menu", 3, 31, Cnf.PulBar)
   CALL QPrintRC("Enter to select the highlighted File", 5, 23, Cnf.HiLite)
   CALL QPrintRC("[ESC] to Quit without selecting a file", 6, 22, Cnf.HiLite)

   MaxLen = 12             'this is the menu width
   BoxBot = 23             'limit the box length to go no lower than line 20
   Action = 0              '0 means stay in the menu until they select something

   LOCATE 8, 32, 0          'set upper left corner of menu, turn off the cursor
   InitMouse Temp%                         'Define the Mouse cursor
   TextCursor -2, -2       'define the Mouse cursor
   SetCursor 1, 1                          'Locate it at upper left of screen
   ShowCursor
   CALL VertMenu(Items$(), Choice, MaxLen, BoxBot, Ky$, Action)

   KeyCode% = KeyPressed%(Ky$)

   '----- Assign the selection to FileName$
   IF Choice AND KeyCode% <> Escape THEN
      FileName$ = Items$(Choice)
   ELSE
      FileName$ = ""
   END IF

   HideCursor
   CALL WindowMgr(0, 0, 0, 0, 0, 0, 0)
   ShowCursor
'   LOCATE , , 0                            'Turn the cursor off

END SUB

'****************************************************************************
' Description: Pull Down Menu Sub Program
'          by: Don Malin,  (C) Crescent Software 1988
'
' Parameters:
'
'   Choice$() -
'     The "Choice$" Array holds the Menu Choices.
'     Choice$(0, Menus) hold the Bar Titles.
'     Choice$(Choices, Menu) hold the Window Choices.
'     A "-" value for a choice indicates a dividing line across the menu.
'
'   Stat() -
'     This is an integer array parallel to the Choice$ array.
'     Any value in the low byte of an element indicates that the choice is
'     inactive (Non Selectable).  The high byte of the element indicates
'     the offset (0 based) of the hot key letter.
'
'   Menu -
'     Current Menu Number.  A nonzero value on entry will cause that menu
'     to be displayed.
'
'   Choice -
'     Current Menu Choice.  A nonzero value on entry will cause that
'     choice to be displayed.
'
'   Ky$ -
'     This string will contain the character or key code that was used to
'     exit the routine.
'
'   Action -
'     Action is a flag which can be used to control the routine in a
'     multi tasking (polled) mode.  The Action commands are as follows:
'
'       0 - PullDnMS will take control until an active choice is selected
'           or the [Esc] key is pressed.  (Single Tasking Mode)
'       1 - Used on initial entry for multitasking mode.  Only saves the
'           background screen, displays the Menu bar and the current menu.
'           Sets the Action variable to 3 for successive calls.
'       2 - Redisplay the current menu.  Reset Action to 3
'       3 - On entry means just check the keyboard and handle keys used by
'           the routine and exit back to the caller.  On exit means nothing
'           was selected.
'       4 - User made a selection from an active choice or pressed the
'           Escape key so you should check the Menu and Choice variables.
'       5 - Restore the screen, erase the screen buffer used by the routine.
'
'****************************************************************************
'
SUB PullDnMs (Choice$(), Stat(), Menu, Choice, Ky$, Action) STATIC

   CALL DisplayTime

    LOCATE , , 0

    IF Action = 4 THEN Action = 1       'See if they forgot to reinit. Action
    IF Action < 3 THEN                  'Redisplaying?
       IF Action < 2 THEN               'Redisplaying whole menu?
          LastMenu = UBOUND(Choice$, 2) 'Find Last Bar Menu Choice
          LastChoice = 0
          Wdth = 0
                                        'Find screen rows to save
          Temp = 80 * (UBOUND(Choice$, 1) + BarLine + 3)
          REDIM ScrBuf(Temp)            'Allocate screen memory
          REDIM BarLin(80)              'Save the Bar Line
          MScrnSave BarLine, 1, BarLine, 80, BarLin(0)
                                        
          BarPrint Choice$(), Stat()    'Display the Menu Bar
          
       END IF

       ArrayEnd = UBOUND(Choice$, 1)

       GOSUB PullDisplay                'Display the Menu box
       
       IF Action THEN Action = 3        'Reset the Action flag to idle state

       PrevChoice = Choice
       PrevMenu = Menu
       ButtonPress 1, Down, Presses, MCol, MRow
       IF Down = 0 THEN GOSUB PullHilite
    END IF

                                        'If Action flag=5, Restore the screen
    IF Action = 5 AND UBOUND(ScrBuf) > 1 THEN
       GOSUB CleanUp
       PrevChoice = Choice
       PrevMenu = Menu
       AltUsed = 0
       EXIT SUB                         'Bail out
    END IF

    IF Choice <> PrevChoice THEN GOSUB PullHilite
    IF Menu <> PrevMenu THEN GOSUB PullDisplay


    DO
                                        'If we have a Window, Highlight Choice
                                        'Get typed characters
       Ky$ = INKEY$

       CALL DisplayTime
            
       'MMMMMMMMMMMMMMMM Start of Mouse Handling Code MMMMMMMMMMMMMMMMMMM
       'MM   If you hate rodents, exterminate the code from here to    MM
       'MM   the next set of "MMM" comments.                           MM
       'MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
       ButtonPress 1, Down, Presses, MCol, MRow
       IF Presses = 0 THEN GetCursor MCol, MRow, Button
       MRow = (MRow \ Cnf.NPixLines) + 1
       MCol = (MCol \ 8) + 1

       IF (MRow <> LMRow OR MCol <> LMCol OR Presses) AND Down = 1 THEN
          WasPressed = -1
          IF MRow - BarLine - 1 <= LastChoice AND MRow - BarLine - 1 > 0 AND MCol >= StartCol AND MCol <= EnBar THEN
             Choice = MRow - BarLine - 1
             IF Choice$(Choice, Menu) = "-" THEN
                Choice = PrevChoice
                WasPressed = 0
             END IF
             GOSUB PullHilite
          ELSEIF MRow = BarLine THEN
             Temp = BarLCol + 2
             FOR N = 0 TO LastMenu
                 IF MCol >= Temp AND MCol <= Temp + LEN(Choice$(0, N)) + 1 THEN EXIT FOR
                 IF N < LastMenu THEN
                    Temp = Temp + Stat(0, N + 1) MOD 256 + LEN(Choice$(0, N)) + 2
                 END IF
             NEXT
             IF N <= LastMenu THEN Menu = N
             IF Menu <> PrevMenu THEN GOSUB PullDisplay
          END IF
          
       ELSEIF Button AND 2 THEN
          Ky$ = CHR$(27)
          DO
             GetCursor MCol, MRow, Button
          LOOP WHILE Button AND 2
       END IF

       IF Down = 0 AND WasPressed THEN
          WasPressed = 0
          IF MRow - BarLine - 1 <= LastChoice AND MRow - BarLine - 1 > 0 AND MCol >= StartCol AND MCol <= EnBar THEN
             Ky$ = CHR$(13)
          ELSEIF MRow = BarLine THEN
             IF Wdth = 0 THEN
                Ky$ = CHR$(13)
             ELSE
                GOSUB PullHilite
             END IF
          ELSE
             Ky$ = CHR$(27)
          END IF
       END IF
          
       IF Down = 0 THEN
          LMRow = -1
          LMCol = -1
       ELSE
          LMRow = MRow
          LMCol = MCol
       END IF
       'MMMMMMMMMMMMMMMMMMM End of mouse handling code MMMMMMMMMMMMMMMMMMMMMM


       IF LEN(Ky$) THEN

          K = ASCII(RIGHT$(Ky$, 1))
          IF LEN(Ky$) = 2 THEN
             IF AltKey THEN
                IF K >= 120 THEN K = K - 119
                K = ASCII(MID$(AltChars$, K, 1))
             ELSE
                K = -K
             END IF
          END IF

          SELECT CASE K
             CASE 13, 27
                IF Stat(Choice, Menu) MOD 256 = 0 OR K = 27 THEN
                   IF Action = 0 THEN
                      GOSUB CleanUp
                      EXIT SUB
                   ELSE
                      Action = 4
                   END IF
                ELSEIF K = 13 THEN
                   Ky$ = ""
                END IF

             CASE 32 TO 254
                IF Wdth AND K <> 45 THEN 'See if User pressed a Choice Letter
                                                'Make Key Uppercase
                   IF K >= 97 AND K <= 122 THEN K = K - 32

                   Temp = 0
             
                   DO
                      Choice = Choice + 1
                      IF Choice > LastChoice THEN
                         IF Temp THEN EXIT DO
                         Choice = 1
                         Temp = Choice
                      END IF
                      Ltr = ASCII(UCASE$(MID$(Choice$(Choice, Menu), Stat(Choice, Menu) \ 256 + 1, 1)))
                   LOOP WHILE K <> Ltr OR Stat(Choice, Menu) MOD 256

                   IF K <> Ltr OR Choice > LastChoice THEN
                      Choice = PrevChoice
                   ELSE
                      Ky$ = CHR$(13)
                      AltUsed = AltKey
                      IF Action = 0 THEN
                         GOSUB CleanUp
                         EXIT SUB
                      ELSE
                         Action = 4
                      END IF
                   END IF
                END IF

             CASE -72                   'Up Arrow Key
                IF Wdth THEN
                   Choice = Choice - 1 + (Choice$(Choice - 1, Menu) = "-")
                   IF Choice < 1 THEN Choice = LastChoice
                END IF

             CASE -80                   'Down Arrow Key
                IF Wdth THEN
                   IF Choice >= LastChoice THEN Choice = 0
                   Choice = Choice + 1 - (Choice$(Choice + 1, Menu) = "-")
                END IF

             CASE -71                   'Home Key
                IF Wdth THEN Choice = 1

             CASE -79                   'End Key
                IF Wdth THEN Choice = LastChoice

             CASE -75                   'Left Arrow Key
                IF Menu = 0 THEN        'If on Far Left Bar Choice, go to
                   Menu = LastMenu      '  Far Right Choice
                ELSE
                   Menu = Menu - 1      'Otherwise Decrement Bar Choice
                END IF
                GOSUB PullDisplay       'Display the New Choice Window

             CASE -77                   'Right Arrow Key
                IF Menu = LastMenu THEN 'If on Far Right Bar Choice, go to
                   Menu = 0             '  Far Left Choice
                ELSE
                   Menu = Menu + 1      'Otherwise Increment Bar Choice
                END IF
                GOSUB PullDisplay       'Display the New Choice Window

             CASE ELSE
          END SELECT

          GOSUB PullHilite
       END IF

       PrevChoice = Choice
       PrevMenu = Menu
    LOOP WHILE Action = 0

    
    EXIT SUB

PullHilite:
    IF Wdth THEN
       IF PrevChoice THEN
          IF Stat(PrevChoice, Menu) MOD 256 THEN
             Clr = Cnf.InActCh
          ELSE
             Clr = Cnf.MenBox
          END IF
          Row = PrevChoice + BarLine + 1
          LCol = StartCol
          RCol = EnBar
          GOSUB PaintIt
          IF Clr = Cnf.MenBox THEN
             LCol = StartCol + 1 + (Stat(PrevChoice, Menu) \ 256)
             RCol = LCol
             Clr = Cnf.ActivCh
             GOSUB PaintIt
          END IF
       END IF

       IF Choice THEN
          Row = Choice + BarLine + 1
          LCol = StartCol
          RCol = EnBar
          Clr = Cnf.HiLite
          GOSUB PaintIt
          IF Stat(Choice, Menu) MOD 256 = 0 THEN
             LCol = StartCol + 1 + (Stat(Choice, Menu) \ 256)
             RCol = LCol
             Clr = Cnf.PulBar
             GOSUB PaintIt
          END IF
       END IF
    END IF
    LOCATE Choice + BarLine + 1, StartCol + 1
RETURN


PullDisplay:
                                        'Restore the screen
    IF Menu <> PrevMenu OR Action < 2 THEN

       IF Wdth THEN
          MScrnRest BarLine + 1, StartCol - 1, LastChoice + BarLine + 3, EnBar + 3, ScrBuf(0)
       END IF
                                        'Repaint the Menu bar
       IF StartCol THEN
          Row = BarLine
          LCol = StartCol
          RCol = EnCol
          Clr = Cnf.MenBox
          GOSUB PaintIt
       END IF
       IF Menu <> PrevMenu OR Choice = 0 THEN Choice = 1
       PrevChoice = Choice
    END IF
    PrevMenu = Menu                     'Save the menu number

    StartCol = BarLCol + 2              'Find the column positions to start
    FOR c = 0 TO Menu - 1               '  and end the highlight on Bar Menu
        StartCol = StartCol + LEN(Choice$(0, c)) + 2 + Stat(0, c + 1) MOD 256
    NEXT
    EnCol = StartCol + LEN(Choice$(0, Menu)) + 1
                                        'Highlight the Bar Menu Choice
    Row = BarLine
    LCol = StartCol
    RCol = EnCol
    Clr = Cnf.HiLite
    GOSUB PaintIt
   
    Wdth = 0                            'Find the Width and # of Choices of
                                        '  the new menu
    FOR c = 1 TO ArrayEnd
       ChLen = LEN(Choice$(c, Menu))
       IF ChLen = 0 THEN EXIT FOR
       IF ChLen > Wdth THEN Wdth = ChLen
    NEXT
    LastChoice = c - 1

    IF StartCol + Wdth + 2 > 80 THEN    'If the Right Side of the window is
       StartCol = 80 - Wdth - 2         '  off the screen, adjust the Left
    END IF                              '  Side of the window


    EnBar = StartCol + Wdth + 1         'Set the Right Side of the Window
    

    IF Wdth THEN                        'If there are window choices, print
                                        '  them
       IF Action <> 2 THEN
          MScrnSave BarLine + 1, StartCol - 1, LastChoice + BarLine + 3, EnBar + 3, ScrBuf(0)
       END IF
       HideCursor

       Box0 BarLine + 1, StartCol - 1, LastChoice + BarLine + 2, EnBar + 1, 1, Cnf.MenBox
       Buf$ = SPACE$(Wdth + 2)

       FOR c = 1 TO LastChoice
           IF Choice$(c, Menu) = "-" THEN
              QPrintRC "Ã" + STRING$(Wdth + 2, "Ä") + "´", c + BarLine + 1, StartCol - 1, Cnf.MenBox
           ELSE

              LSET Buf$ = " " + Choice$(c, Menu)
              IF Stat(c, Menu) MOD 256 THEN
                 Clr = Cnf.InActCh
              ELSE
                 Clr = Cnf.MenBox
              END IF
              QPrintRC Buf$, c + BarLine + 1, StartCol, Clr
           END IF
           IF Stat(c, Menu) MOD 256 = 0 AND Choice$(c, Menu) <> "-" THEN
              Row = c + BarLine + 1
              LCol = StartCol + 1 + (Stat(c, Menu) \ 256)
              RCol = LCol
              Clr = Cnf.ActivCh
              GOSUB PaintIt
           END IF
       NEXT

       ShowCursor

       IF StartCol + Wdth + 4 <= 80 THEN
          MPaintBox BarLine + 2, EnBar + 2, c + BarLine + 2, EnBar + 3, 8
       END IF
       Row = c + BarLine + 2
       LCol = StartCol + 1
       RCol = EnBar + 1
       Clr = 8
       GOSUB PaintIt
    END IF
RETURN


PaintIt:
    MPaintBox Row, LCol, Row, RCol, Clr
RETURN


CleanUp:
    MScrnRest BarLine, 1, BarLine, 80, BarLin(0)
    IF Wdth THEN
       MScrnRest BarLine + 1, StartCol - 1, LastChoice + BarLine + 3, EnBar + 3, ScrBuf(0)
    END IF
    ERASE ScrBuf, BarLin             'Clean up memory
RETURN

END SUB

'****************************************************************************
'Checks for menu bar hot keys (Alt+Menu Letter).  Also checks for mouse
'presses on the menu bar.  Can be polled by your program to see if the User
'has done any thing to invoke the pull down menu.
'
'   Ky$ -
'     Passes the key string to check.
'
'   Choice$() -
'     The "Choice$" Array holds the Menu Choices.
'     Choice$(0, Menus) hold the Bar Titles.
'
'   Stat() -
'     This is an integer array parallel to the Choice$ array.
'     The high byte of the element indicates the offset of the hot key letter.
'
'  The function will return -1 if no menu was selected, otherwise it will
'  return the menu number of the User's choice.
'****************************************************************************
'
FUNCTION PullMenKey% (Ky$, Choice$(), Stat()) STATIC

    SHARED HotKeys$

    IF LEN(Ky$) = 2 THEN
       Menu = INSTR(HotKeys$, RIGHT$(Ky$, 1)) - 1
    ELSE
       Menu = -1
    END IF

    GetCursor MCol, MRow, Button
    IF Button THEN
       LastMenu = UBOUND(Choice$, 2)
       MRow = (MRow \ 8) + 1
       IF MRow = BarLine THEN
          MCol = (MCol \ 8) + 1
          Temp = BarLCol + 2
          FOR N = 0 TO LastMenu
              IF MCol >= Temp AND MCol <= Temp + LEN(Choice$(0, N)) + 1 THEN
                 Menu = N
                 EXIT FOR
              END IF
              IF N < LastMenu THEN
                 Temp = Temp + Stat(0, N + 1) MOD 256 + LEN(Choice$(0, N)) + 2
              END IF
          NEXT
       END IF
    END IF

    IF AltUsed THEN AltUsed = AltKey
   
    IF Menu = -1 AND NOT AltUsed THEN
       IF AltKey THEN
          IF WasDown = 0 THEN
             WasDown = -1
             IF NOT Bar THEN
                LastMenu = UBOUND(Choice$, 2)
                Temp = BarLCol + 2
                Clr = Cnf.ActivCh
                FOR N = 0 TO LastMenu
                    LCol = Temp + (Stat(0, N) \ 256) + 1
                    RCol = LCol
                    GOSUB PaintIt2
                    IF N < LastMenu THEN
                       Temp = Temp + LEN(Choice$(0, N)) + 2 + Stat(0, N + 1) MOD 256
                    END IF
                NEXT
                LOCATE , , 0
             END IF
          ELSEIF LEN(Ky$) THEN
             WasDown = 0
             KeyWasPressed = -1
          END IF

       ELSEIF WasDown THEN
          WasDown = 0
          IF Bar OR InStat OR LEN(Ky$) OR KeyWasPressed THEN
             GOSUB UndoBar
             KeyWasPressed = 0
             IF InStat = 0 AND LEN(Ky$) = 0 THEN Ky$ = CHR$(27)
          ELSE
             Bar = -1
             BarCh = 0
             PrevBarCh = 0
             GOSUB HiLiteBar
          END IF
       END IF

       IF Bar AND Menu = -1 THEN
          IF LEN(Ky$) THEN
             IF LEN(Ky$) = 1 THEN
                Ky$ = UCASE$(Ky$)
                K = ASCII(Ky$)
             ELSE
                K = -ASCII(RIGHT$(Ky$, 1))
             END IF
             SELECT CASE K
                CASE 13, -72, -80
                   Menu = BarCh
                   Ky$ = ""
                CASE 27, -83
                   GOSUB UndoBar
                CASE 32 TO 255
                   FOR N = 0 TO LastMenu
                       IF Ky$ = UCASE$(MID$(Choice$(0, N), Stat(0, N) \ 256 + 1, 1)) THEN
                          Menu = N
                          EXIT FOR
                       END IF
                   NEXT
                   IF N > LastMenu THEN
                      GOSUB UndoBar
                   ELSE
                      Ky$ = ""
                   END IF
                CASE -75
                   BarCh = BarCh - 1
                   IF BarCh < 0 THEN BarCh = LastMenu
                   GOSUB HiLiteBar
                   Ky$ = ""
                CASE -77
                   BarCh = BarCh + 1
                   IF BarCh > LastMenu THEN BarCh = 0
                   GOSUB HiLiteBar
                   Ky$ = ""
                CASE ELSE
                   GOSUB UndoBar
             END SELECT
           
          END IF
       END IF
    END IF

    IF Menu > -1 THEN
       GOSUB UndoBar
       WasDown = 0
       LOCATE , , 1
    END IF

    PullMenKey% = Menu

    EXIT FUNCTION


HiLiteBar:

    IF BarCh <> PrevBarCh THEN
       IF StartCol THEN
          LCol = StartCol
          RCol = EnCol
          Clr = Cnf.MenBox
          GOSUB PaintIt2
          
          LCol = StartCol + 1 + (Stat(0, PrevBarCh) \ 256)
          RCol = LCol
          Clr = Cnf.ActivCh
          GOSUB PaintIt2
       END IF
    END IF
    PrevBarCh = BarCh                   'Save the menu number
                                     
    StartCol = BarLCol + 2              'Find the column positions to start
    FOR N = 0 TO BarCh - 1              '  and end the highlight on Bar Menu
        StartCol = StartCol + LEN(Choice$(0, N)) + 2 + Stat(0, N + 1) MOD 256
    NEXT
    EnCol = StartCol + LEN(Choice$(0, N)) + 1
                                        'Highlight the Bar Menu Choice
    LCol = StartCol
    RCol = EnCol
    Clr = Cnf.HiLite
    GOSUB PaintIt2
    
    LCol = StartCol + 1 + (Stat(0, BarCh) \ 256)
    RCol = LCol
    Clr = Cnf.PulBar
    GOSUB PaintIt2

RETURN

PaintIt2:
    MPaintBox BarLine, LCol, BarLine, RCol, Clr
RETURN


UndoBar:
    Bar = 0
    BarPrint Choice$(), Stat()

RETURN

END FUNCTION

SUB SetColor (ClrDisp%, CGAwMono%, MonoDisp%)
   IF ClrDisp% THEN
      Cnf.InActCh = 52
      Cnf.InActHiLt = 27 '10 '64
      Cnf.NonMen = 112 '30
      Cnf.Revers = 121
      Cnf.Normal = 23
      Cnf.HiLite = 31
      Cnf.PulBar = 30 '48
      Cnf.ActivCh = 63 '48
      Cnf.MenBox = 49
    '  Cnf.MenBox = 112
    '  Cnf.ActivCh = 127
    '  Cnf.PulBar = 15
    '  Cnf.HiLite = 126
    '  Cnf.Revers = 15
    '  Cnf.Normal = 127
      Cnf.ReverseBlink = Cnf.Revers + 128
      Cnf.Blink = Cnf.Normal + 128
   'the way they sent it
   '   CNF.PulBar = 48
   '   CNF.MenBox = 49
   '   CNF.ActivCh = 48
   '   CNF.InActCh = 52
   '   CNF.HiLite = 31
   '   CNF.InActHiLt = 64
   '   CNF.NonMen = 30
   ELSEIF CGAwMono% THEN
      Cnf.PulBar = 113 '26 '48
      Cnf.MenBox = 112 '49
      Cnf.ActivCh = 25 '9  '48
      Cnf.InActCh = 52
      Cnf.HiLite = 31
      Cnf.InActHiLt = 64
      Cnf.NonMen = 9
      Cnf.Revers = 112
      Cnf.Normal = 7
      Cnf.ReverseBlink = Cnf.Revers + 128
      Cnf.Blink = Cnf.Normal + 128
   ELSEIF MonoDisp% THEN  'TRUE MONOCHROME
      Cnf.PulBar = 112
      Cnf.MenBox = 112
      Cnf.ActivCh = 110
      Cnf.InActCh = 112
      Cnf.HiLite = 15
      Cnf.InActHiLt = 80
      Cnf.NonMen = 7
      Cnf.Revers = 75
      Cnf.Normal = 7
      Cnf.ReverseBlink = Cnf.Revers + 128
      Cnf.Blink = Cnf.Normal + 128
   END IF

   'MENU COLORS SENT WITH PULLDNSM BY CRESCENT
   'MenuClr% = 112             'Menu Color
   'InActClr% = 120            'Inactive Choice color
   'HiliteClr% = 7             'Highlight bar color
   '
   'I REPLACED THE MENU COLORS IN THE PULLDNMS ROUTINES WITH
   '  THE CNF VARIABLES BELOW
   '
   ' MenuClr = CNF.MenBox
   ' HotLtrClr = CNF.ActivCh
   ' HiliteClr = CNF.HiLite
   ' HiLiteHotClr = CNF.PulBar
   ' InActClr = CNF.InActCh
END SUB

'****************************************************************************
'Sets the low byte of a word (Stat() element) without affecting the high byte.
'Used to set the Status byte of an element of the "Stat" array without
'disturbing the "Hot key offset" byte.
'
'  Status -
'    Status word to be set.  ie. Stat(Choice, Menu)
'
'  Truth -
'    Value to set.  ie. "Truth = LEN(FileName$) = 0"
'
'Example: CALL SetStatus(Stat(5, 0), LEN(FileName$) = 0)
'****************************************************************************
'
SUB SetStatus (Status, Truth) STATIC

    Status = 256 * (Status \ 256) + ABS(Truth)

END SUB

'****************************************************************************
'   File Name: VertMenu.Bas
' Description: Vertical Menu Sub Program
'          by: Don Malin,  (C) Crescent Software 1988
'
' Parameters:
'
'   Choice$() -
'     The "Choice$" Array holds the menu Choices.
'
'   Choice -
'     Current Menu Choice.  A value on entry will cause that choice to be
'     displayed.
'
'   MaxLen -
'     Length of the incoming Choice strings
'
'   BoxBot -
'     Row number of the bottom of the menu box.
'
'   Ky$ -
'     This string will contain the character or key code that was used to
'     exit the routine.
'
'   Action -
'     Action is a flag which can be used to control the routine in a
'     multi tasking (polled) mode.  The Action commands are as follows:
'       0 - Routine will take control until a choice is selected or the
'           [Esc] key is pressed.  (Single Tasking Mode)
'       1 - Used on initial entry for multi-tasking mode.  Saves the
'           background screen, displays the Menu.  Resets itself to
'           Action = 3.
'       2 - Redisplay the menu contents only.  Reset Action to 3
'       3 - On entry means just check the keyboard and handle keys used by
'           the routine and exit back to the caller.  On exit means nothing
'           was selected.
'       4 - User made a selection or pressed Escape.
'           routine.
'       5 - Restore the screen, erase the screen buffer used by the routine.
'
' Note: Requires a structure named - "Cnf" defined as "Config".  This
'       structure is included in "DefCnf.Bi"
'
' External Routines:  MScrnSave, MScrnRest, MPaintBox, MQPrint, Mouse
'************************************************************************
SUB VertMenu (Item$(), Choice, MaxLen, BoxBot, Ky$, Action) STATIC

                                                'Item$() has one dimension
                                                'Choice is the value returned
    IF Action = 4 THEN Action = 1               'See if they forgot to reset
    IF Action < 2 THEN REDIM ScrBuf(1)          'Temp Dim the Screen buffer
    IF Action = 5 AND UBOUND(ScrBuf) > 1 THEN   'Restore the screen if action=5
       MScrnRest Top - 1, LeftMargin, Bottom + 2, RightMargin + 2, ScrBuf(0)
       ERASE ScrBuf
       EXIT SUB
    END IF

    IF Action < 2 THEN
       GetVMode 0, 0, 0, ScRows, 0
       MouseTrap 1, 1, ScRows, 80

       IF BoxBot = 0 THEN BoxBot = ScRows - 1
       BoxBot = MinInt(BoxBot, ScRows - 1)

       'Find the Inverse box color for the Scroll Bar
       Fgr = Cnf.MenBox MOD 16
       ScrlClr = (Cnf.MenBox \ 16) + 16 * (Fgr - ((Fgr \ 8) * 8))

       Fill$ = SPACE$(MaxLen + 1)               'used in Dsply to pad spaces

       '---- determine how many choices to display, and where to put the menu
       MaxNum = UBOUND(Item$)                   'find number of active items
       MaxNum = FindLast%(VARPTR(Item$(MaxNum)), MaxNum)

       Top = CSRLIN + 1                         'put the menu at the current
       LeftMargin = POS(0)                      '  cursor location
       Bottom = Top + MaxNum - 1
       More = 0
       LastRow = 0

       IF Bottom >= BoxBot THEN  '<---/-- BoxBot sets lowest row to display
          More = -1              '   /
          Bottom = BoxBot - 1    '<-/
       END IF
       WinSize = Bottom - Top + 1
       RightMargin = LeftMargin + MaxLen + 3

       '----- Save the underlying screen area
       Size = (WinSize + 3) * (RightMargin - LeftMargin + 3)
       REDIM ScrBuf(Size + 1)
       MScrnSave Top - 1, LeftMargin, Bottom + 2, RightMargin + 2, ScrBuf(0)

       '----- Make sure the choice # is in range
       Offset = 0
       IF Choice = 0 OR Choice > MaxNum THEN
          Choice = 1
       END IF

       IF Choice > Offset + WinSize THEN
          Offset = Choice - WinSize
       END IF

       '----- draw border and display items on the screen
       HideCursor
       QPrintRC "É" + STRING$(MaxLen + 2, "Í") + "»", CSRLIN, LeftMargin, Cnf.MenBox
       FOR X = Top TO Bottom                  'Draw sides, display items
           APos = X - Top + 1

           IF Item$(APos + Offset) = "-" THEN
              Item$(APos + Offset) = STRING$(MaxLen, "Ä")
           END IF
           LSET Fill$ = Item$(APos + Offset)
                                                   'Left box side and choice
           QPrintRC "º ", X, LeftMargin, Cnf.MenBox
           QPrintRC Fill$, X, LeftMargin + 2, Cnf.ActivCh
           IF X = Top AND More = -1 THEN           'Draw Up arrow box side
              QPrintRC CHR$(24), X, RightMargin, ScrlClr
           ELSEIF X = Bottom AND More = -1 THEN    'Draw Down arrow box side
              QPrintRC CHR$(25), X, RightMargin, ScrlClr
           ELSEIF More THEN                        'Draw Scroll bar
              QPrintRC "²", X, RightMargin, ScrlClr
           ELSE                                    'Draw normal box side
              QPrintRC "º", X, RightMargin, Cnf.MenBox
           END IF
       NEXT
                                                   'Draw the bottom of box
       QPrintRC "È" + STRING$(MaxLen + 2, "Í") + "¼", X, LeftMargin, Cnf.MenBox

       MPaintBox Top, RightMargin + 1, Bottom + 1, RightMargin + 2, 8
       MPaintBox Bottom + 2, LeftMargin + 2, Bottom + 2, RightMargin + 2, 8

       ShowCursor

       Fill$ = ""
       LCol = LeftMargin + 1                  'Set variables for Hilite
       RCol = LCol + MaxLen + 1
       OPtr = 0
       GOSUB VertHiLite
    END IF

    IF Action = 2 THEN                        'Redisplay menu contents
       GOSUB VertDisplay
       GOSUB VertHiLite
    END IF

    IF Action THEN Action = 3                 'Reset Action = 3 (Idle state)


    DO

       DO                                     'Look for keys
           CALL DisplayTime
           Ky$ = INKEY$

           'MMMMMMMMMMMMMMMM Start of Mouse Handling Code MMMMMMMMMMMMMMMMMMM
           'MM   If you hate rodents, exterminate the code from here to    MM
           'MM   the next set of "MMM" comments.                           MM
           'MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
           '----- Get Mouse Coords. and Botton Info.
           IF LEN(MouseStor$) = 0 THEN
              MouseStor$ = SPACE$(MBuffSize%)
           END IF
           MGetState MouseStor$

           ButtonPress 1, Down, Presses, X, Y
           IF Presses = 0 THEN GetCursor X, Y, Button
           MRow = (Y \ Cnf.NPixLines) + 1          'Convert X, Y to Row and Column
           MCol = (X \ 8) + 1

           '----- If User pressed a button inside the Box, set flag.
           IF Presses THEN
              Inside = 0
              IF MRow >= Top - 1 AND MRow <= Bottom + 1 THEN
                 IF MCol > LeftMargin AND MCol < RightMargin - More THEN
                    MouseTrap Top - 1, LeftMargin + 1, Bottom + 1, RightMargin - More - 1
                    Inside = -1
                 END IF
              END IF
           END IF

           'IF Down = 0 THEN LetGo2& = PDQTimer&

           IF (Down AND 2) THEN
              Ky$ = CHR$(27)

           '----- Button pressed outside the box
           ELSEIF Presses AND NOT Inside THEN
              IF PutBack THEN
                 PutBack = 0
              ELSEIF Presses AND LEN(MouseStor$) > 0 THEN
                 HideCursor
                 MSetState MouseStor$
                 PutBack = -1
                 Presses = 0
              END IF

           '----- Handle only Mouse actions that were initiated inside the box
           ELSEIF Inside THEN
              IF Presses THEN WasPressed = -1

              '----- See if button pressed on the Scroll bar
              IF Presses AND More THEN
                 IF MCol = RightMargin AND MRow > Top AND MRow < Bottom THEN
                                                'See if button pressed on the
                    IF MRow = BarPtr THEN       '  Scroll Bar Pointer
                       OnPtr = -1
                       MouseTrap Top + 1, MCol, Bottom - 1, MCol
                    END IF
                    ScrlBar = -1                'Set Scroll Bar action flag
                 END IF
              END IF
              
              '----- Handle Scroll Bar Mouse actions
              IF ScrlBar THEN
                 IF Down = 0 THEN               'If button let go
                    IF MRow > BarPtr THEN       'If Below the Scroll Pointer
                       Ky$ = CHR$(0) + CHR$(81)         'Page Down
                    ELSEIF MRow < BarPtr THEN           'Must be above Scroll Pointer
                       Ky$ = CHR$(0) + CHR$(73)         'Page Up
                    END IF
                    ScrlBar = 0                 'Action complete so reset flag
                    OnPtr = 0
                    WasPressed = 0
                    MouseTrap 1, 1, ScRows, 80

                 ELSEIF OnPtr THEN              'Move the Scroll Bar Pointer
                    IF LMRow <> MRow THEN
                       BarRow = MRow
                       IF BarRow >= Bottom THEN BarRow = Bottom - 1
                       IF BarRow <= Top THEN BarRow = Top + 1
                       Choice = (BarRow - Top - 1&) * (MaxNum - 1) \ (WinSize - 3) + 1
                       Offset = Choice - 1
                       IF Offset + WinSize > MaxNum THEN Offset = MaxNum - WinSize
                       GOSUB VertDisplay
                       GOSUB VertHiLite
                    END IF
                 END IF

              '----- See if button pressed on Top or Bottom Arrow icons
              ELSEIF Down AND MCol = RightMargin AND More THEN
                 IF PDQTimer& > LstScrl& + 1 THEN       'Delay between updates
                    IF MRow = Top THEN                  'If on top, do Up
                       IF Choice > Offset + 1 THEN Choice = Offset + 2
                       Ky$ = CHR$(0) + CHR$(72)         '  Arrow proc.
                    ELSEIF MRow = Bottom THEN           'If on Bottom, do
                       IF Choice < Offset + WinSize - 1 THEN Choice = Offset + WinSize - 1
                       Ky$ = CHR$(0) + CHR$(80)         '  Down Arrow Proc.
                    END IF
                    LstScrl& = PDQTimer&                'Update Delay
                    IF Presses THEN LstScrl& = PDQTimer& + 4 'If first press,
                 END IF                                 'leave more time
             
              '----- Button pressed on Box Bottom or Top
              ELSEIF More AND Down = 1 AND (MRow < Top OR MRow > Bottom) THEN
                 IF PDQTimer& > LstScrl& + 1 THEN       'Delay between updates
                    IF MRow < Top THEN                  'If higher than Top,
                       Ky$ = CHR$(0) + CHR$(72)         '  Arrow proc.
                    ELSEIF MRow > Bottom THEN           'If Lower than Bottom,
                       Ky$ = CHR$(0) + CHR$(80)         '  Down Arrow Proc.
                    END IF
                    LstScrl& = PDQTimer&                'Update Delay
                    IF Presses THEN LstScrl& = PDQTimer& + 4 'If first press,
                 END IF                                 'leave more time

              '----- Button Pressed or Down on a Choice
              ELSEIF Down THEN
                 IF MRow <> LMRow OR MCol <> LMCol OR Presses THEN
                    IF MRow <= Bottom AND MRow >= Top THEN
                       TChoice = MRow - Top + 1 + Offset 'Calc. Choice number
                       IF LEFT$(Item$(TChoice), 1) <> "Ä" THEN
                          Choice = TChoice
                          FstPrsChoice = Choice
                          GOSUB VertHiLite              'Hi-Lite new choice
                       END IF
                    END IF
                 END IF
             
              '----- If the button was let go
              ELSEIF WasPressed AND Down = 0 THEN
                 IF MRow >= Top AND MRow <= Bottom THEN
                    IF MCol > LeftMargin AND MCol < RightMargin THEN
                       IF PDQTimer& < LetGo& + 10 THEN 'If half second since the last
                          IF LEFT$(Item$(FstPrsChoice), 1) <> CHR$(196) THEN
                             Ky$ = CHR$(13)        '  press/release, do Enter
                             Choice = FstPrsChoice '  proc.
                          END IF
                       ELSE                        'Start last-press timer
                          LetGo& = PDQTimer&
                          FstPrsChoice = Choice
                       END IF
                    END IF
                 END IF
                 WasPressed = 0
                 MouseTrap 1, 1, ScRows, 80
              END IF
             
              IF Down = 0 THEN
                 LMRow = -1
                 LMCol = -1
              ELSE
                 LMRow = MRow                 'Remember were we were so we
                 LMCol = MCol                 '  can detect movement.
              END IF

           END IF
           'MMMMMMMMMMMMMMMMMMMMMM End of Mouse Code MMMMMMMMMMMMMMMMMMMMMMMM

       LOOP WHILE Ky$ = "" AND Action = 0

       KyLen = LEN(Ky$)
                                              
       IF KyLen = 1 THEN                      'Non-extended keys

          SELECT CASE ASC(Ky$)
             CASE 13, 27                      'Enter or Escape key
                IF Action THEN
                   Action = 4
                ELSE
                   MScrnRest Top - 1, LeftMargin, Bottom + 2, RightMargin + 2, ScrBuf(0)
                   ERASE ScrBuf               'Clean up memory
                END IF
                LOCATE , , 1
                EXIT SUB

             CASE 33 TO 122                   'Letter key pressed
                                              'Reset color
                Ky = ASC(UCASE$(Ky$))         'Upper case version of letter
                K2 = ASC(LCASE$(Ky$))         'dito for lower case
                Pass = -1                     'First pass through choices
                N = Choice                    'Start at current choice + 1
                DO
                   Pass = Pass + 1
                   IF N < MaxNum THEN
                      N = N + 1
                   ELSE N = 1
                   END IF

                   DO WHILE N <= MaxNum         'was N < MaxNum, EW 4-22-92
                      A = ASC(Item$(N))
                      IF A = 91 THEN A = ASC(MID$(Item$(N), 3, 1))
                      IF Ky = A OR K2 = A THEN EXIT DO
                      N = N + 1
                   LOOP
                   IF Ky = A OR K2 = A THEN
                      Choice = N
                      IF Choice > Offset + (Bottom - Top + 1) THEN
                         Offset = Choice - (Bottom - Top + 1)
                         GOSUB VertDisplay
                      END IF
                      IF Choice <= Offset THEN
                         Offset = Choice - 1
                         GOSUB VertDisplay
                      END IF
                      EXIT DO
                   END IF
                LOOP WHILE Choice > 1 AND Pass = 0
                GOSUB VertHiLite

             CASE ELSE
          END SELECT

       ELSEIF KyLen = 2 THEN                  'Reset color

          SELECT CASE ASC(RIGHT$(Ky$, 1))

             CASE 72
                IF Choice > 1 THEN
                   DO
                      Choice = Choice - 1
                      IF Row - 1 < Top THEN Offset = Offset - 1
                   LOOP WHILE LEFT$(Item$(Choice), 1) = "Ä"
                   GOSUB VertDisplay
                ELSEIF More = 0 THEN
                   Choice = MaxNum
                END IF

             CASE 80
                IF Choice < MaxNum THEN
                   DO
                      Choice = Choice + 1
                      IF Row + 1 > Bottom THEN Offset = Offset + 1
                   LOOP WHILE LEFT$(Item$(Choice), 1) = "Ä"
                   GOSUB VertDisplay
                ELSEIF More = 0 THEN
                   Choice = 1
                END IF

             CASE 73
                IF Offset > 0 THEN
                   IF Offset - WinSize > 0 THEN
                      Offset = Offset - WinSize
                   ELSE Offset = 0
                   END IF
                   Choice = (Row - Top + 1) + Offset
                   GOSUB VertDisplay
                END IF

             CASE 81
                IF Offset + 2 * WinSize < MaxNum THEN
                   Offset = Offset + WinSize
                ELSE Offset = MaxNum - WinSize
                END IF
                Choice = (Row - Top + 1) + Offset
                GOSUB VertDisplay

             CASE 71
                Choice = 1
                Offset = 0
                GOSUB VertDisplay

             CASE 79
                Choice = MaxNum
                Offset = MaxNum - (Bottom - Top + 1)
                GOSUB VertDisplay

             CASE ELSE

          END SELECT
          GOSUB VertHiLite
       END IF

    LOOP WHILE Action = 0                     'get again if not multi-tasking

    MouseStor$ = ""

    EXIT SUB


VertHiLite:
    Row = Top + (Choice - Offset) - 1
    IF LastRow > 0 AND Row <> LastRow THEN MPaintBox LastRow, LCol, LastRow, RCol, Cnf.ActivCh
    MPaintBox Row, LCol, Row, RCol, Cnf.HiLite  'set inverse color
    LastRow = Row
   
    IF More THEN

       BarPtr = Choice * (WinSize - 3&) \ (MaxNum - 1) + Top + 1

       IF BarPtr <> OPtr THEN
          IF OPtr THEN
             LOCATE OPtr, RightMargin  'Update the Scroll Bar position
             MQPrint "²", ScrlClr
          END IF
          LOCATE BarPtr, RightMargin
          MQPrint CHR$(8), Cnf.MenBox
       END IF
       OPtr = BarPtr
    END IF

    LOCATE Row, LCol + 1

RETURN

'----- Print the choices in the window
VertDisplay:
    IF Choice <= Offset THEN Offset = Choice - 1
    IF Choice > Offset + WinSize THEN
       Offset = Choice - WinSize
    END IF

    LOCATE Top, LeftMargin + 2
    APrint VARPTR(Item$(Offset + 1)), Bottom - Top + 1, 1, MaxLen, -1, -1

RETURN

END SUB

'********** WindoMgr.Bas - "front end" to simplify accessing multiple windows
'
'Copyright (c) 1987 Ethan Winer
'
'WindowNumber% can range from 1 to 10, but 0 means open/close next/prior
'Action% is 1 to open a window, 0 to close it
'ULRow%, ULCol%, etc. define the window boundaries
'Colr% is the color for the box border, if the color is zero no box is drawn
'
'If any corner parameters are zero when closing, the original corners are
'used, so you don't have to keep specifying the same parameters each time.
'
'
SUB WindowMgr (WindowNumber%, Action%, ULRow%, ULCol%, LRRow%, LRCol%, Colr%) STATIC

STATIC CurWindow%                               'the last open window
STATIC Init%                                    'tells if we've been here yet
STATIC Table%()                                 'saves original corners

    IF Init% = 0 THEN                           'manager hasn't been used yet
       Init% = 1                                'now it has
       REDIM Table%(10, 4)                      'saves corner parameters
    END IF

    W% = WindowNumber%                                  'work with a copy
    IF Action% = 1 THEN CurWindow% = CurWindow% + 1     'open next window

    IF W% > CurWindow% THEN             'they went past the last active window
       CurWindow% = W%                  '  so make the current window last
    ELSEIF W% = 0 THEN                  'no window specified, so use
       W% = CurWindow%                  '  the next available window
    END IF
                                                        'in case they weren't
    IF CurWindow% < 1 OR CurWindow% > 10 THEN EXIT SUB  '  paying attention
      
    IF Action% = 1 THEN                         'opening a window

       'calculate the memory needed to save this portion of the screen
       Size% = ArraySize%(ULRow%, ULCol%, LRRow%, LRCol%)

       Table%(W%, 1) = ULRow%                   'save the corners so
       Table%(W%, 2) = ULCol%                   '  we can close the
       Table%(W%, 3) = LRRow%                   '  window later, even
       Table%(W%, 4) = LRCol%                   '  if the corners are
                                                '  omitted in the call
       IF W% = 1 THEN                           'make array to hold the screen
          REDIM A1%(Size%)
       ELSEIF W% = 2 THEN
          REDIM A2%(Size%)
       ELSEIF W% = 3 THEN
          REDIM A3%(Size%)
       ELSEIF W% = 4 THEN
          REDIM A4%(Size%)
       ELSEIF W% = 5 THEN
          REDIM A5%(Size%)
       ELSEIF W% = 6 THEN
          REDIM A6%(Size%)
       ELSEIF W% = 7 THEN
          REDIM A7%(Size%)
       ELSEIF W% = 8 THEN
          REDIM A8%(Size%)
       ELSEIF W% = 9 THEN
          REDIM A9%(Size%)
       ELSEIF W% = 10 THEN
          REDIM A10%(Size%)
       ELSE
          EXIT SUB
       END IF
    END IF
      
    IF W% = 1 THEN
       Segment% = VARSEG(A1%(0))
       Address% = VARPTR(A1%(0))
    ELSEIF W% = 2 THEN
       Segment% = VARSEG(A2%(0))
       Address% = VARPTR(A2%(0))
    ELSEIF W% = 3 THEN
       Segment% = VARSEG(A3%(0))
       Address% = VARPTR(A3%(0))
    ELSEIF W% = 4 THEN
       Segment% = VARSEG(A4%(0))
       Address% = VARPTR(A4%(0))
    ELSEIF W% = 5 THEN
       Segment% = VARSEG(A5%(0))
       Address% = VARPTR(A5%(0))
    ELSEIF W% = 6 THEN
       Segment% = VARSEG(A6%(0))
       Address% = VARPTR(A6%(0))
    ELSEIF W% = 7 THEN
       Segment% = VARSEG(A7%(0))
       Address% = VARPTR(A7%(0))
    ELSEIF W% = 8 THEN
       Segment% = VARSEG(A8%(0))
       Address% = VARPTR(A8%(0))
    ELSEIF W% = 9 THEN
       Segment% = VARSEG(A9%(0))
       Address% = VARPTR(A9%(0))
    ELSEIF W% = 10 THEN
       Segment% = VARSEG(A10%(0))
       Address% = VARPTR(A10%(0))
    END IF
      
    IF Action% = 1 THEN
       CALL ScrnSave(ULRow%, ULCol%, LRRow%, LRCol%, BYVAL Segment%, BYVAL Address%, -1)
       CALL ClearScr(ULRow%, ULCol%, LRRow%, LRCol%, Colr%, -1)
       IF Colr% THEN CALL Box(ULRow%, ULCol%, LRRow%, LRCol%, 2, Colr%, -1)
    ELSE        'change this 2 to a 1 for a single line box --^
       IF ULRow% = 0 OR ULCol% = 0 OR LRRow% = 0 OR LRCol% = 0 THEN
          CALL ScrnRest(Table%(W%, 1), Table%(W%, 2), Table%(W%, 3), Table%(W%, 4), BYVAL Segment%, BYVAL Address%, -1)
       ELSE
          CALL ScrnRest(ULRow%, ULCol%, LRRow%, LRCol%, BYVAL Segment%, BYVAL Address%, -1)
       END IF
    END IF
      
    IF Action% = 0 THEN

       IF W% = 1 THEN
          ERASE A1%
       ELSEIF W% = 2 THEN
          ERASE A2%
       ELSEIF W% = 3 THEN
          ERASE A3%
       ELSEIF W% = 4 THEN
          ERASE A4%
       ELSEIF W% = 5 THEN
          ERASE A5%
       ELSEIF W% = 6 THEN
          ERASE A6%
       ELSEIF W% = 7 THEN
          ERASE A7%
       ELSEIF W% = 8 THEN
          ERASE A8%
       ELSEIF W% = 9 THEN
          ERASE A9%
       ELSEIF W% = 10 THEN
          ERASE A10%
       END IF

       CurWindow% = CurWindow% - 1      'show the prior window as being
                                        '  the active one for next time
    END IF

END SUB

