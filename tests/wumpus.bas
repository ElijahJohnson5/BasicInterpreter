0 REM HUNT THE WUMPUS
10 PRINT "HUNT THE WUMPUS"
20 DIM s(3,20)
30 s(1,1)=2:s(2,1)=5:s(3,1)=8:s(1,2)=1:s(2,2)=3:s(3,2)=10:s(1,3)=4:s(2,3)=2:s(3,3)=12:s(1,4)=3:s(2,4)=5:s(3,4)=14:s(1,5)=1:s(2,5)=4:s(3,5)=6:s(1,6)=5:s(2,6)=7:s(3,6)=15:s(1,7)=6:s(2,7)=8:s(3,7)=17:s(1,8)=1:s(2,8)=7:s(3,8)=11:s(1,9)=10:s(2,9)=12:s(3,9)=19:s(1,10)=2:s(2,10)=9:s(3,10)=11:s(1,11)=8:s(2,11)=10:s(3,11)=20:s(1,12)=3:s(2,12)=9:s(3,12)=13:s(1,13)=12:s(2,13)=14:s(3,13)=18:s(1,14)=4:s(2,14)=13:s(3,14)=15:s(1,15)=6:s(2,15)=14:s(3,15)=16:s(1,16)=15:s(2,16)=17:s(3,16)=18:s(1,17)=7:s(2,17)=16:s(3,17)=20:s(1,18)=13:s(2,18)=16:s(3,18)=19:s(1,19)=9:s(2,19)=18:s(3,19)=20:s(1,20)=11:s(2,20)=17:s(3,20)=19
50 LET cave = 1
70 LET wumpus=RND(18)+2
80 LET pit1=RND(18)+2
90 LET pit2=RND(18)+2
100 LET bat1=RND(18)+2
110 LET bat2=RND(18)+2
150 GOTO 2000
200 PRINT ""
201 PRINT "You are in cave: "; cave
205 PRINT "Nearby caves: "; s(1,cave); " "; s(2,cave); " "; s(3,cave)
210 IF s(1,cave)=wumpus THEN 1000
211 IF s(2,cave)=wumpus THEN 1000
212 IF s(3,cave)=wumpus THEN 1000
220 IF s(1,cave)=pit1 THEN 1020
221 IF s(2,cave)=pit1 THEN 1020
222 IF s(3,cave)=pit1 THEN 1020
223 IF s(1,cave)=pit2 THEN 1020
224 IF s(2,cave)=pit2 THEN 1020
225 IF s(3,cave)=pit2 THEN 1020
230 IF s(1,cave)=bat1 THEN 1040
231 IF s(2,cave)=bat1 THEN 1040
232 IF s(3,cave)=bat1 THEN 1040
233 IF s(1,cave)=bat2 THEN 1040
234 IF s(2,cave)=bat2 THEN 1040
235 IF s(3,cave)=bat2 THEN 1040
240 INPUT "Move (0) or Shoot (1)" x
250 IF x = 0 THEN 300
260 IF x = 1 THEN 400 ELSE 240
300 INPUT "Cave to move to:" y
310 IF y>0 AND y<20 THEN 320 ELSE 300
320 IF y=s(1,cave) THEN 330 ELSE 340
330 cave=s(1,cave)
335 GOTO 900
340 IF y=s(2,cave) THEN 350 ELSE 360
350 cave=s(2,cave)
355 GOTO 900
360 IF y=s(3,cave) THEN 370 ELSE 300
370 cave=s(3,cave)
375 GOTO 900
400 INPUT "Cave to shoot at:" y
410 IF y>0 AND y<20 THEN 420 ELSE 400
420 IF y=s(1,cave) THEN 430 ELSE 440
430 cave=s(1,cave)
435 GOTO 800
440 IF y=s(2,cave) THEN 450 ELSE 460
450 cave=s(2,cave)
455 GOTO 800
460 IF y=s(3,cave) THEN 470 ELSE 400
470 cave=s(3,cave)
480 GOTO 800
800 REM CHECK Wumpus
810 IF cave=wumpus THEN 4000
820 PRINT "You have woken the Wumpus, and he ate you."
830 GOTO 3100
900 REM CHECK FOR INTERACTION
910 IF cave=wumpus THEN 3000
920 IF cave=pit1 THEN 3020
930 IF cave=pit2 THEN 3020
940 IF cave=bat1 THEN 960
950 IF cave=bat2 THEN 960 ELSE 200
960 PRINT "You have been transported by the bats!"
970 cave=RND(19)+1
980 GOTO 900
1000 PRINT "I smell a Wumpus!"
1010 GOTO 240
1020 PRINT "I feel a draft from a pit."
1030 GOTO 240
1040 PRINT "I hear bats nearby."
1050 GOTO 240
2000 PRINT "HUNT THE WUMPUS"
2010 PRINT ""
2020 PRINT "You are trapped in a cave with the dangerous Wumpus."
2030 PRINT "You must shoot the Wumpus with your arrow if you are"
2040 PRINT "to survive. There are many obstacles within these"
2050 PRINT "caves:"
2060 PRINT "The Wumpus will kill you if you stumble into his cave."
2070 PRINT "If you fall in a pit, you will fall to your death."
2080 PRINT "If you run into bats, they will transport you to a"
2090 PRINT "random cave, possibly with the Wumpus inside it."
2100 PRINT ""
2110 PRINT "You must traverse the caves to determine where the"
2120 PRINT "Wumpus is and kill him before he wakes. If you shoot"
2130 PRINT "into the wrong cave, he will wake up and kill you."
2140 PRINT ""
2150 PRINT "Good Luck!"
2160 GOTO 200
3000 PRINT "You have been eaten by the Wumpus."
3010 GOTO 3100
3020 PRINT "You have fallen down a pit."
3030 GOTO 3100
3100 PRINT "You have died!"
3110 END
4000 PRINT "You have killed the Wumpus! Congratulations, you win!"
4010 END
