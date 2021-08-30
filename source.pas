program Gruesome;

(* Version 0.0.3
   Coded by Darren Grey, 21st Jan 2009.
   Distributed under GNU license (see license.txt).

   Compiles on Windows, Mac or Linux with FPC (Free Pascal Compiler).

   Uses self-coded ConnectedCaves3, LineDraw and CircleDraw algorithms
   to piece together a dynamic cave generator with a Line of Sight function
   to allow proper exploration.  Interface picks up movement key commands
   and also traverses up or down staircases.  Collision detection with walls
   and "rest on the spot" also implemented.  Going deeper creates more open
   caverns, whilst going higher creates tighter caverns.

   Also added "creatures" in the form of moving, spinning lights.  These
   are supposed to be avoided in the game, or killed under the right
   circumstances.  Several player abilities allow them to be tackled or
   avoided using different tactics.  See readme.txt and history.txt for
   further details and future development plans.

   Known issues:
    - Slow under Wine (due to either crt or FOV algorithm).
    - cursoroff function disabled under Linux.
    - messy code in places. *)


uses
   crt; (* Must be compiled under FPC to work properly. *)

const (* Global *)
   xmin = 1;
   xmax = 80;
   ymin = 3;
   ymax = 23; (* Map limits. *)

type (* Global *)
   mapsquare = record
      Floor : boolean;
      Connected : boolean;
      UpStairs : boolean;
      DownStairs : boolean;
      Occupied : boolean;
      Explored : boolean;
      Visible : boolean;  (* These all start as false. *)
      VisLimit : boolean; (* only used for LOS2 *)
      Light : longint;
      cindex : byte; (* Reference to creature array. *)
   end;

   creaturetype = record
      LightRange : byte; (* Shouldn't be higher than 6. *)
      LightAngle : byte; (* Scale 0 to 180 - around 45 best. *)
      Intensity : shortint; (* Can be positive or negative. *)
      Symbol : char;
      Colour : byte; (* 0 to 15. *)
      xpos : byte;
      ypos : byte;
      Female : boolean;
      Facing : byte; (* Number from 0 to 7 indicating direction.
                        0 is South, 1 SE, 2E, 3 NE .... 7 SW
                            543
                            6x2
                            701  *)
   end;

   mapcoords = array[xmin..xmax, ymin..ymax] of mapsquare;
   patharray = array of smallint;
   creaturearray = array[0..20] of creaturetype;
   namestring = string[10];


var (* Global *)
   coord : mapcoords;
   gruex, gruey, visrange, rnd, D : byte;
   move : char;
   DfBgC, DfTxC, LiBgC, LiTxC, MapBgC, MapTxC, VisBgC, VisTxC,
      (* Display colour variables for background and text.
          Df - default for whole console
          Map - darkened map areas
          Li - lit map areas
          Vis - visible map areas *)
   creaturemax, i, SP, SPMax, LP, LPMax,
   turns, kills, lurkkills, ladykills, lurkcount, spellscount, retreatcount,
   shadowturns
     : longint; (* Many of these are purely for achievements. *)
   Ascending, turncount, ShadowWalk : boolean;
   creatureindex : creaturearray;
   PlayerName : namestring;



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)


procedure DrawTile(x,y : byte);
(* Draws single tile in the console at the specified location.
   Current style is to have lit areas highlighted, unseen areas dimmed
   and viewable area as normal light grey.  This gives a sight halo around
   the character.

   CURSOR MUST BE SET TO CORRECT TILE BEFORE CALLING PROCEDURE! *)


begin
   if coord[x,y].Visible = false then
   begin
      TextBackground(MapBgC);
      TextColor(MapTxC);  (* Unseen areas appear dimmer.
               Distinguishing visible and unseen areas can be vital since
               a sudden appearance of a monster with a light source can mean
               instant death. *)
      if coord[x,y].Explored then
      begin
         if coord[x,y].UpStairs then write ('<')
         else if coord[x,y].Downstairs then write ('>')
         else if coord[x,y].Floor then write ('.')
         else write ('#')
      end                (* End of unseen explored display. *)
      else write (' ');  (* Empty dungeon space or unexplored. *)
   end
   else
   begin
      if coord[x,y].Light < 1 then
      begin
         TextBackground(VisBgC);
         TextColor(VisTxC)
      end
         (* Visible dark areas are default colours. *)
      else
      begin
         TextColor(LiTxC);
         TextBackground(LiBgC) (* Highlights lit areas. *)
      end;
      if coord[x,y].cindex > 1 then
         TextColor(creatureindex[coord[x,y].cindex].Colour);
      if (x = gruex) and (y = gruey) then
      begin
         TextBackground(Black);
         TextColor(Black);
         write (' ') (* Grue appears as black on black.  TextColor not
                        strictly speaking necessary... *)
      end
      else if coord[x,y].cindex > 1 then
              write (creatureindex[coord[x,y].cindex].Symbol)
      else if coord[x,y].UpStairs then write ('<')
      else if coord[x,y].Downstairs then write ('>')
      else if coord[x,y].Floor then write ('.')
      else write ('#'); (* If no others it must be wall. *)
   end;   (* End of Visible display. *)
end; (*** End of Procedure. ***)


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)


procedure DrawMap;
(* Draws map in console using global map array. *)

var
   x, y : byte;

begin
   GoToXY(1,3); (* Map display starts on the 3rd row. *)
   for y := ymin to ymax do  (* Rows *)
   begin
      GoToXY(1, y);
      for x := xmin to xmax do  (* Columns *)
         DrawTile(x,y);
   end;
   TextBackground(DfBgC);
   TextColor(DfTxC); (* Resets to default colours. *)
end; (*** End of Procedure. ***)


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)


procedure DrawSurround (centrex,centrey,visrange : byte);
(* Instead of refreshing the whole map, this only redraws squares within
   a certain radius of the character.  Covers area just moved from too,
   so no artifacts should be left from previous vision. *)

var
   xa, ya, xb, yb, x, y : byte;

begin
   if centrey - visrange - 1 < ymin then ya := ymin
      else ya := centrey - visrange - 1;
   if centrex - visrange - 1 < xmin then xa := xmin
      else xa := centrex - visrange - 1;
   if centrey + visrange + 1 > ymax then yb := ymax
      else yb := centrey + visrange + 1;
   if centrex + visrange + 1 > xmax then xb := xmax
      else xb := centrex + visrange + 1;  (* Ensures boundaries. *)
   for y := ya to yb do
   begin
      GoToXY(xa,y);
      for x := xa to xb do
         DrawTile(x,y)
   end;
   TextBackground(DfBgC);
   TextColor(DfTxC); (* Resets to default colours. *)
end;


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)


procedure RevealMap;

(* Marks whole map as Explored, using a flood fill starting from the given
   point. *)

var
   a, b, c, d : byte;

begin
   for a := (ymin+1) to (ymax-1) do
   begin
      for b := (xmin+1) to (xmax-1) do
      begin
         if coord[b,a].Floor = true then
         begin
            for c := (a-1) to (a+1)do
               for d := (b-1) to (b+1) do
                  coord[d,c].Explored := true;
         end;
      end;
   end;
   DrawMap;
end; (*** End of procedure. ***)


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)


procedure WipeMap;

var
   a, b : byte;

begin
   for a := ymin to ymax do
   begin
      for b := xmin to xmax do
      begin
         with coord[b,a] do
         begin
            Explored := false;
            Floor := false;
            Connected := false;
            UpStairs := false;
            DownStairs := false;
            Visible := false;
            Light := 0;
            cindex := 0;
         end;
      end; (* All squares are reset to null. *)
   end;
end;


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(* Now giving CaveGenerator procedures. *)

(* NOTES:

   Creates random cave system in semi-organic fashion.  Stores details in
   global map array.
   Design intended to produce natural looking open caves with scraggly edges.
   Variables are altered to give tighter or wider cave spaces.
   Follows steps:
     1 - Creates array of records with details of each map point.
     2 - Chooses random seed points to be floor points on the map.
     3 - Each floor point then begins Spread procedure around it.  This
         procedure gives a random chance for each surrounding square that
         it becomes a floor.
     4 - Each new floor square calls on Spread procedure around it too,
         beginning a recursive spread of floor squares.
     5 - Stairs are created, and all points around stairs are dug/spread.
     6 - First stairs are marked as connected.  Procedure begins marking
         all surrounding floor squares as connected, and spreads out through
         the accessible dungeon.
     7 - For each original starting point a procedure checks if it is
         accessible.  If it is not then it performs a connection procedure
         that looks for the nearest connected point and makes a path to it
         This is done for all starting points, ensuring the whole dungeon is
         connected together.

   Note that random chance in Spread can be altered to give very different
   results.  Over 140 gives extremely open dungeons, below 120 gives very
   tight dungeons.  Number of seeds (n) in CaveGenerator also affects
   dungeon shape, and must be customised to the dungeon size.  Lower values
   of n will work well with higher Spread chances.
   Good combinations for these two are:
    - rnd = 125, n = 150
    - rnd = 115, n = 200
    - rnd = 135, n = 50

   Known issues:
     None.        *)


procedure Dig (xin,yin : longint); FORWARD;
   (* Set up for recursive calling between Spread and Dig. *)



procedure Spread (swx, swy : longint);
(* Checks squares around target (within map limits) and randomly digs.
   Begins recursive spreading for dug squares.
   Note: Procedure should ONLY be called for squares that have ALREADY
   been dug.  Ensure that any call is preceded by coord[x,y].Floor := true
   or a check for coord[x,y].Floor = true.  (Procedure does this itself
   when recursing.) *)

var
   a, b : longint;

begin
   if (swy > (ymin + 1)) and (swy < (ymax - 1)) and (swx > (xmin + 1))
       and (swx < (xmax - 1))  then
   begin
      for b := (swy - 1)  to (swy + 1) do
      begin
         for a := (swx - 1) to (swx + 1) do
         begin
            if (random(rnd) > 100) then Dig(a, b);
               (* Small chance of digging squares (random is tweakable for
                  different cave styles).  Dig procedure then begins
                  recursion.  Random value must have chance of fail or
                  recursion will not end.  See notes at top on altering
                  the spread chance for different dungeon styles. *)
         end; (* End of x-range sweep. *)
      end; (* End of y-range sweep. *)
   end; (* End of min/max check. Note that squares can be dug near the
           edges with Spread, but no Spread is begun for those near-edge
           squares.  This leaves more jagged edges to the caverns. *)
end; (*** End of procedure. ***)




procedure Dig (xin,yin : longint);
(* Digs square if undug and calls to recursive Spread procedure. *)

begin
   if (coord[xin, yin].Floor = false) then
      (* Vital to make recursion eventually stop! *)
   begin
      coord[xin, yin].Floor := true;
      Spread(xin,yin);
   end;
end; (*** End of procedure. ***)



procedure RoomMake;
(* Creates a few square rooms in the dungeon, randomly placed. *)

var
   a1, a2, b1, b2, i, j, x, y : longint;

begin
   j := trunc(random(15-D));
   if j > 1 then
   begin
      for i := 1 to j do
      begin
         a1 := trunc(random(69)) + xmin + 3;
         a2 := a1 + 2 + trunc(random(5));
         b1 := ymin + 2 + trunc(random(15));
         b2 := b1 + 2 + trunc(random(2));
         for y := b1 to b2 do
            for x := a1 to a2 do
               coord[x,y].Floor := true;
      end;
   end;
end;



procedure ConnectFill (fillx, filly : longint);
(* Beginning with input square it marks floor squares as connected, and calls
   itself on all surrounding squares.  Stops when surrounded by walls or
   already connected squares. *)

var
   a, b : longint;

begin
   if (coord[fillx,filly].Floor = true) then (* Ignores walls. *)
   begin
      coord[fillx,filly].Connected := true;
      for b := (filly - 1) to (filly + 1) do
         for a := (fillx - 1) to (fillx + 1) do
         begin
            if (coord[a,b].Connected = false) then ConnectFill(a,b);
               (* Necessary limiter - only calls for unconnected squares. *)
         end;
   end;
end; (*** End of procedure. ***)




procedure ConnectDig (x1, y1, x2, y2 : longint);
(* Procedure which connects two points with the shortest path.  Since y-axis
   is smaller than x it tends to make horizontal lines. *)

begin
   while (x2 <> x1) or (y2 <> y1) do (* End condition is reaching target. *)
   begin
      (* x movement direction. *)
           if (x2 > x1) then x1 := x1 + 1
      else if (x2 < x1) then x1 := x1 - 1;
      (* y movement direction. *)
           if (y2 > y1) then y1 := y1 + 1
      else if (y2 < y1) then y1 := y1 - 1;
      coord[x1,y1].Floor := true; (* Creates floor space. *)
      coord[x1,y1].Connected := true; (* Reduces load on the ConnectFill
         procedure.  Safe to state since this procedure is always called with
         a target square that is connected. *)
         (* Alternative idea is to use Dig procedure for the square to make
            less artificial looking paths since it will begin Spread
            procedure around each part of the path.  However this can fill
            up the dungeon too much. *)
   end;
end; (*** End of procedure. ***)




procedure ConnectMake (makex, makey : longint);
(* Searches in circles around the starting point till it finds a square
   marked as connected.  Then sets this as the target destination and makes
   a connecting path to it by calling ConnectDig. *)

var
   a, b, r, i : longint;
   ConnectFound : boolean;

begin
   ConnectFound := false;
   r := 2; (* Ignores immediate squares, as technically if they were
              connected then the beginning square would be too. *)
   repeat
      a := makex + r;
      b := makey + r; (* Starts search in square to top-right.
                       Each subsequent repeat moves out in this direction. *)
      i := 0;

      while (ConnectFound = false) and (i < 2*r) do
      begin
         b := b - 1;
         i := i + 1;
         if (b > ymin) and (b < ymax) and (a > xmin) and (a < xmax) then
            if (coord[a,b].Connected = true) then ConnectFound := true;
      end; (* First arm of spiral going down from start of loop. *)

      i := 0;
      while (ConnectFound = false) and (i < 2*r) do
      begin
         a := a - 1;
         i := i + 1;
         if (b > ymin) and (b < ymax) and (a > xmin) and (a < xmax) then
            if (coord[a,b].Connected = true) then ConnectFound := true;
      end; (* Seconds arm goes from right to left. *)

      i := 0;
      while (ConnectFound = false) and (i < 2*r) do
      begin
         b := b + 1;
         i := i + 1;
         if (b > ymin) and (b < ymax) and (a > xmin) and (a < xmax) then
            if (coord[a,b].Connected = true) then ConnectFound := true;
      end; (* Third arm moves from bottom to top. *)

      i := 0;
      while (ConnectFound = false) and (i < 2*r) do
      begin
         a := a + 1;
         i := i + 1;
         if (b > ymin) and (b < ymax) and (a > xmin) and (a < xmax) then
            if (coord[a,b].Connected = true) then ConnectFound := true;
      end; (* Fourth arm loops back to start. *)

      r := r + 1; (* Spiral moves further out. *)
   until ConnectFound;
      (* This scans in a spiral around the starting point until a connected
         point is found.  The values of a,b will end up as the coordinates of
         the closest connected point.  Spiral starts at radius 2 since no
         immediate points can be already connected (in theory). *)
   ConnectDig(makex,makey,a,b); (* Calls procedure to dig tunnel between
                                   initial point and connect point found. *)
end; (*** End of procedure. ***)




procedure CaveGenerator;
(* Creates random seed points from which Dig/Spread procedures begin
   digging out the cave map.  Stairs are randomly placed, with digs around
   them too.  First stairs are marked as connected, and ConnectFill then
   marks all connected squares.  Then checks all initial starting points
   for unconnected squares and connects them up to the main dungeon.  This
   ensures no isolated areas. *)


type
   tracker = array of byte;

var
   a, b, i, n, q, r : byte;
   xseed : tracker;
   yseed : tracker; (* x and y co-ords for each starting point. *)

begin
   if Ascending = true then D := D - 1 else D := D + 1;
   rnd := 115 + D;
   n := 155 - 6*(rnd-125); (* Algorithm for no seed points based on how
              active the Spread procedure is.  Higher rnd values need lower
              numbers of seed points. *)
   SetLength(xseed,n);
   SetLength(yseed,n);
   xseed[0] := trunc(random(xmax - 8) + xmin + 4);
   yseed[0] := trunc(random(ymax - 8) + ymin + 4); (* Starting stairs. *)
   gruex := xseed[0];
   gruey := yseed[0];

   for i := 1 to (n-2) do xseed[i] := trunc(random(xmax - 6) + xmin + 3);
   for i := 1 to (n-2) do yseed[i] := trunc(random(ymax - 6) + ymin + 2);
                                        (* Random seed points. *)
   repeat
      xseed[n-1] := trunc(random(xmax - 8) + xmin + 4);
      yseed[n-1] := trunc(random(ymax - 8) + ymin + 4)
   until sqrt((sqr(xseed[n-1]-xseed[0]))+(sqr(yseed[n-1]-yseed[0]))) > 10;
      (* Up stairs always generated a certain min distance from down stairs.
         This uses the Pythagorean distance between points, which doesn't
         actually match the game movement distance, but suffices as a min
         distance check. *)

   for i := 0 to (n-1) do Dig(xseed[i],yseed[i]);
      (* Begins digging from all starting points.  This spreads recursively
         throughout dungeon. *)

   if Ascending then
   begin
      coord[xseed[0],yseed[0]].DownStairs := true;
      coord[xseed[n-1],yseed[n-1]].UpStairs := true
   end
   else
   begin
      coord[xseed[n-1],yseed[n-1]].DownStairs := true;
      coord[xseed[0],yseed[0]].UpStairs := true;
   end; (* Places stairs based on entry method. *)

   if D = 20 then
   begin
      coord[xseed[0],yseed[0]].DownStairs := false;
      coord[xseed[n-1],yseed[n-1]].DownStairs := false;
   end;
      (* D20 has no down stairs. *)

   for b := (yseed[0] - 1) to (yseed[0] + 1) do
      for a := (xseed[0] - 1) to (xseed[0] + 1) do
         Dig(a,b);
   for b := (yseed[n-1] - 1) to (yseed[n-1] + 1) do
      for a := (xseed[n-1] - 1) to (xseed[n-1] + 1) do
         Dig(a,b); (* Digs 3x3 square around both stairs and spreads. *)

   ConnectFill(xseed[0],yseed[0]); (* Calls on ConnectFill procedure to mark
      connected squares, starting with level entry point. *)

   for i := 1 to (n-1) do
   begin
      if (coord[xseed[i],yseed[i]].Connected = false) then
      begin
         ConnectMake(xseed[i],yseed[i]);
         ConnectFill(xseed[i],yseed[i]);
      end;
   end; (* Searches through all seed points for any marked unconnected.
           It then calls on the procedures to make a path to the nearest
           connected square and marks them and their surrounding squares as
           connected. *)
end; (*** End of procedure. ***)


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)



(* Now beginning Line of Sight algorithms.  Makes use of line drawing and
   circle drawing, which both produce results in arrays of co-ordinates.
   Arrays must be declared within main LoS procedure, and passed by
   reference to the subprocedures. *)


procedure MarkStep (x1, y1 : longint; yslope : boolean;
                    VAR xpath,ypath : patharray; VAR Li : longint); FORWARD;
procedure StraightLine (x1,y1,x2,y2 : longint; yslope : boolean;
                    VAR xpath,ypath : patharray; VAR Li : longint); FORWARD;
procedure Circle (xstart,ystart,r : longint;
                    VAR circlepathx,circlepathy : patharray); FORWARD;
procedure VisLine (x1,y1,x2,y2,r : longint); FORWARD;
(* Marking for future use. *)



procedure LineofSight (centrex, centrey, visrange : longint);
(* This procedure is currently unnused. *)

var
   a, b, xa, xb, ya, yb, Li : longint;
   circlepathx, circlepathy, xpath, ypath : patharray;
   yslope: boolean;


begin
   if centrey - visrange - 1 < ymin then ya := ymin
      else ya := centrey - visrange -1;
   if centrex - visrange - 1 < xmin then xa := xmin
      else xa := centrex - visrange -1;
   if centrey + visrange + 1 > ymax then yb := ymax
      else yb := centrey + visrange +1;
   if centrex + visrange + 1 > xmax then xb := xmax
      else xb := centrex + visrange +1;
   for a := ya to yb do
      for b := xa to xb do
         coord[b, a].Visible := false; (* Clears previous visible flags. *)

   Circle(centrex,centrey,visrange,circlepathx,circlepathy);
      (* Finds circumference points - end points for line paths. *)
   for a := 0 to (visrange*8 - 1) do
   begin
      if abs(circlepathy[a] - centrey) > abs(circlepathx[a] - centrex) then
         yslope := true
      else yslope := false;  (* Tests for yslope to see ifcalling values
                                should be reversed for StraightLine. *)
      if yslope then
         StraightLine(centrey,centrex,circlepathy[a],circlepathx[a],
                         yslope,xpath,ypath,Li)
      else StraightLine(centrex,centrey,circlepathx[a],circlepathy[a],
                           yslope,xpath,ypath,Li);
      b := -1;
         (* Lines are made to each circle edge.  These lines are followed
            and each square reached is marked as visible and explored. *)
      repeat
         b := b + 1;
         coord[xpath[b],ypath[b]].Explored := true;
         coord[xpath[b],ypath[b]].Visible := true;
         if coord[xpath[b],ypath[b]].Floor = false then b := Li;
            (* Loop ended when circumference reached or wall hit. *)
      until b = Li;
   end;
end; (*** End of Line of Sight procedure. ***)


procedure LineofSight2 (centrex, centrey, visrange : longint);
(* Alternative and more accurate procedure.
   Uses VisLine subprocedure for line-casting.
   Probably much slower than original, but this is made irrelevant by
   the slow speed of redraws. *)

var
   a, b, xa, xb, ya, yb, res : longint;

begin
   if centrey - visrange - 1 < ymin then ya := ymin
      else ya := centrey - visrange -1;
   if centrex - visrange - 1 < xmin then xa := xmin
      else xa := centrex - visrange -1;
   if centrey + visrange + 1 > ymax then yb := ymax
      else yb := centrey + visrange +1;
   if centrex + visrange + 1 > xmax then xb := xmax
      else xb := centrex + visrange +1;
   for a := ya to yb do
      for b := xa to xb do
         coord[b, a].Visible := false; (* Clears previous visible flags. *)

   res := 40; (* Resolution - affects how many lines are drawn in calculating
                 viewable squares.  Diminishing returns for exponentially
                 increasing CPU time as it rises.
                 This also acts as a maximum sight range that can be set
                 for the player. *)
   b := centrey - res;
   for a := (centrex - res) to (centrex + res) do
      VisLine(centrex,centrey,a,b,visrange);
   for b := (centrey - res)  to (centrey + res) do
      VisLine(centrex,centrey,a,b,visrange);
   for a := (centrex + res) downto (centrex - res) do
      VisLine(centrex,centrey,a,b,visrange);
   for b := (centrey + res) downto (centrey - res) do
      VisLine(centrex,centrey,a,b,visrange);
end;


procedure VisLine (x1,y1,x2,y2,r : longint);

var
   xpath, ypath, circlepathx, circlepathy : patharray;
   yslope, finished : boolean;
   Li, b : longint;


begin
   Li := 0;
   if abs(y2-y1) > abs(x2-x1) then yslope := true
      else yslope := false;  (* Tests for yslope to see if calling values
                                should be reversed for StraightLine. *)
   if yslope then
        StraightLine(y1,x1,y2,x2,yslope,xpath,ypath,Li)
   else StraightLine(x1,y1,x2,y2,yslope,xpath,ypath,Li);
   Circle(x1,y1,r,circlepathx,circlepathy);
   for b := 0 to (r*8-1) do
      coord[circlepathx[b],circlepathy[b]].VisLimit := true;
         (* Limit on sight vision is marked by a circle matching the sight
            radius.  This produces nicer circles than bloody trig. *)
   b := -1;
   finished := false;
   repeat
      b := b + 1;
      coord[xpath[b],ypath[b]].Explored := true;
      coord[xpath[b],ypath[b]].Visible := true;
      if (coord[xpath[b],ypath[b]].Floor = false)
         or (coord[xpath[b],ypath[b]].VisLimit = true) then finished := true;
   until finished;
      (* Each point on line is marked visible and explored.
         Line ended when range reached or wall hit. *)
   for b := 0 to (r*8-1) do
      coord[circlepathx[b],circlepathy[b]].VisLimit := false;
      (* Clears VisLimits for next time. *)
end;



(* Subroutines: *)


(* NOTE - calling function must include xpath, ypath and yslope variables.
   Must call StraightLine with potentially reversed y values. *)

procedure StraightLine (x1,y1,x2,y2 : longint; yslope : boolean;
                    VAR xpath,ypath : patharray; VAR Li : longint);
(* Creates a straight line between two points.  Works in 360 degrees as long
   as the x and y values are swapped upon calling if the y distance is
   greater. *)

var
   stepsize, remainder, countmax, n, xdiff, ydiff : longint;
   stepcount, xdir, ydir : longint;

begin
   if x2 > x1 then xdir := 1 else xdir := -1;
   if y2 > y1 then ydir := 1 else ydir := -1;
   xdiff := abs(x2 - x1) + 1;
   ydiff := abs(y2 - y1) + 1;
   SetLength(xpath,xdiff);
   SetLength(ypath,xdiff);
   Li := -1;
   if ydiff = 0 then
   begin
      stepsize := 0;
      remainder := 0;
      countmax := 1;
   end
   else
   begin
      stepsize := xdiff div ydiff;
      remainder := xdiff mod ydiff;
      countmax := ydiff div 2;
   end;
   stepcount := 0; (* End of initial set-up. *)

   repeat
      MarkStep(x1,y1,yslope,xpath,ypath,Li);
      n := 1;
      while n < stepsize do
      begin
         x1 := x1 + xdir;
         n := n + 1;
         Markstep(x1,y1,yslope,xpath,ypath,Li);
      end; (* End of while.  This segment completes long straight parts of
              the line. *)
      if x1 <> x2 then (* Necessary check on whether to continue or not. *)
      begin
         stepcount := stepcount + remainder;
         if stepcount > countmax then
         begin
            stepcount := stepcount - ydiff;
            x1 := x1 + xdir;
            Markstep(x1,y1,yslope,xpath,ypath,Li);
         end;
      end; (* End of both ifs.  This section increases the stepcount on each
              loop until it reaches the limit where an additional x step is
              needed.  It reduces itself when this happens so the loop
              carries on.  Can tweak countmax to get different lines.  *)
      if x1 <> x2 then
      begin
         x1 := x1 + xdir;
         y1 := y1 + ydir; (* Diagonal shift after all x shifts done.
                             May be redundant if target reached. *)
         if x1 = x2 then Markstep(x1,y1,yslope,xpath,ypath,Li);
      end;
   until x1 = x2; (* Necessary end condition. *)
end; (*** End of procedure. ***)



procedure Markstep (x1, y1 : longint; yslope : boolean;
                    VAR xpath,ypath : patharray; VAR Li : longint);
(* Notes the co-ordinates achieved as parts of the path being plotted.
   Reverses the co-ordinates if it's a y-slope.  i is a counter for the
   array of points, initially set to zero. *)

begin
   Li := Li + 1;
   if yslope then
   begin
      ypath[Li] := x1;
      xpath[Li] := y1
   end
   else
   begin
      xpath[Li] := x1;
      ypath[Li] := y1;
   end;
end; (*** End of Procedure. ***)





(* NOTE: Calling procedure must include initialisation of circlepathx and
   circlepathy arrays as type "patharray". *)

procedure Circle (xstart,ystart,r : longint;
                  VAR circlepathx, circlepathy : patharray);
(* Creates an array of points matching a circle of the desired radius.
   First creates an anti-clockwise arc from the min y position using a check
   on which direction follows the circle most truly.  Then uses this arc
   as a basis for completing the whole circle.  All 8 arcs are identical
   through reflection and rotation.

   Outputs two path arrays with co-ordinates for each circumference point.
   Arrays must be passed by reference. *)

type
   arctracker = array of boolean;

var
   x, y : longint;
   i, n : longint;
   xmove : arctracker;

begin
   i := -1;
   x := xstart;
   y := ystart + r;
   SetLength(xmove, r);
   SetLength(circlepathx, 8*r);
   SetLength(circlepathy, 8*r);
   for n := 0 to (r - 1) do
   begin
      if (sqrt(sqr(x-xstart+1) + sqr(y-ystart)) - r) < 0.5 then
         (* Checks if moving in the x direction stays within the circle.
            If not moves in y dir instead.  Note that it uses an effective
            circle radius of r - 0.5, to suit the block style of the circle
            better.  Uses simple circle equation: x^2 + y^2 = r^2 *)
      begin
         x := x + 1;
         xmove[n] := true
      end
      else
      begin
         y := y - 1;
         xmove[n] := false;
      end;
      i := i + 1;
      circlepathx[i] := x;
      circlepathy[i] := y;
   end; (* Arc 1 completed. xmove copies arc 1 for all other arcs. *)

   for n := (r - 1) downto 0 do
   begin
      if xmove[n] = true then y := y - 1 (* Arc 2 reflection of Arc 1. *)
         else x := x + 1;
      i := i + 1;
      circlepathx[i] := x;
      circlepathy[i] := y;
   end;
   for n := 0 to (r - 1) do
   begin
      if xmove[n] = true then y := y - 1 (* Arc 3: x-, y- *)
         else x := x - 1;
      i := i + 1;
      circlepathx[i] := x;
      circlepathy[i] := y;
   end;
   for n := (r - 1) downto 0 do
   begin
      if xmove[n] = true then x := x - 1 (* Arc 4: x-, y-, reflected *)
         else y := y - 1;
      i := i + 1;
      circlepathx[i] := x;
      circlepathy[i] := y;
   end;
   for n := 0 to (r - 1) do
   begin
      if xmove[n] = true then x := x - 1 (* Arc 5: x-, y+ *)
         else y := y + 1;
      i := i + 1;
      circlepathx[i] := x;
      circlepathy[i] := y;
   end;
   for n := (r - 1) downto 0 do
   begin
      if xmove[n] = true then y := y + 1 (* Arc 6: x-, y+, reflected *)
         else x := x - 1;
      i := i + 1;
      circlepathx[i] := x;
      circlepathy[i] := y;
   end;
   for n := 0 to (r - 1) do
   begin
      if xmove[n] = true then y := y + 1 (* Arc 7: x+, y+ *)
         else x := x + 1;
      i := i + 1;
      circlepathx[i] := x;
      circlepathy[i] := y;
   end;
   for n := (r - 1) downto 0 do
   begin
      if xmove[n] = true then x := x + 1 (* Arc 8: x+, y+, reflected *)
         else y := y + 1;
      i := i + 1;
      circlepathx[i] := x;
      circlepathy[i] := y;
   end; (* Circle complete!  Co-ords stored in circlepath arrays. *)

   for n := 0 to i do
   begin
      if circlepathx[n] < xmin then circlepathx[n] := xmin;
      if circlepathx[n] > xmax then circlepathx[n] := xmax;
      if circlepathy[n] < ymin then circlepathy[n] := ymin;
      if circlepathy[n] > ymax then circlepathy[n] := ymax;
   end; (* Enforces range limits on the circle. *)
end; (*** End of procedure. ***)




procedure LightCone(x,y,facing,range,angle,intensity : longint);
(* Procedure to emit light from a point in a cone shape.  Called upon by
   "creatures".  x and y are centre points, other variables relate to
   the cone shape.  Intensity refers to whether it is a constructing or
   destructing light cone.  A destructing cone removes light from an area
   usually when moving a source/creature from one spot to another.

   This is ripped primarily from my original line of sight algorithm.
   A circle is made around the target area, and the start and end points of
   the light arc are calculated.  Lines are drawn from the source to these
   arc points, and each point is marked as lit along the line.  In the
   case of negative intensity these will be unlit, though multiple light
   sources might stop complete darkness.

   Currently has a bug I've been unable to fix - there's a bias towards
   the final line drawn, giving more light on the left-hand side of the
   source's facing.  For now I'll justify this as the adventurer holding
   the torch in their left hand! *)

var
   circlepathx, circlepathy, xpath, ypath : patharray;
   a, b, start, finish, Li : longint;
   yslope : boolean;

begin
   Li := 0;
   Circle(x,y,range,circlepathx,circlepathy);
   for b := 0 to (range*8-1) do
      coord[circlepathx[b],circlepathy[b]].VisLimit := true;
   Circle(x,y,10,circlepathx,circlepathy);
   facing := facing*10;
   start := facing - trunc(angle * 10/45);
   finish := facing + trunc(angle * 10/45) - 1;

   if start < 0 then (* For underflow angles. *)
   begin
      for a := (start+8*10) to (8*10-1) do
      begin
         if abs(circlepathy[a] - y) > abs(circlepathx[a] - x)
               then yslope := true
         else yslope := false;  (* Tests for yslope to see if calling values
                                   should be reversed for StraightLine. *)
         if yslope then StraightLine(y,x,circlepathy[a],circlepathx[a],
                            yslope,xpath,ypath,Li)
         else StraightLine(x,y,circlepathx[a],circlepathy[a],
                            yslope,xpath,ypath,Li);
         b := -1;
            (* Lines are made to each circle edge.  These lines are followed
               and each square reached is marked as lit. *)
         repeat
            b := b + 1;
            coord[xpath[b],ypath[b]].Light :=
               coord[xpath[b],ypath[b]].Light + intensity;
         until (coord[xpath[b],ypath[b]].VisLimit = true)
            or (coord[xpath[b],ypath[b]].Floor = false);
      end;
      start := 0;
   end;

   if finish > (8*10-1) then  (* For overflow angles. *)
   begin
      for a := 0 to (finish-8*10) do
      begin
         if abs(circlepathy[a] - y) > abs(circlepathx[a] - x) then yslope := true
         else yslope := false;  (* Tests for yslope to see if calling values
                                   should be reversed for StraightLine. *)
         if yslope then StraightLine(y,x,circlepathy[a],circlepathx[a],
                            yslope,xpath,ypath,Li)
         else StraightLine(x,y,circlepathx[a],circlepathy[a],
                            yslope,xpath,ypath,Li);
         b := -1;
            (* Lines are made to each circle edge.  These lines are followed
               and each square reached is marked as lit. *)
         repeat
            b := b + 1;
            coord[xpath[b],ypath[b]].Light :=
               coord[xpath[b],ypath[b]].Light + intensity;
         until (coord[xpath[b],ypath[b]].VisLimit = true)
            or (coord[xpath[b],ypath[b]].Floor = false);
      end;
      finish := 8*10-1;
   end;

   for a := start to finish do
   begin
      if abs(circlepathy[a] - y) > abs(circlepathx[a] - x) then yslope := true
      else yslope := false;  (* Tests for yslope to see if calling values
                                should be reversed for StraightLine. *)
      if yslope then StraightLine(y,x,circlepathy[a],circlepathx[a],
                         yslope,xpath,ypath,Li)
      else StraightLine(x,y,circlepathx[a],circlepathy[a],
                         yslope,xpath,ypath,Li);
      b := -1;
         (* Lines are made to each circle edge.  These lines are followed
            and each square reached is marked as lit until limit. *)
      repeat
            b := b + 1;
            coord[xpath[b],ypath[b]].Light :=
               coord[xpath[b],ypath[b]].Light + intensity;
      until (coord[xpath[b],ypath[b]].VisLimit = true)
            or (coord[xpath[b],ypath[b]].Floor = false);
   end;
   Circle(x,y,range,circlepathx,circlepathy);
   for b := 0 to (range*8-1) do
      coord[circlepathx[b],circlepathy[b]].VisLimit := false;
      (* Removes previous circle limit markings. *)
end;



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(* Now all creature-related procedures.  Very basic at present. *)


procedure CreatureMake(creature : byte); FORWARD;

procedure CreaturePopulate;
(* Decides how many creatures will be in the dungeon level and calls on
   procedure to create them all. *)

var
   i, j : longint;

begin
   (* Makes random number of enemies, increasing as player goes higher up
      in dungeon.  Starts at 2 enemies, finishes at 2-11. *)
   case D of
      20 : creaturemax := 3;
      17..19 : creaturemax := 4 + trunc(random(4));
      6..16 : creaturemax := 8 + trunc(random(4));
      1..5 : creaturemax := 8 + trunc(random(8));
   end;
   for i := 2 to creaturemax do CreatureMake(i);

end;


procedure CreatureMake(creature : byte);
(* Initiates a creature record and sets the creature in place on the
   dungeon level.  Input parameter is creature number, which identifies
   individual entities. *)


begin
   with creatureindex[creature] do
   begin
      repeat
         xpos := random(xmax - xmin - 1) + xmin + 1;
         ypos := random(ymax - ymin - 1) + ymin + 1;
      until (coord[xpos,ypos].Floor = true)
            and ( sqrt( sqr(gruex-xpos) + sqr(gruey-ypos)) > 7)
            and (coord[xpos,ypos].cindex = 0);
      Facing := trunc(random(8));
      Intensity := 1;
      coord[xpos,ypos].cindex := creature;
      Symbol := '@';
      (* Chooses random display colour.  Black and Light Grey disabled to
         stop background colour conflict.  Higher colours = higher difficulty
         and are only found below D13. *)
      if D < 13 then Colour := trunc(random(8)) + 1
         else Colour := trunc(random(14)) + 1;
      if Colour > 6 then Colour := Colour + 1;
      Female := Colour in [5, 13, 14];
      case Colour of (* Creature attributes are defined by colour. *)
         1 : begin
                LightRange := 2;
                LightAngle := 25;
             end;
         2 : begin
                LightRange := 2;
                LightAngle := 40;
             end;
         3 : begin
                LightRange := 2;
                LightAngle := 60;
             end;
         4 : begin
                LightRange := 3;
                LightAngle := 25;
             end;
         5 : begin
                LightRange := 3;
                LightAngle := 75;
             end;
         6 : begin
                LightRange := 3;
                LightAngle := 50;
             end;
         8 : begin
                LightRange := 4;
                LightAngle := 25;
             end;
         9 : begin
                LightRange := 4;
                LightAngle := 40;
             end;
         10 : begin
                LightRange := 4;
                LightAngle := 55;
             end;
         11 : begin
                LightRange := 4;
                LightAngle := 70;
             end;
         12 : begin
                LightRange := 5;
                LightAngle := 25;
             end;
         13 : begin
                LightRange := 5;
                LightAngle := 35;
             end;
         14 : begin
                LightRange := 5;
                LightAngle := 50;
             end;
         15 : begin
                LightRange := 5;
                LightAngle := 65;
             end;
     end;
     LightCone(xpos,ypos,Facing,LightRange,LightAngle,Intensity);
   end;
end;


procedure PlayerKill (victim : longint); FORWARD;

procedure CreatureMove(creature : byte);
(* Moves the object in a random direction and spins anti-clockwise. *)

var
   a, b, c : longint;

begin
   with creatureindex[creature] do
   begin
      (* Delete previous light marking. *)
      if Intensity <> 0 then
         LightCone(xpos,ypos,Facing,LightRange,LightAngle,(0-Intensity));
      coord[xpos,ypos].cindex := 0;

      (* Creature will randomly either spin or move.  Chance of each can
         be varied.  Currently set based on light range, so that enemies
         with longer ranges are more likely to turn, whilst short-sighted
         enemies move around more. *)
      c := trunc(random(LightRange));
      if c > 1 then
      begin
         (* Spins creature anti-clockwise. *)
         Facing := Facing + 1;
         if Facing > 7 then Facing := 0
      end
      else
      begin
         (* Chooses random move location.  Collision decetion to stop
            movement into walls or other adventurers. *)
         repeat
            a := trunc(random(3)) - 1;
            b := trunc(random(3)) - 1;
         until (coord[xpos + a,ypos + b].Floor = true)
               and (coord[xpos + a,ypos + b].cindex < 2);
         xpos := xpos + a;
         ypos := ypos + b;
      end;

      if coord[xpos,ypos].cindex = 0 then
      begin
         coord[xpos,ypos].cindex := creature;
         (* Mark new light position. *)
         if Intensity <> 0 then
            LightCone(xpos,ypos,Facing,LightRange,LightAngle,Intensity)
      end
      else PlayerKill(creature); (* Creature dead! *)
   end;
end;


procedure PlayerMove(movex, movey : integer);
(* Controls movement of the player from input commands.
   Includes checks for walls, light and creatures. *)

begin
   turncount := true;
   if coord[movex,movey].Floor = false then
   (* Wall collision - no turns pass.  Extra flavour text for killer grues. *)
   begin
      GoToXY(1,1);
      write ('You snarl at the wall.');
      if kills > 19 then write ('  The wall looks scared!');
      ClrEol;
      turncount := false;
   end;
   if coord[movex,movey].Light > 0 then
   (* Grue refuses to move into presently lit areas.  This also stops
      pouncing on enemies that have active light sources. *)
   begin
      GoToXY(1,1);
      write ('Hsss!  You refuse to step near the hateful light...');
      ClrEol;
      turncount := false;
   end;
   if turncount = true then
   (* If both previous tests fail then square can be moved into and turn
      counter advanced.  Player is moved to new position. *)
   begin
      gruex := movex;
      gruey := movey;
      if coord[gruex,gruey].cindex > 1 then
      begin
         if ShadowWalk = true then
         begin
            GoToXY(1,1);
            write('You cannot attack from the shadow realm.');
            ClrEol;
            turncount := false
         end
         else PlayerKill(coord[gruex,gruey].cindex);
      (* Kills enemies that you walk into.  Only possible if they have no
         light source. *)
      end;
   end;
end;


procedure StatusBar; FORWARD;

procedure PlayerKill(victim : longint);
(* Procedure to process character kill and award experience and SP to the
   player.  Victims passed to PlayerKill must already be darkened. *)

begin
   creatureindex[victim].ypos := 0; (* Deactivates creature from turns. *)
   kills := kills + 1;
   if creatureindex[victim].Female = true then ladykills := ladykills + 1;
   SP := SP + 2;
   if creatureindex[victim].Colour > 7 then SP := SP + 1;
   GoToXY(1,1);
   if creatureindex[victim].Intensity <> 0 then
   begin
      SP := SP + 2;
      lurkkills := lurkkills + 1;
      write ('The adventurer stumbles into your slavering fangs.  ');
      if creatureindex[victim].Female = true then write ('Saucy...')
         else write ('Gruevy...'); (* Lurk kill message. *)
      ClrEol;
   end
   else
   begin
      write ('You pounce on the blind, stumbling adventurer and devour ');
      if creatureindex[victim].Female = true then write ('her')
         else write ('him');
      write (' instantly.  Yum!');
      ClrEol;
   end;
   SPMax := trunc((kills + 2*lurkkills)/8) + 3;
   LPMax := trunc((kills + 2*lurkkills)/10) + 2;
   if kills > 59 then LPMax := LPMax - 1;
   if SP > SPMax then SP := SPMax;
   StatusBar;
   if (kills + 2*lurkkills) > 50 then visrange := 9;
end;



procedure PlayerShadowBall(x, y : integer);
(* This extinguishes all light sources around the player.
   Affects a larger radius for higher kill levels. *)

var
   i, j, r : longint;
   circlepathx, circlepathy : patharray;

begin
   if SP < 2 then
   (* Unable to cast spell without enough shadow points. *)
   begin
      GoToXY(1,1);
      write ('You have not the power of darkness within you.  Maybe after a little snack...');
      ClrEol
   end
   else
   begin
      (* More experienced players have bigger balls.  Weighted towards
         lurk kills. *)
      case (kills + lurkkills) of
         0..9 : r := 1;
         10..24 : r := 2;
         25..49 : r := 3;
         50..99 : r := 4;
         otherwise r := 5;
      end;
      turncount := true;
      SP := SP - 2;
      GoToXY(35,ymax+2);
      write(SP,'/',SPMax,' ');
      spellscount := spellscount + 1;
      GoToXY(1,1);
      write ('You unleash a mighty ball of shadow.');
      ClrEol;
      (* Multiple radiuses of the spell are performed. *)
      for i := 1 to r do
      begin
         Circle(x,y,i,circlepathx,circlepathy);
         for j := 0 to (i*8-1) do
         begin
            GoToXY(circlepathx[j],circlepathy[j]);
            write (' '); (* Blank black display over area of effect. *)
            if (coord[circlepathx[j],circlepathy[j]].cindex > 1)
               and (coord[circlepathx[j],circlepathy[j]].Light > 0) then
            (* Action taken with enemies hit by darkness.
               Only affects enemies with active light sources. *)
            begin
               with creatureindex[coord[circlepathx[j],circlepathy[j]].cindex] do
               begin
                  LightCone(xpos,ypos,Facing,LightRange,LightAngle,(0-Intensity));
                  LightRange := 1;
                  Intensity := 0;
                  GoToXY(1,1);
                  write ('The adventurer screams in fright as ');
                  if Female = true then write ('her ')
                     else write ('his ');
                  if LightAngle < 35 then write ('lantern')
                     else write ('torch'); (* Flavour effect. *)
                  write (' is extinguished.');
                  ClrEol;
               end; (* End of with. *)
            end; (* End of if - end of creature effect. *)
         end; (* End of individual square effect. *)
      end; (* End of circles. *)
   end;
end;


procedure PlayerSense;
(* Draws all level's enemies on map.  Requires and uses 1 SP. *)

var
   i, count : longint;

begin
   if SP = 0 then
   (* Unable to cast spell without shadow points. *)
   begin
      GoToXY(1,1);
      write ('You have not the power of darkness within you.  Maybe after a little snack...');
      ClrEol
   end
   else
   begin
      turncount := true;
      SP := SP - 1;
      GoToXY(35,ymax+2);
      write(SP,'/',SPMax,' ');
      spellscount := spellscount + 1;
      if creaturemax < 2 then (* Ne enemies on map. *)
      begin
         GoToXY(1,1);
         write ('You listen to the sweet music of darkness.');
         ClrEol
      end
      else
      begin
         count := 0;
         for i := 2 to creaturemax do
         begin
            if creatureindex[i].ypos <> 0 then (* Check if enemy still active. *)
            begin
               count := count + 1;
               if coord[creatureindex[i].xpos,creatureindex[i].ypos].Visible
                  = false then   (* Only bothers with creatures *)
               begin             (* already unseen. *)
                  GoToXY(creatureindex[i].xpos,creatureindex[i].ypos);
                  TextColor(creatureindex[i].Colour);
                  write (creatureindex[i].Symbol);
               end;
            end; (* Counts and draw each individual enemy on the map. *)
         end;
         GoToXY(1,1);
         TextColor(DfTxC);
         TextBackground(DfBgC);
         if count > 0 then
            write ('You sense the presence of ',count,' likely prey. [more]')
         else write ('You listen to the sweet sounds of darkness. [more]');
         ClrEol;
         ReadKey; (* Pasues until player responds. *)
         (* Now must redraw squares are they are from normal viewpoint. *)
         for i := 2 to creaturemax do
         begin
            if creatureindex[i].ypos <> 0 then (* Check if enemy still active. *)
            begin
               GoToXY(creatureindex[i].xpos,creatureindex[i].ypos);
               DrawTile(creatureindex[i].xpos,creatureindex[i].ypos);
            end;
         end;
         TextColor(DfTxC);
         TextBackground(DfBgC);
         GoToXY(1,1);
         ClrEol;
      end;
   end;
end;



procedure ColourStart; FORWARD;  (* Used by ShadowWalk. *)

procedure PlayerShadowWalk;
(* Enters player into ShadowWalk state, freezing time and allowing the player
   to move as he/she wishes.  Requires LP and ends when LP runs out.  Does
   not allow actions other than movement, and does not allow movement
   through light.  Relies on checks for "shadowwalk" boolean in other
   areas of the code to prevent this.

   Important aspect is display change.  See ColourStart for details of
   different text and background colours to be changed. *)


begin
   GoToXY(1,1);
   if (LP < 1) and (ShadowWalk = false) then
   begin
      write('You are not at one with the shadows.');
      ClrEol
   end
   else
   begin
      (* Status switch to represent why procedure was called. *)
      if ShadowWalk = true then ShadowWalk := false else ShadowWalk := true;
      if ShadowWalk = true then
      begin
         TextColor(5);
         write('You meld into the world of shadows.');
         DfBgC := 0;
         DfTxC := 8;             (* Currently set to blue explored text,  *)
         MapBgC := 0;            (* magenta visible text, yellow light    *)
         MapTxC := 1;            (* background and regular message/status *)
         LiBgC := 6;             (* text.                                 *)
         LiTxC := 14;
         VisBgC := 0;
         VisTxC := 5
      end
      else
      begin
         GoToXY(1,1);
         write('You emerge from the shadows. [more]');
         ClrEol;
         ReadKey;
         ColourStart; (* Back to default colours. *)
      end;
      TextColor(DfTxC);
      TextBackground(DfBgC);
      ClrScr;
      DrawMap;
      StatusBar; (* Whole screen redrawn in new colours. *)
      end;
end;


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

procedure SplashScreen;
(* Produces initial screens before main game display. *)

begin
   GoToXY(2,2);
   write ('> Welcome to Gruesome, where you play the grue.');
   GoToXY(2,4);
   write ('> What is your name? ');
   read (PlayerName);
   if PlayerName = '' then PlayerName := 'Grue';
   GoToXY(1,6);
   writeln('   You are a grue, a creature of darkness, lurking in the lowest depths of');
   writeln('   the caverns of infinity.  You feed off adventurers foolish enough to');
   writeln('   approach your home.  The only thing that can harm you is their hateful');
   writeln('   light.  The elder grues tell you of a land outside the caverns where a');
   writeln('   burning fire fills the sky and rains searing light over all that dwell');
   writeln('   there, but you find it hard to believe such ridiculous fantasies.');
   writeln;
   writeln('   You feel a twang of hunger inside your belly, and in the distance you');
   writeln('   hear the stumblings of another dumb adventurer.  Time to feast...');
   writeln;
   writeln;
   writeln('   ? for instruction in-game.');
   writeln;
   writeln('   Press any key to begin.');
   ReadKey;
end;


procedure InfoScreen;
(* Gives basic instructions and key commands. *)

begin
   ClrScr;
   GoToXY(36,2);
   writeln('GRUESOME');
   writeln;
   writeln('  Kill adventurers by disabling their light and pouncing on them or letting');
   writeln('  them wander into your slavering jaws (the latter gives more xp).  Build');
   writeln('  up your powers and attempt to head to the surface, but beware of light -');
   writeln('  it is quite lethal.  See readme or RogueBasin for further info.');
   writeln;
   writeln('  Commands:');
   writeln;
   writeln('     Move with the numpad or vikeys.');
   writeln('     </> to ascend/descend stairs.');
   writeln('     5/. to lurk on the spot, gaining LP.');
   writeln('     "a" to cast Dark Eyes to detect prey (uses 1 SP).');
   writeln('     "z" to cast Shadow Ball to blind prey (uses 2 SP).');
   writeln('     "s" to enter ShadowWalk, where time is frozen as long as you have LP.');
   writeln('     "Q" to quit at any time.');
   ReadKey;
   ClrScr;
   DrawMap;
   StatusBar;
end;


procedure Victory;
(* End screen when the player leaves D1. *)

begin
   move := 'Q';
   ClrScr;
   writeln;
   writeln(' You step tentatively out of the cave and look into the vast expanse stretching');
   writeln(' beyond.  Above there is no light, as the yowling mouth of Elyshalia, Goddess');
   writeln(' of Infinity, fills the sky with a choking blackness.  In the dark, dreary land');
   writeln(' below you see a castle in the distance, its lights flickering weakly in this');
   writeln(' tortured, dying world.  You decide to step out and explore...');
   writeln;
   writeln(' Congratulations, you have completed Gruesome v0.0.3!  Stay tuned for a far');
   writeln(' more in-depth game to come in distant future.');
   writeln;
   writeln;
   writeln(' ',PlayerName,' the grue left the Caverns of Infinity to explore the lands beyond.');
   writeln(' You ate ',kills,' adventurers and survived for ',turns,' turns, ',lurkcount,' of which were spent');
   writeln(' lurking in shadows.');
   writeln;
   writeln(' Achievements:');
   if kills > 99 then writeln('    Gruesome (100 or more kills)')
   else if kills > 49 then writeln('    Butcher (50 or more kills)')
   else if kills > 24 then writeln('    Greedy (25 or more kills)');
   if kills = 0 then writeln('    Pacifist (0 kills)');
   if (kills > 24) and (kills = lurkkills) then
      writeln('    Puritan (25 or more kills, all lurk kills)');
   if (kills > 19) and (ladykills > kills*0.8) then
      writeln('    LadyKiller (20 or more kills, 80% of them ladies)');
   if (kills/turns) > 0.05 then writeln('    Ninja (5% kills/turns ratio)');
   if lurkcount = 0 then writeln('    Restless (never lurked)');
   if spellscount = 0 then writeln('    Naturalist (never used magic)');
   if shadowturns = 0 then writeln('    Shadowless (spent no time in the Shadow realm)');
   if turns < 1000 then writeln('    Shadow Speed (under 1000 turns)')
   else if turns < 2500 then writeln('    Light Speed (under 2500 turns)')
   else if turns < 5000 then writeln('    Speedy (under 5000 turns)');
   if retreatcount = 0 then writeln('    Fearless (never used > stairs)');
   if retreatcount > 4 then writeln('    Backtracker (used > stairs more than 4 times)');
   ReadKey;
end;


procedure StatusBar;
(* Refreshes the status bar at the bottom of the screen.
   Must be initialised with correct Bg colours. *)

begin
   GoToXY(1,ymax+2);
   ClrEol;
   write(PlayerName);
   GoToXY(18,ymax+2);
   write('LP: ',LP,'/',LPMax,' ');
   GoToXY(31,ymax+2);
   write('SP: ',SP,'/',SPMax,' ');
   GoToXY(44,ymax+2);
   write('Meals: ',kills,' ');
   GoToXY(58,ymax+2);
   write('Turns: ',turns,' ');
   GoToXY(74,ymax+2);
   write('D: ',D);
   ClrEol;
end;


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)


procedure ColourStart;
(* Procedure sets values for the default text and background colours
   used in the game.  In future this is planned to be read from file. *)

(* Notes on colours:
     0 - Black
     1 - Dark Blue
     2 - Dark Green
     3 - Dark Cyan
     4 - Dark Red
     5 - Dark Pink
     6 - Brown/Dark Yellow
     7 - Light Grey
     8 - Dark Grey
     9 - Bright Blue
    10 - Bright Green
    11 - Bright Cyan
    12 - Bright Red
    13 - Pink/Magenta
    14 - Yellow
    15 - White

   Only 0-7 can be background colours.

   Df = default, used for messages and status bar.
   Map = map colour, used for unexplored areas on the map.
   Li = light, used for lit areas on map.
   Vis = visible, used for immediately visible areas on map.

   Default values:
      DfBgC := 0; (Black)
      DfTxC := 7; (Light Grey)
      MapBgC := 0; (Black)
      MapTxC := 8; (Dark Gray)
      LiBgC := 7; (Light Grey)
      LiTxC := 15; (White)
      VisBgC := 0; (Black)
      VisTxC := 7; (Light Grey)

   PlayerShadowWalk procedure may change these, but should always restore
   them through this procedure. *)


begin
   DfBgC := 0;
   DfTxC := 7;
   MapBgC := 0;
   MapTxC := 8;
   LiBgC := 7;
   LiTxC := 15;
   VisBgC := 0;
   VisTxC := 7;
end;


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(*** MAIN PROGRAM ***)

begin
   randomize;
   ColourStart; (* Initiates default colour scheme. *)
   TextColor(DfTxC);
   TextBackground(DfBgC);
   ClrScr;
   SplashScreen;
   rnd := 125;
   D := 21;
   Ascending := true; (* Note for CaveGenerator. *)
   cursoroff;
   CaveGenerator;  (* Creates cave level. *)
   CreaturePopulate; (* Populates level with enemies. *)
   visrange := 8;
   LP := 2;
   LPMax := 2;
   SP := 2;
   SPMax := 2; (* Initial grue stats. *)
   turns := 0;
   coord[gruex,gruey].cindex := 1;
   ClrScr;
   writeln('It is pitch black.  You are likely to eat someone.');
   StatusBar;
   LineofSight2(gruex,gruey,visrange);
   DrawMap;
   cursoron;
   GoToXY(gruex,gruey); (* D20 is created and drawn, cursor on grue. *)

   (* Beginning main game loop. *)
   repeat
      coord[gruex,gruey].cindex := 0;
      repeat until KeyPressed;
      cursoroff;
      move := ReadKey; (* Waits until key read before action taken. *)
      GoToXY(1,1);
      ClrEol;
      GoToXY(1,2);
      ClrEol;  (* Clears message buffer upon keypress. *)

      case move of
       '1','b' : PlayerMove(gruex-1,gruey+1);
       '2','j' : PlayerMove(gruex,gruey+1);
       '3','n' : PlayerMove(gruex+1,gruey+1);
       '4','h' : PlayerMove(gruex-1,gruey);
       '6','l' : PlayerMove(gruex+1,gruey);
       '7','y' : PlayerMove(gruex-1,gruey-1);
       '8','k' : PlayerMove(gruex,gruey-1);
       '9','u' : PlayerMove(gruex+1,gruey-1);
       '5','.' : begin
                    GoToXY(1,1);
                    if ShadowWalk = true then
                       write('You are already part of the shadows.')
                    else
                    begin
                       TextColor(DarkGray);
                       write('You lurk in the shadows.');
                       ClrEol;
                       TextColor(DfTxC);
                       turncount := true;
                       lurkcount := lurkcount + 1;
                       if LP < LPMax then
                       begin
                          LP := LP + 1;
                          GoToXY(22,ymax+2);
                          write(LP,'/',LPMax,' ');
                       end;
                    end;
                 end;
       '<' : begin
                if coord[gruex,gruey].UpStairs = true then
                begin
                   if D = 1 then Victory else
                   begin
                      Ascending := true;
                      WipeMap;
                      CaveGenerator;
                      CreaturePopulate;
                      DrawMap;
                      GoToXY(77,ymax+2);
                      write (D,' ');
                      turncount := true;
                   end;
                end;
             end;
       '>' : begin
                if coord[gruex,gruey].DownStairs = true then
                begin
                   Ascending := false;
                   WipeMap;
                   CaveGenerator;
                   CreaturePopulate;
                   DrawMap;
                   GoToXY(77,ymax+2);
                   write (D,' ');
                   turncount := true;
                   retreatcount := retreatcount + 1;
                end;
             end;
     (*  'N' : begin                        --Debug commands--
                WipeMap;
                Ascending := true;
                D := D + 1;
                CaveGenerator;
                CreaturePopulate;
                DrawMap;
                GoToXY(77,ymax+2);
                write (D,' ');
                turncount := true;
             end;
       '+' : if visrange < 40 then visrange := visrange + 1;
       '-' : if visrange > 1 then visrange := visrange - 1;
       'R' : RevealMap;                             *)
       'z','Z' : begin
                    if ShadowWalk = true then
                    begin
                       GoToXY(1,1);
                       write ('You cannot cast spells whilst merged with the shadows.')
                    end
                    else PlayerShadowBall(gruex,gruey);
                 end;
       'a','A' : begin
                    if ShadowWalk = true then
                    begin
                       GoToXY(1,1);
                       write ('You cannot cast spells whilst merged with the shadows.')
                    end
                    else PlayerSense;
                 end;
       's','S' : PlayerShadowWalk;
       '?' : InfoScreen;
       (* No otherwise statement - ignores nonsense commands. *)
      end;

      if ShadowWalk = true then
      begin
         LineofSight2(gruex,gruey,visrange);
         DrawSurround(gruex,gruey,visrange);
         if turncount = true then
         begin
            LP := LP - 1;
            shadowturns := shadowturns + 1;
         end;
         turncount := false;
         GoToXY(22,ymax+2);
         write(LP,'/',LPMax,' ');
         if LP < 1 then PlayerShadowWalk;
      end;

      if turncount = true then  (* Checks turn progression for monster actions. *)
      begin
         turns := turns + 1;
         coord[gruex,gruey].cindex := 1;
         GoToXY(65,ymax+2);
         write(turns,'  '); (* Update turn counter. *)

         (* Move and Spin all creatures.  ypos check to see if creature
            still exists. *)
         for i := 2 to creaturemax do
           if creatureindex[i].ypos > 0 then CreatureMove(i);

         LineofSight2(gruex,gruey,visrange);
         DrawSurround(gruex,gruey,visrange);
            (* Marks and redraws surroundings after move. *)

         if coord[gruex,gruey].Light > 0 then
         (* Death procedure.  Gives death message and marks to exit routine
            with move := Q. *)
         begin
            GoToXY(1,1);
            TextColor(White);
            write ('Agh!  Blinding light!  It burns, it BURNS!!!!');
            TextColor(DfTxC);
            write (' [more]');
            ClrEol;
            ReadKey;
            GoToXY(1,1);
            writeln ('Gurgling with fury you flee back to the darkness from whence you came, nursing');
            write ('your grievous wounds until you feel brave enough to try exploring again.');
            ReadKey;
            move := 'Q';
         end;

         turncount := false; (* Reset turncount for next move. *)
      end; (* End of between turn actions. *)
      GoToXY(gruex,gruey);
      cursoron; (* Sets cursor flashing on grue location - easier to see. *)

   until move = 'Q'; (* Exits command loop.  Q can be triggered by player
                        quitting, death or victory. *)
   cursoroff;
   RevealMap;
   GoToXY(1,1);
   write('Till next lurking....'); (* Default exit message. *)
   ClrEol;
   writeln;
   ClrEol;
   cursoron;
   GoToXY(gruex,gruey);
   ReadKey; (* Wait for key read before exit. *)
   ClrScr;
end. (*** END OF PROGRAM. ***)
