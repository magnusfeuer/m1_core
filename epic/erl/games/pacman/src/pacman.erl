%%
%% PacMan game ported from java applet
%%
%% (C)2000
%% Brian Postma
%% b.postma@hetnet.nl
%% http://www.homepages.hetnet.nl/~brianpostma
%%

-module(pacman).

-compile(export_all).
-import(lists, [map/2]).

-ifdef(debug).
-define(dbg(F, A), io:format((F),(A))).
-else.
-define(dbg(F,A), ok).
-endif.

-define(WALL_LEFT,  16#01).
-define(WALL_ABOVE, 16#02).
-define(WALL_RIGHT, 16#04).
-define(WALL_BELOW, 16#08).
-define(FOOD_SMALL, 16#10).
-define(FOOD_BIG,   16#20).

-define(IS_WALL_LEFT(Z), (((Z) band ?WALL_LEFT) =/= 0)).
-define(IS_WALL_RIGHT(Z), (((Z) band ?WALL_RIGHT) =/= 0)).
-define(IS_WALL_ABOVE(Z), (((Z) band ?WALL_ABOVE) =/= 0)).
-define(IS_WALL_BELOW(Z), (((Z) band ?WALL_BELOW) =/= 0)).

-define(NO_WALL_LEFT(Z), (((Z) band ?WALL_LEFT) == 0)).
-define(NO_WALL_RIGHT(Z), (((Z) band ?WALL_RIGHT) == 0)).
-define(NO_WALL_ABOVE(Z), (((Z) band ?WALL_ABOVE) == 0)).
-define(NO_WALL_BELOW(Z), (((Z) band ?WALL_BELOW) == 0)).

-define(IS_FOOD_SMALL(Z), (((Z) band ?FOOD_SMALL) =/= 0)).
-define(IS_FOOD_BIG(Z), (((Z) band ?FOOD_BIG) =/= 0)).

-record(image,
	{
	  ghost1,
	  ghost2,
	  ghostscared1,
	  ghostscared2,
	  pacman1,
	  pacman2up,
	  pacman2left,
	  pacman2right,
	  pacman2down,
	  pacman3up,
	  pacman3down,
	  pacman3left,
	  pacman3right,
	  pacman4up,
	  pacman4down,
	  pacman4left,
	  pacman4right
	 }).

%% 24 can be divided by 1 2 4 4 6 8 hence the valid speeds
-define(BlockSize, 24).      %% in pixels
-define(NBlocks,   15).      %% size of maze in positions
-define(ScreenDelay, 120).
-define(AnimDelay, 8).
-define(PacAnimDelay, 2).
-define(GhostAnimCount, 2).
-define(PacManAnimCount, 4).
-define(MaxGhosts, 12).
-define(PacManSpeed, 6).
-define(MaxScaredTime, 120).
-define(MinScaredTime, 20).
-define(MaxSpeed, 6).
-define(ValidSpeeds, {1,2,3,4,6,8}).

-define(CoordToPos(X,Y), 
	(((X) div ?BlockSize) + ?NBlocks*((Y) div ?BlockSize)) ).
-define(XToLoc(X), ((X) rem ?BlockSize)).
-define(YToLoc(Y), ((Y) rem ?BlockSize)).
-define(LocToPos(I, J), ((I) + ?NBlocks*(J))).  %% I=column J=row
	       

-define(PosToX(Pos), ( ((Pos) rem ?NBlocks)*?BlockSize)).
-define(PosToY(Pos), ( ((Pos) div ?NBlocks)*?BlockSize)).
-define(PosToCoord(Pos), { ?PosToX(Pos), ?PosToY(Pos)}).

	

-record(ghost,
	{
	  x=0,
	  y=0,
	  dx=1,
	  dy=1,
	  speed=0
	 }).

-record(pacman,
	{
	  x = 0, 
	  y = 0, 
	  dx = 1,
	  dy = 1
	 }).


-record(game,
	{
	  goff,            %% offscreem pixmap
	  win,             %% window
	  width,           %% screem width
	  height,          %% screen height

	  image,           %% #image {}

	  largefont,       %% new Font("Helvetica", Font.BOLD, 24);
	  smallfont,       %% new Font("Helvetica", Font.BOLD, 14);

	  fmsmall,         %% FontMetrics
	  fmlarge,         %% FontMetrics

	  dotcolor      = {255,192,192,0},
	  bigdotcolor   = 192,
	  dbigdotcolor  = -2,
	  mazecolor     = {255,32,192,255},

	  quit          = false,
	  ingame        = false,
	  showtitle     = true,
	  scared        = false,
	  dying         = false,

	  animcount     = 8,
	  pacanimcount  = 2,
	  pacanimdir    = 1,
	  count         = 120,
	  ghostanimpos  = 0,
	  pacmananimpos = 0,
	  nrofghosts    = 6,
	  pacsleft,
	  score,
	  deathcounter,

	  ghosts,         %% [ #ghost {} [ nrofghosts
	  pacman,         %% #pacman 

	  reqdx,
	  reqdy,
	  viewdx,
	  viewdy,

	  scaredcount,
	  scaredtime,

	  level1data,

	  currentspeed=3,     %% 1,2,3,4,5,6
	  maze
	 }).

start() ->
    start(ebackend:default()).

start(Backend) ->
    spawn(?MODULE, run_game, [Backend]).

run_game(Backend) ->
    game_loop(init(Backend)).

game_loop(G) when G#game.quit == true ->
    final(G);
game_loop(G) ->
    T0 = now_milli(),
    G1 = paint(G),
    T1 = now_milli(),
    T = (T0+40)-T1,
    if T =< 0 ->
	    game_loop(G1);
       true ->
	    receive 
	    after T -> 
		    game_loop(check_input(G1))
	    end
    end.

%% poll all key events
check_input(G) ->
    receive
	{eevent,_Win, destroy} ->
	    G#game { quit = true };
	{eevent,_Win,{key_press,Sym,_Mod,_Code}} ->
	    io:format("KeyPress: ~w\n", [Sym]),
	    check_input(key_down(Sym, G));
	{eevent,_Win,{key_release,Sym,_Mod,_Code}} ->
	    io:format("KeyRelease: ~w\n", [Sym]),
	    check_input(key_up(Sym, G))
    after 0 ->
	    G
    end.

key_down(Key, G) when G#game.ingame == false ->
    if Key == $s; Key == $S ->
	    io:format("START game\n"),
	    game_init(G#game { ingame = true });
       Key == $q; Key == $Q ->
	    G#game { quit = true };
       true ->
	    G
    end;
key_down(Key, G) when G#game.ingame == true ->       
    case Key of
	27 ->    G#game { ingame = false };
	$q   ->  G#game { quit = true };
	$Q   ->  G#game { quit = true };
	left ->  G#game { reqdx = -1, reqdy = 0 };
	right -> G#game { reqdx = 1,  reqdy = 0 };
	up ->    G#game { reqdx = 0,  reqdy = -1 };
	down ->  G#game { reqdx = 0,  reqdy = 1 };
	_ -> G
    end;
key_down(_, G) ->
    G.


key_up(Key, G) ->
    if Key == left;
       Key == right;
       Key == down;
       Key == up ->
	    G#game { reqdx = 0, reqdy = 0};
       true ->
	    G
    end.

now_milli() ->
    {M,S,Us} = now(),
    1000*(M*1000000+S)+(Us div 1000).

       

load_image(Image) ->
    File = filename:join(code:priv_dir(pacman), Image),
    [{_DelayTime,EPic}|_] = eimage:file(File),
    EPic.
	    
load_images() ->
    #image {
	     ghost1=load_image("Ghost1.gif"),
	     ghost2=load_image("Ghost2.gif"),
	     ghostscared1=load_image("GhostScared1.gif"),
	     ghostscared2=load_image("GhostScared2.gif"),
	     pacman1=load_image("PacMan1.gif"),
	     pacman2up=load_image("PacMan2up.gif"),
	     pacman3up=load_image("PacMan3up.gif"),
	     pacman4up=load_image("PacMan4up.gif"),
	     pacman2down=load_image("PacMan2down.gif"),
	     pacman3down=load_image("PacMan3down.gif"),
	     pacman4down=load_image("PacMan4down.gif"),
	     pacman2left=load_image("PacMan2left.gif"),
	     pacman3left=load_image("PacMan3left.gif"),
	     pacman4left=load_image("PacMan4left.gif"),
	     pacman2right=load_image("PacMan2right.gif"),
	     pacman3right=load_image("PacMan3right.gif"),
	     pacman4right=load_image("PacMan4right.gif")
	 }.

final(G) ->
    epixmap:destroy(G#game.goff),
    ewindow:destroy(G#game.win),
    ok.

init(Backend) ->
    Width = ?BlockSize*?NBlocks,
    Height = (?BlockSize+1)*?NBlocks,
    {ok,Win}   = ewindow:create(50, 50, Width, Height,[key_press,key_release]),
    ewindow:attach(Win, Backend),
    {ok,Goff}  = epixmap:create(Width, Height),
    epixmap:attach(Goff, Backend),
    %% g.setFont(smallfont);
    %% fmsmall = g.getFontMetrics();
    %% g.setFont(largefont);
    %% fmlarge = g.getFontMetrics();

    G = #game {  win    = Win,
		 goff   = Goff,
		 width  = Width,
		 height = Height,
		 image  = load_images(),
		 maze   = {},
		 %% setBackground(Color.black);
		 %% g=getGraphics();
		 ghosts = [],
		 pacman = #pacman {}
		},
    game_init(G).


game_init(G) ->    
    G1 = G#game { pacsleft   = 3,
		  score      = 0,
		  scaredtime = ?MaxScaredTime
		 },
    G2 = level_init(G1),
    G2#game { nrofghosts   = 6,
	      currentspeed = 3,
	      scaredcount = 0,
	      scaredtime   = ?MaxScaredTime
	     }.

level_init(G) ->
    G1 = G#game { maze = level1data() },
    level_continue(G1).

level_continue(G) ->
    CurrentSpeed = G#game.currentspeed,
    NrOfGhosts   = G#game.nrofghosts,

    Ghost = map(fun(I) ->
			Random = random:uniform(CurrentSpeed),
			Speed  = element(Random,?ValidSpeeds),
			#ghost { y  = 7*?BlockSize,
				 x  = 7*?BlockSize,
				 dy = 0,
				 dx = 2*(I band 1) - 1,
				 speed=Speed
				}
		end, lists:seq(1, NrOfGhosts)),
    Maze0 = G#game.maze,
    Maze1 = set_maze_loc(6, 7, Maze0, ?WALL_ABOVE bor ?WALL_BELOW),
    Maze2 = set_maze_loc(8, 7, Maze1, ?WALL_ABOVE bor ?WALL_BELOW),

    G#game { ghosts = Ghost,
	     maze = Maze2,
	     pacman = #pacman { x = 7*?BlockSize,
				y = 11*?BlockSize,
				dx = 0,
				dy = 0 },
	     reqdx   = 0,
	     reqdy   = 0,
	     viewdx  = -1,
	     viewdy  = 0,
	     dying   = false,
	     scared  = false }.


paint(G) ->
    Goff = G#game.goff,
    egc:set_fill_style(solid),
    egc:set_fill_color({255,0,0,0}),
    egc:set_border_width(0),
    epixmap:draw_rectangle(Goff, 0, 0, G#game.width, G#game.height),
    G1 = draw_maze(G),
    G2 = draw_score(G1),
    G3 = do_anim(G2),
    G4 = if G3#game.ingame == true ->
		 play_game(G3);
	    true ->
		 play_demo(G3)
	 end,
    epixmap:draw(Goff, G4#game.win, 0, 0, 0, 0, G4#game.width, G4#game.height),
    G4.

draw_maze(G) ->
    BSz       = ?BlockSize-1,
    Maze      = G#game.maze,
    Goff      = G#game.goff,
    MazeColor = G#game.mazecolor,
    DotColor  = G#game.dotcolor,
    BigDotColor = G#game.bigdotcolor + G#game.dbigdotcolor,
    DBigDotColor = if BigDotColor =< 64;
		      BigDotColor >= 192 ->
			   -G#game.dbigdotcolor;
		      true ->
			   G#game.dbigdotcolor
		   end,
    each(0, size(Maze)-1,
	 fun(I) ->
		 X = ?PosToX(I),
		 Y = ?PosToY(I),
		 Z = get_maze_pos(I,Maze),
		 egc:set_foreground_color(MazeColor),
		 if ?IS_WALL_LEFT(Z) ->
			 epixmap:draw_line(Goff,X,Y,X,Y+BSz);
		    true -> ok
		 end,
		 if ?IS_WALL_ABOVE(Z) ->
			 epixmap:draw_line(Goff,X,Y,X+BSz,Y);
		    true -> ok
		 end,
		 if ?IS_WALL_RIGHT(Z) ->
			 epixmap:draw_line(Goff,X+BSz,Y,X+BSz,Y+BSz);
		    true -> ok
		 end,
		 if ?IS_WALL_BELOW(Z) ->
			 epixmap:draw_line(Goff,X,Y+BSz,X+BSz,Y+BSz);
		    true -> ok
		 end,
		 if ?IS_FOOD_SMALL(Z) ->
			 egc:set_fill_color(DotColor),
			 epixmap:draw_rectangle(Goff,X+11,Y+11,2,2);
		    true -> ok
		 end,
		 if ?IS_FOOD_BIG(Z) ->
			 egc:set_fill_color({255,224,224-BigDotColor,
					     BigDotColor}),
			 epixmap:draw_rectangle(Goff,X+8,Y+8,8,8);
		    true -> ok
		 end
	 end),
    G#game { bigdotcolor = BigDotColor,
	     dbigdotcolor = DBigDotColor }.

draw_score(G) ->
    Image = G#game.image,
    %% epixmap:draw_string(Goff,
    %%  "Score: "+ integer_to_list(G#game.score),
    %%  (G#game.scrsize div 2)+96, G#game.scrsize+16,
    %%  G#game.smallfont, {0,96,128,255}),
    each(0, G#game.pacsleft-1,
	 fun(I) ->
		 draw_image(G, Image#image.pacman3left,
			    I*28+8, G#game.height-1)
	 end),
    G.


do_anim(G) ->
    AnimCount0 = G#game.animcount - 1,
    AnimCount  = if AnimCount0 =< 0 -> 
			 ?AnimDelay;
		    true ->
			 AnimCount0
		 end,
    GhostAnimPos = if AnimCount0 =< 0  ->
			   GhostAnimPos0 = G#game.ghostanimpos + 1,
			   if GhostAnimPos0 >= ?GhostAnimCount ->  0;
			      true ->
				   GhostAnimPos0
			   end;
		      true ->
			   G#game.ghostanimpos
		   end,
    PacAnimCount0 = G#game.pacanimcount - 1,
    PacAnimCount = if PacAnimCount0 =< 0 ->
			   ?PacAnimDelay;
		      true ->
			   PacAnimCount0
		   end,
    if PacAnimCount0 =< 0 ->
	    PacmanAnimPos = G#game.pacmananimpos+G#game.pacanimdir,
	    PacAnimDir =
		if PacmanAnimPos==PacAnimCount-1;
		   PacmanAnimPos == 0 ->
			-G#game.pacanimdir;
		   true ->
			G#game.pacanimdir
		end,
	    G#game { animcount     = AnimCount,
		     ghostanimpos  = GhostAnimPos,
		     pacanimcount  = PacAnimCount,
		     pacmananimpos = PacmanAnimPos,
		     pacanimdir    = PacAnimDir };
       true ->
	    G#game { animcount     = AnimCount,
		     ghostanimpos  = GhostAnimPos,
		     pacanimcount  = PacAnimCount }
    end.

play_game(G) ->
    case G#game.dying of
	true ->
	    death(G);
	false ->
	    G1 = check_scared(G),
	    G2 = move_pacman(G1),
	    draw_pacman(G2),
	    G3 = move_ghosts(G2),
	    check_maze(G3)
    end.

play_demo(G) ->
    G1 = check_scared(G),
    G2 = move_ghosts(G1),
    show_intro_screen(G2).

check_scared(G) ->
    ScaredCount = G#game.scaredcount-1,
    Scared = if ScaredCount =< 0 ->
		     false;
		true -> G#game.scared
	     end,
    MazeColor = if Scared == true, ScaredCount >= 30 ->
			{255,192,32,255};
		   true ->
			{255,32,192,255}
		end,
    Maze0 = G#game.maze,
    Maze = if Scared == true ->
		   Maze1 = set_maze_loc(6, 7, Maze0,
					?WALL_ABOVE bor ?WALL_BELOW bor 
					?WALL_LEFT),
		   set_maze_loc(8, 7, Maze1,
				?WALL_ABOVE bor ?WALL_BELOW bor ?WALL_RIGHT);
	      true ->
		   Maze1 = set_maze_loc(6, 7, Maze0,
					?WALL_ABOVE bor ?WALL_BELOW),
		   set_maze_loc(8, 7, Maze1,
				?WALL_ABOVE bor ?WALL_BELOW)
	   end,
    G#game { maze = Maze,
	     scared = Scared,
	     scaredcount = ScaredCount,
	     mazecolor = MazeColor
	    }.


check_maze(G) ->
    Maze0 = G#game.maze,
    Max = ?NBlocks*?NBlocks,
    I1 = while(0,
	       fun(I) ->
		       (I < Max) andalso 
		       ((get_maze_pos(I, Maze0) band 
			 (?FOOD_SMALL bor ?FOOD_BIG)) == 0)
	       end,
	       fun(I) -> I + 1 end),
    case (I1 >= Max) of
	true ->
	    G1 = G#game { score = G#game.score + 50 },
	    G2 = draw_score(G1),
	    receive after 3000 -> ok end,
	    NrOfGhosts = if G2#game.nrofghosts < ?MaxGhosts ->
				 G2#game.nrofghosts + 1;
			    true ->
				 G2#game.nrofghosts
			 end,
	    CurrentSpeed = if G2#game.currentspeed < ?MaxSpeed ->
				   G2#game.currentspeed+1;
			      true ->
				   G2#game.currentspeed
			   end,
	    ScaredTime0 = G2#game.scaredtime - 20,
	    ScaredTime  = if ScaredTime0 < ?MinScaredTime ->
				  ?MinScaredTime;
			     true ->
				  ScaredTime0
			  end,
	    G3 = G2#game { nrofghosts = NrOfGhosts,
			   currentspeed = CurrentSpeed,
			   scaredtime   = ScaredTime },
	    level_init(G3);
	false ->
	    G
    end.


death(G) ->
    Image = G#game.image,
    DeathCounter = G#game.deathcounter - 1,
    K = (DeathCounter band 15) div 4,
    case K of
	0 -> draw_pacman(G, Image#image.pacman4up);
	1 -> draw_pacman(G, Image#image.pacman4right);
	2 -> draw_pacman(G, Image#image.pacman4down);
	_ -> draw_pacman(G, Image#image.pacman4left)
    end,
    if DeathCounter == 0 ->
	    PacsLeft = G#game.pacsleft - 1,
	    G1 = if PacsLeft == 0 ->
			 G#game { deathcounter = 0,
				  pacsleft = 0,
				  ingame = false };
		    true ->
			 G#game { deathcounter = 0 }
		 end,
	    level_continue(G1);
       true ->
	    G#game { deathcounter = DeathCounter }
    end.


show_intro_screen(G) ->
    G.

move_pacman(G) ->
    move_pacman(G, G#game.pacman).

%% check if user want a direction change
move_pacman(G, P) ->
    Dx = G#game.reqdx,
    Dy = G#game.reqdy,
    if Dx == -P#pacman.dx, Dy == -P#pacman.dy ->
	    check_pacman(G#game { viewdx = Dx, viewdy = Dy },
			 P#pacman { dx=Dx, dy=Dy });
       true ->
	    check_pacman(G, P)
    end.

%% check if we are in a junction
check_pacman(G, P) ->
    if ?XToLoc(P#pacman.x) == 0, ?YToLoc(P#pacman.y) == 0 ->
	    junction_pacman(G, P);
       true ->
	    forward_pacman(G, P, 0)
    end.
%%
%% pacman in a possible junction
%%
junction_pacman(G, P) ->
    Maze = G#game.maze,
    Pos  = ?CoordToPos(P#pacman.x, P#pacman.y),
    Z = get_maze_pos(Pos, Maze),
    if ?IS_FOOD_SMALL(Z) ->
	    Z1 = Z band 16#0f,
	    Maze1 = set_maze_pos(Pos, Maze, Z1), 
	    Score = G#game.score + 1,
	    turn_pacman(G#game { maze = Maze1, score = Score },
			 P, Z1);
       ?IS_FOOD_BIG(Z) ->
	    Z1 = Z band 16#0f,
	    Maze1 = set_maze_pos(Pos, Maze, Z1),
	    Score = G#game.score + 5,
	    turn_pacman(G#game { maze = Maze1, score = Score,
				 scared = true,
				 scaredcount = G#game.scaredtime },
			 P, Z1);
       true ->
	    turn_pacman(G, P, Z)
    end.
    
    
%%% Check if pacman wants to turn
turn_pacman(G,P,Z) ->
    Dx = G#game.reqdx,
    Dy = G#game.reqdy,
    if Dx== 0, Dy== 0 -> block_pacman(G,P,Z);
       Dx==-1, Dy== 0, ?IS_WALL_LEFT(Z)  -> block_pacman(G,P,Z);
       Dx== 1, Dy== 0, ?IS_WALL_RIGHT(Z) -> block_pacman(G,P,Z);
       Dx== 0, Dy==-1, ?IS_WALL_ABOVE(Z) -> block_pacman(G,P,Z);
       Dx== 0, Dy== 1, ?IS_WALL_BELOW(Z) -> block_pacman(G,P,Z);
       true ->
	    block_pacman(G#game { viewdx=Dx, viewdy=Dy },
			 P#pacman { dx = Dx, dy = Dy }, Z)
    end.
    
%% stop pacman from going into wall.
block_pacman(G,P,Z)->
    Dx = P#pacman.dx,
    Dy = P#pacman.dy,
    if Dx==-1, Dy==0, ?IS_WALL_LEFT(Z) ->
	    forward_pacman(G, P#pacman { dx=0, dy=0 }, Z);
       Dx==1, Dy==0, ?IS_WALL_RIGHT(Z) ->
	    forward_pacman(G, P#pacman { dx=0, dy=0 }, Z);
       Dx==0, Dy==-1, ?IS_WALL_ABOVE(Z) ->
	    forward_pacman(G, P#pacman { dx=0, dy=0 }, Z);
       Dx==0, Dy==1, ?IS_WALL_BELOW(Z) ->
	    forward_pacman(G, P#pacman { dx=0, dy=0 }, Z);
       true ->
	    forward_pacman(G, P, Z)
    end.

%% move pacman accoring to direction and speed	
forward_pacman(G,P,_Z) ->
    X = P#pacman.x + ?PacManSpeed*P#pacman.dx,
    Y = P#pacman.y + ?PacManSpeed*P#pacman.dy,
    G#game { pacman = P#pacman { x = X, y = Y }}.

draw_pacman(G) ->
    Image = G#game.image,
    if G#game.viewdx == -1 ->
	    case G#game.pacmananimpos of
		1 -> draw_pacman(G, Image#image.pacman2left);
		2 -> draw_pacman(G, Image#image.pacman3left);
		3 -> draw_pacman(G, Image#image.pacman4left);
		_ -> draw_pacman(G, Image#image.pacman1)
	    end;
       G#game.viewdx == 1 ->
	    case G#game.pacmananimpos of
		1 -> draw_pacman(G, Image#image.pacman2right);
		2 -> draw_pacman(G, Image#image.pacman3right);
		3 -> draw_pacman(G, Image#image.pacman4right);
		_ -> draw_pacman(G, Image#image.pacman1)
	    end;
       G#game.viewdy == -1 ->
	    case G#game.pacmananimpos of
		1 -> draw_pacman(G, Image#image.pacman2up);
		2 -> draw_pacman(G, Image#image.pacman3up);
		3 -> draw_pacman(G, Image#image.pacman4up);
		_ -> draw_pacman(G, Image#image.pacman1)
	    end;
       true -> 
	    case G#game.pacmananimpos of
		1 -> draw_pacman(G, Image#image.pacman2down);
		2 -> draw_pacman(G, Image#image.pacman3down);
		3 -> draw_pacman(G, Image#image.pacman4down);
		_ -> draw_pacman(G, Image#image.pacman1)
	    end
    end.

draw_pacman(G, Image) ->
    PacMan = G#game.pacman,
    draw_image(G, Image, PacMan#pacman.x+1, PacMan#pacman.y+1).

draw_ghost(G, H) ->
    X = H#ghost.x,
    Y = H#ghost.y,
    Image = G#game.image,
    if G#game.ghostanimpos == 0, G#game.scared == false ->
	    draw_image(G, Image#image.ghost1, X+1, Y+1);
       G#game.ghostanimpos == 1, G#game.scared == false ->
	    draw_image(G, Image#image.ghost2, X+1, Y+1);
       G#game.ghostanimpos == 0, G#game.scared == true ->
	    draw_image(G, Image#image.ghostscared1, X+1, Y+1);
       G#game.ghostanimpos == 1, G#game.scared == true ->
	    draw_image(G, Image#image.ghostscared2, X+1, Y+1);
       true ->
	    ok
    end.


draw_image(G, Image, X, Y) ->
    Goff = G#game.goff,
    epixmap:copy_area(Image, Goff, 0, 0, X, Y, 22, 22).

    
move_ghosts(G) ->
    move_ghosts(G, G#game.ghosts, []).

move_ghosts(G, [H|Hs], Acc) ->
    {G1,H1} = move_ghost(G,H),
    move_ghosts(G1, Hs, [H1|Acc]);
move_ghosts(G, [], Acc) ->
    G#game { ghosts = lists:reverse(Acc) }.


move_ghost(G, H) ->
    Maze = G#game.maze,
    {Dx,Dy} =
	if ?XToLoc(H#ghost.x) == 0, ?YToLoc(H#ghost.y) == 0 ->
		Pos = ?CoordToPos(H#ghost.x,H#ghost.y),
		Z = get_maze_pos(Pos, Maze),
		LDxy =
		    if H#ghost.dx =/= 1, ?NO_WALL_LEFT(Z) ->
			    [{-1, 0}];
		       true ->
			    []
		    end ++
		    if H#ghost.dx =/= -1, ?NO_WALL_RIGHT(Z) ->
			    [{1, 0}];
		       true ->
			    []
		    end ++
		    if H#ghost.dy =/= 1, ?NO_WALL_ABOVE(Z) ->
			    [{0, -1}];
		       true ->
			    []
		    end ++
		    if  H#ghost.dy =/= -1, ?NO_WALL_BELOW(Z) ->
			    [{0, 1}];
		       true ->
			    []
		    end,
		Count = length(LDxy),
		if Count == 0 ->
			if Z band 16#0F == 16#0F -> {0,0};
			   true -> { -H#ghost.dx, -H#ghost.dy }
			end;
		   true ->
			case random:uniform(Count) of
			    1 -> [Dxy|_] = LDxy, Dxy;
			    2 -> [_,Dxy|_] = LDxy, Dxy;
			    3 -> [_,_,Dxy|_] = LDxy, Dxy;
			    4 -> [_,_,_,Dxy] = LDxy, Dxy
			end
		end;
	   true ->
		{ H#ghost.dx, H#ghost.dy }
	end,
    Speed = H#ghost.speed,
    X = H#ghost.x + Dx*Speed,
    Y = H#ghost.y + Dy*Speed,
    H1 = H#ghost { x = X, y = Y, dx = Dx, dy = Dy },
    draw_ghost(G, H1),
    P = G#game.pacman,
    if P#pacman.x > X-12, P#pacman.x < X+12,
       P#pacman.y > Y-12, P#pacman.y < Y+12,
       G#game.ingame == true ->
	    if G#game.scared == true ->
		    G1 = G#game { score = G#game.score + 10 },
		    H2 = H1#ghost { x = 7*?BlockSize,
				    y = 7*?BlockSize },
		    {G1,H2};
	       true ->
		    G1 = G#game { dying = true,
				  deathcounter = 64 },
		    {G1, H1}
	    end;
       true ->
	    {G, H1}
    end.

%%
%% Maze stuff
%%
level1data() -> 
    { 
	    19,26,26,22, 9,12,19,26,22, 9,12,19,26,26,22,
	    37,11,14,17,26,26,20,15,17,26,26,20,11,14,37,
	    17,26,26,20,11, 6,17,26,20, 3,14,17,26,26,20,
	    21, 3, 6,25,22, 5,21, 7,21, 5,19,28, 3, 6,21,
	    21, 9, 8,14,21,13,21, 5,21,13,21,11, 8,12,21,
	    25,18,26,18,24,18,28, 5,25,18,24,18,26,18,28,
	    6,21, 7,21, 7,21,11, 8,14,21, 7,21, 7,21,03,
	    4,21, 5,21, 5,21,11,10,14,21, 5,21, 5,21, 1,
	    12,21,13,21,13,21,11,10,14,21,13,21,13,21, 9,
	    19,24,26,24,26,16,26,18,26,16,26,24,26,24,22,
	    21, 3, 2, 2, 6,21,15,21,15,21, 3, 2, 2,06,21,
	    21, 9, 8, 8, 4,17,26, 8,26,20, 1, 8, 8,12,21,
	    17,26,26,22,13,21,11, 2,14,21,13,19,26,26,20,
	    37,11,14,17,26,24,22,13,19,24,26,20,11,14,37,
	    25,26,26,28, 3, 6,25,26,28, 3, 6,25,26,26,28  
	   }.

%% get maze data with either straigh pos, location or coordinate
get_maze_pos(Pos, Maze) ->
    element(Pos+1, Maze).

get_maze_xy(X, Y, Maze) ->
    element(?CoordToPos(X, Y)+1, Maze).

get_maze_loc(I, J, Maze) ->
    element(?LocToPos(I, J)+1, Maze).

%% set maze data with either straigh pos, location or coordinate

set_maze_pos(Pos, Maze, Code) ->
    setelement(Pos+1, Maze, Code).

set_maze_xy(X, Y, Maze, Code) ->
    setelement(?CoordToPos(X,Y)+1, Maze, Code).    

set_maze_loc(I, J, Maze, Code) ->
    setelement(?LocToPos(I,J)+1, Maze, Code).
		    
%%
%% Utilities
%%

each(I, N, Fun) ->
    if I > N -> ok;
       true ->
	    Fun(I),
	    each(I+1, N, Fun)
    end.

each(I, Step, N, Fun) ->
    if I > N -> ok;
       true ->
	    Fun(I),
	    each(I+Step, Step, N, Fun)
    end.


while(Acc, Done, Body) ->
    case Done(Acc) of
	true ->
	    Acc1 = Body(Acc),
	    while(Acc1, Done, Body);
	false -> Acc
    end.
