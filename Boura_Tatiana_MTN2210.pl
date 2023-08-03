%%% EX1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_lower(M) :- length(M,N), is_nxn_and_lower(M,N,0).

is_zero([]).
is_zero([0|Xs]) :- is_zero(Xs).

% Reached diagonal -> check if everything after it is zero
is_zero_after_D([_|Xs],N,N) :- is_zero(Xs).
% Just loop till we reach main diagonal
is_zero_after_D([_|Xs],D,Col) :- is_zero_after_D(Xs,D,Col+1).


is_nxn_and_lower([],_,_).
is_nxn_and_lower([X|Xs],N,Diag) :- length(X,N), 					   % Is of same length
								   is_zero_after_D(X,Diag,0), 		   % Zeros above main diagonal
							  	   is_nxn_and_lower(Xs,N,Diag+1), !.   % Check other rows

%%% EX2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

% The following two work ofc but are not custom
%set_equal(X,Y) :- permutation(X,Y).				  	
%set_equal(X,Y) :- sort(X,Z),sort(Y,Z).		

% Let's implement our own from scratch  

% +List, +Elem, +List
delete([Elem|Xs], Elem, Xs) :- !.
delete([Y, Z|Ys], Elem,  [Y|W]) :- delete([Z|Ys], Elem, W).

set_equal([],[]).
set_equal([X|Xs],Y) :- member(X,Y),         % X exists in second list
					   delete(Y,X,Z),       % .. then delete it in the second
					   set_equal(Xs,Z), !.  % Our goal is to reach two empty lists


loop_over_Y(_,[],[]) :- !.                           % No more elements left in Y, bye
loop_over_Y(X,[X|Ys],Ys) :- !.						 % Found unifiable element (Constant or just List in the same order)
loop_over_Y(X,[Y|Ys],Ys) :- set_equal(X,Y),!.        % Same Set
loop_over_Y(X,[Y|Ys],Ys) :- set_equal_r(X,Y),!.      % For some nested lists

% None of the above rules apply, check next element of Y
% +X,+Y,-Z
loop_over_Y(X,[Y|Ys],[Y|Z]) :- loop_over_Y(X,Ys,Z).

% Goal is to delete all elements from second list since they matched with an element from first one
set_equal_r([],[]). 
set_equal_r([X|Xs],Y) :- loop_over_Y(X,Y,Z), 						% Try and find match for X and delete it from Y 
						 length(Y,N1), length(Z,N2), N1 > N2, 		% Stops earlier if in this "loop" no same found
						 set_equal_r(Xs,Z).  						% Continue until no more elements are in X


   
%%% EX3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

% generates all possible diff combinations of vertex-color
generate([],[],_).		
generate([Vertix-Color|VC],[Vertix|Vs],Colors) :- 
									select(Color, Colors, _ ), % Select one color
									generate(VC,Vs,Colors).    % Do it for the rest Vertices  


checkColor(Color, Vertex, [Vertex-C|_]) :- Color \= C.					% Vertex is found and has different color than the adjustent vertex
checkColor(Color, Vertex, [V-_|Cs]) :- V \= Vertex, 					% If Vertex is NOT found ...
									   checkColor(Color, Vertex, Cs).   % ... try next vertex      

adjNotSameColor(_, [], _ ).
adjNotSameColor(Color, [V|Vs], Coloring ) :- checkColor(Color,V,Coloring),        % For every adjustent vertex check that it is of different color
											 adjNotSameColor(Color,Vs,Coloring).

test([],[],_). 
test([Vertex-AdjEdges|Gs],[Vertex-Color|Cs],Coloring) :- adjNotSameColor(Color,AdjEdges,Coloring),   % Check that all the adjustent edges of corresponding vertex have different color
														 test(Gs,Cs,Coloring).                       % Check next vertices


% Gather all vertices in a list				
vertices([],[]).
vertices([V-_|Gs],[V|Vs]) :- vertices(Gs,Vs).

% Make output in correct format
make_pretty([],[]).
make_pretty([V-C|Cs],[color(V,C)|Gs]) :- make_pretty(Cs,Gs).

color_map(Graph, Colors, Coloring) :- vertices(Graph,Vertices),               	 % Gather all vertices in a list
								  	  generate(ColoringRaw,Vertices,Colors),     % Generate a coloring
								  	  test(Graph,ColoringRaw,ColoringRaw),       % Test if coloring is correct
								  	  make_pretty(ColoringRaw,Coloring).         % Make output in correct format

								
%%% EX4 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

% Enhanced member
in_coordinates([I/J|_],I,J). 
in_coordinates([_/_|Ps],I,J) :- in_coordinates(Ps,I,J).

% Left
move(CurrRow/CurrColumn, CurrRow/NewColumn, _, _, Barriers) :- CurrColumn > 1,                                	% If not on the leftmost side
															   NewColumn is CurrColumn-1,						% Go Left ...
															   \+ in_coordinates(Barriers,CurrRow,NewColumn).   % ... if there is no barrier
% Right
move(CurrRow/CurrColumn, CurrRow/NewColumn, _, N, Barriers) :- CurrColumn < N, 									% If not on the rightmost side
															   NewColumn is CurrColumn+1,						% Go Right ...
															   \+ in_coordinates(Barriers,CurrRow,NewColumn).	% ... if there is no barrier	
% Up
move(CurrRow/CurrColumn, NewRow/CurrColumn, _, _, Barriers) :- CurrRow > 1, 									% If not on the upper side
															   NewRow is CurrRow-1,								% Go Up ...
															   \+ in_coordinates(Barriers,NewRow,CurrColumn).   % ... if there is no barrier		
% Down
move(CurrRow/CurrColumn, NewRow/CurrColumn, M, _, Barriers) :- CurrRow < M, 									% If not on the lowest side
															   NewRow is CurrRow+1,								% Go Down ...
															   \+ in_coordinates(Barriers,NewRow,CurrColumn).	% ... if there is no barrier															  												   
solve(_,_,_,CurrPos,CurrPos,Path,Path,_). 									  									% Reached Destination    									                               
solve(M,N,Barriers,CurrPos,Destination,PathF,Path,SolLength) :- 
											CurrPos \= Destination,               								% Not in destination yet
											move(CurrPos,NextPos,M,N,Barriers),   								% Chose valid move ...
											\+ member(NextPos,Path),              								% ... to go to a state not visited yet
											append(Path,[NextPos],PathUpd),       								% Append to path
											length(PathUpd,L),L =< SolLength,	  								% Solution in depth we want
											solve(M,N,Barriers,NextPos,Destination,PathF,PathUpd,SolLength). 	% Find next path


iter_deep(M,N,_,_,_,SolLength,_) :- SolLength is M*N.													% Stop searching for solutions when depth is equal to number of states in maze
iter_deep(M,N,Barriers,From,To,SolLength,Path) :- solve(M,N,Barriers,From,To,Path,[From],SolLength).	% Solution found in this depth
iter_deep(M,N,Barriers,From,To,SolLength,Path) :- 
							\+ solve(M,N,Barriers,From,To,Path,[From],SolLength),						% Could not find solution in this depth
							MoreLength is SolLength + 1,												% Try deeper
							iter_deep(M,N,Barriers,From,To,MoreLength,Path), !.							% Search for solution deeper


solve_maze(M,N,Barriers,From,To,Path) :- iter_deep(M,N,Barriers,From,To,1,Path),	% Solve maze
										 print_path(M, N,Barriers, Path).			% Print maze and shortest path
										 
% Print maze and shortest path
print_nums(N) :-
	W is N-1, 
	write(' '),write(' '),
	forall(between(1, W, J), (write(J))),
	writeln(N).

print_horizontal(N) :-
    write(' '), write('+'),
    forall(between(1,N, _), write('-')),
    writeln('+').
  
print_path(M, N,Barriers, Path) :-
    print_nums(N), print_horizontal(N),     
    forall(between(1, M, I),
           (write(I),write('|'),
               forall(between(1, N, J),
                          (in_coordinates(Barriers,I,J) -> write('x') ;  (in_coordinates(Path,I,J) -> write('o') ; write('.') ) )),
               writeln('|'))),
    print_horizontal(N).
