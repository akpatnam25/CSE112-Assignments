% $Id: functions.pl,v 1.3 2016-11-08 15:04:13-08 - - $
% Sasank Madineni (smadinen@ucsc.edu)
% Henry Nguyen (hnguye87@ucsc.edu)

not( X) :- X, !, fail.
not( _).

% taken from professor in examples
haversine_radians( Lat1, Lon1, Lat2, Lon2, Distance ) :-
   Dlon is Lon2 - Lon1,
   Dlat is Lat2 - Lat1,
   A is sin( Dlat / 2 ) ** 2
      + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
   Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
   Distance is Dist * 3961.

%converts degree minutes to radians
degmin_to_rad(degmin(Degrees, Minutes), Rad) :-
	Rad is ((Degrees + (Minutes/60)) * (pi/180)).

%find distance between 2 airports
calc_distance( Port1, Port2, Distance) :-
	airport( Port1, _, Lat1, Lon1),
	airport( Port2, _, Lat2, Lon2),
	degmin_to_rad( Lat1, Lat1_rad),
	degmin_to_rad( Lon1, Lon1_rad),
	degmin_to_rad( Lat2, Lat2_rad),
	degmin_to_rad( Lon2, Lon2_rad),
	haversine_radians( Lat1_rad, Lon1_rad, Lat2_rad, Lon2_rad, Distance).

%find time to reach destination
arrival_time( Distance, time( Hours, Mins), ArrivalTime) :-
    TravelTime is (Distance / 500),
    ArrivalTime is (Hours + (Mins/60) + TravelTime).    

%prints time, pads hours and mins 
print_time(Hour, Mins) :-
	((Hour < 10) -> write(0), write(Hour); write(Hour)),
	write(':'),
        ((Mins < 10) -> write(0), write(Mins); write(Mins)).

fly( Node, Node) :-
	!, fail.
	
fly( Node, End) :-
        ((atom(Node), atom(End)) -> 
        flight( Node, Next, Time),	
	listpath( Node, End, [flight( Node, Next, Time)], List, 0),
	writepath( List);
	!, fail).

writepath( []) :-
	nl.

writepath( [flight( Departure, Destination, time( Hour, Mins))| Tail]) :-
	airport( Departure, Port1, _, _),
	airport( Destination, Port2, _, _),
        string_upper( Departure, Upper),
	string_upper( Destination, Upper2),	
	write( 'depart '), write(Upper), write( ' '),
        write(Port1), write( ' '), print_time(Hour, Mins), nl,
	calc_distance( Departure, Destination, Distance),
	arrival_time(Distance, time(Hour, Mins), ArrivalTime),
	DestHour is (integer(float_integer_part(ArrivalTime))),
	DestMin is (integer(float_fractional_part(ArrivalTime) * 60)),
	write( 'arrive '), write(Upper2), write( ' '),
        write(Port2), write( ' '), 
	print_time(DestHour, DestMin), nl,	
	writepath( Tail).

%fly(sea, sfo).

%reached destination in search, stop search
listpath( Node, Node, _, _, _).

%find distination through all flight nodes	
listpath( Node, End, Tried, [flight( Node, Next, time( H, M))|List], CurTime) :-
    flight( Node, Next, time( H, M)), 
    %time of flight in hours
    TakeOff is (H + (M/60)),
    %takes into account 30 minute transfer
    not( (CurTime + 0.5) > TakeOff),
    %write(Node), print_time( H, M), nl, 
    not( member( flight( Next, Next2, time(H2, M2)), Tried)),
    calc_distance( Node, Next, Distance),
    arrival_time(Distance, time(H, M), ArrivalTime),
    %write(Distance), write(' '), write(ArrivalTime), nl,
    listpath( Next, End, [flight( Next, Next2, time(H2, M2))|Tried], List, ArrivalTime).

