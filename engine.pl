:- use_module(regions,[]).
:- use_module(provinces,[]).
:- use_module(tourist_attractions,[]).
:- use_module(categories,[]).
:- use_module(activities,[]).
:- use_module(seasons,[]).
:- use_module(rules,[]).

thai_paradise :-
  writeln('Welcome to Thai Paradise!'),
  thai_paradise_query('\nPlease select your action:').

thai_paradise_query(Message) :-
  writeln(Message),
  writeln('1: Find tourist attractions based on activity'),
  writeln('2: Find tourist attractions based on opening hours'),
  writeln('3: Find tourist attractions based on age'),
  writeln('4: Find tourist attractions based on cost'),
  writeln('5: Find tourist attractions based on weather'),
  writeln('0: Exit'),
  read(Choice),
  (Choice\=0 ->
    (Choice=1 ->
      writeln('\nPlease select an activity'),
      read(Activity),
      findall([TA,PR,RE,CA,RA],rules:suggest_from_activity(Activity,TA,PR,RE,CA,RA),List),
      length(List,Len),
      random(0,Len,Index),
      nth0(Index,List,[TouristAttraction,Province,Region,Category,Rating]),
      writeln('\nResult found!\n'),
      format('Tourist Attraction: ~w\n',TouristAttraction),
      format('Province: ~w\n',Province),
      format('Region: ~w\n',Region),
      format('Category: ~w\n',Category),
      format('Rating: ~w\n',Rating)
    ; Choice=2 ->
      writeln('\nPlease select from:'),
      writeln('1: Time of day'),
      writeln('2: Opening and closing time'),
      writeln('3: Opening time'),
      writeln('4: Closing time'),
      writeln('5: Currently opened'),
      read(InnerChoice),
      (InnerChoice=1 ->
        writeln('Please select time of day'),
        read(Time),
        findall([TA,PR,RE,CA,RA,OT,CT],rules:suggest_from_opening_hours(Time,TA,PR,RE,CA,RA,OT,CT),List),
        length(List,Len),
        random(0,Len,Index),
        nth0(Index,List,[TouristAttraction,Province,Region,Category,Rating,OpeningTime,ClosingTime]),
        writeln('\nResult found!\n'),
        format('Tourist Attraction: ~w\n',TouristAttraction),
        format('Province: ~w\n',Province),
        format('Region: ~w\n',Region),
        format('Category: ~w\n',Category),
        format('Rating: ~w\n',Rating),
        format('Opening Time: ~w\n',OpeningTime),
        format('Closing Time: ~w\n',ClosingTime)
      ; InnerChoice=2 ->
        writeln('Please select opening time'),
        read(Opening),
        writeln('Please select closing time'),
        read(Closing),
        findall([TA,PR,RE,CA,RA,OT,CT],rules:suggest_from_opening_and_closing_time(Opening,Closing,TA,PR,RE,CA,RA,OT,CT),List),
        length(List,Len),
        random(0,Len,Index),
        nth0(Index,List,[TouristAttraction,Province,Region,Category,Rating,OpeningTime,ClosingTime]),
        writeln('\nResult found!\n'),
        format('Tourist Attraction: ~w\n',TouristAttraction),
        format('Province: ~w\n',Province),
        format('Region: ~w\n',Region),
        format('Category: ~w\n',Category),
        format('Rating: ~w\n',Rating),
        format('Opening Time: ~w\n',OpeningTime),
        format('Closing Time: ~w\n',ClosingTime)
      ; InnerChoice=3 ->
        writeln('Please select opening time'),
        read(Opening),
        findall([TA,PR,RE,CA,RA,OT,CT],rules:suggest_from_opening_time(Opening,TA,PR,RE,CA,RA,OT,CT),List),
        length(List,Len),
        random(0,Len,Index),
        nth0(Index,List,[TouristAttraction,Province,Region,Category,Rating,OpeningTime,ClosingTime]),
        writeln('\nResult found!\n'),
        format('Tourist Attraction: ~w\n',TouristAttraction),
        format('Province: ~w\n',Province),
        format('Region: ~w\n',Region),
        format('Category: ~w\n',Category),
        format('Rating: ~w\n',Rating),
        format('Opening Time: ~w\n',OpeningTime),
        format('Closing Time: ~w\n',ClosingTime)
      ; InnerChoice=4 ->
        writeln('Please select closing time'),
        read(Closing),
        findall([TA,PR,RE,CA,RA,OT,CT],rules:suggest_from_closing_time(Closing,TA,PR,RE,CA,RA,OT,CT),List),
        length(List,Len),
        random(0,Len,Index),
        nth0(Index,List,[TouristAttraction,Province,Region,Category,Rating,OpeningTime,ClosingTime]),
        writeln('\nResult found!\n'),
        format('Tourist Attraction: ~w\n',TouristAttraction),
        format('Province: ~w\n',Province),
        format('Region: ~w\n',Region),
        format('Category: ~w\n',Category),
        format('Rating: ~w\n',Rating),
        format('Opening Time: ~w\n',OpeningTime),
        format('Closing Time: ~w\n',ClosingTime)
      ; InnerChoice=5 ->
        writeln('Calculating current time'),
        findall([TA,PR,RE,CA,RA,OT,CT],rules:suggest_from_opening_now(TA,PR,RE,CA,RA,OT,CT),List),
        length(List,Len),
        random(0,Len,Index),
        nth0(Index,List,[TouristAttraction,Province,Region,Category,Rating,OpeningTime,ClosingTime]),
        writeln('\nResult found!\n'),
        format('Tourist Attraction: ~w\n',TouristAttraction),
        format('Province: ~w\n',Province),
        format('Region: ~w\n',Region),
        format('Category: ~w\n',Category),
        format('Rating: ~w\n',Rating),
        format('Opening Time: ~w\n',OpeningTime),
        format('Closing Time: ~w\n',ClosingTime)
      )
    ),
    writeln('\nQuery finished!'),
    thai_paradise_query('\nPlease select your next action:')
  ;
    write('Thank you for using Thai Paradise!')
  ).
