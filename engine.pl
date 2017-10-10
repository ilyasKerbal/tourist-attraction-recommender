:- use_module(regions,[]).
:- use_module(provinces,[]).
:- use_module(tourist_attractions,[]).
:- use_module(categories,[]).
:- use_module(activities,[]).
:- use_module(seasons,[]).
:- use_module(rules,[]).

thai_paradise :-
  writeln('Welcome to Thai Paradise!'),
  thai_paradise_query.

thai_paradise_query :-
  writeln('\nPlease select your action:'),
  writeln('\t1: Find tourist attractions based on activity'),
  writeln('\t2: Find tourist attractions based on opening hours'),
  writeln('\t3: Find tourist attractions based on age'),
  writeln('\t4: Find tourist attractions based on cost'),
  writeln('\t5: Find tourist attractions based on weather'),
  writeln('\t0: Exit'),
  read(Choice),
  (Choice\=0 ->
    (Choice=1 ->
      write('You have selected '),
      writeln(Choice),
      writeln('Please select an activity'),
      read(Activity),
      findall([TA,PR,RE,CA,RA],rules:suggest_from_activity(Activity,TA,PR,RE,CA,RA),List),
      length(List,Len),
      random(0,Len,Index),
      nth0(Index,List,[TouristAttraction,Province,Region,Category,Rating]),
      writeln(''),
      writeln('Result found!\n'),
      format('Tourist Attraction: ~w\n',TouristAttraction),
      format('Province: ~w\n',Province),
      format('Region: ~w\n',Region),
      format('Category: ~w\n',Category),
      format('Rating: ~w\n',Rating),
      writeln('\nQuery finished!')
    ;
      writeln('Not a valid action!')
    ),
    thai_paradise_query
  ;
    write('Thank you for using Thai Paradise!')
  ).
